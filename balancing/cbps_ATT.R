# Covariate Balancing / Calibrated Propensity Score
# This simple script uses Base R's `optim` to solve the balancing calibration loss described in
# Wu et al. (2023): https://doi.org/10.1126/sciadv.adi4123 (appendix).
# More details are in chapter 7 of http://web.stanford.edu/~swager/stats361.pdf and https://arxiv.org/abs/1601.05890, https://arxiv.org/abs/1710.08074
#
# Input:
#   X: nXp numeric covariate matrix (this script is intended as a "zero-dependency Base R" solution for balancing with n in the low 100 000s and p in the low 1000s.)
#   W: binary treatment assignment vector
#   intercept: whether to include an intercept in logistic model, default is TRUE.
#   theta.init: optional starting values for theta.
#   method: method argument passed to `optim`.
#   control: control argument passed to `optim`.
#   lambda: optional ridge penalty (remember to scale X's appropriately if used)
# Output:
#   theta.hat: estimated thetas
#   weights.0: IPW weights for control
#   weights.1: IPW weights for treated
#   convergence: optim's convergence status. 0=success.
#   balance condition: the LHS and RHS of the balance condition.
#' Covariate Balancing / Calibrated Propensity Score
#'
#' This script implements ATT covariate balancing using Base R's optim, following Wu et al. (2023) and related literature.
#' Input: X (covariate matrix), W (treatment vector), and options for intercept, starting values, optimizer, and penalty.
#' Output: Estimated weights and diagnostics for ATT balancing.

cbps_att = function(X, W, intercept = TRUE, theta.init = NULL, method = "BFGS", control = list(), lambda = NULL) {
  #' Fit ATT covariate balancing weights using calibration loss.
  #' @param X Numeric covariate matrix.
  #' @param W Binary treatment assignment vector.
  #' @param intercept Whether to include intercept in logistic model.
  #' @param theta.init Optional starting values for theta.
  #' @param method Optimizer method for optim.
  #' @param control Control list for optim.
  #' @param lambda Optional ridge penalty vector.
  #' @return List with estimated thetas, weights, convergence status, and balance diagnostics.
  if (!all(W %in% c(0, 1))) {
    stop("W should be a binary vector.")
  }
  if (!is.numeric(X) || nrow(X) != length(W) || is.null(dim(X)) || anyNA(X)) {
    stop("X should be a numeric matrix with nrows = length(W).")
  }

  # ATT balance constraint and objective:
  # 1/n1 \sum_{Wi = 0} exp(theta * X) Xi = 1/n1 \sum_{Wi=1} Xi
  # which gives loss: (sum_{W0} exp(X theta) - sum_{W1} X theta)/n1 + lambda||theta||^2
  .objective = function(theta, X, W0.idx, W1.idx, lambda) {
    Xtheta <- as.vector(X %*% theta)
    (sum(exp(Xtheta[W0.idx])) - sum(Xtheta[W1.idx])) / length(W1.idx) + sum(lambda * theta^2)
  }

  .objective.gradient = function(theta, X, Xsum1, W0.idx, n1, lambda) {
    Xtheta <- as.vector(X %*% theta)
    exp_vec <- exp(Xtheta[W0.idx])
    X0 <- X[W0.idx, , drop = FALSE]
    (colSums(X0 * exp_vec) - Xsum1) / n1 + 2 * lambda * theta
  }
  if (is.null(lambda)) {
    lambda = rep(0, ncol(X))
  }
  if (intercept) {
    X = cbind(1, X)
    lambda = c(0, lambda)
  }

  # ensure optimizer control has reasonable defaults for hard problems
  if (is.null(control$maxit)) control$maxit <- 5000
  if (is.null(control$reltol)) control$reltol <- 1e-10
  if (is.null(control$trace)) control$trace <- 0

  W1.idx = which(W == 1)
  W0.idx = which(W == 0)
  if (is.null(theta.init)) {
    # Use "naive" logistic starting values
    idx.small = c(W1.idx, sample(W0.idx, length(W1.idx)))
    glm = glm.fit(X[idx.small, ], W[idx.small], family = binomial())
    theta.init = glm$coefficients
    # update the intercept, (7) in https://gking.harvard.edu/files/0s.pdf
    if (intercept) {
      pi = mean(W)
      theta.init[1] = theta.init[1] - log((1 - pi) / pi) * length(idx.small) / sum(W)
    }
    # If glm starting values failed (NA/Inf) or length mismatch, fall back to zeros
    if (is.null(theta.init) || length(theta.init) != ncol(X) || any(!is.finite(theta.init))) {
      warning('glm starting values invalid; using zero start')
      theta.init <- rep(0, ncol(X))
    }
  }

  # X0 = X[W0.idx, , drop = FALSE]
  Xsum1 = colSums(X[W1.idx, , drop = FALSE])
  n1 <- length(W1.idx)

  # Helper to validate a fit
  validate_fit <- function(res_fit, X, W0.idx) {
    if (is.null(res_fit)) return(FALSE)
    if (!is.list(res_fit)) return(FALSE)
    if (is.null(res_fit$par)) return(FALSE)
    # compute weights and check finiteness
    theta_try <- res_fit$par
    w_try <- tryCatch(as.vector(exp(as.vector(X %*% theta_try))), error = function(e) NULL)
    if (is.null(w_try)) return(FALSE)
    w_ctrl <- w_try[W0.idx]
    if (any(!is.finite(w_ctrl)) || all(w_ctrl == 0)) return(FALSE)
    return(TRUE)
  }

  # Prepare fallback sequences for methods and lambdas
  fallback_methods <- unique(c(method, 'BFGS', 'CG', 'Nelder-Mead'))
  # if lambda provided as vector:
  # - if its length equals ncol(X): treat it as a per-covariate penalty vector (use directly)
  # - otherwise: fall back to using unique scalar candidates from the provided values
  # When scalar, expand a sensible ladder around it.
  if (length(lambda) > 1) {
    if (length(lambda) == ncol(X)) {
      lambda_seq <- list(as.numeric(lambda))
    } else {
      lambda_seq <- as.list(unique(as.numeric(lambda)))
    }
  } else {
    lam0 <- as.numeric(lambda[1])
    if (is.na(lam0) || lam0 <= 0) lam0 <- 0
    lambda_seq <- as.list(unique(c(lam0, 1e-6, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10)))
  }

  attempts <- list()
  res <- NULL
  for (lam_try in lambda_seq) {
    # lam_try may be a scalar (single penalty) or a full per-covariate vector.
    if (is.numeric(lam_try) && length(lam_try) == ncol(X)) {
      lambda_vec_try <- as.numeric(lam_try)
    } else if (is.numeric(lam_try) && length(lam_try) == 1) {
      lambda_vec_try <- rep(as.numeric(lam_try), ncol(X))
    } else {
      lam_try_num <- as.numeric(lam_try)
      if (length(lam_try_num) == ncol(X)) {
        lambda_vec_try <- lam_try_num
      } else {
        lambda_vec_try <- rep(lam_try_num[1], ncol(X))
      }
    }
    for (mtry in fallback_methods) {
      attempt <- list(lambda = lam_try, method = mtry, ok = FALSE, err = NULL)
      res_try <- tryCatch({
        optim(
          par = theta.init,
          fn = function(x) .objective(x, X, W0.idx, W1.idx, lambda_vec_try),
          gr = function(x) .objective.gradient(x, X, Xsum1, W0.idx, n1, lambda_vec_try),
          method = mtry,
          lower = -Inf,
          upper = Inf,
          control = control,
          hessian = FALSE
        )
      }, error = function(e) {
        attempt$err <- e$message
        NULL
      })
      if (!is.null(res_try)) {
        attempt$ok <- TRUE
        attempt$conv <- res_try$convergence
      }
      attempts[[length(attempts) + 1]] <- attempt
      if (!is.null(res_try) && res_try$convergence == 0 && validate_fit(res_try, X, W0.idx)) {
        res <- res_try
        lambda <- lambda_vec_try
        method <- mtry
        break
      }
    }
    if (!is.null(res)) break
  }
  # if no valid fit found, keep last attempted res_try if any, else error
  if (is.null(res)) {
    warning('cbps_att: no valid fit found after fallback attempts; returning last attempt (may be NULL)')
    # pick last successful attempt even if non-converged
    last_ok <- NULL
    for (i in seq_along(attempts)) {
      if (isTRUE(attempts[[i]]$ok)) last_ok <- attempts[[i]]
    }
    if (!is.null(last_ok)) {
      # try to re-run to get res object
      lambda_vec_try <- rep(last_ok$lambda, ncol(X))
      res <- tryCatch(optim(par = theta.init,
                           fn = function(x) .objective(x, X, W0.idx, W1.idx, lambda_vec_try),
                           gr = function(x) .objective.gradient(x, X, Xsum1, W0.idx, n1, lambda_vec_try),
                           method = last_ok$method, control = control, hessian = FALSE), error = function(e) NULL)
    }
  }
  # attach attempts trace
  attr(res, 'fallback_attempts') <- attempts

  theta.hat = res$par
  weights.0 = as.vector(exp(as.vector(X %*% theta.hat)))
  # Normalize ATT control weights so controls sum to n1 (sum of treated)
  sum_ctrl_w <- sum(weights.0[W0.idx], na.rm = TRUE)
  if (!is.na(sum_ctrl_w) && sum_ctrl_w > 0) {
    weights.0 <- weights.0 * (length(W1.idx) / sum_ctrl_w)
  }
  LHS = colSums((1 - W) * X * weights.0) / sum(W == 1)
  RHS = colSums(W * X) / sum(W==1)

  sd.W1 = apply(X[W1.idx, ], 2, sd)
  sd.W1[sd.W1 == 0] = 1
  sd.W = apply(X, 2, sd)
  sd.W[sd.W == 0] = 1
  mean.diff = colMeans(X[W1.idx, ]) -
    apply(X[W0.idx, ], 2, function(x) weighted.mean(x, weights.0[W0.idx]))
  balance.std = mean.diff / sd.W1
  balance.std.pre = (colMeans(X[W1.idx, ]) - colMeans(X[W0.idx, ])) / sd.W1
  balance.std.all = mean.diff / sd.W
  balance.std.pre.all = (colMeans(X[W1.idx, ]) - colMeans(X[W0.idx, ])) / sd.W


  list(
    theta.hat = theta.hat,
    weights.0 = weights.0,
    weights.1 = rep(1, nrow(X)),
    convergence = res$convergence,
    balance.condition = cbind(LHS = LHS, RHS = RHS),
    balance.std = if (intercept) balance.std[-1] else balance.std,
    balance.std.pre = if (intercept) balance.std.pre[-1] else balance.std.pre,
    balance.std.all = if (intercept) balance.std.all[-1] else balance.std.all,
    balance.std.pre.all = if (intercept) balance.std.pre.all[-1] else balance.std.pre.all,
    optim.control = control
  )
}


if (FALSE) {
  n = 15000
  p = 40
  X = matrix(rnorm(n*p), n, p)
  W = rbinom(n, 1, 1 / (1 + exp(2.5 - X[, 1])))

  system.time(res <- cbps_att(X, W, control = list(trace=10, maxit=5000)))

  head(res$balance.condition)
  plot(res$balance.std)
  abline(h = 0)
}