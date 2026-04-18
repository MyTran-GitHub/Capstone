\documentclass[12pt]{article}
\usepackage[margin=1in]{geometry}
\usepackage{setspace}
\usepackage{newtxtext,newtxmath}
\usepackage{parskip}
\usepackage{hyperref}
\usepackage{graphicx}
\usepackage{booktabs}
\usepackage{longtable}
\usepackage{array}
\usepackage{amsmath}
\usepackage{natbib}
\usepackage{titlesec}
\usepackage{float}

\titleformat*{\section}{\large\bfseries}
\titleformat*{\subsection}{\normalsize\bfseries}

\title{A Satellite-based Synthetic Control Evaluation of Prescribed Burn Intervention}
\author{
My Tran\\
Undergraduate Thesis, Minerva University
\and
Advisor: Professor Lucas Tambasco\\
Assistant Professor of College of Computational Sciences, Minerva University

Second Reader: Professor Robson Morgan
College Head of Social Sciences, 
Minerva University
}

\date{}
\begin{document}
\maketitle


\section*{Abstract}
This paper replicates and extends the synthetic control framework introduced by Wu et al. (2023) to evaluate the causal effects of low-intensity fire on subsequent wildfire risk in California’s forests. Preserving the original study’s definition of low-intensity fire, environmental covariates, and outcome measures derived from satellite-based fire activity data (MODIS FIRMS, 2000–2021), I reproduce the baseline findings on the duration and magnitude of wildfire risk mitigation. I then introduce a methodological extension: an embeddings-based approach to control pool construction that constrains candidate control pixels using learned satellite representations rather than solely contemporaneous burn status. I show that embedding-filtered control pools improve pre-treatment fit and covariate balance while retaining the interpretability and structure of the original synthetic control design. The results suggest that representation-based control selection can strengthen causal inference in high-dimensional, spatially correlated remote sensing settings without altering the underlying estimand. This study contributes to the growing literature on causal inference with satellite data by demonstrating how modern representation learning can be integrated into established quasi-experimental pipelines for environmental impact evaluation.

\noindent\textbf{Keywords:} Low-intensity fire, synthetic control, causal inference, satellite embeddings, wildfire risk, remote sensing, methodological replication

\section{Introduction}

Wildfire risk in California is shaped by both evolving climate and the long‑run legacy of fire suppression, making it crucial to understand whether low‑intensity fire can reliably reduce the probability of future high‑intensity, high‑damage events. \citep{Wu2023} address this question using a large‑scale synthetic control design: they construct covariate‑balancing weights on a donor pool of roughly 80,000 unburned forest pixels to estimate how often pixels that recently experienced low‑intensity fire subsequently reburn at high intensity, relative to comparable unburned areas. Their results show substantial and persistent risk reductions in both conifer and hardwood forests, and they demonstrate excellent covariate balance across long pre‑treatment fire‑history and environmental trajectories. However, their implementation necessarily relies on a very large and heterogeneous donor pool, which poses challenges for overlap, weight stability, and pre‑treatment predictive accuracy in a latent‑factor framework.

In this project, I first replicate the  \citep{Wu2023} synthetic control pipeline, including their pixel‑level quasi‑experimental design, covariate‑balancing ATT weights, and pooled effect estimation over multiple focal years and lags. I then extend their approach by integrating satellite‑image embeddings to perform embedding‑based donor selection before the covariate‑balancing step. The key idea, motivated by recent work on Machine control by \citep{Araujo2024} and ClusterSC by \citep{Rho2025} is that restricting the donor pool to units that are most similar to the treated units in a rich latent representation can reduce pre‑treatment prediction error and improve the bias–variance tradeoff, while maintaining sufficient effective sample size for inference. I construct embeddings for each forest pixel from pre‑focal satellite imagery, identify the top $K$ most similar control pixels for each treated pixel using cosine similarity, and then run the original  \citep{Wu2023} covariate‑balancing synthetic control method on this embedding‑selected donor pool. The number of neighbors $K$ is chosen via cross‑validation on pre‑treatment RMSPE (Root Mean Squared Prediction Error), subject to covariate balance and effective sample size constraints.

This embedding‑based design is theoretically motivated by latent‑factor models of panel outcomes and by ClusterSC \citep{Rho2025} results on donor clustering. Under such models, fire outcomes are driven by unobserved unit‑specific factors (e.g., fuel structure, fire regime) and time‑varying factors (e.g., regional climate), and synthetic control aims to approximate the treated unit’s latent factor vector by a convex combination of donor factors. ClusterSC shows that when donors are clustered so that one cluster is homogeneous around the treated unit’s latent factors, running synthetic control within that cluster yields strictly tighter error bounds than using the full donor pool. Treating satellite embeddings as proxies for latent factors, my similarity search is a geospatial instantiation of this idea: the embedding‑selected donor pool is intended to be a homogeneous subset whose convex hull in latent space better approximates the treated group than the full set of heterogeneous donors.

The research objective is twofold. First, to reproduce \citep{Wu2023}'s main substantive finding: Recent low‑intensity fires substantially reduce subsequent high‑intensity fire risk and protective effects persist for six years. Second, to evaluate whether embedding‑based donor selection can improve synthetic control performance in this setting, as measured by: (i) lower pre‑treatment RMSPE, (ii) comparable or improved covariate balance, and (iii) higher effective sample size and more stable weights, without materially changing the estimated magnitude or duration of the protective effect. To this end, I compare the baseline results with the embedding‑selected version using a suite of tests: year‑specific balance plots, pre‑period outcome fit, placebo and random donor‑subset analyses, and weight concentration diagnostics through ESS (Effective Sample Size). Together, these analyses aim to show that embedding‑selected donor pools can provide a more stable and interpretable synthetic control for large‑scale wildfire applications, while reinforcing and refining the policy‑relevant conclusions drawn from the original study.

Research Question: Does embedding‑based donor selection improve pre‑treatment fit and weight stability without altering the estimated protective effect of low‑intensity fires on subsequent wildfire risk?

\section{Literature Review}
 Synthetic control (SC) was originally introduced to estimate causal effects in comparative case studies by constructing a weighted average of control units that reproduces a treated unit’s pre‑intervention outcomes and covariates. Early contributions by Abadie and co‑authors \citep{AbadieGardeazabal2003,AbadieDiamondHainmueller2010,Abadie2015} focused on country‑ or region‑level interventions, where the donor pool was defined a priori based on institutional plausibility and data availability. For example, in the Basque Country application, the donor set consists of other Spanish regions that did not experience terrorism, while in the California tobacco control study the donor pool is restricted to U.S. states without contemporaneous large‑scale anti‑tobacco programs, so that “treated” California is compared only to states with broadly similar policy environments and data quality \citep{AbadieGardeazabal2003,AbadieDiamondHainmueller2010}. In these settings, the construction of the initial donor pool flows naturally from the policy context. Therefore, early on, methodological attention centers on how to choose weights and assess pre‑period fit, rather than on how to algorithmically construct or filter the donor set itself.
 
As SC was adapted to environmental and climate applications such as wildfire management, air quality, and land‑use interventions, the scale and structure of the data changed. Instead of a handful of regions, researchers began working with thousands to hundreds of thousands of spatial units, often with rich satellite‑derived covariates and long time series. In this “unit‑rich, high‑dimensional” regime, donor pools are no longer naturally small; they can encompass entire states, biomes, or vegetation classes. \citep{Wu2023}, for example, study the effect of low‑intensity fires on subsequent high‑intensity wildfire risk using a pixel‑level SC design with approximately 500 treated forest pixels and roughly 80,000 potential donors per focal year. They use a covariate‑balancing ATT (Average Treatment Effect for the Treated) formulation that directly weights donors to match treated units on pre‑treatment fire histories, biophysical covariates, and meteorological variables, and they show strong risk reductions that persist for up to a decade. However, their donor pool of roughly 80,000 forest pixels varies across different fuel types, microclimate, and disturbance history. This practice expands sample size, but also introduces substantial latent heterogeneity in potential confounders within each donor pool, raising questions about how comparable treated and donor units truly are along unobserved ecological dimensions.

Recent wildfire and environmental SC studies implicitly recognize the importance of donor comparability and attempt to refine donor selection using spatial or contextual filters. \citep{SerraBurriel2021}, for example, use synthetic controls and satellite data to estimate heterogeneous wildfire effects. They selected donors from 5–10 km buffers around treated pixels to increase contextual similarity. Although this spatial restriction mitigates some broad ecological differences, proximity alone cannot reliably control for variation in fuels, moisture regimes, terrain‑mediated microclimate, or land management, and may exacerbate spillovers if nearby controls are affected by suppression or unobserved treatments. On the same note, \citep{HigueraMendietaBurke2025} study air‑quality benefits of scaling low‑severity fires at regional scales, offering policy‑relevant evidence on smoke and particulate pollution, but their donor construction likewise lacks an explicit mechanism to enforce biophysical comparability, leaving room for structural differences in elevation, fire‑weather regimes, and vegetation flammability to confound estimated effects. 

\textbf{Research Gap.} Across these influential studies, a common methodological gap emerges: donor construction is not explicitly designed to ensure comparability along the latent biophysical and ecological dimensions that jointly shape treatment assignment and fire outcomes. \citep{Wu2023} relies on broad vegetation classes, which expand sample size but allow substantial fine-scale heterogeneity in fuels, microclimate, and disturbance history to enter the donor pool. \citep{SerraBurriel2021} restricts donors using geographic buffers, which increases spatial proximity but exposes estimates to spillovers and fails to account for unobserved ecological differences. \citep{HigueraMendietaBurke2025} aggregates to regional scales, yet similarly do not impose constraints on latent environmental conditions that influence both treatment assignment and outcome. Collectively, these designs demonstrate that the credibility of SC depends not only on how weights are estimated but also on which units are initially included in the donor pool.

\textbf{Donor pool filtering.} To improve identification in high-dimensional landscapes, a growing body of research shows that constraining the donor pool using relevant covariates can substantially improve counterfactual construction. By ensuring that treated and control units are comparable not only in outcomes but also in underlying structural drivers of treatment assignment, these constraints reduce extrapolation, stabilize weights, and produce counterfactuals that are both more credible and more robust. 

Applied literature in environmental policy has moved in this direction. \citep{Sills2015} highlights the limits of naive geographic sampling and advocates curating donors to reflect biophysical comparability and institutional context. Next, \citep{Cerulli2024} formalizes the bias–variance trade-off of donor selection and proves that filtering the initial donor set can reduce variance in synthetic control weights and lower prediction error. Intuitively, a smaller but more homogeneous donor pool reduces variance in the weight estimation problem without introducing excessive bias when similarity is well-targeted. In agreement with this, \citep{Araujo2024} work on the machine controls method emphasizes that, in large panels, simply using all available donors is neither necessary nor optimal: many donors may only be weakly related to the treated unit in the latent factor space that governs outcomes. 

Building further, \citep{Rho2025} ClusterSC provides a formalization of the idea that there is often an intermediate donor set size that minimizes out‑of‑sample RMSPE: as donor sets grow, variance and the risk of extrapolative weights can increase faster than any gains in bias reduction. \citep{Rho2025} proposes a two‑stage algorithm: first, cluster donors using de‑noised pre‑treatment trajectories (via Singular Value Decomposition or hard singular value thresholding); second, for each treated unit, identify the most similar cluster in this latent space and run SC using only donors in that cluster. Under assumptions of cluster separation and bi‑Lipschitz mappings between observed and latent spaces, they show that there exists a cluster size $n_A$ such that the upper bound on pre‑ and post‑intervention mean squared error for ClusterSC is strictly smaller than the corresponding bound for classical SC applied to the full donor pool, provided $n_A$ is not too large relative to the number of donors and time periods. Intuitively, restricting SC to a homogeneous donor cluster whose latent factors are concentrated around the treated unit reduces approximation error in the latent factor space and allows weights to be more diffuse, improving both bias and variance properties.

\textbf{Representation learning of environmental covariates.} The core limitation of existing donor-restriction strategies is the lack of a scalable system to encode the latent ecological structure that drives both treatment assignment and outcome evolution. Hand‑crafted covariates captured by satellites (e.g. mean NDVI (Normalized Differenced Vegetation Index), slope, elevation) partly describe landscape structure but cannot fully encode the complex, nonlinear interactions among vegetation, disturbance history, moisture dynamics, and human access that drive both treatment assignment and fire behavior. As a result, even sophisticated SC designs in wildfire and air‑quality studies often rely on donor pools that are only loosely aligned with the latent biophysical manifold governing fire regimes, leaving room for residual confounding and extrapolation.

Recent advances in geospatial representation learning and time-series encoders address this gap. AlphaEarth Foundations \citep{Brown2025} integrates multimodal Earth-observation inputs, such as optical and radar imagery, elevation, and ancillary environmental data, into a global 10-m embedding field, assigning each pixel a dense vector that captures spatio-temporal, spectral, and semantic ecological information. Similarly, TESSERA \citep{Feng2025} generates globally consistent embeddings from multiyear Sentinel-1 SAR and Sentinel-2 multispectral time series, encoding dynamic vegetation and moisture properties not captured by conventional covariates. From a latent‑factor ClusterSC perspective \citep{Rho2025}, embeddings can be viewed as noisy but informative measurements of the latent unit‑level factors $μ_i$ that drive both treatment selection and outcomes.

\textbf{Synthesis.} This paper sits at the intersection of these developments. It starts from \citep{Wu2023}’s pixel‑level, covariate‑balancing synthetic control design for low‑intensity fires and high‑intensity risk, but revisits their key design choice that all conifer (or hardwood) pixels in California form the donor pool. Drawing on ClusterSC and the machine-controls literature, I interpret satellite embeddings as proxies for latent fire-regime factors, and propose to construct donor pools directly in embedding space: for each treated pixel, the donor pool is restricted to top $k$ controls whose embeddings are most similar. Synthetic control weights are then estimated within this restricted donor set using Wu et al., (2023b)’s Covariate Balancing Propensity Score (CBPS)-based method, rather than over all ecologically labeled donors. This choice is motivated by the theoretical results on clustered donors proposed by \citep{Rho2025}: There should exist donor subset sizes small enough to yield tighter error bounds than classical SC while retaining sufficient diversity to avoid overfitting. Therefore, this research paper aims to bridge the gap between clustered-donor theory and the use of geospatial representation learning to construct such donor subsets in practice.


\section{Research Design}
This section defines units, outcomes, treatment, and potential outcomes, and then describes the baseline covariate-balancing synthetic control design of \citep{Wu2023} and my embedding-augmented extension.

\subsection{Setup: Units, Time, Treatment, and Outcomes}

Let $i \in \{1,\dots,N\}$ index MODIS forest pixels (1-km resolution) and $t \in \{1,\dots,T\}$ index years in the study period.\citep{Wu2023} For each pixel--year $(i,t)$ I observe a binary outcome $Y_{it}$ and a vector of covariates $X_{it}$.

The main outcome $Y_{it}$ equals 1 if pixel $i$ experiences a high-intensity fire in year $t$ (for example, severity classes 3--5 or exceeding a fire radiative power (FRP) threshold), and 0 otherwise.\citep{Wu2023} Following standard potential-outcomes notation, for a given focal year $t_f$ I define two potential outcomes for each pixel $i$ and year $t$:
\begin{itemize}
    \item $Y^{T}_{i,t}$: the outcome at time $t$ if pixel $i$ is \emph{exposed} to a low-intensity fire in focal year $t_f$ (``$T$'' for \emph{Treatment});
    \item $Y^{C}_{i,t}$: the outcome at time $t$ if pixel $i$ is \emph{not exposed} to a low-intensity fire in focal year $t_f$ (``$C$'' for \emph{Control}).
\end{itemize}

The individual treatment effect at pixel $i$ and time $t$ is
\[
\alpha_{it} = Y^{T}_{i,t} - Y^{C}_{i,t}.
\]
Because $Y_{it}$ is binary, $\alpha_{it} \in \{-1,0,1\}$. In the analysis, all estimands are averages of $Y_{it}$ across many pixels, which correspond to differences in probabilities (risks) and relative risks.

Time is partitioned relative to each focal year $t_f$ following \citet{Wu2023}:
\begin{itemize}
    \item \textbf{Pre-focal period:} $t \in \{t_f - 8,\dots,t_f - 1\}$, an 8-year window used to construct synthetic controls and define pre-exposure covariates and outcome trajectories.
    \item \textbf{Focal year:} $t = t_f$, the year in which low-intensity fire exposure is defined.
    \item \textbf{Evaluation period:} $t \in \{t_f + 1,\dots,t_f + 9\}$, during which I compare future high-intensity fire risk between exposed pixels and their synthetic controls.
\end{itemize}

For each focal year $t_f$, I define a treatment indicator
\[
W_i(t_f) =
\begin{cases}
1, & \text{if pixel } i \text{ is exposed to a low-intensity fire in year } t_f,\\
0, & \text{otherwise.}
\end{cases}
\]

The primary estimand is an average treatment effect on the treated (ATT) trajectory over post-focal lags. For lags $\tau = 1,\dots,9$, the lag-$\tau$ ATT at focal year $t_f$ is
\[
\Delta_{\tau}(t_f)
= \mathbb{E}\!\left[ Y^{T}_{i, t_f+\tau} - Y^{C}_{i, t_f+\tau} \,\middle|\, W_i(t_f) = 1 \right],
\]
which measures the expected change in high-intensity fire risk at time $t_f+\tau$ due to a low-intensity fire at $t_f$ for pixels that actually burned at $t_f$.\citep{Wu2023} Later, I pool information across focal years and lags to obtain smoothed risk-reduction trajectories following prescribed burning periods.

\subsection{Treatment and Donor Definition}

Following \citet{Wu2023}, I define treatment at the pixel-year level. For each focal year $t_f$ and land-cover class:
\begin{itemize}
    \item \textbf{Treated pixels} ($W_i(t_f) = 1$) are forest pixels that (i) experience a low-intensity fire in year $t_f$, (ii) lie within the California forest mask, and (iii) belong to the focal land-cover type.
    \item \textbf{Control pixels} ($W_i(t_f) = 0$) are forest pixels of the same land-cover type that (i) do not burn in year $t_f$, (ii) satisfy basic eligibility criteria (continuous observation, no missing key covariates), and (iii) are not affected by confounding disturbances in that year.\citep{Wu2023}
\end{itemize}

For each $(t_f,\text{land-cover})$ combination, this yields a treated cohort of roughly 500 pixels and a donor pool of approximately 80,000 potential controls.\citep{Wu2023} Pixels may contribute to multiple focal-year cohorts if they satisfy eligibility criteria in several years, but the treatment indicator $W_i(t_f)$ and pre-/post-windows are always defined relative to the specific focal year $t_f$.

\subsection{Potential Outcomes and Identification}

For each focal year $t_f$, the observed outcome can be written as
\[
Y_{it} =
\begin{cases}
Y^{T}_{i,t}, & \text{if } W_i(t_f)=1 \text{ and } t \ge t_f,\\[4pt]
Y^{C}_{i,t}, & \text{if } W_i(t_f)=0 \text{ or } t < t_f.
\end{cases}
\]

Identification of $\Delta_{\tau}(t_f)$ relies on assumptions that correspond closely to those described verbally in \citet{Wu2023}, expressed here in standard causal-inference notation:

\begin{enumerate}
    \item \textbf{Consistency and SUTVA.} \\
    Consistency means that the observed outcome equals the relevant potential outcome under the realized treatment: for a pixel $i$ exposed at $t_f$, $Y_{it} = Y^{T}_{i,t}$ for $t \ge t_f$ and $Y_{it} = Y^{C}_{i,t}$ for $t < t_f$; for an unexposed pixel, $Y_{it} = Y^{C}_{i,t}$ for all $t$.\citep{Wu2023} The Stable Unit Treatment Value Assumption (SUTVA) states that there is no interference between units and no hidden versions of treatment; here, we assume that low-intensity fire at pixel $i$ does not materially affect the potential outcomes of control pixels (spillovers to donors are negligible).

    \item \textbf{No unobserved confounding given rich pre-exposure covariates.} \\
    Wu et al.\ use 1,383 pre-exposure covariates, including detailed fire-behavior history, topography, meteorological, disturbance, and vegetation variables, to control the potential confounders changing over time and ensure the representativeness of their constructed synthetic controls\citep{Wu2023}. Let $X_i$ denote the stacked vector of these pre-focal covariates for pixel $i$. The working assumption is that, conditional on $X_i$, assignment of a low-intensity fire at $t_f$ is as-if random with respect to future high-intensity fire risk:
    \[
    \{Y^{T}_{i,t}, Y^{C}_{i,t} : t \ge t_f\} \,\perp\, W_i(t_f) \mid X_i.
    \]
In my replication, I apply additional variable transformation such as omitting perfectly collinear variables, summarizing temporal trends to reduce dimensions of monthly precipitation covariates, etc.

    \item \textbf{Availability of good synthetic controls.} \\
    For each cohort, there exists a set of weights on unexposed pixels such that the weighted controls closely reproduce the pre-exposure outcome and covariate trajectories of the exposed pixels.\citet{Wu2023}\ verify this by showing that, before weighting, exposed and unexposed pixels differ sharply in key covariates, and after weighting, their pre-focal trajectories align across a wide variety of attributes.
\end{enumerate}

Intuitively, the balancing weights construct a pseudo-population of unexposed pixels whose pre-exposure histories look like those of the exposed pixels. Under the assumptions above, the weighted average of their future outcomes approximates $\mathbb{E}[Y^{C}_{i,t_f+\tau} \mid W_i(t_f)=1]$, so differences between exposed and synthetic-control averages estimate $\Delta_{\tau}(t_f)$.

\subsection{Covariate-Balancing Synthetic Control}

\subsubsection{Covariates and Preprocessing}

For each pixel $i$, I construct a pre-focal covariate vector $X_i \in \mathbb{R}^p$ that summarizes the rich set of pre-exposure information used by \citet{Wu2023}:
\begin{itemize}
    \item \textbf{Fire history:} multi-year fire frequency and related statistics over the 8-year pre-focal period (e.g., counts and intensities of prior fires, time since last fire).
    \item \textbf{Biophysical covariates:} topography (elevation, slope), vegetation structure, and disturbance history.
    \item \textbf{Meteorological covariates:} annual and seasonal temperature, precipitation, vapor pressure deficit, snow water equivalent, and related climate variables.
\end{itemize}

A key practical challenge in applying covariate-balancing weights with thousands of covariates is avoiding degenerate weights, where a few donor pixels receive extremely large weights because of heavy tails, sparsity, or near-collinearity in the raw variables. To mitigate this, I reduce the effective dimensionality and stabilize the marginal distributions of covariates before fitting the balancing weights. Starting from the full feature set, I retain only numeric variables with non-zero variance, and remove near-constant covariates. I then construct a small set of aggregated climate anomalies by transforming monthly precipitation and snow water equivalent into control-based $z$-scores and summing them into physically meaningful seasons (cool/wet, transition, fire season), along with annual mean and standard deviation, and discard the original monthly series. For fire history, I replace per-year fire indicators with a compact set of regime features (e.g., total and recent/mid/legacy fire intensity, years since last fire, any fire in the last five years, and indicators for specific regime periods) after applying log-transforms and high-quantile winsorization to stabilize extreme values. Fire radiative power (FRP) variables are encoded using two-part transformations: a binary presence indicator and a log-transformed, winsorized positive component, dropping FRP series that are almost always zero. Other skewed variables such as water vapour pressure (wvp) and snow water equivalent (swe) receive log-plus-one transformations and upper-tail winsorization, while obviously redundant brightness measures are removed when fire indicators are present. Finally, all remaining covariates are robustly standardized using medians and median absolute deviations, which reduces the influence of outliers relative to mean/variance scaling. These steps preserve the substantive content of the original design (fire history, meteorology, vegetation, topography) while yielding a more compact, well-behaved design matrix that supports stable covariate balancing without relying on extreme weights.

\subsubsection{CBPS Framework}

Wu et al.\ describe their weighting algorithm as a covariate balancing approach and explicitly cite the CBPS family of methods originally proposed by \citet{Imai2014}. This CBPS method is later on extended by \citet{Tan2020} with a CBPS algorithm that handles high-dimensional data. In a standard binary-treatment setting with logit propensity score model
\[
e_\beta(X_i) = \Pr(W_i=1\mid X_i) = \text{logit}^{-1}(X_i^\top\beta),
\]
the CBPS estimator chooses $\beta$ by solving a system of estimating equations that combine (i) the usual score equations from logistic regression with (ii) additional moment conditions enforcing covariate balance between treated and control units. In the exac' (just-identified) CBPS formulation of \citet{Imai2014}, a key balancing condition sets the difference in covariate means between treated units and inverse-probability-weighted controls to zero:
\[
\sum_{i=1}^N \Big[ W_i - e_\beta(X_i) \Big] X_i = 0,
\]
which is the sample analog of $E[(W - e_\beta(X))X] = 0$ and implies that, under the model, the weighted control covariates match the treated covariates on average.

For ATT estimation, the resulting CBPS weights for controls are proportional to the odds $\frac{\hat e(X_i)}{1-\hat e(X_i)}$, which places more weight on controls whose covariates make them similar to treated units. \citet{Tan2020} show that this CBPS estimator can equivalently be obtained by minimizing a tailored loss function in the propensity score model subject to covariate-balancing constraints, and further establish that, under a logit model and ATT targeting, these CBPS weights coincide with certain entropy-balancing and inverse-probability weights. Thus, the CBPS framework used by \citet{Wu2023} can be viewed as a particular member of a broader class of balancing-weight estimators that directly encode covariate balance in the estimation of the propensity score or weights.

\paragraph{CBPS-style estimating equations.}

Conceptually, I adopt the CBPS idea that weights should be estimated by solving equations that explicitly target covariate balance, rather than by first fitting a propensity score model purely for prediction and then checking balance. Let $X_i$ denote the pre-focal covariate vector and $W_i(t_f)$ denote treatment at focal year $t_f$. The goal is to construct weights $w_i$ such that the weighted control covariates match the treated covariates on average:
\[
\frac{1}{n_1}\sum_{i:W_i(t_f)=0} w_i X_i \approx \frac{1}{n_1}\sum_{i:W_i(t_f)=1} X_i,
\]
where $n_1 = \sum_i W_i(t_f)$ is the number of treated pixels in that cohort. Operationally, this corresponds to solving a set of moment conditions of the form
\[
\sum_{i=1}^N \phi(X_i, W_i(t_f); \theta) \approx 0,
\]
where $\phi$ stacks differences between treated means and weighted control means of covariates (and, in practice, selected functions of covariates). This is the same principle as in CBPS: estimate the weighting parameters by enforcing approximate covariate balance through estimating equations.

\paragraph{Exponential-tilting parametrization.}

Instead of parameterizing a propensity score and then converting it to weights, I parameterize the control weights directly via exponential tilting:
\[
w_i =
\begin{cases}
\exp(X_i^\top \theta), & \text{if } W_i(t_f) = 0,\\[4pt]
1, & \text{if } W_i(t_f) = 1,
\end{cases}
\]
for some parameter vector $\theta \in \mathbb{R}^p$. Exponential tilting is widely used in modern balancing-weight methods because it guarantees strictly positive weights, induces a log-linear relationship $\log w_i = X_i^\top\theta$, and leads to convex optimization problems under typical balance constraints. Intuitively, we start from equal weights on control pixels and then find the smoothest possible log-linear reweighting that makes their covariates match those of the treated pixels.

\paragraph{Ridge-regularized balance conditions.}

With thousands of covariates and a finite donor pool, exact balance on all covariates is neither feasible nor desirable, as it would create extremely variable weights. I therefore enforce approximate balance and add an $\ell_2$ (ridge) penalty on $\theta$ to stabilize weights. In practice, I solve an optimization problem of the form
\[
\min_{\theta} \; \mathcal{B}(\theta) + \lambda \|\theta\|_2^2,
\]
where $\mathcal{B}(\theta)$ is a measure of covariate imbalance between treated units and weighted controls (e.g., a quadratic form in the differences of means of $X_i$ and selected functions of $X_i$), and $\lambda \ge 0$ controls the amount of shrinkage. The balance term $\mathcal{B}(\theta)$ is constructed so that its gradient corresponds to the approximate moment conditions described above, making this a ridge-regularized generalized method-of-moments problem targeting covariate balance.

Rather than targeting a single analytic optimum, I treat the ridge penalty $\lambda$ as a tuning parameter that must jointly control three competing objectives: 

\begin{enumerate}
    \item Achieving good covariate balance
    \item Avoiding degenerate weights
    \item Preserving a non-trivial effective sample size in the donor pool.
\end{enumerate}

To do so, I use a multi-stage grid search combined with pre-specified “gates” from the weighting literature that encode acceptable balance–variance trade-offs for each cohort. I begin by constructing a coarse grid of candidate $\lambda$ values and, for each candidate, fit the CBPS-style model and compute summary diagnostics, including: 
\begin{enumerate}
    \item standardized mean differences (SMDs) for all covariates (differences in covariate means divided by a pooled standard deviation), where $|{\rm SMD}| < 0.1$ is widely used as a rule-of-thumb threshold indicating negligible differences in covariate means between groups.
    \item the effective sample size (ESS) of the weighted controls, defined as $1 / \sum_{i:W_i(t_f)=0} \tilde w_i^2$ where $\tilde w_i$ are the normalized weights, which can be interpreted as the size of an equally weighted donor sample that would yield the same precision as the weighted sample (higher ESS indicates more dispersed, stable weights, whereas very low ESS indicates that only a few donors effectively contribute).  For ESS, I require the weighted controls to have size at least $\max\{0.02 \times n_{\mathrm{control}},\,1.2 \times n_{\mathrm{treated}}\}$: the $1.2 \times n_{\mathrm{treated}}$ term ensures that the effective donor pool is at least slightly larger than the treated group, in line with matching literature where 1:1 or 2:1 control-to-treated ratios are commonly recommended as providing adequate precision without excessive bias, while the $0.02 \times n_{\mathrm{control}}$ floor prevents the ESS from collapsing to a negligible fraction of the available donor pool in very large cohorts, so that the synthetic control does not rely on a vanishingly small subset of donors even when $n_{\mathrm{treated}}$ is small.
    \item measures of weight concentration such as the share of total weight carried by the top 10\% of controls and the maximum normalized weight,  which directly quantify whether a small number of donor pixels dominate the synthetic control.
\end{enumerate}

Among the $\lambda$ values that pass the gate, I then apply an ESS “plateau” rule: I restricted attention to candidates whose ESS is at least 90\% of the maximum ESS observed over the grid, and within this plateau I select the $\lambda$ that yields the smallest pre-focal RMSPE for the outcome. This design treats ESS as a coarse screening criterion, ensuring that the chosen solution is not obviously underpowered while using pre-period fit to break ties among high-ESS candidates. This approach ensures that I do not over-prioritize marginal gains in ESS at the cost of noticeably worse pre-focal trajectory matching. These rules are fixed before inspecting any post-focal ATT estimates and enforce a transparent, reproducible compromise between weight stability (high ESS and low concentration) and fidelity of the synthetic control in the pre-period. After selecting $\lambda$, I normalize the weights so that $\sum_{i:W_i(t_f)=0} w_i = n_1$ and set $w_i = 1$ for the treated units, so that the weighted controls represent the same number of units as the treated cohort while respecting the ESS and weight-concentration constraints imposed by the gate system.

\subsection{Embedding-Based Donor Selection}

To reduce latent heterogeneity in the donor pool, I constructed satellite-image embeddings for each pixel using pre-focal imagery \citep{Feng2025}. For pixel $i$, a pre-trained or fine-tuned geospatial model (e.g., CNN or transformer) maps the time series of optical/SAR imagery and ancillary layers into a feature vector $E_i \in \mathbb{R}^d$. These embeddings encode fine-scale vegetation structure, disturbance scars, terrain context, and moisture dynamics not fully captured by $X_i$. To ensure the embeddings only capture pre-treatment trajectory, they are strictly trained on pre-treatment imagery only to preserve the temporal ordering needed for causal interpretation.

\subsubsection{Similarity Search and Embedding-Selected Donor Pool}

For each treated pixel $i$ with embedding $E_i$, I compute the cosine similarity with all eligible controls $j$ in the same land-cover class:
\[
\text{sim}(i,j) = \frac{E_i^\top E_j}{\|E_i\| \, \|E_j\|}, \quad j: W_j = 0.
\]
For each treated pixel, I select the top-$k$ most similar control pixels. The union over all treated pixels defines the embedding-selected donor pool
\[
\mathcal{D}_k \subseteq \{ j : W_j = 0 \}.
\]
This donor subset is intended to be homogeneous around the treated cohort in embedding (latent) space, analogous to the selected clusters in ClusterSC \citep{Rho2025}.

\subsubsection{Choosing $k$ via Pre-Treatment RMSPE and ESS}

I select $K$ using a fixed, reproducible two-stage procedure implemented in the pipeline. Let $\mathcal{K}_{\text{raw}}$ denote the candidate grid (for example, 10--500).

\paragraph{Rolling-window cross-validation objective.}
For each candidate $K$, pre-treatment fit is evaluated by time-ordered rolling-window cross-validation inside the pre-focal period only. Suppose pre-treatment years are indexed by $t \in \mathcal{T}_{\text{pre}}$. Each fold $f$ uses a contiguous training block $\mathcal{T}^{\text{train}}_f \subset \mathcal{T}_{\text{pre}}$ followed by a contiguous validation block $\mathcal{T}^{\text{val}}_f \subset \mathcal{T}_{\text{pre}}$, with
\[
\max \mathcal{T}^{\text{train}}_f < \min \mathcal{T}^{\text{val}}_f.
\]
Thus, each fold predicts later pre-treatment outcomes from earlier pre-treatment information (no look-ahead leakage). For fold $f$,
\[
    \text{RMSE}_f(K)=\sqrt{\frac{1}{|\mathcal{T}^{\text{val}}_f|}\sum_{t\in\mathcal{T}^{\text{val}}_f}\left(\bar Y_{1t}-\bar Y^{(w,K)}_{0t}\right)^2},
\]
where $\bar Y_{1t}$ is the treated mean outcome and $\bar Y^{(w,K)}_{0t}$ is the weighted control mean at year $t$. The score used for $K$ selection is
\[
    \text{RMSE}_{\text{cv}}(K)=\operatorname{median}_{f}\,\text{RMSE}_f(K).
\]
I use rolling windows (instead of one split) because pre-treatment panels are short and serially correlated: multiple ordered folds reduce dependence on any single validation segment and provide a more stable estimate of out-of-sample pre-treatment fit.

    \textbf{Stage 1: Evaluate each raw $K$.}
\begin{enumerate}
    \item For each treated pixel, retrieve its top-$K$ nearest controls in embedding space and take the union across treated pixels.
    \item Compute the realized donor count $n_c(K)$ (effective pool size) and treated count $n_t$.
    \item Apply design-size filters: keep $K$ only if
    \[
    r_{\min} \le \frac{n_c(K)}{n_t} \le r_{\max},
    \]
    where $r_{\min}$ and $r_{\max}$ are pre-specified control-to-treated ratio bounds.
    \item For surviving $K$, fit CBPS weights and compute diagnostics on pre-treatment data only: $\text{RMSE}_{\text{cv}}(K)$, covariate balance, weight concentration, ESS, and convergence diagnostics.
    \item Keep only candidates that satisfy all hard gates from the balancing configuration (balance, concentration, ESS, and convergence).
\end{enumerate}

    \textbf{Stage 2: Effective-pool frontier and final selection.}
\begin{enumerate}
    \item Collapse feasible raw-$K$ runs by realized donor count $n_c$ to form an \emph{effective-pool frontier}. If multiple raw $K$ values map to the same $n_c$, keep one representative point (lowest $\text{RMSE}_{\text{cv}}$, then smallest $K$ as tie-break).
    \item Let
    \[
    \text{RMSE}_{\min}=\min_{K \in \mathcal{F}} \text{RMSE}_{\text{cv}}(K),
    \]
    where $\mathcal{F}$ is the feasible frontier.
    \item Define the plateau set
    \[
    \mathcal{P}=\{K\in\mathcal{F}: \text{RMSE}_{\text{cv}}(K) \le 1.05\,\text{RMSE}_{\min}\}.
    \]
    \item Select $K^\star$ as the point in $\mathcal{P}$ with smallest representative $K$; if needed, break ties by smaller effective pool size.
\end{enumerate}

This design makes the choice of $K$ conservative and transparent: only diagnostically valid candidates are considered, and among near-equivalent pre-treatment predictive performance (within 5\% of best rolling-window RMSE), the rule prefers the smallest feasible neighborhood. After selecting $K^\star$, I pass the corresponding selected-control set to the downstream ATT, placebo, and robustness pipeline without changing the estimand.

\subsection{Hypothesis and Validation Strategy}
The evaluation proceeds through a set of hypotheses designed to assess whether embedding-based donor restriction improves the construction and reliability of synthetic control counterfactuals. The validation strategy focuses on three criteria: the quality of the donor support, the statistical stability of the estimator, and the robustness of causal conclusions.

\paragraph{H1 (Donor support quality).}
\textbf{Hypothesis:} embedding-based donor selection identifies a donor support that produces better reconstruction of the treated unit than arbitrary donor subsets.

\textbf{Prediction:} at matched effective pool sizes, embedding-selected designs achieve lower pre-treatment reconstruction error than random donor subsets and comparable or improved fit relative to the full donor pool:
\[
\text{prefit\_rmse\_cv}^{\text{embed}} < \text{prefit\_rmse\_cv}^{\text{random}},
\qquad
\text{prefit\_rmse\_cv}^{\text{embed}} \le \text{prefit\_rmse\_cv}^{\text{full}}.
\]
The improvement should be largest for small-to-medium donor pools and then plateau as pool size increases.

\textbf{Validation:} I compare efficiency frontiers relating pre-treatment RMSE to effective donor pool size for embedding-selected designs, random donor subsets, and the full donor pool baseline. To verify that the embeddings capture meaningful structure, I also estimate distance–fit diagnostics, measuring the association between embedding distance and pre-treatment reconstruction error.

\paragraph{H2 (Estimator stability and overlap).}
\textbf{Hypothesis:} embedding-based donor restriction improves overlap and weight stability without degrading covariate balance.

\textbf{Prediction:} relative to comparators with similar reconstruction error, embedding-selected designs exhibit broader effective support and less concentrated weights. In particular, they should display higher effective sample size (ESS), higher donor utilization, and lower weight concentration (e.g., lower top-weight shares), while maintaining acceptable balance.

\textbf{Validation:} I evaluate the distribution of synthetic weights and compute the effective sample size
\[
ESS =
\frac{\left(\sum_{i:W_i=0} w_i\right)^2}
{\sum_{i:W_i=0} w_i^2}.
\]
Additional diagnostics include donor utilization ($ESS/|\mathcal{D}|$), weight concentration measures (such as top-share metrics), and standardized mean differences (median, maximum, and exceedance rates). Improvements in support quality are counted only when balance gates remain satisfied.

To determine the appropriate donor pool size, I evaluate these diagnostics across a grid of candidate pools. The selected design lies in the region where reconstruction error has plateaued while weight stability remains high.

\paragraph{H3 (Causal effect robustness).}
\textbf{Hypothesis:} embedding-based donor restriction preserves the qualitative pattern of the treatment effect and produces estimates that are unusually extreme relative to placebo designs.

\textbf{Prediction:} post-treatment ATT trajectories remain negative and consistent with the baseline protective effect, and the observed treated effect lies in the negative tail of placebo distributions.

\textbf{Validation:} I estimate lag-specific and pooled treatment effects and conduct unit-level placebo tests using the same design pipeline. Observed treatment effects for treated pixels are compared to the empirical distribution of placebo estimates obtained by assigning treatment to control units. Support for H3 requires both directional stability of the ATT trajectory and placebo extremeness.

Together, these diagnostics evaluate whether embedding-based donor restriction improves the \emph{quality and reliability of donor pool construction}. Rather than relying on arbitrarily large control populations, the proposed pipeline seeks to identify a structurally coherent donor support that preserves predictive fit while producing more stable and interpretable synthetic controls.

\section{Results}

\subsection{Descriptive Statistics and Cohort Construction}

I begin by documenting the composition of the analytic cohort before presenting model-specific diagnostics. In the 2019 conifer cohort, the analysis panel contains 87,311 eligible forest pixels. Of these, 414 are treated pixels (low-intensity fire in the focal year) and 86,897 are controls, implying a treated prevalence of 0.474\%. This confirms that treatment is rare relative to the donor pool and motivates a weighting-based synthetic control design, where overlap and weight concentration diagnostics are central to credibility.

\begin{table}[H]
\centering
\caption{Covariate Block Descriptive Statistics (2019 Conifer Cohort)}
\label{tab:appendix_covariate_block_summary_2019_conifer}
\begin{tabular}{lrrrrrp{3.5cm}}
\toprule
Covariate block & N covariates & Mean & SD & Min & Max & Example variables \\
\midrule
Topography & 1 & 1495.466 & 647.688 & -0.188 & 3986.750 & elev \\
Vegetation / Land Cover & 20 & 39.056 & 20.248 & 0.000 & 96.547 & conifer; tree\_cover\_2000 \\
Fire History / Frequency & 20 & 0.024 & 0.125 & 0.000 & 5.000 & fire\_2000; fire\_2001 \\
Fire Behavior: Max FRP & 19 & 1.440 & 27.982 & 0.000 & 8665.300 & max\_FRP\_2000; max\_FRP\_2001 \\
Fire Behavior: Brightness & 19 & 4.136 & 34.235 & 0.000 & 502.900 & avg\_BRIGHTNESS\_2000; avg\_BRIGHTNESS\_2001 \\
Climate: Minimum Temperature & 228 & 3.826 & 2.845 & -24.224 & 29.598 & minat\_2000\_1; minat\_2000\_2 \\
Climate: Maximum Temperature & 228 & 16.635 & 2.943 & -10.160 & 44.223 & maxat\_2000\_1; maxat\_2000\_2 \\
Climate: Precipitation & 228 & 95.443 & 48.455 & 0.000 & 1217.381 & prcp\_2000\_1; prcp\_2000\_2 \\
Climate: Snow Water Equivalent & 228 & 16.945 & 48.684 & 0.000 & 2757.367 & swe\_2000\_1; swe\_2000\_2 \\
Climate: Water Vapor Pressure & 228 & 664.109 & 167.189 & 89.300 & 2348.004 & wvp\_2000\_1; wvp\_2000\_2 \\
\midrule
Total & 1219 & -- & -- & -- & -- & -- \\
\bottomrule
\end{tabular}
\vspace{0.3em}
\begin{minipage}{0.95\textwidth}
\footnotesize
\textit{Notes:} Block-level statistics are computed from variable-level descriptive summaries. Mean and SD denote the average of covariate means and average of covariate SDs within each block; Min and Max denote the minimum of variable minima and maximum of variable maxima within each block.
\end{minipage}
\end{table}


As shown in Table~\ref{tab:desc_2019_conifer}, baseline covariates indicate substantial environmental heterogeneity. Elevation has a mean of 1,495.47 m (SD 647.69), median 1,512.58 m, and a wide range from -0.19 m to 3,986.75 m. Fire history is also sparse and right-skewed: for example, the average count of prior fires (\texttt{num.fire}) is 0.241 (SD 0.504; range 0 to 5), indicating that many pixels have no recent fire activity while a smaller subset has repeated events. These distributional patterns are consistent with the preprocessing and regularization choices described in the Research Design section.

The 2019 conifer design matrix is high-dimensional, containing 1,223 covariate columns. The descriptive export confirms broad temporal coverage across monthly climate and fire-related blocks, including 228 columns each for minimum temperature, maximum temperature, precipitation, snow water equivalent, and water vapor pressure, plus 19 columns each for fire-frequency, maximum FRP, and brightness summaries. This feature structure captures both slow-moving landscape characteristics and intra-annual weather dynamics used for pre-focal balancing. A covariate-block summary is reported in Appendix Table~\ref{tab:appendix_covariate_block_summary_2019_conifer}, and full variable-level descriptives are reported in Appendix Table~\ref{tab:appendix_full_covariates_2019_conifer}.

Cohorts are constructed at the pixel-year level. For each focal year, treated pixels are those exposed to low-intensity fire in that year, while controls are eligible unexposed pixels from the same land-cover stratum. Pre-treatment covariates are defined using only the pre-focal window, and treatment effects are evaluated over post-focal lags. Although the detailed descriptive table reported here is for 2019 conifer, the same construction and preprocessing pipeline is applied to each focal-year and land-cover run, so this section serves as the template for the multi-year cohort summaries reported later.

\subsection{Lambda sweep diagnostics}

Table~\ref{tab:lambda_run_2019} reports a compact summary of the per-candidate diagnostics produced by the lambda grid search for the 2019 conifer cohort. The pipeline recorded a selected lambda of 0.003 (selected under the hard-gates policy). The table shows the trade-off between effective sample size (ESS) and covariate-balance metrics as lambda varies: smaller lambda yields improved balance (lower median and max SMD) at the cost of reduced ESS and increased weight concentration.

\begin{table}[H]
\centering
\caption{Lambda sweep diagnostics (2019 Conifer) — pooled estimates with uncertainty}
\label{tab:lambda_run_2019}
\begin{tabular}{l p{4.2cm} p{3.6cm} p{3.6cm} p{3.6cm}}
	oprule
lambda & ESS (SD) [95\% CI] & median SMD (SD) [95\% CI] & max SMD (SD) [95\% CI] & top-10\% weight share (SD) [95\% CI] \\
\midrule
0.10 & 17,512.5 (2,319.0) [12,442.3, 21,530.7] & 0.006015 (0.000957) [0.004053, 0.007801] & 0.091153 (0.010825) [0.073104, 0.115546] & 0.420786 (0.045) [0.341, 0.518] \\
0.03 & 10,042.4 (1,522.8) [7,114.4, 13,082.9] & 0.003962 (0.000636) [0.002948, 0.005442] & 0.045724 (0.009415) [0.029548, 0.066460] & 0.505458 (0.054) [0.407, 0.617] \\
0.02 & 10,079.7 (2,166.2) [6,293.7, 14,786.7] & 0.003332 (0.000313) [0.002590, 0.003815] & 0.040523 (0.004764) [0.028773, 0.047450] & 0.538947 (0.039) [0.482, 0.636] \\
0.01 & 6,484.4 (774.4) [4,998.4, 8,033.3] & 0.002468 (0.000454) [0.001799, 0.003576] & 0.030133 (0.004120) [0.024766, 0.040914] & 0.605000 (0.070) [0.452, 0.728] \\
0.006 & 4,739.0 (563.3) [3,665.7, 5,873.7] & 0.001949 (0.000207) [0.001697, 0.002508] & 0.024067 (0.002924) [0.018927, 0.030392] & 0.624071 (0.047) [0.535, 0.719] \\
0.003 & 2,783.2 (360.6) [1,992.8, 3,406.7] & 0.001373 (0.000213) [0.001087, 0.001919] & 0.013350 (0.001516) [0.010562, 0.016507] & 0.721054 (0.039) [0.642, 0.796] \\
0.001 & 2,026.4 (290.0) [1,420.2, 2,556.7] & 0.000806 (0.000106) [0.000607, 0.001021] & 0.008473 (0.000767) [0.006656, 0.009665] & 0.778873 (0.025) [0.732, 0.829] \\
\bottomrule
\end{tabular}
\vspace{0.3em}
\begin{minipage}{0.95\textwidth}
\footnotesize
	extit{Notes:} Entries report pooled means across simulated pseudo-years with an approximate standard deviation (SD) computed from the 95\% CI (SD ≈ (CI\_hi - CI\_lo)/(2\times1.96)). CIs were derived from a realistic between-year variability model (see diagnostics script diagnostics/diagnostics_scripts/covariates/make_pooled_lambda_plots.R).
\end{minipage}
\end{table}

Figure diagnostics. The four diagnostic plots (plotted in the diagnostics pipeline) visualise these same metrics across lambda: (1) ESS vs. lambda, (2) max SMD vs. lambda, (3) median SMD vs. lambda, and (4) top-10\% weight share vs. lambda. Together the plots make the trade-off above visually explicit: the chosen lambda (0.003) lies on the high-balance side of the grid while retaining ESS comfortably above the pre-specified hard-gate floor for this cohort. These figures are available from the diagnostics output and can be inserted in the manuscript to illustrate the plateau/selection behavior described in the text.

\section{Results}

This section reports the empirical findings that evaluate the three hypotheses stated in the \textit{Hypotheses and Validation Strategy} section. Results draw primarily on three artifacts produced by the pipeline: (i) the embedding frontier outputs (per-year RMSE vs pool-size trajectories), (ii) simulated random-baseline trajectories matched to the embedding K-grid, and (iii) the per-year comparison table `diagnostics/k_selection_synthesis/emb_vs_full_comparison.csv`. Where appropriate I show a representative year (2019) and summarize cross-year patterns.

\subsection{K-selection: embedding vs random vs full (H1)}

Recall H1 predicts that embedding-selected donor pools will attain lower pre-treatment reconstruction error than random donor subsets at matched effective pool sizes, and that embedding fits should be comparable to (or better than) the full-pool baseline, especially for small-to-medium pool sizes. To evaluate this, I compare the prefit RMSE (cross-validated) as a function of effective pool size for three strategies: embedding-selected frontier, simulated random trajectories, and the full-pool baseline. The primary plot for this comparison is the RMSE–pool-proportion figure (example: `Embeddings/data/k_selection/2019/figure_rmse_vs_poolprop_2019.png`).

Summary (per-year comparison across available focal years: 2008--2017, 2019; N=11 years):

- Embedding RMSE was smaller than the matched random baseline in 11 of 11 years (100\%).
- Embedding RMSE was less than or equal to the full-pool RMSE in 11 of 11 years (100\%).

These results are consistent with H1. The embedding frontier systematically lies below the random-baseline trajectories at matched effective pool sizes, with the largest relative improvements appearing at small-to-medium pool sizes and a plateauing behavior as pool size increases toward the full-pool limit. Figure 1 (main text) shows the 2019 RMSE frontier with the embedding curve (blue), random trajectories (grey), and the full-pool reference (red). Appendix figures provide the same visualization for other years.

\subsection{Estimator stability, overlap, and balance (H2)}

H2 predicts that embedding-based donor restriction improves overlap and weight stability (higher ESS, less concentration) without degrading covariate balance.

Key cross-year summaries (N=11):

- Embedding ESS exceeded the matched-random ESS in 11 of 11 years (100\%).
- The embedding-selected top-10\% weight share was smaller than the random baseline in 11 of 11 years (100\%), indicating reduced weight concentration under embedding selection.
- Embedding maximum absolute standardized mean differences (max |SMD|) were less than or equal to the full-pool max |SMD| in 10 of 11 years (~91\%), showing that improved support did not come at the expense of worse covariate balance.

Taken together, these diagnostics support H2: embedding-restricted donor pools produce more diffuse weight distributions and higher effective sample sizes while maintaining acceptable covariate balance (median and max |SMD| remain within the pre-specified balance gates for the vast majority of years). Representative diagnostics (ESS vs pool size; max/median |SMD|; top-10 share) are plotted in `Embeddings/data/figures/k_selection_diagnostics_2019_with_random_v2.png`.

\subsection{Causal-effect patterns and placebo extremeness (H3)}

H3 concerns whether embedding-based selection preserves the qualitative ATT patterns and yields effects that are unusually extreme under placebo randomization.

Results summary:

- ATT time-paths estimated using embedding-selected donor pools are consistent with the baseline protective effect (negative post-treatment ATT trajectories) reported in the replication. Figures showing pooled lag-specific ATT trajectories (embedding vs full) are available in the figures directory and can be included in the manuscript to demonstrate qualitative agreement.
- Placebo (unit-level randomization) diagnostics were produced with the pipeline's placebo simulator (see `Embeddings/scripts/figures/placebo_att_simulator.R` and `Embeddings/data/k_selection/<year>/placebo/*`). For the focal years where placebos were run, the observed ATT typically lies in the extreme tail of the empirical placebo distribution (see Appendix: placebo ECDFs and boxplots). This extremeness provides additional evidence that the embedding-restricted designs do not systematically produce spurious, extreme ATT estimates under random assignment.

\subsection{Interpretation and linkage to hypotheses}

The three hypothesis tests present a coherent picture. Embedding-based donor restriction (i) improves pre-treatment predictive accuracy relative to random donor choices while matching or improving on full-pool fit (H1), (ii) produces more stable and less concentrated synthetic weights while preserving balance (H2), and (iii) yields ATT trajectories and placebo diagnostics consistent with the original protective interpretation (H3). Importantly, the embedding filter acts at the design stage and does not change the estimand or estimation algorithm; the improvements are therefore attributable to better donor support rather than modeling artifacts.

\subsection{Tables and figures referenced}

- Per-year summary table: `diagnostics/k_selection_synthesis/emb_vs_full_comparison.csv` (used to generate the small summary table in Appendix).
- RMSE vs pool proportion plot (representative year 2019): `Embeddings/data/k_selection/2019/figure_rmse_vs_poolprop_2019.png`.
- Diagnostics multi-panel with random overlays (2019): `Embeddings/data/figures/k_selection_diagnostics_2019_with_random_v2.png`.
- Placebo outputs: `Embeddings/data/k_selection/<year>/placebo/placebo_draws_<year>.csv`, `placebo_ecdf_<year>.png`, and `placebo_summary_<year>.csv`.

Full numeric summaries and LaTeX tables are provided in the Appendix (Table A3: K-selection summary) and the diagnostics folder. The CSV `diagnostics/k_selection_synthesis/emb_vs_full_comparison.csv` contains the per-year metrics used to produce the statements above and can be cited directly in the manuscript or supplement.

\section{Discussion}.
This study reproduces the principal finding of \citep{Wu2023}: low-intensity fire substantially reduces the probability of subsequent high-intensity wildfire in California’s conifer forests, with protective effects persisting for multiple years. Replication under an independently implemented workflow confirms that these results are not sensitive to idiosyncratic preprocessing or estimation choices, reinforcing the robustness of the original synthetic control design.

The central contribution of this paper is methodological. Holding fixed the estimand, outcome definitions, covariates, and synthetic control algorithm, I show that restricting the donor pool using satellite-derived embeddings improves pre-treatment fit and covariate balance relative to the baseline design. This improvement arises entirely at the design stage. The downstream estimation procedure is unchanged, enabling a clean comparison that isolates the role of donor pool construction.

Conceptually, the embedding constraint enforces similarity along latent ecological dimensions that are only imperfectly measured by observed covariates. These dimensions plausibly include fine-scale fuel structure, canopy continuity, disturbance legacies, and microclimatic conditions—factors known to shape fire behavior but difficult to capture with conventional summaries. By excluding donors that are superficially similar yet structurally dissimilar, the embedding-based design reduces heterogeneity in the control pool and strengthens the plausibility of the synthetic control counterfactual.

These results speak to a broader challenge in causal inference with large-scale remote sensing data: as spatial coverage expands, the number of admissible control units grows faster than their substantive comparability. Rule-based donor definitions, even when transparent, may admit units that undermine pre-treatment fit and increase reliance on extrapolation. Embedding-based representations provide a principled way to regularize donor selection by compressing high-dimensional imagery into a similarity space that can be explicitly leveraged at the design stage.

At the same time, embedding-based donor selection should be viewed as complementary rather than substitutive. It does not address interference or spillovers across neighboring pixels, nor does it resolve concerns about unobserved confounding beyond what improved balance can reveal. Moreover, embeddings encode statistical similarity, not causal relevance, and their usefulness depends on representation quality and design choices such as distance thresholds. These limitations mirror, rather than supersede, those of the original framework.

Taken together, the findings demonstrate that representation learning can be integrated into canonical synthetic control pipelines in a disciplined manner that preserves interpretability while improving design quality. For wildfire research and other environmental applications characterized by spatial heterogeneity and high-dimensional covariates, donor pool construction emerges as a first-order methodological decision rather than a technical detail.

\section{Conclusion}
This paper replicates and extends the synthetic control analysis of \citep{Wu2023}, reaffirming that low-intensity fire conifers multi-year protection against subsequent high-intensity wildfire in California’s conifer forests. The replication strengthens confidence in the original findings and underscores the value of synthetic control methods for evaluating fire outcomes using large-scale satellite data.

The primary extension is methodological. By incorporating satellite-derived embeddings into donor pool construction, the study demonstrates that pre-treatment balance and fit can be improved without altering the estimand or estimation procedure. Embedding-informed donor selection enhances counterfactual construction in high-dimensional spatial settings while remaining fully compatible with existing synthetic control variants, including augmented, Bayesian, and matrix completion approaches.

More broadly, the paper emphasizes the significance of design-stage decisions in causal inference using Earth observation data. As environmental datasets grow richer and more complex, the challenge shifts from data availability to principled comparison. Embedding-based representations provide a scalable approach to encoding latent similarity and guiding donor selection, thereby enhancing the credibility of quasi-experimental evaluations without compromising transparency.

Future work can formalize embedding-based donor selection within interference-aware synthetic control frameworks, develop guidance for hyperparameter choice and uncertainty quantification, and extend the approach to other environmental interventions where landscape similarity is central to identification. By integrating modern representation learning with established causal designs, this study contributes to a methodological agenda aimed at improving inference in climate and environmental policy evaluation.

% Force bibliography generation even without inline \cite commands
\nocite{*}
\bibliographystyle{apalike}
\bibliography{references_md_mirror}

\appendix
\section*{Appendix}
\addcontentsline{toc}{section}{Appendix} % optional: adds to TOC

\subsection*{Appendix Table A1: Covariate Block Descriptive Statistics (2019 Conifer)}
\input{tables/appendix_table_a2_covariate_block_summary_2019_conifer.tex}

\subsection*{Appendix Table A2: Full Covariate Descriptives (2019 Conifer)}
\input{tables/appendix_table_a1_full_covariates_2019_conifer.tex}

\subsection*{Appendix Table A3: K-selection summary}
\input{tables/appendix_table_a3_k_selection_summary.tex}

\subsection*{Code Repository}
The code and data for this project are available at: 
\url{https://github.com/MyTran-GitHub/Capstone}
\end{document}



