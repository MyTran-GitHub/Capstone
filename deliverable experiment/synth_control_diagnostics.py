import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

np.random.seed(42)

# Simulate synthetic control weights for 20 treated units and 30 controls
n_treated = 20
n_controls = 30
weights = np.abs(np.random.randn(n_treated, n_controls))
weights = weights / weights.sum(axis=1, keepdims=True)

# Simulate pre-treatment outcomes (10 time points)
n_time = 10
pre_treated = np.random.normal(0, 1, (n_treated, n_time))
pre_synth = pre_treated + np.random.normal(0, 0.2, (n_treated, n_time))

# Simulate covariate balance (e.g., elevation)
covariate_treated = np.random.normal(1000, 50, n_treated)
covariate_synth = covariate_treated + np.random.normal(0, 10, n_treated)
covariate_pool = np.random.normal(1000, 60, n_controls)

# 1. Weight Distribution Heatmap
plt.figure(figsize=(10,6))
sns.heatmap(weights, cmap='Blues', cbar_kws={'label': 'Weight'})
plt.xlabel('Control Unit')
plt.ylabel('Treated Unit')
plt.title('Synthetic Control Weights Heatmap')
plt.tight_layout()
plt.savefig('deliverable experiment/weights_heatmap.png')
plt.close()

# 2. Pre-Treatment Fit Example
plt.figure(figsize=(8,5))
plt.plot(range(n_time), pre_treated[0], label='Treated', marker='o')
plt.plot(range(n_time), pre_synth[0], label='Synthetic Control', marker='x')
plt.xlabel('Pre-Treatment Time')
plt.ylabel('Outcome')
plt.title('Pre-Treatment Fit: Example Treated Unit')
plt.legend()
plt.tight_layout()
plt.savefig('deliverable experiment/pre_treatment_fit_example.png')
plt.close()

# 3. Covariate Balance Boxplot
plt.figure(figsize=(7,5))
sns.boxplot(data=[covariate_treated, covariate_synth, covariate_pool],
            palette=['red','blue','gray'])
plt.xticks([0,1,2], ['Treated', 'Synthetic Control', 'Control Pool'])
plt.ylabel('Elevation')
plt.title('Covariate Balance: Elevation')
plt.tight_layout()
plt.savefig('deliverable experiment/covariate_balance_boxplot.png')
plt.close()

# 4. Regularization Sensitivity (simulated)
reg_params = np.linspace(0, 1, 10)
fit = 1 - 0.2*reg_params + np.random.normal(0, 0.01, len(reg_params))
sparsity = 1 - reg_params + np.random.normal(0, 0.02, len(reg_params))
plt.figure(figsize=(8,5))
plt.plot(reg_params, fit, label='Pre-Treatment Fit', marker='o')
plt.plot(reg_params, sparsity, label='Weight Sparsity', marker='x')
plt.xlabel('Regularization Parameter')
plt.ylabel('Metric')
plt.title('Regularization Sensitivity')
plt.legend()
plt.tight_layout()
plt.savefig('deliverable experiment/regularization_sensitivity.png')
plt.close()

print('All synthetic control diagnostic plots saved in deliverable experiment/')
