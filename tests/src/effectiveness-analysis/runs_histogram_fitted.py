#!/usr/bin/env python3
import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
from scipy.optimize import minimize

data = np.array([int(line.strip()) for line in open(sys.argv[1]) if line.strip()])
n = len(data)
mean, var = data.mean(), data.var()

# --- Fit Negative Binomial via MLE ---
# NB parameterized as: number of successes r (can be non-integer), prob p
# Mean = r*(1-p)/p, Var = r*(1-p)/p^2
# Method-of-moments starting point:
p_mom = mean / var
r_mom = mean * p_mom / (1 - p_mom)

def neg_log_likelihood(params):
    r, p = params
    if r <= 0 or p <= 0 or p >= 1:
        return np.inf
    return -np.sum(stats.nbinom.logpmf(data, r, p))

result = minimize(neg_log_likelihood, [r_mom, p_mom], method='Nelder-Mead',
                  options={'xatol': 1e-6, 'fatol': 1e-6, 'maxiter': 10000})
r_fit, p_fit = result.x

# --- Percentiles ---
percentiles = [50, 75, 90, 95, 99]
empirical = np.percentile(data, percentiles)
fitted_ppf = [stats.nbinom.ppf(pct/100, r_fit, p_fit) for pct in percentiles]

print(f"n={n}  mean={mean:.1f}  median={np.median(data):.0f}  std={data.std():.1f}  max={data.max()}")
print(f"\nFitted Negative Binomial: r={r_fit:.3f}, p={p_fit:.4f}")
print(f"  Interpretation: ~{r_fit:.1f} independent events to trigger, each with p={p_fit:.4f} per try")
print(f"  Fitted mean={r_fit*(1-p_fit)/p_fit:.1f}  fitted std={np.sqrt(r_fit*(1-p_fit)/p_fit**2):.1f}")
print(f"\n{'Percentile':>10}  {'Empirical':>10}  {'NB fit':>10}  {'--tries N covers X% of seeds'}")
print("-" * 60)
for pct, emp, fit in zip(percentiles, empirical, fitted_ppf):
    print(f"{pct:>9}%  {emp:>10.0f}  {fit:>10.0f}")

# --- Plot ---
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# Left: histogram + fitted PMF
ax = axes[0]
counts, edges, _ = ax.hist(data, bins=60, density=True, alpha=0.6, label='Empirical')
xs = np.arange(0, data.max() + 1)
pmf = stats.nbinom.pmf(xs, r_fit, p_fit)
ax.plot(xs, pmf, 'r-', linewidth=1.5, label=f'NB fit (r={r_fit:.2f}, p={p_fit:.4f})')
ax.set_xlabel('Tries needed')
ax.set_ylabel('Density')
ax.set_title('Histogram vs Fitted Negative Binomial')
ax.legend()

# Right: empirical CDF + fitted CDF, with percentile guidelines
ax = axes[1]
xs_sorted = np.sort(data)
cdf_empirical = np.arange(1, n+1) / n
ax.plot(xs_sorted, cdf_empirical, 'b-', linewidth=1.5, label='Empirical CDF')
xs_fit = np.arange(0, data.max() + 1)
cdf_fit = stats.nbinom.cdf(xs_fit, r_fit, p_fit)
ax.plot(xs_fit, cdf_fit, 'r--', linewidth=1.5, label='NB fit CDF')
for pct in [0.90, 0.95, 0.99]:
    x_emp = np.percentile(data, pct * 100)
    ax.axhline(pct, color='gray', linewidth=0.7, linestyle=':')
    ax.axvline(x_emp, color='gray', linewidth=0.7, linestyle=':')
    ax.text(x_emp + 5, pct - 0.03, f'{int(pct*100)}th\n({x_emp:.0f})', fontsize=8)
ax.set_xlabel('--tries N')
ax.set_ylabel('Fraction of seeds fully covered')
ax.set_title('CDF: coverage vs --tries N')
ax.legend()

plt.tight_layout()
plt.savefig('tries_fit.png', dpi=150)
print(f"\nSaved tries_fit.png")
