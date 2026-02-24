# Monte Carlo Simulation of a Hypothesis Test for Binomial Proportions

This repository contains a Monte Carlo simulation study of the **one-sample z-test for a proportion** when data are **binomially distributed**. The goal is to explore how well the **normal approximation** performs under different settings (sample size, true proportion, significance level, and one- vs two-sided alternatives), and to visualize the resulting **p-value behavior**, **Type I error**, **Type II error**, and **power**.

---

## Project idea

We simulate
\[
X \sim \mathrm{Binomial}(n, p)
\]
and test
\[
H_0: p = p_0 \quad \text{vs.} \quad H_a: p \neq p_0 \; (\text{or one-sided}).
\]

For each simulated dataset, we compute the **z statistic**
\[
\hat p = \frac{X}{n}, \qquad
Z = \frac{\hat p - p_0}{\sqrt{\frac{p_0(1-p_0)}{n}}}
\]
then convert it to a **p-value** according to the chosen alternative. Repeating this many times gives an empirical approximation to:
- the distribution of p-values under \(H_0\),
- **Type I error** at level \(\alpha\),
- **Type II error** under specified alternatives \(p=p_1\),
- **power** as a function of \(p\). 

---

## Outputs you should expect to see

From the proposal, the core outputs are:
- A **table** reporting estimated **Type I error**, **Type II error**, and **power**
- A **histogram of simulated p-values** with a vertical line at the chosen significance level \(\alpha\)
- (Planned) a **Shiny app** so users can interactively adjust parameters and instantly see how results change :contentReference[oaicite:3]{index=3}

---

