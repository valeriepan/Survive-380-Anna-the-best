# Monte Carlo Simulation of a Hypothesis Test for Binomial Proportions

This repository contains a Monte Carlo simulation study of the **one-sample z-test for a proportion** when data are **binomially distributed**. The goal is to explore how well the **Monte Carlo Estimation** performs under different settings (sample size, true proportion, significance level, and one- vs two-sided alternatives), and to visualize the resulting **p-value behavior**, **Type I error**, **Type II error**, and **power**.

---

## Project idea

We simulate $X \sim \mathrm{Binomial}(n, p)$ and test
- $H_0: p = p_0$
- v.s. $H_a: p \neq p_0$ (or one-sided alternatives).

For each simulated dataset, we compute the test-statistic:

$$
\hat p = \frac{X}{n}, \qquad
Z = \frac{\hat p - p_0}{\sqrt{\frac{p_0(1-p_0)}{n}}}
$$

Then we convert $Z$ to a **p-value** according to the chosen alternative. Repeating this many times gives Monte Carlo estimates of:

- the distribution of p-values under $H_0$,
- **Type I error** at level $\alpha$,
- **Type II error** under specified alternatives (e.g., $p=p_1$),
- **power** as a function of $p$.

---

## Outputs you should expect to see

- A **table** reporting estimated **Type I error**, **Type II error**, and **power**
- A **histogram of simulated p-values** with a vertical line at the chosen significance level $\alpha$
- A **Shiny app** so users can interactively adjust parameters and instantly see how results change

You can download the contents of this `R` package using

```
remotes::install_github("valeriepan/Survive380AnnaTheBest")
```

---

## Link to Shiny App:
https://019cea6f-68fe-92d5-b9e9-6feaeb32de6f.share.connect.posit.cloud
