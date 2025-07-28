
<!-- README.md is generated from README.Rmd. Please edit that file -->

<table>

<tr>

<td style="vertical-align: middle;">

<a href="https://sign-r.github.io/SiGN/">
<img src="man/figures/logo.svg" width="120"/> </a>
</td>

<td style="vertical-align: middle; padding-left: 15px;">

<!-- <h2>**SiGN**</h2> -->

<span style="font-size: 200%; font-weight: bold;">SiGN</span>
<p>

<em>The Signal for Good News Model R Package</em>
</p>

</td>

</tr>

</table>

<!-- # SiGN <a href="https://jpisklak.github.io/SiGN/"><img src="man/figures/logo.png" align="right" height="139" alt="SiGN website" /></a> -->

<!-- badges: start -->

<!-- badges: end -->

## Overview

Designed for researchers in behavioural science, SiGN is an R package
that applies the Signal for Good News (SiGN) model to predict choice
behaviour in operant conditioning settings.

## Installation

You can install SiGN’s development version from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("https://github.com/SiGN-R/SiGN")
```

Then load the package with:

``` r
library(SiGN)
```

## Usage

To get started with the package, begin by reading the [Get
Started](articles/SiGN.html) article, which introduces the core
functionality of the SiGN package. Then, explore the following articles
to see other uses of the package:

- [Revisiting Some Classic Studies](articles/squires_fantino.html)
  - A re-analysis of classic concurrent-chain experiments using SiGN
    predictions.
  - Illustrates how to compute model predictions across a range of
    parameter values.
- [Generating SiGN Model Predictions from a Data
  Frame](articles/batch-predict.html)
  - Demonstrates how to generate multiple SiGN model predictions from a
    data frame of parameters.

## Ready-to-Use Suboptimal Choice Data and Profiles

The package includes:

- Built-in datasets — Two curated datasets from Dunn et al. (2024):

  - `subopt_full`: the complete dataset of suboptimal choice studies.

  - `subopt_avian`: a filtered subset focused on pigeons and starlings.

- Built-in parameter profiles — Predefined setups for well-known
  procedures from the literature, including:

  - `"zentall"` (Stagner and Zentall, 2010)

  - `"kendall"` (Kendall, 1985)

  - `"fantino"` (Fantino, 1969)

- Fully vectorised parameter input — All model parameters can be
  customised and passed as vectors, enabling efficient simulation of
  multiple conditions.

## Try the Package Without Installing R

[![Launch
Calculator](https://img.shields.io/badge/try%20it-online%20calculator-brightgreen)](https://jpisklak.shinyapps.io/SiGN_Calc/)

You can generate predictions with the SiGN model in your browser using
the free, interactive SiGN Calculator — no installation or coding
required.

Set up custom choice scenarios and instantly view predicted preferences
and model diagnostics. It’s a great way to explore how delay reduction
and reinforcement schedules influence decision-making.

📎 This is ideal for students, collaborators, or researchers who want to
explore the model without any writing code.

## Planned Features

The SiGN package is currently under active development. Future updates
will include functionality for model evaluation and comparison,
including:

- Descriptive error metrics (e.g., RMSE, MAE, residual-based $R^2$)

- Log-likelihood computation using a Beta-distributed error model
  tailored to choice proportion data

- Model comparison tools:

  - Akaike Information Criterion (AIC)

  - Bayesian Information Criterion (BIC)

These additions will allow users to assess model fit in a more
principled and quantitative manner, especially when comparing models
with and without free parameters.

Stay tuned for updates, and feel free to suggest features or contribute!

## References

Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024).
Suboptimal choice: A review and quantification of the signal for good
news (SiGN) model. *Psychological Review*. *131*(1), 58-78.
<https://doi.org/10.1037/rev0000416>

Fantino, E. (1969). Choice and rate of reinforcement. *Journal of the
Experimental Analysis of Behavior*, *12*(5), 723–730.
<https://doi.org/10.1901/jeab.1969.12-723>

Kendall, S. B. (1985). A further study of choice and percentage
reinforcement. *Behavioural Processes*, *10*(4), 399–413.
<https://doi.org/10.1016/0376-6357(85)90040-3>

Stagner, J. P., & Zentall, T. R. (2010). Suboptimal choice behavior by
pigeons. *Psychonomic Bulletin & Review*, *17*(3), 412–416.
<https://doi.org/10.3758/PBR.17.3.412>
