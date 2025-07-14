
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SiGN

<!-- badges: start -->

<!-- badges: end -->

## Overview

SiGN is an R package that implements the Signals for Good News (SiGN)
model to compute behavioural predictions in common operant choice
paradigms.

## Installation

You can install the development version of SiGN from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jpisklak/SiGN")
```

## Usage

``` r
library(SiGN)

# Load built-in profile
kendall <- choice_params("kendall")

# Generate predictions
kendall_pred <- SiGN(kendall)

# View Results and intermediate calculations
kendall_pred$cp
#> [1] 0.5083678
kendall_pred$details
#>          cp        r_a    r_b r_a_com r_b_com    Big_T     dr_a     dr_b
#> 1 0.5083678 0.05555556 0.0625      NA      NA 16.66667 1.938827 1.666667
#>    dr_avg_a dr_avg_b dr_bonus_a dr_bonus_b  beta_a beta_b sig_a sig_b tr_p_a
#> 1 0.3333333 1.666667   1.333333          0 1.20412      1  TRUE FALSE    0.5
#>   tr_p_b s_delta
#> 1      1       1
```

## Available Data and Profiles

The package includes:

- Built-in parameter profiles — Predefined setups for well-known
  procedures from the literature, including:

  - `"zentall"` (Stagner and Zentall 2010)

  - `"kendall"` (Kendall 1985)

  - `"fantino"` (Fantino 1969)

- Fully vectorised parameter input — All model parameters can be
  customised and passed as vectors, enabling efficient simulation of
  multiple conditions.

- Built-in datasets — Two curated datasets from Dunn et al. (2024):

  - `subopt_full`: the complete dataset of suboptimal choice studies

  - `subopt_avian`: a filtered subset focused on pigeons and starlings

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Fantino_1969" class="csl-entry">

Fantino, E. 1969. “Choice and Rate of Reinforcement.” *Journal of the
Experimental Analysis of Behavior* 12 (5): 723–30.
<https://doi.org/10.1901/jeab.1969.12-723>.

</div>

<div id="ref-Kendall_1985" class="csl-entry">

Kendall, S. B. 1985. “A Further Study of Choice and Percentage
Reinforcement.” *Behavioural Processes* 10 (4): 399–413.
<https://doi.org/10.1016/0376-6357(85)90040-3>.

</div>

<div id="ref-Stagner_Zentall_2010" class="csl-entry">

Stagner, J. P., and T. R. Zentall. 2010. “Suboptimal Choice Behavior by
Pigeons.” *Psychonomic Bulletin & Review* 17 (3): 412–16.
<https://doi.org/10.3758/PBR.17.3.412>.

</div>

</div>
