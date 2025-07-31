# SiGN (development version)

## 2025-07-20
- Updated TL entry probabilities and choice proportions in `subopt_full` for Cunningham & Shahan 2019 (newly digitized values).
- Adding versioning to `subopt_full` and `subopt_avian` data sets.
- Improved "Signal Discrimination" notes in `choice_params()` documentation.

## 2025-07-22
- Updated `choice_params()` to only display recycling info message if three or more arguments differ in length.

## 2025-07-26
- Added function for Lin's Concordance Correlation Coefficient.
- Added function SiGN eval (incomplete)
  - To Do: Add $\phi$, log-likelihood, AIC, and BIC, for Beta Distribution error model. Throw warning at low N.

## 2025-07-28
- Added function to compute (negative) log-Likelihood for a Beta error model
- Finished `SiGN_eval()` function to evaluate model fit with descriptive and likelihood-based metrics

## 2025-07-31
- Added `SiGN_gen_eval()`, a function for applying free parameters to the SiGN model and returning model evaluation metrics.
