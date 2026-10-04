# Marketing-mix-model
Custom Marketing Mix Model in R using nonlinear regression, adstock and saturation effects, with bootstrap validation and ROI analysis.
# Marketing Mix Modeling with Nonlinear Regression

This project was developed as my Bachelor's thesis in Statistics and Information Management at the University of Milano-Bicocca.

The objective was to build a custom Marketing Mix Model (MMM) to quantify the impact of baseline, promotional and media variables on weekly sales in the skincare sector.

The analysis was developed in R using real-world marketing and sales data. Due to confidentiality constraints, the original dataset is not included in this repository and all company, brand and campaign references have been anonymized.

---

## Project Overview

Marketing Mix Modeling is an econometric approach used to estimate how different marketing activities contribute to sales performance.

The project focuses on three main objectives:

- estimate the contribution of baseline and promotional variables;
- quantify the incremental effect of different media channels;
- evaluate media effectiveness through contribution analysis and ROI.

The final model combines multiplicative baseline effects with nonlinear media transformations in order to capture real-world advertising dynamics.

---

## Methodology

The project followed an iterative modeling process composed of:

1. Data preparation and exploratory analysis
2. Seasonality estimation
3. Media variable aggregation
4. Adstock transformation
5. Saturation modeling
6. Nonlinear parameter estimation
7. Model diagnostics
8. Bootstrap validation
9. Contribution decomposition
10. ROI analysis

---

## Nonlinear Marketing Mix Model

The final model was estimated using nonlinear least squares with the Levenberg-Marquardt algorithm through the `nlsLM` function from the `minpack.lm` R package.

The model combines:

### Baseline variables

- seasonality
- base price
- product distribution / availability
- promotional intensity

These variables interact through a multiplicative component representing baseline sales.

### Media variables

Media channels were modeled using nonlinear transformations designed to capture:

- **Carryover effect**: advertising continues to influence consumers after the initial exposure.
- **Saturation effect**: incremental advertising effectiveness decreases as media pressure increases.

Adstock transformations were applied over multiple temporal lags, while nonlinear saturation functions were used to represent diminishing returns.

---

## Modeling Challenges

One of the main challenges of the project was the large number of parameters required to model several media channels simultaneously.

During model development, this generated:

- multicollinearity between parameter estimates;
- instability in nonlinear saturation curves;
- sensitivity to initial parameter values;
- convergence problems during optimization.

To improve model stability, several strategies were adopted.

Related media channels were grouped into broader categories when their business interpretation and behavior were sufficiently similar.

Transformation parameters were also estimated separately for individual media channels before being used in the final model. This reduced parameter correlation and improved convergence.

The original nonlinear least squares algorithm (`nls`) was replaced with the Levenberg-Marquardt implementation (`nlsLM`), which proved more robust during optimization.

---

## Model Validation

Model performance was evaluated using multiple metrics and diagnostic tests.

The final model achieved approximately:

| Metric | Result |
|---|---:|
| R² | 0.79 |
| MAPE | 8% |

Model stability was further evaluated using **1,000 bootstrap replications**.

The bootstrap analysis produced an average R² of approximately **0.79**, with a 95% confidence interval between approximately **0.73 and 0.85**.

Additional diagnostics included:

- residual analysis;
- Q-Q plots;
- Breusch-Pagan test;
- parameter correlation analysis;
- RMSE and MAE evaluation.

---

## Business Interpretation

The final model was used to decompose predicted sales into contributions attributable to:

- baseline demand;
- promotions;
- TV advertising;
- paid social / online video;
- search marketing;
- competitor media pressure.

Media contributions were then combined with investment data to estimate channel-level ROI.

The objective was therefore not only to obtain a statistically reliable model, but also to translate model outputs into interpretable business insights.

---

## Linear Regression Benchmark

In addition to the nonlinear model, a Marketing Mix Model based on linear regression was developed following a standard data-mining pipeline.

This model was used as a simpler benchmark and helped highlight the limitations of purely linear approaches when modeling advertising carryover and diminishing returns.

---

## Technologies

- R
- RStudio
- `minpack.lm`
- `prophet`
- `ggplot2`
- `dplyr`
- `tidyverse`
- `boot`
- `lmtest`
- `corrplot`

---

## Repository Structure

```text
marketing-mix-model/
│
├── R/
│   ├── 01_data_preparation.R
│   ├── 02_exploratory_analysis.R
│   ├── 03_media_transformations.R
│   ├── 04_parameter_estimation.R
│   ├── 05_final_model.R
│   ├── 06_model_validation.R
│   └── 07_contribution_roi.R
│
├── benchmark/
│   └── linear_mmm.Rmd
│
├── figures/
│
├── docs/
│   └── methodology.md
│
└── README.md
