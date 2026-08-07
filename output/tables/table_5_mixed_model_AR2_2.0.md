### Table 3. Results of the Linear Mixed Model (LMM, AR(2) residual correlation) evaluating the effect of seasonality, year, multidimensional poverty, population density, altitude, and residential biomass heating on daily PM2.5 concentrations.



|Predictors                      |     Estimate      |        95% CI         |  DF  | p value  |
|:-------------------------------|:-----------------:|:---------------------:|:----:|:--------:|
|**Fixed Effects**               |                   |                       |  NA  |          |
|(Intercept)                     |     -2627.04      | [-3897.57 – -1356.52] | 9772 | < 0.001* |
|Year                            |       -1.31       |    [-1.94 – -0.68]    | 9772 | < 0.001* |
|Season [Winter (GEC)]           |       19.83       |    [18.70 – 20.96]    | 9772 | < 0.001* |
|Biomass heaters^a               |      -0.0060      |    [-0.02 – 0.00]     |  5   |  0.182   |
|Poverty (%)                     |       0.16        |    [-0.43 – 0.75]     |  5   |  0.509   |
|Density (1,000 inhabitants/km²) |       -0.00       |    [-0.38 – 0.37]     |  5   |  0.987   |
|Altitude (per 100m)             |       -1.33       |    [-4.37 – 1.72]     |  5   |  0.313   |
|Winter × Biomass                |      0.0029       |    [-0.00 – 0.01]     | 9772 |  0.087   |
|**Model Fit**                   |                   |                       |  NA  |          |
|AIC / BIC                       | 71471.2 / 71557.5 |                       |  NA  |          |
|ICC                             |       0.03        |                       |  NA  |          |
|Observations                    |       9785        |                       |  NA  |          |
|Marginal R² / Cond. R²          |   0.368 / 0.385   |                       |  NA  |          |


 _Note: CI: Confidence Interval (95%). DF: denominator degrees of freedom per term (nlme::lme) -- note the much smaller DF for municipality-level, time-invariant predictors (poverty, density, altitude, biomass) compared to day-level predictors (year, winter, winter x biomass), reflecting that the former are effectively estimated from 10 municipalities, not 10,410 daily records. AR(2): autoregressive order-2 correlation structure on residuals within each municipality, selected over AR(1) via likelihood ratio test (see comparison above). Reference category for Season: [Rest of the year]. a: Biomass heaters rate per 10,000 households. * indicate statistical significance (p<0.05)._ 
