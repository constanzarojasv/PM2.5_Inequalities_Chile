
> cat("### Table 5. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density a ..." ... [TRUNCATED] 
### Table 5. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density and residential biomass heating on daily PM2.5 concentrations.


> print(kable(tabla_final, format = "markdown", align = "lccc"))


|Predictors                 |   Estimate    |     95% CI      | p value  |
|:--------------------------|:-------------:|:---------------:|:--------:|
|**Fixed Effects**          |               |                 |          |
|(Intercept)                |     23.74     | [-2.73 – 50.20] |  0.069   |
|Year                       |     -1.40     | [-1.71 – -1.10] | < 0.001* |
|altitude                   |     -0.01     | [-0.04 – 0.02]  |  0.326   |
|Poverty (%)                |     0.17      | [-0.42 – 0.77]  |  0.486   |
|Density (inhabitants/km²)  |    -9e-06     | [-0.00 – 0.00]  |  0.952   |
|Season [Winter (GEC)]      |     22.12     | [21.54 – 22.70] | < 0.001* |
|Biomass heaters^a          |    -0.0059    | [-0.02 – 0.00]  |  0.185   |
|Winter × Biomass           |    0.0025     |  [0.00 – 0.00]  |  0.004*  |
|**Random Effects**         |               |                 |          |
|σ² (Residual Variance)     |    165.65     |                 |          |
|τ00 (Between-municipality) |     5.04      |                 |          |
|ICC                        |     0.03      |                 |          |
|**Model Fit**              |               |                 |          |
|Observations               |     10410     |                 |          |
|Marginal R² / Cond. R²     | 0.415 / 0.432 |                 |          |

> cat("\n\n", nota_pie, "\n")


 _Note: CI: Confidence Interval (95%). ICC: Intraclass Correlation Coefficient. Reference category for Season: [Rest of the year]. a: Biomass heaters rate per 10,000 households. * indicate statistical significance (p<0.05)._ 

> sink()
