
> cat("### Table 4. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density,  ..." ... [TRUNCATED] 
### Table 4. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density, residential biomass heating, and altitude on daily PM2.5 concentrations.


> print(kable(tabla_final_geo, format = "markdown", align = "lccc"))


|Predictors                      |   Estimate    |        95% CI         | p value  |
|:-------------------------------|:-------------:|:---------------------:|:--------:|
|**Fixed Effects**               |               |                       |          |
|(Intercept)                     |   -2631.61    | [-3267.64 – -1995.59] | < 0.001* |
|Year                            |     -1.31     |    [-1.63 – -1.00]    | < 0.001* |
|Poverty (%)                     |     0.15      |    [-0.45 – 0.75]     |  0.549   |
|Density (1,000 inhabitants/km²) |     0.00      |    [-0.38 – 0.38]     |  0.990   |
|Altitude (per 100m)             |     -1.32     |    [-4.42 – 1.79]     |  0.325   |
|Season [Winter (GEC)]           |     21.85     |    [21.26 – 22.44]    | < 0.001* |
|Biomass heaters^a               |    -0.0060    |    [-0.02 – 0.00]     |  0.184   |
|Winter × Biomass                |    0.0030     |     [0.00 – 0.00]     | < 0.001* |
|**Random Effects**              |               |                       |          |
|σ² (Residual Variance)          |    162.19     |                       |          |
|τ00 (Between-municipality)      |     5.17      |                       |          |
|ICC                             |     0.03      |                       |          |
|**Model Fit**                   |               |                       |          |
|Observations                    |     9785      |                       |          |
|Marginal R² / Cond. R²          | 0.413 / 0.431 |                       |          |

> cat("\n\n", nota_pie_geo, "\n")


 _Note: CI: Confidence Interval (95%). ICC: Intraclass Correlation Coefficient. Reference category for Season: [Rest of the year]. a: Biomass heaters rate per 10,000 households. * indicate statistical significance (p<0.05)._ 

> sink()
