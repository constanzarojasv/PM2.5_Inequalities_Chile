# ==============================================================================
# 03. MIXED MODELS ANALYSIS
# Purpose: Run Linear Mixed Models (LMM) and export summary tables
# ==============================================================================

source("processing/00_setup_functions.R", encoding = "UTF-8")


# 1. Load Clean Data
df_analisis <- read_rds("input/data_processed/datos_analisis_final.rds")

# 2. Data Preparation for Modeling
datos_modelo <- df_analisis %>%
  mutate(
    year_num = as.numeric(anio),
    winter = relevel(as.factor(es_invierno), ref = "Rest of year")
  )

# ==============================================================================
# MODEL 1: INTERACTION (Season * Biomass)
# ==============================================================================
print("--- RUNNING MODEL 1: INTERACTION ---")

modelo_interaccion <- lmer(
  mp25_prom_valid ~ year_num + winter * biomass_total + (1 | comuna), 
  data = datos_modelo
)

# ==============================================================================
# MODEL 2: ADJUSTED (Controlling for Poverty, Density, Altitude)
# ==============================================================================
print("--- RUNNING MODEL 2: FULLY ADJUSTED ---")

modelo_completo <- lmer(
  mp25_prom_valid ~ year_num + altitude + poverty + density + winter * biomass_total + (1 | comuna), 
  data = datos_modelo
)

summary(modelo_completo)

# ==============================================================================
# BUILD CUSTOM TABLE
# ==============================================================================

# A. Extraer Efectos Fijos
fixed_effects <- tidy(modelo_completo, conf.int = TRUE) %>%
  filter(effect == "fixed") %>% 
  mutate(
    p_label = case_when(
      p.value < 0.001 ~ "< 0.001*",
      p.value < 0.05  ~ paste0(sprintf("%.3f", p.value), "*"),
      TRUE            ~ sprintf("%.3f", p.value)
    ),
    ci_label = paste0("[", sprintf("%.2f", conf.low), " \u2013 ", sprintf("%.2f", conf.high), "]"),
    estimate_label = case_when(
      term == "density" ~ as.character(signif(estimate, 1)), # Evita que quede en 0.00
      term == "biomass_total" ~ sprintf("%.4f", estimate),
      term == "winterWinter:biomass_total" ~ sprintf("%.4f", estimate),
      TRUE ~ sprintf("%.2f", estimate)
    ),
    term_clean = case_when(
      term == "(Intercept)" ~ "(Intercept)",
      term == "year_num" ~ "Year",
      term == "poverty" ~ "Poverty (%)",
      term == "density" ~ "Density (inhabitants/km²)",
      term == "winterWinter" ~ "Season [Winter (GEC)]",
      term == "biomass_total" ~ "Biomass heaters^a",
      term == "winterWinter:biomass_total" ~ "Winter × Biomass",
      TRUE ~ term
    )
  ) %>%
  select(Predictors = term_clean, Estimate = estimate_label, `95% CI` = ci_label, `p value` = p_label)

# B. Extraer Efectos Aleatorios
re_raw <- tidy(modelo_completo) %>% filter(effect == "ran_pars")
var_residual <- (re_raw %>% filter(group == "Residual") %>% pull(estimate))^2
var_comuna   <- (re_raw %>% filter(group == "comuna") %>% pull(estimate))^2
icc_val      <- performance::icc(modelo_completo)$ICC_adjusted

# C. Extraer Ajuste del Modelo
r2_vals <- performance::r2(modelo_completo)
r2_marg <- r2_vals$R2_marginal
r2_cond <- r2_vals$R2_conditional
n_obs   <- nobs(modelo_completo)

# D. Construir filas extra
extra_rows <- tibble::tribble(
  ~Predictors, ~Estimate, ~`95% CI`, ~`p value`,
  "**Random Effects**", "", "", "",
  "σ² (Residual Variance)", sprintf("%.2f", var_residual), "", "",
  "τ00 (Between-municipality)", sprintf("%.2f", var_comuna), "", "",
  "ICC", sprintf("%.2f", icc_val), "", "",
  "**Model Fit**", "", "", "",
  "Observations", as.character(n_obs), "", "",
  "Marginal R² / Cond. R²", paste0(sprintf("%.3f", r2_marg), " / ", sprintf("%.3f", r2_cond)), "", ""
)

# E. Unir tabla final
tabla_final <- bind_rows(
  tibble(Predictors = "**Fixed Effects**", Estimate = "", `95% CI` = "", `p value` = ""),
  fixed_effects,
  extra_rows
)

# ==============================================================================
# EXPORT TO MARKDOWN
# ==============================================================================

nota_pie <- "_Note: CI: Confidence Interval (95%). ICC: Intraclass Correlation Coefficient. Reference category for Season: [Rest of the year]. a: Biomass heaters rate per 10,000 households. * indicate statistical significance (p<0.05)._"

sink("output/tables/table_3_mixed_model.md")
cat("### Table 5. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density and residential biomass heating on daily PM2.5 concentrations.\n\n")
print(kable(tabla_final, format = "markdown", align = "lccc"))
cat("\n\n", nota_pie, "\n")
sink()

print("Model saved and full Table 5 generated in 'output/tables/table_5_mixed_model.md'")


# ==============================================================================
# MODELO 3: COMPLETAMENTE AJUSTADO Y REESCALADO (SIN WARNINGS)
# ==============================================================================
print("--- RUNNING MODEL 3: FULLY ADJUSTED (RE-SCALED) ---")

# 1. Ajustar las variables en los datos (se agregan a la base que ya tienes)
datos_modelo <- datos_modelo %>%
  mutate(
    year_centrado = year_num - 2022,       # Ajuste de escala: 2022 será el año 0
    density_miles = density / 1000,         # Ajuste de escala: miles de hab/km2
    alt_100m = altitude / 100
  )

# 2. Correr el modelo con las nuevas variables
modelo_completo_rescaled <- lmer(
  mp25_prom_valid ~ year_centrado + poverty + density_miles + alt_100m + winter * biomass_total + (1 | comuna), 
  data = datos_modelo
)

# ==============================================================================
# BUILD CUSTOM TABLE (RESCALED)
# ==============================================================================

# A. Extraer Efectos Fijos
fixed_effects_geo <- tidy(modelo_completo_rescaled, conf.int = TRUE) %>%
  filter(effect == "fixed") %>% 
  mutate(
    p_label = case_when(
      p.value < 0.001 ~ "< 0.001*",
      p.value < 0.05  ~ paste0(sprintf("%.3f", p.value), "*"),
      TRUE            ~ sprintf("%.3f", p.value)
    ),
    estimate_label = case_when(
      term == "biomass_total" ~ sprintf("%.4f", estimate),
      term == "winterWinter:biomass_total" ~ sprintf("%.4f", estimate),
      TRUE ~ sprintf("%.2f", estimate) 
    ),
    ci_label = paste0("[", sprintf("%.2f", conf.low), " \u2013 ", sprintf("%.2f", conf.high), "]"),
    term_clean = case_when(
      term == "(Intercept)" ~ "(Intercept)",
      term == "year_centrado" ~ "Year",
      term == "poverty" ~ "Poverty (%)",
      term == "density_miles" ~ "Density (1,000 inhabitants/km²)", 
      term == "alt_100m" ~ "Altitude (per 100m)", # Etiqueta para altitud
      term == "winterWinter" ~ "Season [Winter (GEC)]",
      term == "biomass_total" ~ "Biomass heaters^a",
      term == "winterWinter:biomass_total" ~ "Winter × Biomass",
      TRUE ~ term
    )
  ) %>%
  select(Predictors = term_clean, Estimate = estimate_label, `95% CI` = ci_label, `p value` = p_label)

# B. Extraer Efectos Aleatorios
re_raw_geo <- tidy(modelo_completo_rescaled) %>% filter(effect == "ran_pars")
var_residual_geo <- (re_raw_geo %>% filter(group == "Residual") %>% pull(estimate))^2
var_comuna_geo   <- (re_raw_geo %>% filter(group == "comuna") %>% pull(estimate))^2
icc_val_geo      <- performance::icc(modelo_completo_rescaled)$ICC_adjusted

# C. Extraer Ajuste del Modelo
r2_vals_geo <- performance::r2(modelo_completo_rescaled)
r2_marg_geo <- r2_vals_geo$R2_marginal
r2_cond_geo <- r2_vals_geo$R2_conditional
n_obs_geo   <- nobs(modelo_completo_rescaled)

# D. Construir filas extra
extra_rows_geo <- tibble::tribble(
  ~Predictors, ~Estimate, ~`95% CI`, ~`p value`,
  "**Random Effects**", "", "", "",
  "σ² (Residual Variance)", sprintf("%.2f", var_residual_geo), "", "",
  "τ00 (Between-municipality)", sprintf("%.2f", var_comuna_geo), "", "",
  "ICC", sprintf("%.2f", icc_val_geo), "", "",
  "**Model Fit**", "", "", "",
  "Observations", as.character(n_obs_geo), "", "",
  "Marginal R² / Cond. R²", paste0(sprintf("%.3f", r2_marg_geo), " / ", sprintf("%.3f", r2_cond_geo)), "", ""
)

# E. Unir tabla final
tabla_final_geo <- bind_rows(
  tibble(Predictors = "**Fixed Effects**", Estimate = "", `95% CI` = "", `p value` = ""),
  fixed_effects_geo,
  extra_rows_geo
)

# ==============================================================================
# EXPORT TO MARKDOWN
# ==============================================================================

nota_pie_geo <- "_Note: CI: Confidence Interval (95%). ICC: Intraclass Correlation Coefficient. Reference category for Season: [Rest of the year]. a: Biomass heaters rate per 10,000 households. * indicate statistical significance (p<0.05)._"

sink("output/tables/table_4_mixed_model_geo.md")
cat("### Table 4. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density, residential biomass heating, and altitude on daily PM2.5 concentrations.\n\n")
print(kable(tabla_final_geo, format = "markdown", align = "lccc"))
cat("\n\n", nota_pie_geo, "\n")
sink()

print("Model saved and full Table 4 generated in 'output/tables/table_4_mixed_model_geo.md'")

rm(list = ls())