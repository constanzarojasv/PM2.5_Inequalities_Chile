# ==============================================================================
# 03. MIXED MODELS ANALYSIS
# Purpose: Run Linear Mixed Models (LMM) and export summary tables
# ==============================================================================

source("processing/analisis2.0/00_setup_functions.R", encoding = "UTF-8")


# 1. Load Clean Data
df_analisis <- read_rds("input/data_processed/datos_analisis_final_2.0.rds")

# 2. Data Preparation for Modeling
# Exclude daily records from station-years that don't meet the Decreto
# 12/2011 completeness criterion (>=11 valid months, or 9-10 with
# imputation), so the LMM uses the same "eligible" data as Tables 1/2/S1 and
# the maps -- otherwise an incomplete year (e.g. Cerrillos 2022, missing
# Jan-Apr) could still bias the pooled daily-level trend/season estimates.
n_filas_excluidas <- df_analisis %>% filter(!anio_conforme) %>% nrow()
print(paste("--- Excluding", n_filas_excluidas,
            "daily records from non-compliant station-years (Decreto 12/2011) ---"))

datos_modelo <- df_analisis %>%
  filter(anio_conforme) %>%
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
    ci_label = paste0("[", sprintf("%.2f", conf.low), " – ", sprintf("%.2f", conf.high), "]"),
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

sink("output/tables/table_3_mixed_model_2.0.md")
cat("### Table 5. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density and residential biomass heating on daily PM2.5 concentrations.\n\n")
print(kable(tabla_final, format = "markdown", align = "lccc"))
cat("\n\n", nota_pie, "\n")
sink()

print("Model saved and full table generated in 'output/tables/table_3_mixed_model_2.0.md'")


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
    ci_label = paste0("[", sprintf("%.2f", conf.low), " – ", sprintf("%.2f", conf.high), "]"),
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

sink("output/tables/table_4_mixed_model_geo_2.0.md")
cat("### Table 4. Results of the Linear Mixed Model (LMM) evaluating the effect of seasonality, year, multidimensional poverty, population density, residential biomass heating, and altitude on daily PM2.5 concentrations.\n\n")
print(kable(tabla_final_geo, format = "markdown", align = "lccc"))
cat("\n\n", nota_pie_geo, "\n")
sink()

print("Model saved and full table generated in 'output/tables/table_4_mixed_model_geo_2.0.md'")

# ==============================================================================
# MODELO 4: COMPLETAMENTE AJUSTADO, REESCALADO Y AUTORREGRESIVO
# ==============================================================================
# Responde al comentario del Revisor 2 sobre autocorrelacion temporal en el
# LMM (Tabla 3): se agrega una estructura de correlacion AR(1) sobre los
# residuos DENTRO de cada comuna (ordenados por fecha_local), en vez de
# asumir que cada dia es independiente. Mismos predictores y mismo nivel
# diario que el Modelo 3 -- el unico cambio es el supuesto de correlacion
# de los residuos.

library(nlme)

print("--- RUNNING MODEL 4: FULLY ADJUSTED, RE-SCALED, AUTOREGRESSIVE (AR1) ---")

modelo_ar1 <- lme(
  mp25_prom_valid ~ year_centrado + winter * biomass_total + poverty + density_miles + alt_100m,
  random = ~ 1 | comuna,
  correlation = corAR1(form = ~ fecha_local | comuna),  # AR(1) DENTRO de cada comuna, ordenado por fecha
  data = datos_modelo
)

summary(modelo_ar1)

acf(residuals(modelo_ar1, type = "normalized"))
qqnorm(residuals(modelo_ar1, type = "normalized")); qqline(residuals(modelo_ar1, type = "normalized"))
hist(residuals(modelo_ar1, type = "normalized"), breaks = 30)
pacf(residuals(modelo_ar1, type = "normalized"))

# ==============================================================================
# MODELO 5: COMPLETAMENTE AJUSTADO, REESCALADO Y AUTORREGRESIVO DE ORDEN 2
# ==============================================================================

modelo_ar2 <- lme(
  mp25_prom_valid ~ year_centrado + winter * biomass_total + poverty + density_miles + alt_100m,
  random = ~ 1 | comuna,
  correlation = corARMA(form = ~ fecha_local | comuna, p = 2, q = 0),  # AR(2) DENTRO de cada comuna, ordenado por fecha
  data = datos_modelo
)

summary(modelo_ar2)

class(datos_modelo$winter)
class(datos_modelo$year_centrado)
class(datos_modelo$alt_100m)

anova(modelo_ar2)

acf(residuals(modelo_ar2, type = "normalized"))
qqnorm(residuals(modelo_ar2, type = "normalized")); qqline(residuals(modelo_ar2, type = "normalized"))
hist(residuals(modelo_ar2, type = "normalized"), breaks = 30)
pacf(residuals(modelo_ar2, type = "normalized"))

anova(modelo_ar1, modelo_ar2)

# ------------------------------------------------------------------------------
# Tirar medida de bondad del ajuste. Agregar justificacion de los grados de libertad.
#
# la tabla de efectos fijos trae una columna DF distinta POR TERMINO, chica
# para los predictores de nivel-comuna y grande para los que varian dia a
# dia (year_centrado, winter, winter:biomass_total). Se extrae y se muestra
# explicitamente en vez de solo el p-value, para que la limitacion quede
# transparente con numeros concretos.
tabla_gl_ar2 <- as.data.frame(summary(modelo_ar2)$tTable) %>%
  tibble::rownames_to_column("term") %>%
  rename(estimate = Value, std_error = `Std.Error`, df = DF, t_value = `t-value`, p_value = `p-value`) %>%
  mutate(
    nivel = case_when(
      term %in% c("(Intercept)", "poverty", "density_miles", "alt_100m", "biomass_total") ~ "Nivel comuna (constante en el tiempo)",
      TRUE ~ "Nivel dia (varia dentro de la comuna)"
    )
  ) %>%
  select(term, nivel, estimate, std_error, df, p_value)

print(tabla_gl_ar2)

# Medidas de bondad de ajuste (mismo paquete que ya usan para los modelos
# lmer: performance::r2()/icc(), mas AIC/BIC/logLik que ya calcula nlme)
print(AIC(modelo_ar2))
print(BIC(modelo_ar2))
print(logLik(modelo_ar2))
print(performance::r2(modelo_ar2))
print(performance::icc(modelo_ar2))

# ------------------------------------------------------------------------------
# Helper: extrae UN termino de la tabla de efectos fijos de un modelo lme,
# con su DF -- se reusa para todas las comparaciones de sensibilidad de aqui
# en adelante (Talagante, sin pobreza, sin biomasa) en vez de repetir el
# mismo bloque de codigo 4 veces.
# ------------------------------------------------------------------------------
extraer_termino <- function(modelo, termino, nombre_modelo) {
  as.data.frame(summary(modelo)$tTable) %>%
    tibble::rownames_to_column("term") %>%
    rename(estimate = Value, std_error = `Std.Error`, df = DF, p_value = `p-value`) %>%
    filter(term == termino) %>%
    mutate(modelo = nombre_modelo) %>%
    select(modelo, term, estimate, std_error, df, p_value)
}

# ==============================================================================
# MODELO 6: SENSIBILIDAD - EXCLUYENDO TALAGANTE (AR2)
# ==============================================================================
# Interaccion invierno x biomasa esta impulsada
# casi enteramente por Talagante (1.049 calefactores/10.000 hogares, dos
# ordenes de magnitud por sobre la siguiente comuna mas alta, ~150 en Puente
# Alto). Se reajusta el modelo final (AR2, mismos predictores) sin Talagante.
print("--- RUNNING MODEL 6: SENSITIVITY - EXCLUDING TALAGANTE (AR2) ---")

datos_modelo_sin_talagante <- datos_modelo %>%
  filter(comuna != "Talagante") %>%
  mutate(comuna = droplevels(as.factor(comuna))) # saca el nivel de factor vacio

n_filas_talagante <- datos_modelo %>% filter(comuna == "Talagante") %>% nrow()
print(paste("--- Excluding", n_filas_talagante, "daily records from Talagante ---"))

modelo_ar2_sin_talagante <- lme(
  mp25_prom_valid ~ year_centrado + winter * biomass_total + poverty + density_miles + alt_100m,
  random = ~ 1 | comuna,
  correlation = corARMA(form = ~ fecha_local | comuna, p = 2, q = 0),
  data = datos_modelo_sin_talagante
)

summary(modelo_ar2_sin_talagante)

# ==============================================================================
# MODELO 7: SENSIBILIDAD - SIN POBREZA (AR2)
# ==============================================================================
# Si al sacar pobreza la interaccion invierno x biomasa no cambia mucho, refuerza que
# biomasa (no pobreza) es el driver real.
print("--- RUNNING MODEL 7: SENSITIVITY - WITHOUT POVERTY (AR2) ---")

modelo_ar2_sin_pobreza <- lme(
  mp25_prom_valid ~ year_centrado + winter * biomass_total + density_miles + alt_100m,
  random = ~ 1 | comuna,
  correlation = corARMA(form = ~ fecha_local | comuna, p = 2, q = 0),
  data = datos_modelo
)

summary(modelo_ar2_sin_pobreza)

# ==============================================================================
# MODELO 8: SENSIBILIDAD - SIN BIOMASA (AR2)
# ==============================================================================
# Version complementaria: si al sacar biomasa (y su interaccion con invierno,
# que no puede estimarse sin ella) la pobreza SI se vuelve significativa,
# sugeriria que biomasa esta actuando como mediador/confundidor de pobreza.
print("--- RUNNING MODEL 8: SENSITIVITY - WITHOUT BIOMASS (AR2) ---")

modelo_ar2_sin_biomasa <- lme(
  mp25_prom_valid ~ year_centrado + winter + poverty + density_miles + alt_100m,
  random = ~ 1 | comuna,
  correlation = corARMA(form = ~ fecha_local | comuna, p = 2, q = 0),
  data = datos_modelo
)

summary(modelo_ar2_sin_biomasa)

# ==============================================================================
# TABLA COMPARATIVA DE SENSIBILIDAD (Talagante / sin pobreza / sin biomasa)
# ==============================================================================
# Junta, en una sola tabla, como se mueven los coeficientes clave (la
# interaccion invierno x biomasa, y pobreza) entre el modelo completo y cada
# variante de sensibilidad -- es la forma compacta de responder a la vez el
# comentario de Talagante y el de "disentangle poverty and biomass".
comparacion_interaccion <- bind_rows(
  extraer_termino(modelo_ar2, "winterWinter:biomass_total", "Completo (Modelo 5, AR2)"),
  extraer_termino(modelo_ar2_sin_talagante, "winterWinter:biomass_total", "Sin Talagante (Modelo 6)"),
  extraer_termino(modelo_ar2_sin_pobreza, "winterWinter:biomass_total", "Sin pobreza (Modelo 7)")
)

comparacion_pobreza <- bind_rows(
  extraer_termino(modelo_ar2, "poverty", "Completo (Modelo 5, AR2)"),
  extraer_termino(modelo_ar2_sin_biomasa, "poverty", "Sin biomasa (Modelo 8)")
)

tabla_sensibilidad <- bind_rows(
  comparacion_interaccion %>% mutate(coeficiente = "Winter x Biomass interaction"),
  comparacion_pobreza %>% mutate(coeficiente = "Poverty")
) %>%
  select(coeficiente, modelo, estimate, std_error, df, p_value)

print(tabla_sensibilidad)

writeLines(kable(tabla_sensibilidad, format = "markdown", digits = 4),
           "output/tables/table_6_sensitivity_AR2_2.0.md")

# ==============================================================================
# TABLA FINAL FORMATEADA DEL MODELO AR(2) (para el paper, reemplaza el
# Modelo 2/3 sin AR como Tabla 3 -- estos quedan como referencia de que
# cambio al corregir la autocorrelacion)
# ==============================================================================
ci_ar2 <- as.data.frame(nlme::intervals(modelo_ar2, which = "fixed")$fixed) %>%
  tibble::rownames_to_column("term")
# NOTA: si `names(ci_ar2)` no trae columnas "lower"/"est."/"upper" exactas,
# revisar con str(intervals(modelo_ar2, which="fixed")) y ajustar el
# left_join/select de abajo a los nombres reales.

fixed_effects_ar2 <- tabla_gl_ar2 %>%
  left_join(ci_ar2, by = "term") %>%
  mutate(
    p_label = case_when(
      p_value < 0.001 ~ "< 0.001*",
      p_value < 0.05  ~ paste0(sprintf("%.3f", p_value), "*"),
      TRUE            ~ sprintf("%.3f", p_value)
    ),
    ci_label = paste0("[", sprintf("%.2f", lower), " – ", sprintf("%.2f", upper), "]"),
    estimate_label = case_when(
      term == "biomass_total" ~ sprintf("%.4f", estimate),
      term == "winterWinter:biomass_total" ~ sprintf("%.4f", estimate),
      TRUE ~ sprintf("%.2f", estimate)
    ),
    term_clean = case_when(
      term == "(Intercept)" ~ "(Intercept)",
      term == "year_centrado" ~ "Year",
      term == "poverty" ~ "Poverty (%)",
      term == "density_miles" ~ "Density (1,000 inhabitants/km²)",
      term == "alt_100m" ~ "Altitude (per 100m)",
      term == "winterWinter" ~ "Season [Winter (GEC)]",
      term == "biomass_total" ~ "Biomass heaters^a",
      term == "winterWinter:biomass_total" ~ "Winter × Biomass",
      TRUE ~ term
    )
  ) %>%
  select(Predictors = term_clean, Estimate = estimate_label, `95% CI` = ci_label, DF = df, `p value` = p_label)

r2_ar2 <- performance::r2(modelo_ar2)
icc_ar2 <- performance::icc(modelo_ar2)

# Random effects (varianza), vía VarCorr() porque modelo_ar2 es un objeto
# lme (nlme), no lmer -- tidy(effects="ran_pars") no aplica aquí igual que
# en la tabla vieja.
vc_ar2 <- nlme::VarCorr(modelo_ar2)
var_comuna_ar2   <- as.numeric(vc_ar2["(Intercept)", "Variance"])
var_residual_ar2 <- as.numeric(vc_ar2["Residual", "Variance"])

extra_rows_ar2 <- tibble::tribble(
  ~Predictors, ~Estimate, ~`95% CI`, ~DF, ~`p value`,
  "**Random Effects**", "", "", NA, "",
  "σ² (Residual Variance)", sprintf("%.2f", var_residual_ar2), "", NA, "",
  "τ₀₀ (Between-municipality)", sprintf("%.2f", var_comuna_ar2), "", NA, "",
  "ICC", sprintf("%.2f", icc_ar2$ICC_adjusted), "", NA, "",
  "**Model Fit**", "", "", NA, "",
  "AIC / BIC", paste0(sprintf("%.1f", AIC(modelo_ar2)), " / ", sprintf("%.1f", BIC(modelo_ar2))), "", NA, "",
  "Observations", as.character(nobs(modelo_ar2)), "", NA, "",
  "Marginal R² / Cond. R²", paste0(sprintf("%.3f", r2_ar2$R2_marginal), " / ", sprintf("%.3f", r2_ar2$R2_conditional)), "", NA, ""
)

tabla_final_ar2 <- bind_rows(
  tibble(Predictors = "**Fixed Effects**", Estimate = "", `95% CI` = "", DF = NA, `p value` = ""),
  fixed_effects_ar2,
  extra_rows_ar2
)

nota_pie_ar2 <- "_Note: CI: Confidence Interval (95%). DF: denominator degrees of freedom per term (nlme::lme) -- note the much smaller DF for municipality-level, time-invariant predictors (poverty, density, altitude, biomass) compared to day-level predictors (year, winter, winter x biomass), reflecting that the former are effectively estimated from 10 municipalities, not 10,410 daily records. AR(2): autoregressive order-2 correlation structure on residuals within each municipality, selected over AR(1) via likelihood ratio test (see comparison above). Reference category for Season: [Rest of the year]. a: Biomass heaters rate per 10,000 households. * indicate statistical significance (p<0.05)._"

sink("output/tables/table_5_mixed_model_AR2_2.0.md")
cat("### Table 3. Results of the Linear Mixed Model (LMM, AR(2) residual correlation) evaluating the effect of seasonality, year, multidimensional poverty, population density, altitude, and residential biomass heating on daily PM2.5 concentrations.\n\n")
print(kable(tabla_final_ar2, format = "markdown", align = "lcccc"))
cat("\n\n", nota_pie_ar2, "\n")
sink()

print("Model saved and full table generated in 'output/tables/table_5_mixed_model_AR2_2.0.md'")

