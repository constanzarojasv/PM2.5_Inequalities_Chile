# ==============================================================================
# 02. DESCRIPTIVE ANALYSIS (TABLES)
# Purpose: Generate summary tables for Annual and Winter periods
# ==============================================================================

source("processing/analisis2.0/00_setup_functions.R", encoding = "UTF-8")

# 1. Load data
df_analisis <- read_rds("input/data_processed/datos_analisis_final_2.0.rds")

# 2. Define summary function
# `periodo` controls which completeness flag is used to gate the reported
# mean/SD/etc. and which station-years enter the ANOVA:
#  - "anual": uses anio_conforme (Decreto 12/2011, >=11 valid months, or
#    9-10 with imputation) and reports the regulatory annual value
#    (mean of monthly means) instead of a naive pooled mean of all days.
#  - "invierno": uses invierno_conforme (the 4 winter months, May-Aug, each
#    individually meeting the decree's 75%-per-month rule).
calcular_tabla_resumen <- function(df, titulo_tabla, periodo = c("anual", "invierno")) {
  periodo <- match.arg(periodo)

  if (periodo == "anual") {
    df <- df %>%
      mutate(
        Conforme = anio_conforme,
        Motivo_no_conforme = motivo_no_conforme,
        Meses_validos_periodo = n_meses_validos, # out of 12 months
        # TRUE when the annual value relied on imputing missing months
        # (9-10 valid months) with the highest monthly value from the prior
        # 12 months (Decreto 12/2011) -- see output/tables/table_S1_completeness.csv
        # for exactly which month(s) and value(s) were used.
        Imputado = n_meses_imputados > 0
      )
  } else {
    df <- df %>%
      mutate(
        Conforme = invierno_conforme,
        Motivo_no_conforme = if_else(
          invierno_conforme, NA_character_,
          paste0("Mes(es) de invierno bajo 75% de cobertura: ", meses_invierno_no_validos)
        ),
        # out of the 4 winter months (May-Aug); NOT the annual n_meses_validos
        Meses_validos_periodo = 4L - lengths(strsplit(meses_invierno_no_validos, ",")),
        # The decree's imputation rule is only defined for the annual value;
        # it does not apply to the winter sub-period (see decision log).
        Imputado = NA
      )
  }

  # A) Descriptive statistics (N_days / Min / Max / P98 are still computed
  # from daily data for transparency, but Mean/SD are only reported for
  # compliant station-years/winters -- see B).
  descriptivos <- df %>%
    group_by(comuna, anio) %>%
    summarise(
      N_days = sum(!is.na(mp25_prom_valid)),
      Meses_validos_periodo = first(Meses_validos_periodo),
      Conforme = first(Conforme),
      Imputado = first(Imputado),
      Motivo_no_conforme = first(Motivo_no_conforme),
      Min = min(mp25_prom_valid, na.rm = TRUE),
      Max = max(mp25_prom_valid, na.rm = TRUE),
      P98 = quantile(mp25_prom_valid, probs = 0.98, na.rm = TRUE, type = 7),
      .groups = "drop"
    )

  # B) Reported mean/SD: regulatory annual value (mean of monthly means) for
  # "anual"; mean of the 4 winter daily values for "invierno" -- but only
  # when the period is Decreto-compliant. Non-compliant station-years/
  # winters are reported as NA instead of a potentially coverage-biased
  # number (this is what caused the Cerrillos 2022 artifact flagged by
  # Reviewer 2).
  medias <- df %>%
    filter(Conforme) %>%
    group_by(comuna, anio) %>%
    summarise(
      Mean = if (periodo == "anual") first(promedio_anual_regulatorio) else mean(mp25_prom_valid, na.rm = TRUE),
      SD = sd(mp25_prom_valid, na.rm = TRUE),
      .groups = "drop"
    )

  # C) ANOVA (Year comparison p-value), run ONLY on compliant station-years
  # so an incomplete year (e.g. Cerrillos 2022, missing Jan-Mar) doesn't
  # bias the between-year significance test (Reviewer 2).
  p_valores <- df %>%
    filter(Conforme) %>%
    group_by(comuna) %>%
    summarise(
      n_anios_conformes = n_distinct(anio),
      p_val_num = if (n_anios_conformes >= 2) summary(aov(mp25_prom_valid ~ droplevels(anio)))[[1]][["Pr(>F)"]][1] else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(
      p_value = case_when(
        n_anios_conformes < 2 ~ "Insufficient data",
        p_val_num < 0.001     ~ "< 0.001 *",
        p_val_num < 0.05      ~ paste0(round(p_val_num, 3), " *"),
        TRUE                  ~ as.character(round(p_val_num, 3))
      )
    ) %>%
    select(comuna, p_value)

  # D) Final table assembly
  tabla_final <- descriptivos %>%
    left_join(medias, by = c("comuna", "anio")) %>%
    left_join(p_valores, by = "comuna") %>%
    mutate(
      `Mean (SD)` = if_else(Conforme, paste0(round(Mean, 1), " (", round(SD, 1), ")"), "NA (non-compliant)"),
      P98 = round(P98, 1),
      Min = round(Min, 1),
      Max = round(Max, 1)
    ) %>%
    select(comuna, anio, N_days, Meses_validos_periodo, Conforme, Imputado,
           Motivo_no_conforme, `Mean (SD)`, Min, Max, P98, p_value)

  print(paste("---", titulo_tabla, "---"))
  return(tabla_final)
}

# 3. Generate Tables
# Table 1: Annual
tabla_anual <- calcular_tabla_resumen(df_analisis, "TABLE 1: ANNUAL STATISTICS", periodo = "anual")

# Table 2: Winter Only (We filter by the factor "Winter" created in Script 01)
tabla_invierno <- df_analisis %>%
  filter(es_invierno == "Winter") %>%
  calcular_tabla_resumen("TABLE 2: WINTER STATISTICS", periodo = "invierno")

#4. Save tables as .md
write_xlsx(tabla_anual, "output/tables/table_1_anual_2.0.xlsx")
write_xlsx(tabla_invierno, "output/tables/table_2_winter_2.0.xlsx")
writeLines(kable(tabla_anual, format = "markdown"), "output/tables/table_1_anual_2.0.md")
writeLines(kable(tabla_invierno, format = "markdown"), "output/tables/table_2_winter_2.0.md")

rm(list = ls())
