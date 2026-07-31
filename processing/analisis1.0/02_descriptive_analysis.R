# ==============================================================================
# 02. DESCRIPTIVE ANALYSIS (TABLES)
# Purpose: Generate summary tables for Annual and Winter periods
# ==============================================================================

source("processing/00_setup_functions.R", encoding = "UTF-8")

# 1. Load data
df_analisis <- read_rds("input/data_processed/datos_analisis_final.rds")

# 2. Define summary function
calcular_tabla_resumen <- function(df, titulo_tabla) {
  
  # A) Descriptive statistics
  descriptivos <- df %>%
    group_by(comuna, anio) %>%
    summarise(
      N_days = sum(!is.na(mp25_prom_valid)),
      Mean = mean(mp25_prom_valid, na.rm = TRUE),
      SD = sd(mp25_prom_valid, na.rm = TRUE),
      Min = min(mp25_prom_valid, na.rm = TRUE),
      Max = max(mp25_prom_valid, na.rm = TRUE),
      P98 = quantile(mp25_prom_valid, probs = 0.98, na.rm = TRUE, type = 7),
      .groups = "drop"
    )
  
  # B) ANOVA (Year comparison p-value)
  p_valores <- df %>%
    group_by(comuna) %>%
    summarise(
      p_val_num = summary(aov(mp25_prom_valid ~ anio))[[1]][["Pr(>F)"]][1]
    ) %>%
    mutate(
      p_value = case_when(
        p_val_num < 0.001 ~ "< 0.001 *",
        p_val_num < 0.05  ~ paste0(round(p_val_num, 3), " *"),
        TRUE              ~ as.character(round(p_val_num, 3))
      )
    )
  
  # C) Final table assembly
  tabla_final <- descriptivos %>%
    left_join(p_valores, by = "comuna") %>%
    mutate(
      `Mean (SD)` = paste0(round(Mean, 1), " (", round(SD, 1), ")"),
      P98 = round(P98, 1),
      Min = round(Min, 1),
      Max = round(Max, 1)
    ) %>%
    select(comuna, anio, N_days, `Mean (SD)`, Min, Max, P98, p_value)
  
  print(paste("---", titulo_tabla, "---"))
  return(tabla_final)
}

# 3. Generate Tables
# Table 1: Annual
tabla_anual <- calcular_tabla_resumen(df_analisis, "TABLE 1: ANNUAL STATISTICS")

# Table 2: Winter Only (We filter by the factor "Winter" created in Script 01)
tabla_invierno <- df_analisis %>% 
  filter(es_invierno == "Winter") %>%
  calcular_tabla_resumen("TABLE 2: WINTER STATISTICS")

#4. Save tables as .md
#write_csv(tabla_anual, "output/tables/table_1_anual.csv")
#write_csv(tabla_invierno, "output/tables/table_2_winter.csv")
#writeLines(kable(tabla_anual, format = "markdown"), "output/tables/table_1_anual.md")
#writeLines(kable(tabla_invierno, format = "markdown"), "output/tables/table_2_winter.md")

rm(list = ls())