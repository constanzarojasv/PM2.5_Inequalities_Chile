# ==============================================================================
# TABLA S3: PORCENTAJE DE DÍAS SALUDABLES (HEALTHY DAYS)
# ==============================================================================

source("processing/analisis2.0/00_setup_functions.R", encoding = "UTF-8")
df_analisis <- read_rds("input/data_processed/datos_analisis_final_2.0.rds")

print("--- Generando Supplementary Table 3: Healthy Days ---")

# 1. FUNCIÓN BASE PARA CÁLCULO
# Usamos el truco lógico de R: mean(condición) calcula automáticamente la proporción
# `conforme_col` indica qué flag de completitud (Decreto 12/2011) usar:
# anio_conforme para el bloque anual, invierno_conforme para el bloque de invierno.
# El % de cumplimiento es en sí una proporción (promedio de un indicador binario)
# sobre los días válidos del período, por lo que queda sujeto al mismo sesgo de
# cobertura que la media anual/invierno: se deja en NA para las estación-años/
# inviernos que no cumplen el criterio de completitud, en vez de reportar un
# porcentaje calculado sobre un subconjunto de días no representativo (Revisor 2).
calcular_dias_saludables <- function(df_filtrado, conforme_col) {
  df_filtrado %>%
    mutate(Conforme = .data[[conforme_col]]) %>%
    group_by(comuna, anio) %>%
    summarise(
      valid_days = sum(!is.na(mp25_prom_valid)),
      Conforme = first(Conforme),
      chile = round(mean(mp25_prom_valid <= 50, na.rm = TRUE) * 100, 2),
      epa   = round(mean(mp25_prom_valid <= 35, na.rm = TRUE) * 100, 2),
      who   = round(mean(mp25_prom_valid <= 15, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(
      chile = if_else(Conforme, chile, NA_real_),
      epa   = if_else(Conforme, epa, NA_real_),
      who   = if_else(Conforme, who, NA_real_)
    )
}

# 2. CÁLCULO PERIODO ANUAL
tabla_anual_hw <- calcular_dias_saludables(df_analisis, "anio_conforme") %>%
  rename(
    `Valid days_Annual` = valid_days,
    `Conforme_Annual` = Conforme,
    `Chile_Annual` = chile,
    `EPA_Annual` = epa,
    `WHO_Annual` = who
  )

# 3. CÁLCULO PERIODO INVIERNO
# Filtramos de forma robusta por si la variable dice "Invierno" o "Winter"
tabla_invierno_hw <- df_analisis %>%
  filter(es_invierno %in% c("Invierno", "Winter")) %>%
  calcular_dias_saludables("invierno_conforme") %>%
  rename(
    `Valid days_Winter` = valid_days,
    `Conforme_Winter` = Conforme,
    `Chile_Winter` = chile,
    `EPA_Winter` = epa,
    `WHO_Winter` = who
  )

# 4. UNIÓN Y FORMATEO FINAL
tabla_s3 <- tabla_anual_hw %>%
  left_join(tabla_invierno_hw, by = c("comuna", "anio")) %>%
  # Renombramos Parque O'Higgins para que quede exactamente como en tu imagen
  mutate(comuna = recode(comuna, `Parque O'Higgins` = "Santiago (Parque)")) %>%
  rename(Municipality = comuna, Year = anio)

# 5. EXPORTAR
# La guardamos como CSV/XLSX para que puedas copiarla y pegarla fácil en tu Word/Excel si necesitas darle formato visual.
write_csv(tabla_s3, "output/tables/table_S3_healthy_days_2.0.csv")
write_xlsx(tabla_s3, "output/tables/table_S3_healthy_days_2.0.xlsx")
writeLines(kable(tabla_s3, format = "markdown"), "output/tables/table_S3_healthy_days_2.0.md")

rm(list = ls())

