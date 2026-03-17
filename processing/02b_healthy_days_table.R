# ==============================================================================
# TABLA S2: PORCENTAJE DE DÍAS SALUDABLES (HEALTHY DAYS)
# ==============================================================================

source("processing/00_setup_functions.R", encoding = "UTF-8")
df_analisis <- read_rds("input/data_processed/datos_analisis_final.rds")

print("--- Generando Tabla S2: Healthy Days ---")

# 1. FUNCIÓN BASE PARA CÁLCULO
# Usamos el truco lógico de R: mean(condición) calcula automáticamente la proporción
calcular_dias_saludables <- function(df_filtrado) {
  df_filtrado %>%
    group_by(comuna, anio) %>%
    summarise(
      valid_days = sum(!is.na(mp25_prom_valid)),
      chile = round(mean(mp25_prom_valid <= 50, na.rm = TRUE) * 100, 2),
      epa   = round(mean(mp25_prom_valid <= 35, na.rm = TRUE) * 100, 2),
      who   = round(mean(mp25_prom_valid <= 15, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    )
}

# 2. CÁLCULO PERIODO ANUAL
tabla_anual_hw <- calcular_dias_saludables(df_analisis) %>%
  rename(
    `Valid days_Annual` = valid_days,
    `Chile_Annual` = chile,
    `EPA_Annual` = epa,
    `WHO_Annual` = who
  )

# 3. CÁLCULO PERIODO INVIERNO
# Filtramos de forma robusta por si la variable dice "Invierno" o "Winter"
tabla_invierno_hw <- df_analisis %>%
  filter(es_invierno %in% c("Invierno", "Winter")) %>% 
  calcular_dias_saludables() %>%
  rename(
    `Valid days_Winter` = valid_days,
    `Chile_Winter` = chile,
    `EPA_Winter` = epa,
    `WHO_Winter` = who
  )

# 4. UNIÓN Y FORMATEO FINAL
tabla_s2 <- tabla_anual_hw %>%
  left_join(tabla_invierno_hw, by = c("comuna", "anio")) %>%
  # Renombramos Parque O'Higgins para que quede exactamente como en tu imagen
  mutate(comuna = recode(comuna, `Parque O'Higgins` = "Santiago (Parque)")) %>%
  rename(Municipality = comuna, Year = anio)

# 5. EXPORTAR A CSV
# La guardamos como CSV para que puedas copiarla y pegarla fácil en tu Word/Excel si necesitas darle formato visual.

#write_csv(tabla_s2, "output/tables/table_S2_healthy_days.csv")
#writeLines(kable(tabla_s2, format = "markdown"), "output/tables/table_S2_healthy_days.md")

