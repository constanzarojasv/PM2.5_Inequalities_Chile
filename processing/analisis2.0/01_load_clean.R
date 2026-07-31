# ==============================================================================
# 01. LOAD AND CLEAN DATA
# ==============================================================================

# Load functions from script 00
source("processing/analisis2.0/00_setup_functions.R", encoding = "UTF-8")

# 1. INDIVIDUAL LOAD (Manual control)
# We call the function explicitly for each municipality
df_1 <- procesar_mp25("Puente Alto",      "Puente-Alto-mp25-2022-2024.csv")
df_2 <- procesar_mp25("Quilicura",        "Quilicura-mp25-2022-2024.csv")
df_3 <- procesar_mp25("Cerro Navia",      "Cerro-Navia-mp25-2022-2024.csv")
df_4 <- procesar_mp25("Parque O'Higgins", "Parque-Ohiggins-mp25-2022-2024.csv")
df_5 <- procesar_mp25("Pudahuel",         "Pudahuel-mp25-2022-2024.csv")
df_6 <- procesar_mp25("Talagante",        "Talagante-mp25-2022-2024.csv")
df_7 <- procesar_mp25("Las Condes",       "Las-Condes-mp25-2022-2024.csv")
df_8 <- procesar_mp25("La Florida",       "La-Florida-mp25-2022-2024.csv")
df_9 <- procesar_mp25("El Bosque",        "El-Bosque-mp25-2022-2024.csv")
df_10 <- procesar_mp25("Cerrillos",       "Cerrillos-mp25-2022-2024.csv")

# 2. MERGE ALL (Consolidation)
df_crudo <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10)

# 3. DATA COMPLETENESS (Decreto 12/2011 MMA)
# Computed on df_crudo (BEFORE dropping invalid days) so that fully missing
# months/days are counted correctly, and so the imputation rule can look
# back at the full comuna history (not just the current calendar year).
tabla_completitud_mensual <- evaluar_completitud_mensual(df_crudo)

resultado_anual            <- evaluar_completitud_anual(tabla_completitud_mensual)
tabla_completitud_anual    <- resultado_anual$tabla_anual
detalle_imputacion         <- resultado_anual$detalle_imputacion # which months were imputed, with what value and source month
tabla_completitud_invierno <- evaluar_completitud_invierno(tabla_completitud_mensual)

# Add imputation transparency to the monthly table: for every month that was
# actually used to complete a 9-10-valid-month year, flag it and record the
# value used and exactly which prior month it was borrowed from (Reviewer 1 /
# Reviewer 2: full documentation of how missing values were handled).
tabla_completitud_mensual <- tabla_completitud_mensual %>%
  left_join(
    detalle_imputacion %>%
      select(comuna, anio_n = anio, mes, valor_imputado, mes_origen_anio, mes_origen_mes),
    by = c("comuna", "anio_n", "mes")
  ) %>%
  mutate(imputado = !is.na(valor_imputado))

# 4. FILTERS AND VARIABLES
df_analisis <- df_crudo %>%
  filter(!is.na(mp25_prom_valid)) %>%
  filter(year(fecha_local) %in% 2022:2024) %>%
  mutate(
    anio = as.factor(year(fecha_local)),
    comuna = as.factor(comuna),
    mes = month(fecha_local),
    # Define Season (Winter: May-August)
    es_invierno = as.factor(ifelse(mes %in% 5:8, "Winter", "Rest of year"))
  ) %>%
  # Join with Sociodemographics
  left_join(datos_comunas, by = "comuna") %>%
  # Join with completeness flags (Decreto 12/2011): every downstream script
  # reads anio_conforme / invierno_conforme from here instead of recomputing
  # its own (previously inconsistent) coverage logic.
  left_join(
    tabla_completitud_anual %>% mutate(anio = as.factor(anio)),
    by = c("comuna", "anio")
  ) %>%
  left_join(
    tabla_completitud_invierno %>% mutate(anio = as.factor(anio)),
    by = c("comuna", "anio")
  )

# 5. SAVE
write_rds(df_analisis, "input/data_processed/datos_analisis_final_2.0.rds")

# Monthly completeness table, exported so the % of missing/invalid data per
# station-month is documented and citable (Reviewer 1: "what percentage of
# missing values / how were they handled").
write_csv(tabla_completitud_mensual, "output/tables/table_S1_completeness.csv")
write_xlsx(tabla_completitud_mensual, "output/tables/table_S1_completeness.xlsx")


rm(list = ls())

#Missing values 
tabla_s1 <- read_csv("output/tables/table_S1_completeness.csv")

# % global de días válidos vs programados
tabla_s1 %>% summarise(pct_dias_validos_global = sum(dias_validos)/sum(dias_programados)*100)

# cuántas estaciones-año quedaron excluidas / cuántas imputadas
df <- readRDS("input/data_processed/datos_analisis_final_2.0.rds")
df %>% distinct(comuna, anio, anio_conforme, n_meses_imputados) %>%
  summarise(
    n_total = n(),
    n_excluidas = sum(!anio_conforme),
    n_imputadas = sum(anio_conforme & n_meses_imputados > 0)
  )

rm(list = ls())