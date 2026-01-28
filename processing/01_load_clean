# ==============================================================================
# 01. LOAD AND CLEAN DATA
# ==============================================================================

# Load functions from script 00
source("processing/00_setup_functions.R", encoding = "UTF-8")

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

# 3. FILTERS AND VARIABLES
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
  left_join(datos_comunas, by = "comuna")

# 4. SAVE
write_rds(df_analisis, "input/data_processed/datos_analisis_final.rds")
