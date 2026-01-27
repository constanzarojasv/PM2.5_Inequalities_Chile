# ==============================================================================
# 00. SETUP Y FUNCTIONS
# ==============================================================================

# 0. INSTALL LIBRARIES (Uncomment to run once)
# install.packages(c(
#   "readr", 
#   "dplyr", 
#   "lubridate", 
#   "purrr", 
#   "tidyr", 
#   "ggplot2", 
#   "janitor", 
#   "sf", 
#   "chilemapas", 
#   "shadowtext", 
#   "lme4", 
#   "lmerTest", 
#   "ggrepel",
#   "performance"
# ))

# 1. LOAD LIBRARIES 
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2)
library(janitor)
library(sf)           
library(chilemapas)   
library(shadowtext)   
library(lme4)         
library(lmerTest)     
library(ggrepel)

#2. COLOR SETUP
aqi_cols <- c("#00E400","#FFFF00","#FF7E00","#FF0000","#8F3F97","#7E0023")
#These are the colours that we will use to create the maps 


# 3. SOCIODEMOGRAPHICS DATA
#We manually add the sociodemographic data. 
# SOURCES: 2022 CASEN SURVEY AND 2024 CENSUS, for each municipality.
#Population Density = population / land area SOURCE: 2024 CENSUS and Library of the National Congress of Chile  

datos_comunas <- tibble(
  comuna = c("Cerrillos", "Cerro Navia", "El Bosque", "La Florida", 
             "Las Condes", "Parque O'Higgins", "Pudahuel", "Puente Alto", 
             "Quilicura", "Talagante"),
  biomass_total = c(28.4, 29.9, 40.4, 37.4, 22.4, 6.5, 150.1, 93.2, 40.3, 1048.9),
  poverty = c(22.6, 23.0, 17.5, 13.2, 4.4, 16.5, 16.9, 18.9, 17.0, 17.0),
  density = c(4050, 11568, 11090, 5279, 2991, 19948, 1156, 6456, 3545, 607),
  altitude = c(512, 500, 579, 612, 807, 541, 485, 676, 490, 343)
)

# 4. CLEANING FUNCTION
# This function takes the municipality name and filename, searches for it in input/data_raw, and cleans it.
procesar_mp25 <- function(nombre_comuna, nombre_archivo) {
  
  # Construct manual path: "input/data_raw/" + filename
  ruta_completa <- paste0("input/data_raw/", nombre_archivo)
  
  # 1. Read .csv databases
  df <- read_delim(ruta_completa, delim = ";", escape_double = FALSE, 
                   trim_ws = TRUE, show_col_types = FALSE, name_repair = "unique") %>%
    clean_names() 
  
  # 2. Detect value column (validated vs mp25_val)  DD
  # PM2.5 validated values are named differently depending on the raw file.
  # Some have "registros_validados" and others "mp25_val".
  # This step standarizez them to "valor_detectado"
  if ("registros_validados" %in% names(df)) {
    df <- df %>% rename(valor_detectado = registros_validados)
  } else if ("mp25_val" %in% names(df)) {
    df <- df %>% rename(valor_detectado = mp25_val)
  } else { stop(paste("ERROR en", nombre_comuna, ": No encontré columna de valor.")) }
  
  # 3. Process dates and averages
  df %>%
    mutate(
      fecha_hora = as.POSIXct(paste(fecha_yymmdd, hora_hhmm), format = "%y%m%d %H%M", tz = "UTC"),
      fecha_local = as.Date(with_tz(fecha_hora, "America/Santiago")),
      mp25_val = valor_detectado
    ) %>%
    group_by(fecha_local) %>%
    summarise(
      mp25_prom = mean(mp25_val, na.rm = TRUE), #Calculate daily PM2.5 averages
      n_registros = sum(!is.na(mp25_val)), #Count valid records per day
      .groups = "drop"
    ) %>%
    mutate(
      comuna = nombre_comuna,
      dia_valido = n_registros >= 18,
      mp25_prom_valid = if_else(dia_valido, mp25_prom, NA_real_)
    ) #Validation criteria: if there are >= 18 records per day, it is considered as valid day.
}