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
#   "performance",
#   "knitr",
#   "broom.mixed",
#   "performance",
#   "tibble",
#   "elevatr",
#   "ggspatial",
#   "writexl"
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
#library(chilemapas)   
library(shadowtext)   
library(lme4)         
library(lmerTest)     
library(ggrepel)
library(knitr)
library(broom.mixed)
library(performance)
library(tibble)
library(elevatr)
library(ggspatial)
library(writexl)

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
  # This step standarize them to "valor_detectado"
  if ("registros_validados" %in% names(df)) {
    df <- df %>% rename(valor_detectado = registros_validados)
  } else if ("mp25_val" %in% names(df)) {
    df <- df %>% rename(valor_detectado = mp25_val)
  } else { stop(paste("ERROR en", nombre_comuna, ": No encontré columna de valor.")) }
  
  # 3. Process dates and averages
  df %>%
    mutate(
      fecha_hora = as.POSIXct(paste(fecha_yymmdd, hora_hhmm), format = "%y%m%d %H%M", tz = "UTC"),
      fecha_local = as.Date(fecha_hora),
      mp25_val = valor_detectado
    ) %>%
    group_by(fecha_local) %>%
    summarise(
      mp25_prom = mean(mp25_val, na.rm = TRUE), #Calculate daily PM2.5 averages
      n_registros = sum(!is.na(mp25_val)), #Count valid records per day
      # Longest run of consecutive valid hours (0-23h) within the day.
      # Decreto 61/2008 (MINSAL) requires the daily average to be based on
      # >=18 CONTINUOUS hours of measurement
      horas_continuas_max = {
        horas_validas <- hour(fecha_hora)[!is.na(mp25_val)]
        presente <- (0:23) %in% horas_validas
        r <- rle(presente)
        max(c(0L, r$lengths[r$values]))
      },
      .groups = "drop"
    ) %>%
    mutate(
      comuna = nombre_comuna,
      dia_valido = n_registros >= 18 & horas_continuas_max >= 18,
      mp25_prom_valid = if_else(dia_valido, mp25_prom, NA_real_)
    ) #Validation criteria (Decreto 61/2008): a day is valid with >=18 hourly
      #records AND those valid hours forming a continuous run of >=18h.
}

# 5. DATA COMPLETENESS (Decreto 12/2011 MMA)
# Regulatory chain used to decide whether a station-year has a valid annual
# concentration value, applied on top of the daily-validity rule above:
#   - Monthly value valid: >=75% of the days scheduled for the month have a
#     valid daily average.
#   - Annual value: requires >=11 valid months -> annual value = mean of the
#     valid monthly means. With 9-10 valid months, each missing month is
#     imputed with the highest monthly mean measured in the 12 months
#     immediately prior to that month (rolling window across the full
#     station history), if that history exists. With <=8 valid months, or a
#     missing month with no 12-month history available to impute from
#     (e.g. gaps at the very start of the study period), no annual value is
#     calculated.

# 5a. Monthly completeness table (one row per comuna x year x month, 2022-2024)
evaluar_completitud_mensual <- function(df_diario) {
  tabla <- df_diario %>%
    filter(year(fecha_local) %in% 2022:2024) %>%
    mutate(anio_n = year(fecha_local), mes = month(fecha_local)) %>%
    group_by(comuna, anio_n, mes) %>%
    summarise(
      dias_validos = sum(dia_valido, na.rm = TRUE),
      media_mensual_bruta = mean(mp25_prom_valid, na.rm = TRUE),
      .groups = "drop"
    )

  # Complete grid so months with ZERO rows in the raw file (not just NA
  # values) are still counted as missing, instead of silently disappearing.
  grid_completo <- tidyr::crossing(
    comuna = unique(df_diario$comuna),
    anio_n = 2022:2024,
    mes = 1:12
  )

  grid_completo %>%
    left_join(tabla, by = c("comuna", "anio_n", "mes")) %>%
    mutate(
      dias_validos = tidyr::replace_na(dias_validos, 0),
      dias_programados = days_in_month(make_date(anio_n, mes, 1)),
      pct_cobertura_mes = dias_validos / dias_programados,
      mes_valido = pct_cobertura_mes >= 0.75,
      media_mensual = if_else(mes_valido, media_mensual_bruta, NA_real_),
      mes_idx = anio_n * 12 + mes
    ) %>%
    select(comuna, anio_n, mes, mes_idx, dias_validos, dias_programados,
           pct_cobertura_mes, mes_valido, media_mensual)
}

# 5b. Annual completeness + regulatory annual value (per comuna x year)
# Returns a LIST with two tables, for full transparency about imputation:
#   $tabla_anual        -- one row per comuna x year (n_meses_validos,
#                           anio_conforme, n_meses_imputados, meses_imputados,
#                           promedio_anual_regulatorio).
#   $detalle_imputacion -- one row per (comuna, anio, mes) that was actually
#                           imputed, with the value used and exactly which
#                           prior month it was borrowed from (mes_origen_*).
#                           Empty (0-row) if no station-year needed imputation.
evaluar_completitud_anual <- function(tabla_mensual) {
  comunas <- unique(tabla_mensual$comuna)
  anios <- 2022:2024
  resultados <- vector("list", length(comunas) * length(anios))
  detalle_imputacion <- list()
  i <- 0

  for (cm in comunas) {
    serie_comuna <- tabla_mensual %>% filter(comuna == cm) %>% arrange(mes_idx)

    for (an in anios) {
      i <- i + 1
      meses_anio <- serie_comuna %>% filter(anio_n == an)
      n_validos <- sum(meses_anio$mes_valido)
      meses_faltantes <- meses_anio$mes[!meses_anio$mes_valido]
      valores_mensuales <- meses_anio$media_mensual[meses_anio$mes_valido]

      motivo <- NA_character_
      conforme <- FALSE
      valores_finales <- NA_real_
      n_meses_imputados <- 0L
      meses_imputados_str <- NA_character_
      detalle_anio <- list()

      if (n_validos >= 11) {
        conforme <- TRUE
        valores_finales <- valores_mensuales
      } else if (n_validos >= 9) {
        valores_imputados <- numeric(0)
        imputacion_ok <- TRUE
        for (m_falt in meses_faltantes) {
          idx_falt <- an * 12 + m_falt
          ventana <- serie_comuna %>%
            filter(mes_idx >= idx_falt - 12, mes_idx <= idx_falt - 1, mes_valido)
          if (nrow(ventana) == 0) {
            imputacion_ok <- FALSE
            break
          }
          # Highest monthly mean in the 12 months prior to the missing month
          # (Decreto 12/2011); keep track of exactly which month it came from.
          fila_origen <- ventana %>% arrange(desc(media_mensual)) %>% slice(1)
          valores_imputados <- c(valores_imputados, fila_origen$media_mensual)
          detalle_anio[[length(detalle_anio) + 1]] <- tibble(
            comuna = cm, anio = an, mes = m_falt,
            valor_imputado = fila_origen$media_mensual,
            mes_origen_anio = fila_origen$anio_n,
            mes_origen_mes = fila_origen$mes
          )
        }
        if (imputacion_ok) {
          conforme <- TRUE
          valores_finales <- c(valores_mensuales, valores_imputados)
          n_meses_imputados <- length(meses_faltantes)
          meses_imputados_str <- paste(meses_faltantes, collapse = ",")
          detalle_imputacion <- c(detalle_imputacion, detalle_anio)
        } else {
          motivo <- paste0(n_validos, " meses válidos (9-10), pero sin 12 meses de ",
                            "historial previo para imputar el/los mes(es) faltante(s)")
        }
      } else {
        motivo <- paste0("Solo ", n_validos, " meses válidos (se requieren >=11, ",
                          "o 9-10 con imputación)")
      }

      resultados[[i]] <- tibble(
        comuna = cm,
        anio = an,
        n_meses_validos = n_validos,
        anio_conforme = conforme,
        n_meses_imputados = n_meses_imputados,
        meses_imputados = meses_imputados_str,
        motivo_no_conforme = motivo,
        promedio_anual_regulatorio = if (conforme) mean(valores_finales, na.rm = TRUE) else NA_real_
      )
    }
  }

  detalle_imputacion <- if (length(detalle_imputacion) == 0) {
    tibble(
      comuna = character(), anio = integer(), mes = integer(),
      valor_imputado = double(), mes_origen_anio = integer(), mes_origen_mes = integer()
    )
  } else {
    bind_rows(detalle_imputacion)
  }

  list(tabla_anual = bind_rows(resultados), detalle_imputacion = detalle_imputacion)
}

# 5c. Winter completeness (May-Aug): reuses the SAME 75%-per-month rule from
# the decree (5a) -- no new ad-hoc threshold is introduced for the season.
# A station-winter is compliant only if all 4 winter months are valid months.
evaluar_completitud_invierno <- function(tabla_mensual) {
  tabla_mensual %>%
    filter(mes %in% 5:8) %>%
    group_by(comuna, anio_n) %>%
    summarise(
      meses_invierno_no_validos = paste(mes[!mes_valido], collapse = ","),
      invierno_conforme = all(mes_valido),
      .groups = "drop"
    ) %>%
    rename(anio = anio_n)
}