# ==============================================================================
# 04. SPATIAL ANALYSIS & MAPS (STATIONS, ELEVATION & HEALTHY DAYS)
# Purpose: Calculate elevations, map monitoring stations, and plot PM2.5 metrics
# ==============================================================================

source("processing/analisis2.0/00_setup_functions.R", encoding = "UTF-8")

# 1. LOAD CLEAN DATA
df_analisis <- read_rds("input/data_processed/datos_analisis_final_2.0.rds")

# 2. CONFIGURATION: IDs AND NAMES
id_map <- c("Cerrillos"="1", "Cerro Navia"="2", "El Bosque"="3", "La Florida"="4",
            "Las Condes"="5", "Santiago"="6", "Pudahuel"="7", "Puente Alto"="8",
            "Quilicura"="9", "Talagante"="10")

edit_com <- c("Cerro Navia"="Cerro Navia", "El Bosque"="El Bosque", "La Florida"="La Florida",
              "Las Condes"="Las Condes", "Pudahuel"="Pudahuel", "Puente Alto"="Puente Alto",
              "Quilicura"="Quilicura", "Parque O'Higgins"="Santiago",
              "Talagante"="Talagante", "Cerrillos"="Cerrillos")

# ==============================================================================
# 3. HEALTHY DAYS CALCULATION (MULTI-NORM COMPLIANCE)
# ==============================================================================
print("--- Calculating Healthy Days and Metrics ---")
# Chile/EPA/WHO % y promedio_anual quedan en NA para las estacion-anios no
# conformes (Decreto 12/2011) -- mismo criterio que Tabla 2 y Supplementary
# Table 3 (healthy days), para que los mapas no pinten un valor calculado
# sobre datos incompletos (Revisor 2). p98 se deja SIEMPRE visible, igual
# que en Tabla 2 (Min/Max/P98 se muestran por transparencia aunque el anio
# no sea conforme; solo el promedio/los % se suprimen).
datos_mapa_long <- df_analisis %>%
  group_by(comuna, anio) %>%
  summarise(
    conforme_anio = first(anio_conforme),
    `Chile (50 µg/m³)` = if_else(conforme_anio, mean(mp25_prom_valid <= 50, na.rm=TRUE) * 100, NA_real_),
    `EPA (35 µg/m³)`   = if_else(conforme_anio, mean(mp25_prom_valid <= 35, na.rm=TRUE) * 100, NA_real_),
    `WHO (15 µg/m³)`   = if_else(conforme_anio, mean(mp25_prom_valid <= 15, na.rm=TRUE) * 100, NA_real_),
    p98 = quantile(mp25_prom_valid, 0.98, type=7, na.rm=TRUE),
    promedio_anual = if_else(conforme_anio, first(promedio_anual_regulatorio), NA_real_),
    .groups = "drop"
  ) %>%
  select(-conforme_anio) %>%
  mutate(name_com = recode(comuna, !!!edit_com)) %>%
  pivot_longer(cols = c(`Chile (50 µg/m³)`, `EPA (35 µg/m³)`, `WHO (15 µg/m³)`),
               names_to = "norma",
               values_to = "pct_cumple") %>%
  mutate(norma = factor(norma, levels = c("Chile (50 µg/m³)", "EPA (35 µg/m³)", "WHO (15 µg/m³)")))

# ==============================================================================
# 4. GEOSPATIAL GEOMETRIES (CHILEMAPAS)
# ==============================================================================
print("--- Preparing Spatial Data ---")
comunas_excluir <- c("San Jose De Maipo","Pirque","Paine","Alhue","San Pedro",
                     "Melipilla","El Monte","Maria Pinto","Curacavi",
                     "Lampa","Tiltil","Colina","Lo Barnechea","Buin","Isla De Maipo")

rm_sf <- chilemapas::mapa_comunas %>%
  filter(codigo_region == "13") %>%
  left_join(chilemapas::codigos_territoriales %>% filter(codigo_region=="13"), by="codigo_comuna") %>%
  mutate(name_com = stringr::str_to_title(nombre_comuna)) %>%
  filter(!name_com %in% comunas_excluir) %>%
  st_as_sf() %>%
  st_transform(4326) # Transformamos a WGS84 inmediatamente para cruzar con las estaciones

# ==============================================================================
# 5. STATIONS LOCATION AND ELEVATION (SINCA)
# ==============================================================================
#Calculating Elevation for SINCA Stations
# Leer shapefile desde la nueva carpeta
est <- sf::st_read("input/shp_SINCA/EstacionesdeCalidaddeAire.shp")

# A WGS84 (recomendado para elevatr)
est_wgs <- st_transform(est, 4326)

# Obtener altitud (msnm) para cada punto
est_h <- elevatr::get_elev_point(est_wgs, src = "aws")

# Guardar en la columna 'altura' y exportar
est_wgs$altura <- est_h$elevation
#write_csv(
#  cbind(st_drop_geometry(est_wgs),
#        longitud = st_coordinates(est_wgs)[,1],
#        latitud = st_coordinates(est_wgs)[,2],
#        altura = est_wgs$altura),
#  "output/tables/table_estaciones_con_altura.csv"
#)

# 1. Identificamos estrictamente las 10 comunas del estudio
comunas_estudio <- rm_sf %>% filter(name_com %in% names(id_map))

# 2. Intersectamos las estaciones SOLO con esas 10 comunas
estaciones_rm <- st_intersection(est_wgs, comunas_estudio)

# ==============================================================================
# 6. GENERATING MAPS
# ==============================================================================
# Preparar etiquetas numéricas para los mapas
sf_use_s2(FALSE)
label_pts <- rm_sf %>% st_point_on_surface() %>%
  mutate(label_id = id_map[name_com]) %>%
  filter(!is.na(label_id))

# ------------------------------------------------------------------------------
# MAPA 0: MAPA BASE CON ESTACIONES Y ROSA DE LOS VIENTOS
# ------------------------------------------------------------------------------
mapa_estaciones <- ggplot() +
  # Geometría de las comunas
  geom_sf(data = rm_sf, fill = "grey95", color = "grey60", linewidth = 0.3) +
  # Puntos de las estaciones (Triángulos = shape 17)
  geom_sf(data = estaciones_rm, shape = 17, size = 3, color = "darkred") +
  # Etiquetas de comunas
  geom_shadowtext(data = label_pts, aes(geometry = geometry, label = label_id),
                  stat = "sf_coordinates", size = 3, fontface = "bold",
                  color = "black", bg.color = "white") +
  # Rosa de los vientos (Norte)
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  # Escala espacial (Opcional, pero muy útil para papers)
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_void()

ggsave("output/figures/mapa_00_estaciones_base_2.0.png", plot = mapa_estaciones, width = 8, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# MAPAS 1, 2 Y 3 (PM2.5 METRICS)
# ------------------------------------------------------------------------------
# Unimos datos a la estructura espacial
base_grid <- tibble(name_com = unique(rm_sf$name_com)) %>%
  crossing(anio = unique(datos_mapa_long$anio), norma = unique(datos_mapa_long$norma))
datos_finales_mapa <- base_grid %>% left_join(datos_mapa_long, by = c("name_com", "anio", "norma"))
df_mapa_completo <- rm_sf %>% left_join(datos_finales_mapa, by = "name_com")

# A) MAP: MULTI-NORM COMPLIANCE (3x3)
mapa_multinorma <- ggplot(df_mapa_completo) +
  geom_sf(aes(fill = pct_cumple), color = "grey60", linewidth = 0.2) +
  scale_fill_gradientn(colours = rev(aqi_cols), na.value = "grey90", name = "% Healthy days", limits = c(0, 100)) +
  geom_shadowtext(data = label_pts, aes(geometry = geometry, label = label_id), stat = "sf_coordinates", size = 2.5, fontface = "bold", color = "black", bg.color = "white") +
  facet_grid(anio ~ norma) +
  theme_void() +
  theme(plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
        strip.text = element_text(face = "bold", size = 9, margin = margin(b = 10)),
        legend.position = "right", legend.title = element_text(size = 8, face = "bold"), legend.text = element_text(size = 7))

ggsave("output/figures/mapa_01_multinorma_2.0.png", plot = mapa_multinorma, width = 8, height = 6, dpi = 300)

# B) MAP: P98
df_mapa_unico <- df_mapa_completo %>% filter(norma == "Chile (50 µg/m³)")
mapa_p98 <- ggplot(df_mapa_unico) +
  geom_sf(aes(fill = p98), color = "grey60", linewidth = 0.2) +
  scale_fill_gradientn(colours = aqi_cols, na.value = "grey90", name = expression(P[98]~"("*mu*"g/"*m^3*")")) +
  geom_shadowtext(data = label_pts, aes(geometry = geometry, label = label_id), stat = "sf_coordinates", size = 3, fontface = "bold", color = "black", bg.color = "white") +
  facet_wrap(~anio) +
  theme_void() +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        strip.text = element_text(face = "bold", size = 11, margin = margin(b=10)),
        legend.title = element_text(size = 9, face = "bold"), legend.position = "right")

ggsave("output/figures/mapa_02_p98_2.0.png", plot = mapa_p98, width = 8, height = 4, dpi = 300)

# C) MAP: ANNUAL AVERAGE
mapa_promedio <- ggplot(df_mapa_unico) +
  geom_sf(aes(fill = promedio_anual), color = "grey60", linewidth = 0.2) +
  scale_fill_gradientn(colours = aqi_cols, na.value = "grey90", name = "Promedio") +
  geom_shadowtext(data = label_pts, aes(geometry = geometry, label = label_id), stat = "sf_coordinates", size = 3, fontface = "bold", color = "black", bg.color = "white") +
  facet_wrap(~anio) +
  theme_void()

ggsave("output/figures/mapa_03_promedio_anual_2.0.png", plot = mapa_promedio, width = 8, height = 4, dpi = 300)

# ------------------------------------------------------------------------------
# D) MAP: P98 WINTER ONLY (PERIOD GEC)
# ------------------------------------------------------------------------------

# 1. Calculate P98 exclusively for winter using daily data (se deja siempre
# visible, igual que el p98 anual -- ver nota en el punto 3)
datos_invierno_p98 <- df_analisis %>%
  filter(es_invierno == "Winter") %>%
  group_by(comuna, anio) %>%
  summarise(p98 = quantile(mp25_prom_valid, 0.98, type = 7, na.rm = TRUE), .groups = "drop") %>%
  mutate(name_com = recode(comuna, !!!edit_com))

# 2. Create a base grid to ensure ALL background communes appear in every facet year
base_grid_invierno <- tibble(name_com = unique(rm_sf$name_com)) %>%
  crossing(anio = unique(datos_invierno_p98$anio))

# 3. Join the data: First to the grid, then to the spatial geometry
df_mapa_invierno <- rm_sf %>%
  left_join(
    base_grid_invierno %>% left_join(datos_invierno_p98, by = c("name_com", "anio")),
    by = "name_com"
  )

# 4. Generate the map
mapa_p98_invierno <- ggplot(df_mapa_invierno) +
  geom_sf(aes(fill = p98), color = "grey60", linewidth = 0.2) +
  # Usamos grey90 de vuelta para mantener coherencia estética con tu mapa original
  scale_fill_gradientn(colours = aqi_cols, na.value = "grey90", name = expression("Winter "~P[98]~"("*mu*"g/"*m^3*")")) +
  geom_shadowtext(data = label_pts, aes(geometry = geometry, label = label_id), stat = "sf_coordinates", size = 3, fontface = "bold", color = "black", bg.color = "white") +
  facet_wrap(~anio) +
  theme_void() +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        strip.text = element_text(face = "bold", size = 11, margin = margin(b=10)),
        legend.title = element_text(size = 9, face = "bold"), legend.position = "right")

# 5. Save the plot
ggsave("output/figures/map_04_p98_winter_2.0.png", plot = mapa_p98_invierno, width = 8, height = 4, dpi = 300)

# ------------------------------------------------------------------------------
# E) MAP: MULTI-NORM COMPLIANCE (3x3) - WINTER ONLY
# ------------------------------------------------------------------------------

# 1. Calcular el cumplimiento de las normativas exclusivamente para invierno
# Usa el MISMO criterio que Tabla 2/Supplementary Table 3 para invierno: con
# 3 o 4 de los 4 meses de invierno validos se mantiene el resultado, con
# menos de 3 queda NA. NO se usa la columna invierno_conforme cruda del
# dataset (esa exige los 4 meses completos, era la regla vieja) -- se
# recalcula aqui mismo para quedar consistente con lo que ya se decidio
# para las tablas.
datos_invierno_multinorma <- df_analisis %>%
  filter(es_invierno == "Winter") %>%
  mutate(
    meses_validos_invierno = 4L - lengths(strsplit(meses_invierno_no_validos, ",")),
    conforme_invierno = meses_validos_invierno >= 3
  ) %>%
  group_by(comuna, anio) %>%
  summarise(
    conforme_invierno = first(conforme_invierno),
    `Chile (50 µg/m³)` = if_else(conforme_invierno, mean(mp25_prom_valid <= 50, na.rm=TRUE) * 100, NA_real_),
    `EPA (35 µg/m³)`   = if_else(conforme_invierno, mean(mp25_prom_valid <= 35, na.rm=TRUE) * 100, NA_real_),
    `WHO (15 µg/m³)`   = if_else(conforme_invierno, mean(mp25_prom_valid <= 15, na.rm=TRUE) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  select(-conforme_invierno) %>%
  mutate(name_com = recode(comuna, !!!edit_com)) %>%
  pivot_longer(cols = c(`Chile (50 µg/m³)`, `EPA (35 µg/m³)`, `WHO (15 µg/m³)`),
               names_to = "norma",
               values_to = "pct_cumple") %>%
  mutate(norma = factor(norma, levels = c("Chile (50 µg/m³)", "EPA (35 µg/m³)", "WHO (15 µg/m³)")))

# 2. Crear grilla base para asegurar que aparezcan todas las comunas de fondo (Comuna x Año x Norma)
base_grid_invierno_multi <- tibble(name_com = unique(rm_sf$name_com)) %>%
  crossing(
    anio = unique(datos_invierno_multinorma$anio),
    norma = unique(datos_invierno_multinorma$norma)
  )

# 3. Unir datos: Grilla -> Datos de invierno -> Geometría espacial
df_mapa_invierno_multi <- rm_sf %>%
  left_join(
    base_grid_invierno_multi %>% left_join(datos_invierno_multinorma, by = c("name_com", "anio", "norma")),
    by = "name_com"
  )

# 4. Generar el mapa facetado (3x3)
mapa_multinorma_invierno <- ggplot(df_mapa_invierno_multi) +
  geom_sf(aes(fill = pct_cumple), color = "grey60", linewidth = 0.2) +
  # Usamos rev(aqi_cols) para que los porcentajes altos (100% cumple) tengan colores "sanos"
  scale_fill_gradientn(colours = rev(aqi_cols), na.value = "grey90", name = "% Healthy days\n(Winter)", limits = c(0, 100)) +
  geom_shadowtext(data = label_pts, aes(geometry = geometry, label = label_id), stat = "sf_coordinates", size = 2.5, fontface = "bold", color = "black", bg.color = "white") +
  facet_grid(anio ~ norma) +
  theme_void() +
  theme(plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
        strip.text = element_text(face = "bold", size = 9, margin = margin(b = 10)),
        legend.position = "right",
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 7))

# 5. Guardar el gráfico
ggsave("output/figures/mapa_05_multinorma_winter_2.0.png", plot = mapa_multinorma_invierno, width = 8, height = 6, dpi = 300)

rm(list = ls())
