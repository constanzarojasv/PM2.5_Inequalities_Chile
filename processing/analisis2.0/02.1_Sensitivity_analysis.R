# ==============================================================================
# SENSITIVITY ANALYSIS: repeated-measures tests for the year comparison
# (Table "p_value" column)

# Idea: instead of comparing ~350 non-independent DAILY values per year
# (current Table ANOVA -- pseudo-replication + no control for season),
# compare MONTHLY means, paired/blocked by calendar month across years.
# Only genuinely measured months are used (mes_valido == TRUE); months that
# were imputed under the Decreto 12/2011 rule are EXCLUDED from these
# significance tests
# Per municipality:
#   - 2 compliant years  -> paired t-test + Wilcoxon signed-rank test
#   - 3 compliant years  -> classical repeated-measures ANOVA (aov + Error)
#                           and a Friedman test (non-parametric equivalent)
# Shapiro-Wilk + note on normality is printed before each parametric test.
# ==============================================================================

library(readr)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------------------------
df_analisis   <- read_rds("input/data_processed/datos_analisis_final_2.0.rds")
tabla_mensual <- read_csv("output/tables/table_S1_completeness.csv", show_col_types = FALSE)

anios_conformes <- df_analisis %>%
  distinct(comuna, anio, anio_conforme) %>%
  mutate(comuna = as.character(comuna), anio = as.integer(as.character(anio)))

# Reference only: p-values CURRENTLY reported in Table 1 (daily one-way
# ANOVA, pseudo-replicated across ~350 non-independent days) for the
# comunas checked below, taken from output/tables/table_1_anual.csv, so you
# can compare "before vs after" side by side. Update if that file changes.
p_valor_original <- tribble(
  ~comuna,            ~p_value_original_daily_anova,
  "Cerrillos",         "< 0.001 *",
  "La Florida",        "0.002 *",
  "Parque O'Higgins",  "0.008 *",
  "Quilicura",         "0.006 *"
)

# ------------------------------------------------------------------------------
# 2. Monthly series for one comuna: real (non-imputed) valid months only,
#    restricted to years that meet the Decreto 12/2011 annual criterion.
# ------------------------------------------------------------------------------
obtener_serie_mensual <- function(comuna_objetivo) {
  anios_ok <- anios_conformes %>%
    filter(comuna == comuna_objetivo, anio_conforme) %>%
    pull(anio)

  tabla_mensual %>%
    filter(comuna == comuna_objetivo, anio_n %in% anios_ok, mes_valido) %>%
    select(comuna, anio_n, mes, media_mensual)
}

# ------------------------------------------------------------------------------
# 3. Run the appropriate sensitivity test for one comuna
# ------------------------------------------------------------------------------
analizar_comuna <- function(comuna_objetivo) {
  cat("\n==============================================================\n")
  cat("COMUNA:", comuna_objetivo, "\n")
  cat("==============================================================\n")

  serie <- obtener_serie_mensual(comuna_objetivo)
  anios_disponibles <- sort(unique(serie$anio_n))
  cat("Anios conformes disponibles:", paste(anios_disponibles, collapse = ", "), "\n")

  if (length(anios_disponibles) < 2) {
    cat("Menos de 2 anios conformes -- no se puede evaluar tendencia temporal.\n")
    return(invisible(NULL))
  }

  if (length(anios_disponibles) == 2) {
    # --- Caso 2 anios: t-test pareado + Wilcoxon --------------------------
    ancha <- serie %>%
      pivot_wider(names_from = anio_n, values_from = media_mensual, names_prefix = "y") %>%
      drop_na() # solo meses validos EN AMBOS anios (pareo estricto)

    nombres_y <- paste0("y", anios_disponibles)
    y1 <- ancha[[nombres_y[1]]]
    y2 <- ancha[[nombres_y[2]]]
    diferencias <- y2 - y1

    cat("N pares (meses validos en ambos anios):", length(diferencias), "\n\n")

    cat("--- Shapiro-Wilk (normalidad de las diferencias) ---\n")
    print(shapiro.test(diferencias))

    cat("\n--- t-test pareado ---\n")
    print(t.test(y2, y1, paired = TRUE))

    cat("\n--- Wilcoxon signed-rank (pareado, no parametrico) ---\n")
    print(wilcox.test(y2, y1, paired = TRUE))

  } else {
    # --- Caso 3 anios: RM-ANOVA + Friedman ----------------------------------
    serie_completa <- serie %>%
      group_by(mes) %>%
      filter(n() == length(anios_disponibles)) %>% # solo meses validos en LOS 3 anios (balanceado)
      ungroup() %>%
      mutate(anio_f = factor(anio_n), mes_f = factor(mes))

    cat("N meses balanceados (validos en los", length(anios_disponibles), "anios):",
        n_distinct(serie_completa$mes), "de 12\n\n")

    cat("--- Shapiro-Wilk (normalidad de media_mensual, por anio) ---\n")
    for (a in anios_disponibles) {
      vals <- serie_completa$media_mensual[serie_completa$anio_n == a]
      if (length(vals) >= 3) {
        cat("Anio", a, ": "); print(shapiro.test(vals))
      }
    }

    cat("\n--- ANOVA de medidas repetitivas (aov + Error(mes/anio)) ---\n")
    modelo_rm <- tryCatch(
      aov(media_mensual ~ anio_f + Error(mes_f / anio_f), data = serie_completa),
      error = function(e) NULL
    )
    if (!is.null(modelo_rm)) print(summary(modelo_rm)) else cat("No se pudo ajustar (datos no balanceados).\n")

    cat("\n--- Friedman test (no parametrico, equivalente a RM-ANOVA) ---\n")
    ancha <- serie_completa %>%
      select(mes, anio_f, media_mensual) %>%
      pivot_wider(names_from = anio_f, values_from = media_mensual) %>%
      select(-mes) %>%
      as.matrix()
    print(friedman.test(ancha))
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------
# 4. RUN: Cerrillos (referencia, ya revisado a mano) + 3 comunas adicionales
# ------------------------------------------------------------------------------
comunas_a_revisar <- c("Cerrillos", "La Florida", "Parque O'Higgins", "Quilicura")

for (cm in comunas_a_revisar) {
  analizar_comuna(cm)
}


