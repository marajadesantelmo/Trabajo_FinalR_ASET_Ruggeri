
# ==============================================================================
# TRABAJO FINAL ASET - PROGRAMACIÓN Y VISUALIZACIÓN EN ESTADÍSTICAS LABORALES
# Script de procesamiento EPH (.xls / .xlsx) - Gran Mendoza (Aglomerado 10)
# Tasas básicas mercado de trabajo - comparación jóvenes y adultos/as y varones y mujeres
# ==============================================================================

# 0. LIMPIEZA Y CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
rm(list = ls())       # Borra variables de la memoria
graphics.off()        # Cierra gráficos
cat("\014")           # Limpia la consola
gc()                  # Libera memoria RAM

# Fijar carpeta de trabajo: la ruta fija si existe; si no, la raíz del repositorio
# (carpeta donde está este script)
dir_fijo <- "C:/Trabajo_FinalR_ASET_Ruggeri"
if (dir.exists(dir_fijo)) {
  setwd(dir_fijo)
} else {
  ruta_script <- tryCatch({
    args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(args) > 0) {
      sub("^--file=", "", args[1])                                  # Rscript
    } else if (!is.null(sys.frames()[[1]]$ofile)) {
      sys.frames()[[1]]$ofile                                       # source()
    } else {
      rstudioapi::getSourceEditorContext()$path                     # RStudio
    }
  }, error = function(e) "")
  if (nzchar(ruta_script)) setwd(dirname(normalizePath(ruta_script)))
}

# 1. Cargar librerías necesarias
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)     # Para archivos .xls y .xlsx
  library(haven)      # Por si hubiera archivos .sav
})

# 2. Registro en archivo LOG (Requisito trabajo): 
log_file <- "log.txt"
time_stamp <- paste0("Script ejecutado con éxito el: ", Sys.time(), "\n")
cat(time_stamp, file = log_file, append = TRUE)
message("--> Registro agregado al archivo log.txt")

# 3. Lectura de todas las bases - carpeta "bases"
archivos_bases <- list.files(
  path = "bases", 
  pattern = "[.](xls|xlsx|rds|sav|csv|txt)$", 
  full.names = TRUE, 
  recursive = TRUE,
  ignore.case = TRUE
)

# 4. Función para cargar, estandarizar y homogeneizar tipos de datos
procesar_eph <- function(ruta_archivo) {
  ext <- tolower(tools::file_ext(ruta_archivo))
  
  if (ext %in% c("xls", "xlsx")) {
    df <- read_excel(ruta_archivo)
  } else if (ext == "rds") {
    df <- read_rds(ruta_archivo)
  } else if (ext == "sav") {
    df <- read_sav(ruta_archivo)
  } else if (ext %in% c("csv", "txt")) {
    df <- read_delim(ruta_archivo, delim = ";", show_col_types = FALSE)
  }
  
  # Normalizar nombres de columnas a mayúsculas
  colnames(df) <- toupper(colnames(df))
  
  # Seleccionar solo variables de interés
  vars_interes <- c("ANO4", "TRIMESTRE", "AGLOMERADO", "CH04", "CH06", "ESTADO", "CAT_OCUP", "PP07H", "PONDERA")
  vars_presentes <- intersect(vars_interes, colnames(df))
  
  # Seleccionar y convertir todas las columnas a formato numérico para evitar errores de tipo
  df %>% 
    select(all_of(vars_presentes)) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.))))
}

# Unir todas las bases individuales sin conflicto de tipos
eph_unida <- map_dfr(archivos_bases, procesar_eph)

# 5. Filtrado Gran Mendoza (AGLOMERADO == 10) y recodificación
eph_trabajo <- eph_unida %>%
  filter(AGLOMERADO == 10) %>%  # Gran Mendoza
  mutate(
    # grupos etarios
    grupo_etario = case_when(
      CH06 >= 14 & CH06 <= 29 ~ "Jóvenes (14-29)",
      CH06 >= 30 & CH06 <= 64 ~ "Adultos (30-64)",
      TRUE ~ "Otros"
    ),
    # Sexo
    sexo = case_when(
      CH04 == 1 ~ "Varones",
      CH04 == 2 ~ "Mujeres"
    ),
# 6. Creación de DUMMY DE PRECARIEDAD LABORAL (Requisito trabajo):
# Ocupados Asalariados (ESTADO == 1 & CAT_OCUP == 3)
# PP07H == 2: No tiene descuento jubilatorio (Informal)
# PP07H == 1: Tiene descuento jubilatorio (Registrado / Formal)
informal_dummy = case_when(
  ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2 ~ 1,
  ESTADO == 1 & CAT_OCUP == 3 & PP07H == 1 ~ 0,
  TRUE ~ NA_real_
)
  ) %>%
  filter(grupo_etario %in% c("Jóvenes (14-29)", "Adultos (30-64)"))

# 7. Conteo de Casos (total Nacional y muestra Gran Mendoza) (Requisito trabajo):
# A. Muestra Total Nacional por año (sin filtrar)
conteo_nacional <- eph_unida %>%
  group_by(ANO4) %>%
  summarise(
    casos_muestrales = n(),
    poblacion_expandida = sum(PONDERA, na.rm = TRUE),
    asalariados_muestra = NA_real_,
    .groups = "drop"
  ) %>%
  mutate(cobertura = "Total Nacional", grupo_etario = "Total", sexo = "Total")

# B. Muestra Total Gran Mendoza por año
conteo_mendoza_total <- eph_trabajo %>%
  group_by(ANO4) %>%
  summarise(
    casos_muestrales = n(),
    poblacion_expandida = sum(PONDERA, na.rm = TRUE),
    asalariados_muestra = sum(!is.na(informal_dummy)),
    .groups = "drop"
  ) %>%
  mutate(cobertura = "Gran Mendoza", grupo_etario = "Total", sexo = "Total")

# C. Muestra Gran Mendoza Desglosada (por Grupo Etario y Sexo)
conteo_mendoza_desglose <- eph_trabajo %>%
  group_by(ANO4, grupo_etario, sexo) %>%
  summarise(
    casos_muestrales = n(),
    poblacion_expandida = sum(PONDERA, na.rm = TRUE),
    asalariados_muestra = sum(!is.na(informal_dummy)),
    .groups = "drop"
  ) %>%
  mutate(cobertura = "Gran Mendoza")

# Unir todas las coberturas en la misma tabla
conteo_casos <- bind_rows(conteo_nacional, conteo_mendoza_total, conteo_mendoza_desglose) %>%
  select(ANO4, cobertura, grupo_etario, sexo, casos_muestrales, poblacion_expandida, asalariados_muestra)

# Guardar único archivo CSV
write_csv(conteo_casos, "resultados/01_conteo_casos_muestra.csv")

# 8. Tasas laborales anualizadas e informalidad asalariados/as (Totales, Por Edad, Por Sexo y Cruce)
calcular_tasas <- function(df, ...) {
  df %>%
    group_by(ANO4, ...) %>%
    summarise(
      poblacion_total = sum(PONDERA, na.rm = TRUE),
      pea = sum(PONDERA[ESTADO %in% c(1, 2)], na.rm = TRUE),
      ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
      desocupados = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
      asalariados_totales = sum(PONDERA[!is.na(informal_dummy)], na.rm = TRUE),
      asalariados_informales = sum(PONDERA[informal_dummy == 1], na.rm = TRUE),
      
      # Tasas (%)
      tasa_actividad = round((pea / poblacion_total) * 100, 1),
      tasa_empleo = round((ocupados / poblacion_total) * 100, 1),
      tasa_desocupacion = round((desocupados / pea) * 100, 1),
      tasa_informalidad_asalariados = round((asalariados_informales / asalariados_totales) * 100, 1),
      .groups = "drop"
    )
}

# A. Total Poblacional (Gran Mendoza)
tasas_total <- calcular_tasas(eph_trabajo) %>% 
  mutate(grupo_etario = "Total Poblacional", sexo = "Total")

# B. Solo por Grupo Etario (Jóvenes vs Adultos)
tasas_etario <- calcular_tasas(eph_trabajo, grupo_etario) %>% 
  mutate(sexo = "Total")

# C. Solo por Sexo (Varones vs Mujeres)
tasas_sexo <- calcular_tasas(eph_trabajo, sexo) %>% 
  mutate(grupo_etario = "Total")

# D. Cruce completo (Grupo Etario x Sexo)
tasas_cruce <- calcular_tasas(eph_trabajo, grupo_etario, sexo)

# Unir todas las aperturas en una única tabla de resultados
tasas_anualizadas <- bind_rows(tasas_total, tasas_etario, tasas_sexo, tasas_cruce)

# Guardar en la carpeta resultados
write_csv(tasas_anualizadas, "resultados/02_tasas_laborales_anualizadas.csv")



  # TRABAJO FINAL ASET - PROGRAMACIÓN Y VISUALIZACIÓN EN ESTADÍSTICAS LABORALES
  # Script generación de gráficos
  # Tasas básicas mercado de trabajo - comparación jóvenes y adultos/as y varones y mujeres
  # ==============================================================================

graphics.off()
library(tidyverse)

# 1. Cargar datos procesados de tasas (detectando automáticamente el separador ',' o ';')
linea1 <- readLines("resultados/02_tasas_laborales_anualizadas.csv", n = 1)
sep_char <- if (grepl(";", linea1)) ";" else ","

tasas <- read_delim("resultados/02_tasas_laborales_anualizadas.csv", delim = sep_char, show_col_types = FALSE)

# Asegurar formato de variables
tasas <- tasas %>%
  mutate(
    ANO4 = as.factor(ANO4),
    tasa_asalarizacion = ifelse(
      "tasa_asalarizacion" %in% colnames(.), 
      tasa_asalarizacion, 
      round((asalariados_totales / ocupados) * 100, 1)
    )
  )

# 2. Paletas de colores 
colores_edad <- c(
  "Total Poblacional" = "#005b96", # Azul
  "Jóvenes (14-29)"   = "#00a676", # Verde/Turquesa
  "Adultos (30-64)"   = "#d35400"  # Naranja
)

colores_sexo <- c(
  "Total Poblacional" = "#005b96", # Azul
  "Varones"           = "#2980b9", # Celeste/Azul
  "Mujeres"           = "#8e44ad"  # Violeta
)

# 3. Tema visual (sin grilla de fondo)
tema_visual <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_blank(),                         # Sin grilla de fondo
    axis.line.x = element_line(color = "black", linewidth = 0.6), # Eje X continuo
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text = element_text(color = "black", size = 10),
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 12))
  )

# 4. Filtrado de Subsets de Datos
datos_edad <- tasas %>%
  filter(sexo == "Total" & grupo_etario %in% c("Total Poblacional", "Jóvenes (14-29)", "Adultos (30-64)")) %>%
  mutate(grupo_etario = factor(grupo_etario, levels = c("Total Poblacional", "Jóvenes (14-29)", "Adultos (30-64)")))

datos_sexo <- tasas %>%
  filter(grupo_etario %in% c("Total Poblacional", "Total") & sexo %in% c("Total", "Varones", "Mujeres")) %>%
  mutate(
    categoria_sexo = case_when(
      sexo == "Total" ~ "Total Poblacional",
      TRUE ~ sexo
    ),
    categoria_sexo = factor(categoria_sexo, levels = c("Total Poblacional", "Varones", "Mujeres"))
  )

# 5. Definición de las 5 tasas a graficar
lista_tasas <- list(
  list(var = "tasa_actividad", nombre = "Tasa de Actividad", sub = "Porcentaje de la PEA sobre población total"),
  list(var = "tasa_empleo", nombre = "Tasa de Empleo", sub = "Porcentaje de ocupados sobre población total"),
  list(var = "tasa_desocupacion", nombre = "Tasa de Desocupación", sub = "Porcentaje de desocupados sobre la PEA"),
  list(var = "tasa_asalarizacion", nombre = "Tasa de Asalarización", sub = "Porcentaje de asalariados sobre el total de ocupados"),
  list(var = "tasa_informalidad_asalariados", nombre = "Tasa de Informalidad Asalariada", sub = "Porcentaje de asalariados sin descuento jubilatorio")
)


# GENERACIÓN DE LOS 10 GRÁFICOS
# ==============================================================================

contador <- 1

for (tasa in lista_tasas) {
  var_name <- tasa$var
  titulo_tasa <- tasa$nombre
  sub_tasa <- tasa$sub
  
  # A. GRÁFICO POR GRUPO ETARIO
  g_edad <- ggplot(datos_edad, aes(x = ANO4, y = .data[[var_name]], fill = grupo_etario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = paste0(.data[[var_name]], "%")),
      position = position_dodge(width = 0.8),
      vjust = -0.5, size = 3.1, fontface = "bold"
    ) +
    scale_fill_manual(values = colores_edad) +
    scale_y_continuous(limits = c(0, max(datos_edad[[var_name]], na.rm = TRUE) + 10), expand = c(0, 0)) +
    labs(
      title = paste0("Gran Mendoza: ", titulo_tasa, " por Grupo Etario"),
      subtitle = paste0(sub_tasa, ". Promedios anuales (2019-2025)"),
      x = "Año", y = paste0(titulo_tasa, " (%)"),
      caption = "Fuente: Elaboración propia en base a microdatos de la EPH-INDEC (Aglomerado Gran Mendoza)."
    ) +
    tema_visual
  
  file_edad <- sprintf("resultados/grafico_%02d_%s_edad.png", contador, var_name)
  ggsave(file_edad, g_edad, width = 9, height = 5.5, dpi = 300)
  contador <- contador + 1
  
  # B. GRÁFICO POR SEXO
  g_sexo <- ggplot(datos_sexo, aes(x = ANO4, y = .data[[var_name]], fill = categoria_sexo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = paste0(.data[[var_name]], "%")),
      position = position_dodge(width = 0.8),
      vjust = -0.5, size = 3.1, fontface = "bold"
    ) +
    scale_fill_manual(values = colores_sexo) +
    scale_y_continuous(limits = c(0, max(datos_sexo[[var_name]], na.rm = TRUE) + 10), expand = c(0, 0)) +
    labs(
      title = paste0("Gran Mendoza: ", titulo_tasa, " por Sexo"),
      subtitle = paste0(sub_tasa, ". Promedios anuales (2019-2025)"),
      x = "Año", y = paste0(titulo_tasa, " (%)"),
      caption = "Fuente: Elaboración propia en base a microdatos de la EPH-INDEC (Aglomerado Gran Mendoza)."
    ) +
    tema_visual
  
  file_sexo <- sprintf("resultados/grafico_%02d_%s_sexo.png", contador, var_name)
  ggsave(file_sexo, g_sexo, width = 9, height = 5.5, dpi = 300)
  contador <- contador + 1
}


