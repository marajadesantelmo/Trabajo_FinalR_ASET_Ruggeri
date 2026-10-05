# ==============================================================================
# DESCARGA DE BASES EPH-INDEC (2019-2025) con el paquete {eph}
# Guarda un .rds por trimestre en la carpeta "bases/", que luego lee
# Script_procesamientoR.R. Si el archivo ya existe, no lo vuelve a descargar.
# ==============================================================================

if (!requireNamespace("eph", quietly = TRUE)) install.packages("eph")

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

anios      <- 2019:2025
trimestres <- 1:4

dir.create("bases", showWarnings = FALSE)

for (anio in anios) {
  for (trim in trimestres) {
    destino <- file.path("bases", sprintf("EPH_individual_T%d_%d.rds", trim, anio))

    if (file.exists(destino)) {
      message("Ya existe: ", destino)
      next
    }

    message("Descargando ", anio, " T", trim, " ...")
    base <- tryCatch(
      eph::get_microdata(year = anio, trimester = trim, type = "individual"),
      error = function(e) {
        warning("No se pudo descargar ", anio, " T", trim, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(base)) saveRDS(base, destino)
  }
}

message("--> Bases disponibles en 'bases/': ", length(list.files("bases", pattern = "[.]rds$")))
