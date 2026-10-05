# Trabajo Final - Curso Programación y Visualización en Estadísticas Laborales (ASET)

**Autora:** Sabrina Ruggeri  
**Fuente de datos:** Encuesta Permanente de Hogares (EPH-INDEC, 2019-2025)  
**Aglomerado:** Gran Mendoza  

## Estructura del Repositorio
* `descarga_bases.R`: Descarga los microdatos trimestrales de la EPH (paquete `eph`) a `bases/`. Ejecutar primero.
* `bases/`: Microdatos trimestrales de la EPH (.rds generados por `descarga_bases.R`; no se versionan).
* `resultados/`: Tablas procesadas (.csv) y gráficos (.png).
* `script_procesamiento.R`: Script automatizado de R.
* `Trabajo_final_RASET_Ruggeri.Rmd`: Código fuente del informe RMarkdown.
* `Trabajo_final_RASET_Ruggeri.html`: Reporte final compilado.
* `log.txt`: Registro de ejecución del script.