# Script para adaptar la base de datos sinfonevada al formato de datos de Linaria

# Load libraries
library(tidyverse)
library(openxlsx)

# Visitas

dicc_parcelas <- read.csv("C:/SCIENCE/2025_UGR_Biod22/Sinfonevada_csvs/diccionarios_sf_v2.xlsx - dicc_parcelas.csv", na.strings = "")

dicc_parcelas <- dicc_parcelas %>%
  # fill hora_inicio and hora_fin with 00:00:00 when missing
  mutate(hora_inicio = case_when(is.na(hora_inicio) & is.na(hora_fin) ~ "00:00:00",
                                 is.na(hora_inicio) & !is.na(hora_fin) ~ hora_fin,
                                 .default = hora_inicio),
         hora_fin = case_when(is.na(hora_fin) ~ hora_inicio,
                              .default = hora_fin)) %>% 
  # create fecha_inicio and fecha_fin as datetime columns
  mutate(fecha_inicio = as_datetime(paste0(fecha, " ", hora_inicio), format = "%d/%m/%Y %H:%M:%S"),
         fecha_fin = as_datetime(paste0(fecha, " ", hora_fin), format = "%d/%m/%Y %H:%M:%S"))


glimpse(dicc_parcelas)

# id	validated_by	created_by	geo_id	fecha_inicio	observador_1_id	observador_2_id	dicc_viento_id	temperatura	dicc_nubes_id	
# observaciones	created_at	updated_at	ptf	protocolo_id	fecha_fin	longitud

visitas <- tibble(
  .rows = nrow(dicc_parcelas),
  id = NA,
  validated_by = NA,
  created_by = NA,
  geo_id = dicc_parcelas$cod_parcela,
  fecha_inicio = dicc_parcelas$fecha_inicio,
  observador_1_id = 0,
  observador_2_id = NA,
  dicc_viento_id = NA,
  temperatura = NA,
  dicc_nubes_id = NA,
  observaciones = NA,
  created_at = NA,
  updated_at = NA,
  ptf = NA,
  protocolo_id = NA,
  fecha_fin = dicc_parcelas$fecha_fin,
  longitud = dicc_parcelas$longitud
)
