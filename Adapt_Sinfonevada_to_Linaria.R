# Script para adaptar la base de datos sinfonevada al formato de datos de Linaria

# Load libraries
library(tidyverse)
library(openxlsx)
library(sf)

#### Parámetros generales que se pueden cambiar ####
protocolo = 68



##################

# Visitas

dicc_parcelas <- read.csv("H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/Sinfonevada_csvs/diccionarios_sf_v2.xlsx - dicc_parcelas.csv", na.strings = "")

dicc_parcelas <- dicc_parcelas %>%
  # fill hora_inicio and hora_fin with 00:00:00 when missing
  mutate(hora_inicio = case_when(is.na(hora_inicio) & is.na(hora_fin) ~ "00:00:00",
                                 is.na(hora_inicio) & !is.na(hora_fin) ~ hora_fin,
                                 .default = hora_inicio),
         hora_fin = case_when(is.na(hora_fin) ~ hora_inicio,
                              .default = hora_fin)) %>% 
  # Genero un campo de observaciones para indicar que se ha corregido el año en los casos en que el año no es 2004 (fecha del muestreo) y rellenar las fechas vacías con 01/01/2004
  mutate(fecha = as_date(fecha, format = "%d/%m/%Y"),
         obs = case_when(year(fecha) != 2004 ~ "Se ha corregido el año, pues no coincidía con la fecha de realización del inventario",
                         is.na(fecha) ~ "Fecha no proporcionada, se ha rellenado con 01/01/2004",
                         TRUE ~ NA_character_)) %>% 
  # Convertir fechas y corregir años manteniendo el día y mes cuando el año no es 2004
  mutate(fecha = case_when(
    !is.na(fecha) & year(fecha) != 2004 ~ as_date(paste0("2004-", month(fecha), "-", day(fecha))), 
    is.na(fecha) ~ as_date("2004-01-01"), 
    TRUE ~ fecha)) %>% 
  # create fecha_inicio and fecha_fin as datetime columns
  mutate(fecha_inicio = as_datetime(paste0(fecha, " ", hora_inicio), format = "%Y-%m-%d %H:%M:%S"),
         fecha_fin = as_datetime(paste0(fecha, " ", hora_fin), format = "%Y-%m-%d %H:%M:%S"))
  


glimpse(dicc_parcelas)

#### visitas ####
visitas <- tibble(
  .rows = nrow(dicc_parcelas),
  id = 1:nrow(dicc_parcelas),
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
  protocolo_id = protocolo,
  ptf = paste(protocolo_id, geo_id, format(fecha_inicio, "%Y%m%d"), sep = "-"), #generar protocolo/geo_id/fecha
  fecha_fin = dicc_parcelas$fecha_fin,
  longitud = NA
) %>% 
  select(id:updated_at, ptf, protocolo_id, fecha_fin, longitud)

write.csv(visitas, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/Output_csv/visitas.csv", row.names = FALSE)

#### geo ####

# convertir a shapefile
geo <- tibble(
  .rows = nrow(dicc_parcelas),
  id = dicc_parcelas$cod_parcela,
  nombre = dicc_parcelas$cod_parcela,
  transecto_padre = NA,
  n_tramo = NA,
  longitud_m = NA,
  min_altitud = dicc_parcelas$elevacion,
  max_altitud = dicc_parcelas$elevacion,
  habitat = NA,
  protocolo_id = protocolo,
  geom_x = dicc_parcelas$coord_x,
  geom_y = dicc_parcelas$coord_y,
  activo = TRUE,
  instrumentacion = NA,
  orientacion = dicc_parcelas$orientacion,
  observaciones = NA
) %>% 
  st_as_sf(coords = c("geom_x", "geom_y"), crs = 23030) %>% 
  st_transform(4326)

st_write(geo, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/Output_csv/geo.shp")


#### geo_aux ####
# Leo los diccionarios necesarios
municipios <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_municipios", fillMergedCells = T, check.names = T) %>% 
  select(nombre_municipio, cod_municipio_sf) %>% 
  mutate(nombre_municipio = str_trim(nombre_municipio))

coberturas <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_fcc", fillMergedCells = T, check.names = T)
montes <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_montes", fillMergedCells = T, check.names = T) %>% 
  mutate(nombre_monte = str_trim(nombre_monte),
         cod_monte = as.integer(cod_monte))

pedregosidad <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_cod_pedregosidad", fillMergedCells = T, check.names = T) %>% 
  mutate(cod_pedregosidad = as.character(cod_pedregosidad))

clase_de_suelo <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_cod_clase_suelo", fillMergedCells = T, check.names = T) %>% 
  mutate(cod_clase_suelo = as.character(cod_clase_suelo))

espesor_capa_muerta <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_cod_espesor_capa_muerta", fillMergedCells = T, check.names = T) %>% 
  mutate(cod_espesor_capa_muerta = as.character(cod_espesor_capa_muerta))

cobertura_suelo <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", sheet = "dicc_cod_cobertura_suelo", fillMergedCells = T, check.names = T)

geo_aux <- dicc_parcelas %>% 
  select(cod_parcela, provincia:monte, pendiente) %>% 
  mutate(provincia = case_when(provincia == 1 ~ "Granada",
                               provincia == 2 ~ "Almería",
                               .default = NA_character_),
  ) %>% 
  left_join(municipios, by = c("municipio" = "cod_municipio_sf")) %>%
  select(-municipio) %>%
  left_join(montes, by = c("monte" = "cod_monte")) %>%
  select(-monte) %>%
  rename(monte = nombre_monte) %>%

  rename(geo_id = cod_parcela) %>%
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(geo_id), names_to = "variable", values_to = "valor") %>% 
  drop_na(valor) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, geo_id, variable, valor)

write.csv(geo_aux, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/Output_csv/geo_aux.csv", row.names = FALSE)

