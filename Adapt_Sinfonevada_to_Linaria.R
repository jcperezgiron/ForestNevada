# Script para adaptar la base de datos sinfonevada al formato de datos de Linaria

# Load libraries
library(tidyverse)
library(openxlsx)
library(sf)
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

#### visitas ####
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

write.csv(visitas, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/visitas.csv", row.names = FALSE)

#### geo ####
dicc_parcelas_sf <- st_as_sf(dicc_parcelas, coords = c("coord_x", "coord_y"), crs = 23030) %>% 
  st_transform(4326) %>% 
  select(cod_parcela)

st_write(dicc_parcelas_sf, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/geo.geojson")

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
  protocolo_id = NA,
  geom_x = st_coordinates(dicc_parcelas_sf)[,1],
  geom_y = st_coordinates(dicc_parcelas_sf)[,2],
  activo = TRUE,
  instrumentacion = NA,
  orientacion = dicc_parcelas$orientacion,
  observaciones = NA
)

write.csv(geo, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/geo.csv", row.names = FALSE)


#### geo_aux ####
# habrá que completarla cuando se sepa que variables no han entrando en otras tablas
geo_aux <- tibble(
  .rows = nrow(dicc_parcelas),
  id = NA,
  geo_id = dicc_parcelas$cod_parcela,
  variable = NA,
  valor = NA
)

# write.csv(geo_aux, "H:/Mi unidad/ForestNevada/Sinfonevada_to_Linaria/geo_aux.csv", row.names = FALSE)

