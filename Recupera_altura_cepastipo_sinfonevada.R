# Script para recuperar las alturas de las cepas tipo de sinfonevada

# Load libraries
library(tidyverse)
library(openxlsx)

sheets <- 1:3
altura <- list()
# Loop over the sheets
for(i in sheets){
  todo_dasometrico <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/todo_dasometrico.xlsx", 
                                sheet = i, 
                                fillMergedCells = T, 
                                check.names = T
  )
  
  names(todo_dasometrico)
  
  alturas_tmp <- todo_dasometrico %>%
    filter(Apart. %in% c("MONTE BAJO ESPECIE 1", "MONTE BAJO ESPECIE 2")) %>% 
    filter(Campo == "Altura de la cepa tipo") %>% 
    rename(cod_tipo_sp  = Apart.,
           cod_cepa_tipo = Apart..1) %>%
    pivot_longer(cols = all_of(names(todo_dasometrico)[5:ncol(todo_dasometrico)]), 
                 names_to = "cod_parcela", 
                 values_to = "Altura") %>% 
    mutate(cod_tipo_sp = case_when(
      cod_tipo_sp == "MONTE BAJO ESPECIE 1" ~ "SP1",
      cod_tipo_sp == "MONTE BAJO ESPECIE 2" ~ "SP2"),
      # substract the number from cepa tipo
      cod_cepa_tipo = as.character(str_extract(cod_cepa_tipo, "\\d+"))) %>% 
    # replace zeros with NA
    mutate(Altura = ifelse(Altura == 0, NA, Altura)) %>%
    # remove rows with NA
    drop_na(Altura) %>% 
    mutate(Altura = round(as.numeric(Altura), 1)) %>%
    select(-Campo, -Ud.)
  
  altura[[i]] <- alturas_tmp
}

# Bind the list of dataframes
altura <- bind_rows(altura)

# Read the excel sheet with all info
alturas_v2 <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", 
                              sheet = "cepa_tipo", 
                              fillMergedCells = T, 
                              check.names = T
) %>% 
  mutate(cod_cepa_tipo = as.character(cod_cepa_tipo))

# Merge the dataframes
alturas_v2_fixed <- alturas_v2 %>% 
  left_join(altura, by = c("cod_parcela", "cod_tipo_sp", "cod_cepa_tipo"))

write.xlsx(alturas_v2_fixed, "H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/Corrected_alturas_cepa_tipo.xlsx")
