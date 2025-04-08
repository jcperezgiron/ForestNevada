# Script para recuperar el area y volumen de la parcela

# Load libraries
library(tidyverse)
library(openxlsx)

sheets <- 1:3


output <- list()
# Loop over the sheets
for(i in sheets){
  todo_dasometrico <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/todo_dasometrico.xlsx", 
                                sheet = i, 
                                fillMergedCells = T, 
                                check.names = T
  )
  
  names(todo_dasometrico)
  unique(todo_dasometrico$Campo)
  
    tmp <- todo_dasometrico %>%
    filter(Campo %in% c("Superficie parcela corregida", "Volumen medio total /  Ha ")) %>% 
      dplyr::select(-Apart., -Apart..1, -Ud.) %>%
    pivot_longer(cols = -c(Campo), 
                 names_to = "cod_parcela", 
                 values_to = "valor") %>%
      pivot_wider(
        names_from = Campo,
        values_from = valor
      ) %>% 
      mutate_at(vars(-cod_parcela), as.numeric) 
  
    output[[i]] <- tmp
}

# Bind the list of dataframes
output <- bind_rows(output)

# Read the excel sheet with all info
parcelas_v2 <- read.xlsx("H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/diccionarios_sf_v2.xlsx", 
                              sheet = "dicc_parcelas", 
                              fillMergedCells = T, 
                              check.names = T
)

# Merge the dataframes
parcelas_v2_fixed <- parcelas_v2 %>% 
  left_join(output, by = c("cod_parcela"))

write.xlsx(parcelas_v2_fixed, "H:/Mi unidad/ForestNevada/datos_brutos/sinfonevada/Volumen_y_area_parcela.xlsx")
