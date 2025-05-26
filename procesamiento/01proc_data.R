# Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse,   # manipulacion datos
               sjPlot,      # tablas
               confintr,    # IC
               gginference, # visualizacion 
               rempsyc,     # reporte
               broom,       # varios
               sjmisc,      # para descriptivos
               knitr)       # para       

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo


# Carga datos ------------------------------------------------------------------

load("Github/trabajo1-grupo-4/input/data/WVS_Cross-National_Wave_7_Rdata_v6_0.RData")


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
data <- `WVS_Cross-National_Wave_7_v6_0` %>% 
  select(pais=B_COUNTRY, sexo=Q260, nacionalismo=Q254, democ, meanschooling, 
         hdi, giniWB,  Q121, Q122, Q123, Q124, Q125, Q126, Q127, Q128, 
         Q129, pos_pol=Q240, personal_income=Q288) #con variables contextuales, sin alfabetismo 
## Remover NA's ----------------------------------------------------------------

data <- data %>% 
  set_na(., na = c(-1, -2, -3, -4, -5, -999, -9999)) #Recodificamos variables a NA

colSums(is.na(data))

data <- na.omit(data)
## Recodificar y crear variables --------------------------------------------------------

data <- data %>%
  mutate(across(c(Q122, Q123, Q125, Q127), ~ case_when(
    .x == 2 ~ 0,
    .x == 0 ~ 2,
    TRUE ~ .x
  ))) #recodificado para mantener sentido

data <- data %>%
  mutate(across(c(sexo), ~ case_when(
    .x == 1 ~ 0,
    .x == 2 ~ 1,
    TRUE ~ .x
  )))

data <- data %>%
  mutate(across(c(Q121), ~ case_when(
    .x %in% c(1,2) ~ 2,
    .x == 3 ~ 1,
    .x %in% c(4, 5) ~ 0,
    TRUE ~ .x
  )))

data <- data %>% 
  rowwise() %>%
  mutate(perc_mig = sum(c(Q121, Q122, Q123, Q124, Q125, Q126, Q127, Q128,Q129))) %>% 
  ungroup() #Escala sumativa percepción de migración

# Guardar datos ----sum()# Guardar datos ----------------------------------------------------------------
save(data, file="Github/trabajo1-grupo-4/output/data.RData")
