# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Estadística Correlacional 2023
#
#             Plantilla procesamiento trabajo final curso
#
# ******************************************************************************


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

load("input/data/WVS_Cross-National_Wave_7_Rdata_v6_0.RData")


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
data <- `WVS_Cross-National_Wave_7_v6_0` %>% 
  select(pais=B_COUNTRY, sexo=Q260, niv_educ=Q275, nacionalismo=Q254, democ, 
         polregfh, incomeWB, btidemstatus, meanschooling, 
         incomeindexHDI, hdi, Q121, Q122, Q123, Q124, Q125, Q126, Q127, Q128, 
         Q129, Q251, personal_income=Q288) #con variables contextuales, sin alfabetismo 

data_2 <- `WVS_Cross-National_Wave_7_v6_0` %>% 
  select(pais=B_COUNTRY, sexo=Q260, alf=E1_LITERACY, niv_educ=Q275, 
         nacionalismo=Q254, Q121, Q122, Q123, Q124, Q125, Q126, Q127, Q128, Q129, Q240,
         Q251, personal_income=Q288) #con alfabetismo
## Remover NA's ----------------------------------------------------------------

data <- data %>% 
  set_na(., na = c(-1, -2, -3, -4, -5, -999, -9999)) #Recodificamos variables a NA

data_2 <- data_2 %>% 
  set_na(., na = c(-1, -2, -3, -4, -5))

colSums(is.na(data))
colSums(is.na(data_2))

data <- na.omit(data)
data_2 <- na.omit(data_2)

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
  mutate(across(c(polregfh), ~ case_when(
    .x == 1 ~ 0,
    .x == 2 ~ 1,
    .x == 3 ~ 2,
    TRUE ~ .x
  )))

data <- data %>% 
  rowwise() %>%
  mutate(perc_mig = mean(c(Q121:Q129))) %>% 
  ungroup() #Escala sumativa percepción de migración

# Guardar datos ----------------------------------------------------------------
save(data,file="output/data.RData")
