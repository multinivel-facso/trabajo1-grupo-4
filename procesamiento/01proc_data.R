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

load("~/GitHub/trabajo1-grupo-4/input/data/WVS_Cross-National_Wave_7_Rdata_v6_0.RData")


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
data <- `WVS_Cross-National_Wave_7_v6_0` %>% 
  select(pais=B_COUNTRY, female=Q260, nacionalismo=Q254, meanschooling, Q121, 
         Q124, Q126, Q128, Q129, pos_pol=Q240, seguridad=Q141, personal_income=Q288, 
         happiness=Q46, Life_satisfaction=Q49 ) #con variables contextuales, sin alfabetismo 

## Remover NA's ----------------------------------------------------------------

data <- data %>% 
  set_na(., na = c(-1, -2, -3, -4, -5, -999, -9999)) #Recodificamos variables a NA

colSums(is.na(data))

data <- na.omit(data)
## Recodificar y crear variables --------------------------------------------------------

data <- data %>%
  mutate(across(c(female), ~ case_when(
    .x == 1 ~ 0,
    .x == 2 ~ 1,
    TRUE ~ .x
  )))

data <- data %>%
  mutate(nacionalismo = case_when(
    nacionalismo == 1 ~ 4,
    nacionalismo == 2 ~ 3,
    nacionalismo == 3 ~ 2,
    nacionalismo == 4 ~ 1,
    nacionalismo == 5 ~ NA_real_,  # Borra la respuesta 5
    TRUE ~ NA_real_        # Por si hay otros valores inesperados
  ))


data <- na.omit(data)

data <- data %>%
  mutate(across(c(happiness), ~ case_when(
    .x == 1 ~ 4,
    .x == 2 ~ 3,
    .x == 3 ~ 2,
    .x == 4 ~ 1,
    TRUE ~ .x
  )))

data <- data %>%
  mutate(across(c(seguridad), ~ case_when(
    .x == 1 ~ 4,
    .x == 2 ~ 3,
    .x == 3 ~ 2,
    .x == 4 ~ 1,
    TRUE ~ .x
  )))

data <- data %>%
  mutate(across(c(Q124, Q126, Q128, Q129), ~ case_when(
    .x == 0 ~ 2,
    .x == 2 ~ 0,
    TRUE ~ .x  # para mantener los demás valores sin cambios
  )))


data <- data %>%
  mutate(across(c(Q121), ~ case_when(
    .x %in% c(1,2) ~ 0,
    .x == 3 ~ 1,
    .x %in% c(4, 5) ~ 2,
    TRUE ~ .x
  )))


dataescala <- data %>% select(Q121, Q124, Q126, Q128, Q129) #Escala migración

psych::alpha(dataescala) #Alfa de Cronbach = 0.77

data <- data %>% 
  rowwise() %>%
  mutate(op_mig = sum(c(Q121, Q124, Q126, Q128, Q129))) %>% 
  ungroup() #Escala sumativa percepción de migración

data <- data %>%
  group_by(pais) %>%
  mutate(happines_promedio_pais = mean(happiness, na.rm = TRUE)) %>%
  ungroup()

data <- data %>%
  group_by(pais) %>%
  mutate(Life_satisfaction_promedio = mean(Life_satisfaction, na.rm = TRUE)) %>%
  ungroup()

data <- data %>%
  group_by(pais) %>%
  mutate(seguridad_prom = mean(seguridad, na.rm = TRUE)) %>%
  ungroup()

# Guardar datos ----sum()# Guardar datos ----------------------------------------------------------------
save(data, file="output/data.rdata")

