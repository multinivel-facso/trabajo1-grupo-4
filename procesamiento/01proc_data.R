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

load("input/WVS_Cross-National_Wave_7_Rdata_v6_0.RData")


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
data <- `WVS_Cross-National_Wave_7_v6_0` %>% 
  select(pais=B_COUNTRY, female=Q260, nacionalismo=Q254, democ, meanschooling, 
         hdi, Q121, Q124, migrationrate, Q126, Q128, Q129, pos_pol=Q240, personal_income=Q288, unemploytotal,
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
  mutate(across(c(Q121), ~ case_when(
    .x %in% c(1,2) ~ 2,
    .x == 3 ~ 1,
    .x %in% c(4, 5) ~ 0,
    TRUE ~ .x
  )))



dataescala <- data %>% select(Q121, Q124, Q126, Q128, Q129) #Escala migración

psych::alpha(dataescala) #Alfa de Cronbach = 0.7

data <- data %>% 
  rowwise() %>%
  mutate(perc_mig = sum(c(Q121, Q124, Q126, Q128, Q129))) %>% 
  ungroup() #Escala sumativa percepción de migración

data <- data %>%
  group_by(pais) %>%
  mutate(happines_promedio_pais = mean(happiness, na.rm = TRUE)) %>%
  ungroup()

data3 <- data %>%
  group_by(pais) %>%
  mutate(Life_satisfaction_promedio = mean(Life_satisfaction, na.rm = TRUE)) %>%
  ungroup()

#Centrado a la Gran Media--------------------------------------------

install.packages("gt")

pacman::p_load(tidyverse, lme4, texreg, gt)

load(file="output/data3.RData")

data3 %>% 
  nest(-schnum) %>% 
  mutate(fit = map(data3, ~ lm(perc_mig ~ meanschooling, data=.)),
         fit.c = map(data3, ~ lm(perc_mig ~ happiness_promedio_pais, data=.)),
         results = map(fit, broom::tidy),
         results.c = map(fit.c, broom::tidy)) %>% 
  unnest(results, results.c) %>% 
  dplyr::select(schnum, term, estimate, term1, estimate1 ) %>% 
  pivot_wider(id_cols = "schnum", 
              values_from=c("estimate", "estimate1"), 
              names_from=
                "term") %>% 
  rename(Intercept = "estimate_(Intercept)", Slope=estimate_homework,
         Intercept.gmc = "estimate1_(Intercept)", Slope.gmc=estimate1_homework)%>%
  unnest() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  gt::gt()


# Guardar datos ----sum()# Guardar datos ----------------------------------------------------------------
save(data3, file="output/data3.rdata")

getwd()
ls()  # para ver los objetos disponibles

