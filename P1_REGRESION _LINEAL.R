###################################################################
######                                                       ######
#                      UNIVERSIDAD DEL QUINDÍO                    #
#                       PROGRAMA DE ECONOMIA                      #
#                            ECONOMETRÍA I                        #
######                                                       ######
###################################################################

# BY: Angela Casallas Romero
# luza.casallasr@uqvirtual.edu.co
# +57 3145033287

## REGRESIÓN LINEAL SIMPLE ----
# https://microdatos.dane.gov.co/index.php/catalog/853

getwd()
options('scipen' = 100 , 'digits' = 4)
rm(list = ls())

# Paquetes o librerias ----
library("skimr")
library("readxl")
library("stringr")
library("stringi")
library("haven")
library("tidyverse")
library("plyr")
library("rstatix")
library("descr")
library("splitstackshape")
library("e1071")
## Cargamos la base de datos
DATA = readxl::read_excel("C:/Users/angel/Downloads/BETA_ESTIMADO_ANGELA.xlsx") |> data.frame()

## Descriptivo-----
head(DATA)
str(DATA , list.len = 3)
skimr::skim(DATA)

## Ojo recordemos que es una muestra muy co... de ANGELA CASALLAS ROMERO
Luz_Angela_Casallas 
8:58_P:M
## Ojo recordemos que es una muestra muy corta
## Analisis descriptivo
### Indicadores de posición y centro -----
for (variable in c("Tasa.de.ahorro", "Tasa.de.interes")) {
  DATA |> 
    dplyr::summarise(
      variable       = variable,
      mediana        = median(.data[[variable]], na.rm = TRUE),
      media          = mean(.data[[variable]], na.rm = TRUE),
      rango_medio    = (max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE)) / 2,
      minimo         = min(.data[[variable]], na.rm = TRUE),
      Q1             = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      Q3             = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      rango_q        = IQR(.data[[variable]], na.rm = TRUE),
      maximo         = max(.data[[variable]], na.rm = TRUE)
    ) |> 
    print()
}
library(dplyr)
library(moments) # Requerido para kurtosis() y skewness()

### Indicadores de dispersión --------
for (variable in c("Tasa.de.ahorro", "Tasa.de.interes")) {
  cat("\n--- Variable:", variable, "---\n")
  
  DATA |>
    dplyr::summarise(
      rango = max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE),
      desviacion = sd(.data[[variable]], na.rm = TRUE),
      varianza = var(.data[[variable]], na.rm = TRUE),
      coef_variacion = (sd(.data[[variable]], na.rm = TRUE) / mean(.data[[variable]], na.rm = TRUE)) * 100,
      curtosis = moments::kurtosis(.data[[variable]], na.rm = TRUE),
      asimetria = moments::skewness(.data[[variable]], na.rm = TRUE)
    ) |>
    print()
}
#correlacion
cor(DATA$Tasa.de.ahorro, DATA$Tasa.de.interes, method = "pearson" , use = "complete,obs") #correlacion comun
cor(DATA$Tasa.de.ahorro, DATA$Tasa.de.interes, method = "spearman" , use = "complete,obs") #correlacion de rango
cor(DATA$Tasa.de.ahorro, DATA$Tasa.de.interes, method = "kendall" , use = "complete,obs") #basada en concordancia

cor.test(DATA$Tasa.de.ahorro, DATA$Tasa.de.interes, method = "pearson")

REG = lm(Tasa.de.ahorro ~ Tasa.de.interes , data = DATA)

summary(REG)

# \widehat{y} = \widehat{\beta_{0}} + \widehat{{\beta_{1}} \cdot x_{i} + \varepsilon_{i}

mean(DATA$Tasa.de.interes)

DATA = DATA |> dplyr::mutate(
  x_2 = Tasa.de.interes^2,
  x_XPRO = Tasa.de.interes - mean(Tasa.de.interes, na.rm = TRUE),
  y_YPRO = Tasa.de.ahorro - mean(Tasa.de.ahorro, na.rm = TRUE),
  x_XPRO_y_YPRO = (Tasa.de.interes - mean(Tasa.de.interes, na.rm = TRUE)) * (Tasa.de.ahorro - mean(Tasa.de.ahorro, na.rm = TRUE)),
  x_XPRO_2 = (Tasa.de.interes - mean(Tasa.de.interes, na.rm = TRUE))^2,
  y_EST = 2.1833 + (1.7167 * Tasa.de.interes),
  ERROR = Tasa.de.ahorro - y_EST
)

print("beta_{1} estimado")
sum(DATA$x_XPRO_y_YPRO, na.rm = TRUE) / sum(DATA$x_XPRO_2, na.rm = TRUE)

View(DATA)


