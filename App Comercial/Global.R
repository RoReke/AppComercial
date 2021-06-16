### librerías ----
library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(openxlsx)
require(shinydashboard)
library(DT)
library(shinyalert)
library(leaflet)
library(shinyjs)
library(plotly)
library(sodium)
library(reshape)
# library(hrbrthemes)
# require(rdrop2)

users <- reactiveValues(count = 0)

limite_de_usuarios <- 20
data_vigencias <- readRDS("data_vigencias.rds") %>% as.data.frame()


## funciones ----
`%nin%` <- Negate(`%in%`)

## data ----
amigas <- c("27 - PREVENCION ART", "272 - GALENO ART", "280 - SWISS MEDICAL ART", "221 - EXPERTA ART", "60 - LA SEGUNDA ART", "507 - OMINT ART S.A.")
a <- 14950 * 1.4
b <- 23595 * 1.4
c <- 37505 * 1.4

df_enfoque <- data_vigencias %>%
  filter(Trabajadores < 101) %>%
  filter(Aseguradora %in% amigas) %>%
  # filter (CUIT == input$map_marker_click$id) %>%
  select(CUIT, alicuota, LS.20930, LS.33033, LS.52507, LS.9999999, Remuneracion, Canttrabajadores.Up) %>%
  mutate(SP_Mercado = round(Remuneracion / Canttrabajadores.Up, 0)) %>%
  mutate(
    Alic.enfoque = if_else(SP_Mercado > c, LS.9999999,
      if_else(SP_Mercado > b, LS.52507,
        if_else(SP_Mercado > a, LS.33033, LS.20930)
      )
    ),
    filter_enfoque = if_else(alicuota > Alic.enfoque, 1, 0)
  ) %>%
  filter(filter_enfoque == 1) %>%
  select(CUIT)
source("./Rsource/SwitchButton.R")

data_mercado <- readRDS("data_mercado.rds")
data_mercado <- data_mercado %>%
  filter(!is.na(CIIU.V1.DESC)) %>%
  filter(Segmento != "0 trab.") %>%
  filter(alicuota >= 0.5 & alicuota < 26)
data_mercado <- data_mercado %>% mutate(
  Segmento_A = if_else(Segmento == "101-500 trab." | Segmento == "Mas de 1500" | Segmento == "501-1500 trab.", "Mas de 100",
    if_else(Segmento == "51-100 trab.", "51 a 100",
      if_else(Segmento == "26-50 trab.", "26 a 50",
        if_else(Segmento == "11-25 trab.", "11 a 25",
          if_else(Segmento == "1 trab." | Segmento == "2-5 trab." | Segmento == "6-10 trab.", "1 a 10",
            "Error"
          )
        )
      )
    )
  )
)




data_vigencias <- data_vigencias %>% mutate(
  Observaciones = "",
  precio_competencia = ""
)



user_data <- data.frame()


data_vigencias <- data_vigencias %>% mutate(enfoque_swicht = if_else(CUIT %in% df_enfoque$CUIT, 1, 0))

## label
data_vigencias$Estado.Actual.Cotiz.[is.na(data_vigencias$Estado.Actual.Cotiz.)] <- ""
data_vigencias$label <- with(data_vigencias, paste(
  CUIT, " ",
  Estado.Actual.Cotiz.
))
data_vigencias <-
  data_vigencias %>% mutate(Contactado_int = if_else(Contactado == "Contactado", 1, 0))
