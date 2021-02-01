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

source("./Rsource/SwitchButton.R")


data_vigencias = readRDS('data_vigencias.rds')
