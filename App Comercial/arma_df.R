### Librerías ----
library(tidyverse)
library(openxlsx)

### Data  y transformación de data----
data_cotizaciones = read.xlsx("C:/Users/jrr/Desktop/App Comercial/cotis.xlsx", startRow = 2) %>% 
  mutate(Fecha.Alta.Cotización = as.Date(Fecha.Alta.Cotización, origin = "1899-12-30")  ) %>% 
  rename (CUIT = X9,
          'Fecha_Cotizacion' = Fecha.Alta.Cotización,
          Alicuota.Variable.Cotizacion = Alícuota.Variable.Cotización,
          Alicuota.Competencia = Alícuota.Variable.Competencia.Cotización) %>% 
  select(CUIT, Cliente,'Fecha_Cotizacion', Alicuota.Variable.Cotizacion, Alicuota.Competencia)

data_mercado_masa =  read.csv2("C:/Users/jrr/Desktop/Boletin gerencia tecnica cobertura/base_mercado_masa.txt",
                               sep = "\t",
                               dec = ".",
                               colClasses=c("integer","character","numeric","character","character","character")) %>% 
                      rename (
                        masa_salarial = valor
                              )

data_mercado_cp =  read.csv2("C:/Users/jrr/Desktop/Boletin gerencia tecnica cobertura/base_mercado_cp.txt",
                             sep = "\t",
                             dec = ".",
                             colClasses=c("integer","character","numeric","character","character","character")) %>% 
                      rename (
                        cuota_pactada = valor
                      )# se extrae de scripts de python que minan un tableau de mercado

data_mercado = full_join(data_mercado_masa,data_mercado_cp,
                         by = c("ciiu6", "Segmento", "ART", "Provincia"))

 data_vigencias = read.xlsx("c:/SharePoint/CTD - Pricing ART/Sprints/Sprint 54/HU Cotizaciones - Solicitud AS/Contratos vigentes base entera.xlsx")  # toma la base de vigencias obtenida por gerencia tÃ©cnica
 data_vigencias <- data_vigencias[ , !duplicated(colnames(data_vigencias))]
 
 ## filtros
 data_vigencias <- data_vigencias %>% 
                        arrange(Fecha.operación.Desde) %>% 
                        group_by(CUIT) %>% 
                            slice(1) %>% 
                        ungroup() # me quedó con un cuit por aseguradora. 
 
 data_vigencias <- data_vigencias %>% 
                           filter (Contrato != 0 & !is.null(Contrato)) %>% 
                           filter (Fecha.operación.Hasta == 1 | 
                                   Fecha.operación.Hasta == "1/1/1900 00:00:00" |
                                   is.na(Fecha.operación.Hasta) |
                                   Fecha.operación.Hasta == "NULL")# elimino las que no tienen contrato y los registro con "fecha de operación hasta"

data_vigencias = data_vigencias %>% 
                    filter(Trabajadores > 0)

## agrego latitudes y longitudes
data_codigos_postales = read.xlsx("//acen0104/PyC/1- Planificación y Control/varios/listados/Códigos Postales.xlsx") %>% 
                        filter (
                          CP.repetido == 0
                        )
data_vigencias$CP = as.integer(data_vigencias$CP )
data_vigencias = left_join(data_vigencias, data_codigos_postales,
                           by = c("CP" ="postal.code")) 

# modificó las latitudes y longitudes para que no se superpongan cuando hay datos iguales

data_vigencias$latitude <- jitter(data_vigencias$latitude, factor = 20)
data_vigencias$longitude <- jitter(data_vigencias$longitude, factor = 20)

## correción de Aseguradoras 

data_vigencias$Aseguradora[grepl("00272 - GALENO ART", data_vigencias$Aseguradora)] <- "272 - GALENO ART"
data_vigencias$Aseguradora[grepl("00027 - PREVENCION ART", data_vigencias$Aseguradora)] <- "27 - PREVENCION ART"
data_vigencias$Aseguradora[grepl("00060 - LA SEGUNDA ART", data_vigencias$Aseguradora)] <- "60 - LA SEGUNDA ART"
data_vigencias$Aseguradora[grepl("00310 - INST.AUTARQ.ENTRE RIOS", data_vigencias$Aseguradora)] <- "310 - INST.AUTARQ.ENTRE RIOS"
data_vigencias$Aseguradora[grepl("00396 - ASOCIART S.A. ART" , data_vigencias$Aseguradora)] <- "396 - ASOCIART S.A. ART"
data_vigencias$Aseguradora[grepl("00280 - SWISS MEDICAL ART" , data_vigencias$Aseguradora)] <- "280 - SWISS MEDICAL ART"
data_vigencias$Aseguradora[grepl("00094 - FEDERACION PATRONAL ART" , data_vigencias$Aseguradora)] <- "94 - FEDERACION PATRONAL ART"
data_vigencias$Aseguradora[grepl("00450 - LATITUD SUR ART S.A."  , data_vigencias$Aseguradora)] <- "450 - LATITUD SUR ART S.A."
data_vigencias$Aseguradora[grepl("00426 - CAJA POPULAR DE TUCUMAN", data_vigencias$Aseguradora)] <- "426 - CAJA POPULAR DE TUCUMAN"
data_vigencias$Aseguradora[grepl("00515 - MEOPP ART", data_vigencias$Aseguradora)] <- "515 - MEOPP ART"
data_vigencias$Aseguradora[grepl("00418 - HORIZONTE ART", data_vigencias$Aseguradora)] <- "418 - HORIZONTE ART"
data_vigencias$Aseguradora[grepl("00051 - PROVINCIA ART", data_vigencias$Aseguradora)] <- "51 - PROVINCIA ART"
data_vigencias$Aseguradora[grepl("00507 - OMINT ART S.A.", data_vigencias$Aseguradora)] <- "507 - OMINT ART S.A."
data_vigencias$Aseguradora[grepl("00248 - VICTORIA ART" , data_vigencias$Aseguradora)] <- "248 - VICTORIA ART"
data_vigencias$Aseguradora[grepl("00140 - PRODUCTORES DE FRUTA ART", data_vigencias$Aseguradora)] <- "140 - PRODUCTORES DE FRUTA ART"
data_vigencias$Aseguradora[grepl("00019 - BERKLEY INTERNATIONAL ART" , data_vigencias$Aseguradora)] <- "19 - BERKLEY INTERNATIONAL ART"
data_vigencias$Aseguradora[grepl("00221 - EXPERTA ART" , data_vigencias$Aseguradora)] <- "221 - EXPERTA ART"
data_vigencias$Aseguradora[grepl("00469 - RECONQUISTA ART"  , data_vigencias$Aseguradora)] <- "469 - RECONQUISTA ART"
data_vigencias$Aseguradora[grepl("00183 - LA HOLANDO ART"  , data_vigencias$Aseguradora)] <- "183 - LA HOLANDO ART"
data_vigencias <- data_vigencias %>% filter( Aseguradora != "NULL")

####Llevar a ciiu r2 ---- // A modificar para pasar todo a ciiu 4. 

ciiu_equivalencias_r4_r3 <- read.xlsx("C:/Users/jrr/Desktop/Listados/CIIU_EQUIVALENCIAS.xlsx", sheet = 2)
ciiu_equivalencias_r2_r4 <- read.xlsx("C:/Users/jrr/Desktop/Listados/CIIU_EQUIVALENCIAS.xlsx", sheet = 1)
ciiu_r2 <- read.xlsx("//acen0104/PyC/1- Planificación y Control/varios/listados/CIIU.xlsx")
ciiu_equivalencias_r2_r4$CIIU.REV.2 = as.character(ciiu_equivalencias_r2_r4$CIIU.REV.2)
ciiu_equivalencias_r4_r3$CIIU.REV.3 = as.character(ciiu_equivalencias_r4_r3$CIIU.REV.3)
ciiu_equivalencias_r4_r3$CIIU.REV.4 = as.character(ciiu_equivalencias_r4_r3$CIIU.REV.4)
ciiu_r2$ciiu_r2 = as.character(ciiu_r2$CIIU.V6)

ciiu_r2_ <- left_join(ciiu_r2,ciiu_equivalencias_r2_r4 %>% 
                        select(CIIU.REV.4,CIIU.REV.2),
                      by = c("CIIU.V6" = "CIIU.REV.2" ))
ciiu_r2_$CIIU.REV.4 = as.character(ciiu_r2_$CIIU.REV.4)

ciiu_r2_ <- left_join(ciiu_r2_,ciiu_equivalencias_r4_r3 %>% 
                        select(CIIU.REV.4,CIIU.REV.3),
                      by = c("CIIU.REV.4" = "CIIU.REV.4" ))



a = left_join(data_vigencias, ciiu_r2_ %>%
                select(CIIU.V6,CIIU.REV.3),
              by = c("CIIU"="CIIU.REV.3"))
a = left_join(a, ciiu_r2_ %>%
                select(CIIU.V6,CIIU.REV.4),
              by = c("CIIU"="CIIU.REV.4"))
a <-a[!duplicated(a$CUIT), ]

a <-  a %>% mutate ( CIIU_R2 = ifelse(
  !is.na(CIIU.V6.x),CIIU.V6.x,
  ifelse(!is.na(CIIU.V6.y),CIIU.V6.y,
         CIIU)
))
a <- a %>% select (-CIIU.V6.y,-CIIU.V6.x
)
data_vigencias <- left_join(a,ciiu_r2, by = c("CIIU_R2"="CIIU.V6"))

ciiu_equivalencias_r2_r4$CIIU.REV.4 <- as.character(ciiu_equivalencias_r2_r4$CIIU.REV.4)

data_mercado <- left_join(data_mercado, ciiu_equivalencias_r2_r4 %>% 
                            select(CIIU.REV.4,CIIU.REV.2), 
                          by = c("ciiu6" = "CIIU.REV.4") )
data_mercado = left_join(data_mercado,ciiu_r2 %>%
                           select(CIIU.V6,CIIU.V6.DESC),
                         by = c("CIIU.REV.2" = "CIIU.V6"))

## JUNTO data_vigencias con cotizaciones 

data_vigencias <- left_join(data_vigencias,data_cotizaciones, by = "CUIT" )
data_vigencias <- data_vigencias %>% mutate (
  Contactado = ifelse(is.na(Cliente), "No Contactado", "Contactado")
)


#### Construcción de nuevas variables ----

data_mercado = data_mercado %>% 
                    mutate (
                      alicuota = cuota_pactada / masa_salarial
                    )

data_vigencias$Trabajadores <- as.integer(data_vigencias$Trabajadores)

data_vigencias = data_vigencias %>% 
  mutate (
    Segmento = if_else(Trabajadores >100, "Mas de 100",
                       if_else(Trabajadores >50, "51 a 100",
                       if_else(Trabajadores >25, "26 a 50",
                       if_else(Trabajadores >10, "11 a 25", 
                                "1 a 10")
                               )))
    )


data_vigencias <- data_vigencias %>% 
  mutate(color = case_when(str_detect(Contactado , "No Contactado") ~ "green",
                           str_detect(Contactado , "Contactado") ~ "orange"))

## junto data vigencias con mercado

data_mercado <- data_mercado %>% group_by(CIIU.V6.DESC,Segmento,ART,Provincia) %>% top_n(-1,alicuota)
data_vigencias = left_join(data_vigencias,data_mercado,
                           by = c(  "CIIU.V6.DESC" = "CIIU.V6.DESC", 
                                    "Segmento" = "Segmento", 
                                    "Aseguradora" = "ART", 
                                    "Provincia" ="Provincia"))

saveRDS(data_vigencias, "data_vigencias.rds")
