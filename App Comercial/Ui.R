header <- dashboardHeader(title = p("Oportunidades Comerciales"),
                          titleWidth = 400)

body <- dashboardBody(
  tags$style(type="text/css",
   ".shiny-output-error { visibility: hidden; }",
   ".shiny-output-error:before { visibility: hidden; }",
  tags$head(
   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
               )
                  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  fluidRow(
     column(width =4,
            box(width = 400, title =tagList(shiny::icon("filter",class = 'fa-lg'), "Filtros") ,
                solidHeader = T, collapsible = T, status = 'primary',
                pickerInput('Aseguradora','Aseguradora', choices = unique(data_vigencias$Aseguradora), selected = "51 - PROVINCIA ART", options = list(`actions-box` = TRUE),multiple = T, width = NULL ),
                pickerInput('ge0', 'Segmento', "", options = list(`actions-box` = TRUE),multiple = T, width = NULL ),
                pickerInput('ge', 'Ciiu_1', "", options = list(`actions-box` = TRUE),multiple = T, width = NULL ),
                pickerInput('ge1', 'Ciiu_6', "", options = list(`actions-box` = TRUE),multiple = T, width = NULL ),
                pickerInput('ge2', 'Provincia', "", options = list(`actions-box` = TRUE),multiple = T, width = NULL )),
            br(),
            box(width = 400,switchButton(inputId = "Switch.1",
                                         
                         label = "Oportunidades Comerciales", 
                         value = FALSE , col = "GB", type = "OO")),
            br(),
            box(width = 400,valueBoxOutput("precio_m",width = 400))),
            
                  
      column(width = 8,
           tabsetPanel(
            tabPanel("Mapa",icon = icon("globe"), "",width = 400, box(width = NULL, solidHeader = TRUE,
               leafletOutput('map',height = 500)), 
               box(width = 400,
                   id = 'dataset',
                     tabPanel("Sample Bank", 
                     DT::dataTableOutput("banking.df_data"),
                     br(), useShinyalert(),
                     actionButton("saveBtn","Save")
                     )
                   )
               ),
            tabPanel("Tabla", "",icon = icon("table"), DT::dataTableOutput("tabla")
               )
          )
           
       )
    )
    
  )


ui <- dashboardPage(
                    skin = 'red',
                    header,
                    dashboardSidebar(disable = T),
                    body
                    )