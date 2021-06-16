marketUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      )
    ),
    tags$style("
      .checkbox { /* checkbox is a div class*/
        line-height: 20px;
        margin-bottom: 40px; /*set the margin, so boxes don't overlap*/
      }
      input[type='checkbox']{ /* style for checkboxes */
        width: 30px; /*Desired width*/
        height: 20px; /*Desired height*/
        line-height: 30px;
      }
       span {
           margin-left: 0px;  /*set the margin, so boxes don't overlap labels*/
           line-height: 30px;
       }
  "),
    tags$div(tags$style(HTML(".dropdown-menu{z-index:10000 !important;}"))),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # fluidRow(
    column(
      width = 4,
      box(
        width = 400, title = tagList(shiny::icon("filter", class = "fa-lg"), "Filtros"),
        solidHeader = T, collapsible = T, status = "primary",
        pickerInput(ns("Aseguradoram"), "Aseguradora", choices = unique(data_vigencias$Aseguradora), selected = unique(data_vigencias$Aseguradora), options = list(`actions-box` = TRUE), multiple = T, width = NULL),
        pickerInput(ns("ge0m"), "Segmento", choices = unique(data_mercado$Segmento_A), selected = unique(data_mercado$Segmento_A), options = list(`actions-box` = TRUE), multiple = T, width = NULL),
        pickerInput(ns("gem"), "Ciiu_1", choices = unique(data_mercado$CIIU.V1.DESC), selected = unique(data_mercado$CIIU.V1.DESC), options = list(`actions-box` = TRUE), multiple = T, width = NULL),
        pickerInput(ns("ge1m"), "Ciiu_6", choices = unique(data_mercado$CIIU.V6.DESC), options = list(`actions-box` = TRUE), multiple = T, width = NULL),
        pickerInput(ns("ge2m"), "Provincia", choices = unique(data_mercado$Provincia), selected = unique(data_mercado$Provincia), options = list(`actions-box` = TRUE), multiple = T, width = NULL)
      ),
      br()
    ),
    column(
      width = 8,
      # DT::dataTableOutput("market_share")
      box(
        width = NULL, valueBoxOutput(ns("empleadores"), width = 4),
        valueBoxOutput(ns("trabajadores"), width = 4),
        valueBoxOutput(ns("premio"), width = 4)
      ),
      plotOutput(ns("barplot"))
    )
  )
}

marketServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$gem,
                 {
                   ciiu6_ <- data_mercado %>% filter(CIIU.V1.DESC %in% input$gem)
                   updatePickerInput(
                     session = session,
                     inputId = "ge1m",
                     choices = unique(ciiu6_$CIIU.V6.DESC),
                     selected = unique(ciiu6_$CIIU.V6.DESC)
                   )
                 },
                 ignoreInit = TRUE
    )
    observe(mercada_data_filtered() %>% head(1))
  mercada_data_filtered <-reactive({
        data_mercado %>%
          # filter(ART %in% input$Aseguradoram)  %>%
          filter(CIIU.V1.DESC %in% input$gem) %>%
          filter(Segmento_A %in% input$ge0m) %>%
          filter(Provincia %in% input$ge2m) %>%
          filter(CIIU.V6.DESC %in% input$ge1m)
      })
    
    mercada_data_filtered_aseg <-
      reactive({
        mercada_data_filtered() %>%
          filter(ART %in% input$Aseguradoram)
      })
    
    cuota_total <- reactive({
      suma_cuota <- sum(mercada_data_filtered()$Cuotaffe)
      return(suma_cuota)
    })
    
    market_share <- reactive({
      data <- mercada_data_filtered_aseg() %>%
        group_by(ART) %>%
        summarise(
          share = round((sum(Cuotaffe) / cuota_total()) * 100, 2)
        )
      
      return(data)
    })
    
    #library(ggthemes)
    output$barplot <- renderPlot({
      ggplot(data = market_share(), aes(x = reorder(ART, -share), y = share)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(y = "% Market Share", x = "Aseguradoras") +
        geom_text(aes(x = as.factor(market_share()$ART), y = market_share()$share, label = paste(market_share()$share, "%", sep = "")), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_fill_manual(values = c("#1F497D", "#4F81BD", "#8DB4E3")) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title = element_text(size = 14)
        ) +
        theme(
          rect = element_rect(fill = "lightgray") # all rectangles
        ) +
        ggtitle("Market Share s/ Cuota Pactada") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) 
    })
    
    mercada_data_filtered2 <-
      reactive({
        data_mercado %>%
          filter(ART %in% input$Aseguradoram) %>%
          filter(CIIU.V1.DESC %in% input$gem) %>%
          filter(Segmento_A %in% input$ge0m) %>%
          filter(Provincia %in% input$ge2m) %>%
          filter(CIIU.V6.DESC %in% input$ge1m)
      })
    
    output$empleadores <- renderValueBox({
      #  df2 <-paste(prettyNum(round(dd_aso(),2),big.mark=",",scientific=FALSE),"%")
      valueBox(
        formatC(sum(mercada_data_filtered2()$Empleadores), format = "d", big.mark = "."),
        "Empleadores",
        icon = icon("industry"),
        color = "blue" # ,
        # width = 4)
      )
    })
    output$trabajadores <- renderValueBox({
      #  df2 <-paste(prettyNum(round(dd_aso(),2),big.mark=",",scientific=FALSE),"%")
      valueBox(
        formatC(sum(mercada_data_filtered2()$Canttrabajadores.Up), format = "d", big.mark = "."),
        "Trabajadores",
        icon = icon("user-injured"),
        color = "blue"
      )
    })
    output$premio <- renderValueBox({
      df <- paste0("$", prettyNum(round(sum(mercada_data_filtered2()$Cuotaffe) / (10^6), 2), format = "d", big.mark = ".", decimal.mark = ","), "M")
      valueBox(
        df,
        "Cuota Pactada Mensual",
        icon = icon("money-bill"),
        color = "blue"
      )
    }) 
  })
}