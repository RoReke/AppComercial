curvapreciosID <- function(id){
  ns <- NS(id)
fluidRow(
    column(width =8,box(width = 400, title =tagList(shiny::icon("arrow-alt-circle-down"), "") ,
                        solidHeader = T, collapsible = F, status = 'primary', 
                        plotOutput(ns("lineplot")),
                        plotOutput(ns("barplotT")),
                        plotOutput(ns("barplotE")))),
    column(width =4,
           box(width = 400, title =tagList(shiny::icon("arrow-alt-circle-down"), "Mercado") ,
               solidHeader = T, collapsible = F, status = 'primary', 
               h4(textOutput(ns("ciiu_1_mp")),solidHeader =TRUE),
               h4(textOutput(ns("ciiu_6_mp")),solidHeader =TRUE),
               h4(textOutput(ns("pcia_mp")),solidHeader =TRUE),
               h4(textOutput(ns("segmento_mp")),solidHeader =TRUE)
           )))
}

curvapreciosServer <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$as <- renderPrint(dat())
    
    data_curve <- reactive({
      a <- data_vigencias %>%
        filter(CUIT == r$map_data) %>%
        select(CIIU.V6.DESC, Segmento_model, Provincia)
      
      data <- data_mercado %>%
        filter(CIIU.V6.DESC == a$CIIU.V6.DESC) %>%
        filter(Provincia == a$Provincia) %>%
        filter(Segmento == a$Segmento_model) %>%
        group_by(ART) %>%
        summarise(
          alicuota = mean(alicuota),
          Empleadores = sum(Empleadores),
          Canttrabajadores.Up = sum(Canttrabajadores.Up)
        )
      return(data)
    })
    
    data_curve2 <- reactive({
      a <- data_vigencias %>%
        filter(CUIT == r$map_data) %>%
        select(CIIU.V6.DESC, Segmento_model, Provincia)
      
      data <- data_mercado %>%
        filter(CIIU.V6.DESC == a$CIIU.V6.DESC) %>%
        filter(Provincia == a$Provincia) %>%
        filter(Segmento == a$Segmento_model)
      return(data)
    })
    
    output$lineplot <- renderPlot({
      ggplot(data_curve(), aes(x = reorder(ART, -alicuota), y = alicuota, group = 1)) +
        geom_line(linetype = "dashed", color = "blue", size = 1.2) +
        geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 6) +
        theme_ipsum() +
        ggtitle("Market Pricing") +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title = element_text(size = 14)
        ) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
        labs(y = "ALICUOTA", x = "Aseguradoras", size = 12)
    })
    
    output$barplotT <- renderPlot({
      ggplot(data_curve(), aes(x = reorder(ART, -alicuota), y = Canttrabajadores.Up)) +
        geom_bar(stat = "identity", color = "lightblue", size = 1.2) +
        #  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
        theme_ipsum() +
        ggtitle("Trabajadores") +
        theme(axis.text.x = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
        labs(y = "Trabajadores", x = element_blank(), size = 12)
    })
    
    output$barplotE <- renderPlot({
      ggplot(data_curve(), aes(x = reorder(ART, -alicuota), y = Empleadores)) +
        geom_bar(stat = "identity", color = "lightblue", size = 1.2) +
        #  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
        theme_ipsum() +
        ggtitle("Empleadores") +
        theme(axis.text.x = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
        labs(y = "Empleadores", x = element_blank(), size = 12)
    })
    
    
    output$ciiu_1_mp <- renderText({
      paste("Sector:  ", data_curve2()$CIIU.V1.DESC[1])
    })
    
    output$ciiu_6_mp <- renderText({
      paste("CIIU 6 Desc:  ", data_curve2()$CIIU.V6.DESC[1])
    })
    
    output$pcia_mp <- renderText({
      paste("Provincia:  ", data_curve2()$Provincia[1])
    })
    output$segmento_mp <- renderText({
      paste("Segmento:  ", data_curve2()$Segmento[1])
    })
    
    
  })
}