customerUI <- function(id) {
  ns <- NS(id)
  tabPanel("Customer File", "",
    icon = icon("map-pin"),
    box(h2(textOutput(ns("nombrecliente"))),
      width = "100%", align = "center", solidHeader = FALSE, status = "primary",
      background = "light-blue"
    ),
    h4(textOutput(ns("aseg")), solidHeader = TRUE),
    h4(textOutput(ns("trab")), solidHeader = TRUE),
    h4(textOutput(ns("ciiu_1")), solidHeader = TRUE),
    h4(textOutput(ns("ciiu_6")), solidHeader = TRUE),
    h4(textOutput(ns("pcia")), solidHeader = TRUE),
    h4(textOutput(ns("fechacoti")), solidHeader = TRUE),
    br(),
    br(),
    textInput(ns("p_c"), "Precio Competencia: ", "", width = 100),
    checkboxInput(
      inputId = ns("contactado"),
      label = "-  Contactado/No Contactado",
      value = FALSE
    ),
    textInput(ns("obs"), "Observaciones", ""),
    verbatimTextOutput(ns("value")),
    useShinyalert(),
    br(),
    br(),
    br(),
    br(),
    actionButton(ns("enviar"), "Enviar Cambios")
  )
}
customerServer <- function(id, df, r, new_data, USER) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (r$map == 1) {
        updateTabsetPanel(
          session = r$session,
          "inTabset",
          selected = "Customer File"
        )
        r$map <- 0
      }
    })
     
    dd1 <- reactive({
      if (!is.null(r$map_data)) {
      df <-  df() %>%
          filter(CUIT == r$map_data) %>%
          select(
            Cliente, CUIT, Aseguradora, Trabajadores,
            CIIU.V1.DESC, CIIU.V6.DESC, admin.name1, Contactado,
            Observaciones, precio_competencia, Fecha_Cotizacion,Contactado_int
          )
      if(nrow(df) == 0){
        NULL
      }
      else{df}
      }
    })
   
    output$nombrecliente <- renderText({
      paste(
        "Empleador:      ",
        if (is.null(dd1()$Cliente)) {
          ""
        } else {
          dd1()$Cliente
        }, dd1()$CUIT
      )
    })

    output$aseg <- renderText({
      paste("Aseguradora:  ", dd1()$Aseguradora)
    })

    output$trab <- renderText({
      paste("Cantidad de Trabajadores:  ", dd1()$Trabajadores)
    })

    output$ciiu_1 <- renderText({
      paste("Sector:  ", dd1()$CIIU.V1.DESC)
    })

    output$ciiu_6 <- renderText({
      paste("CIIU 6 Desc:  ", dd1()$CIIU.V6.DESC)
    })

    output$pcia <- renderText({
      paste("Provincia:  ", dd1()$admin.name1)
    })


    output$fechacoti <- renderText({
      paste("Fecha Cotizacion:  ", if (is.null(dd1()$Fecha_Cotizacion)) {
        ""
      } else {
        dd1()$Fecha_Cotizacion
      })
    })

    observeEvent(input$enviar, {
      shinyalert::shinyalert(title = "Guardado!", type = "success")
      a <- read.xlsx("primeras observaciones.xlsx")
      b <- rbind(a, data.frame(
        CUIT = r$map_data,
        Observaciones = paste(USER$user, ": ", input$obs),
        Contactado = input$contactado,
        Precio_Competencia = input$p_c,
        Fecha = as.character(Sys.time())
      ))
      write.xlsx(b, "primeras observaciones.xlsx")
      c <- read.xlsx("primeras observaciones.xlsx")
      new_data$df <- c
      shinyjs::reset("obs")
    })

    observe({
      updateCheckboxInput(session,
        "contactado",
        label = NULL,
        value = dd1()$Contactado_int
      )

      updateTextInput(session,
        "obs",
        label = NULL,
        dd1()$Observaciones
      )
      updateTextInput(session,
        "p_c",
        label = NULL,
        dd1()$precio_competencia
      )

      output$value <- renderText({
        if (is.null(dd1()$Observaciones)) {
          "Observaciones..."
        }
        else {
          dd1()$Observaciones
        }
      })
    })
  })
}
