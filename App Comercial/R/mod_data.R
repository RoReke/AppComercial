dataUI <- function(id) {
  ns <- NS(id)
  column(
    width = 4,
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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # fluidRow(
    
    box(
      width = 400, title = tagList(shiny::icon("filter", class = "fa-lg"), "Filtros"),
      solidHeader = T, collapsible = T, status = "primary",
      pickerInput(ns("Aseguradora"), "Aseguradora", choices = unique(data_vigencias$Aseguradora), multiple = FALSE, selected = "51 - PROVINCIA ART", options = list(`actions-box` = TRUE), width = NULL),
      pickerInput(ns("ge0"), "Segmento", choices = unique(data_vigencias$Segmento), selected = "Mas de 100", options = list(`actions-box` = TRUE), multiple = T, width = NULL),
      pickerInput(ns("ge"), "Ciiu_1", choices = unique(data_vigencias$CIIU.V1.DESC), selected = unique(data_vigencias$CIIU.V1.DESC), options = list(`actions-box` = TRUE), multiple = T, width = NULL),
      pickerInput(ns("ge1"), "Ciiu_6", choices = unique(data_vigencias$CIIU.V6.DESC), options = list(`actions-box` = TRUE), multiple = T, width = NULL),
      pickerInput(ns("ge2"), "Provincia", choices = unique(data_vigencias$Provincia), selected = unique(data_vigencias$Provincia), options = list(`actions-box` = TRUE), multiple = T, width = NULL)
    ),
    box(width = 400, searchInput(
      inputId = ns("search"), label = "CUIT :",
      placeholder = "Ingresar CUIT",
      resetValue = "",
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "450px"
    )),
    br(),
    box(width = 400, switchButton(
      inputId = ns("Switch.1"),
      label = "Activar modelo BI.V1.01",
      value = FALSE, col = "GB", type = "OO"
    )),
    br(),
    box(width = 400, valueBoxOutput(ns("precio_m"), width = 400)),
    DT::dataTableOutput(ns("table_enfoque")),
    box(width = 400, switchButton(
      inputId = ns("Switch.2"),
      label = "Activar regla enfoque (ET < PM)",
      value = FALSE, col = "GB", type = "OO"
    )
    )
  )
}

dataServer <- function(id, data_vigencias, new_data, r) {
  moduleServer(id, function(input, output, session) {

   data_con_cambios_de_usuario <- reactive({
      new_data$df$CUIT <- as.character(new_data$df$CUIT)
      if (!is.null(new_data$df)) {
        zz <- new_data$df %>%
          group_by(CUIT) %>%
          top_n(1, Fecha) %>%
          ungroup() %>%
          as.data.frame()
        data <- left_join(data_vigencias, zz, by = "CUIT") %>%
          mutate(
            Observaciones = ifelse(is.na(Observaciones.y), Observaciones.x, Observaciones.y),
            Contactado = if_else(!is.na(Contactado.y) & Contactado.y == 1, "Contactado",
              if_else(!is.na(Contactado.y) & Contactado.y == 0, "No Contactado",
                Contactado.x
              )
            ),
            color = ifelse(Observaciones != "", "purple",
              ifelse(Contactado == "Contactado" & Estado.Actual.Cotiz. != "Rechazada", "orange",
                color
              )
            ),
            precio_competencia = if_else(is.na(Precio_Competencia), precio_competencia, as.character(Precio_Competencia)),
            Contactado_int = if_else(Contactado == "Contactado" , 1 , 0)
          )
      } else {
        data <- data_vigencias
      }
      return(data)
    })
    ## swicht enfoque ----

    switch_enfoque <- reactive({
      if (input$Switch.2 == 0) {
        z <- c(0, 1)
      }
      else {
        z <- 1
      }
      return(z)
    })

    ## logica de filtros ----

    filteredData <- reactive({

   if(input$search == ""){
      a <- data_con_cambios_de_usuario() %>%
        filter(Aseguradora %in% input$Aseguradora) %>%
        filter(CIIU.V1.DESC %in% input$ge) %>%
        filter(Segmento %in% input$ge0) %>%
        filter(admin.name1 %in% input$ge2) %>%
        filter(CIIU.V6.DESC %in% input$ge1) %>%
        filter(model_predict %in% switch_()) %>%
        filter(enfoque_swicht %in% switch_enfoque())
    } else {
      a <- data_con_cambios_de_usuario() %>% filter(CUIT %in% input$search)
         }
        return(a)
    })
   
    switch_enfoque <- reactive({
      if (input$Switch.2 == 0) {
        z <- c(0, 1)
      }
      else {
        z <- 1
      }
      return(z)
    })

    ## filtro de oportunidades comerciales
    switch_ <- reactive({
      if (input$Switch.1 == 0) {
        z <- c(0, 1)
      }
      else {
        z <- 1
      }
      return(z)
    })
    
    observeEvent(input$ge,
      {
        ciiu6_ <- data_vigencias %>% filter(CIIU.V1.DESC %in% input$ge)
        updatePickerInput(
          session = session,
          inputId = "ge1",
          choices = unique(ciiu6_$CIIU.V6.DESC),
          selected = unique(ciiu6_$CIIU.V6.DESC)
        )
      },
      ignoreInit = TRUE
    )
    
    
    
    dd <- reactive({
      req(r$map_data > 0) 
      df <- filteredData() %>%
        filter(CUIT == r$map_data) %>%
        select(alicuota)
      df <- if (length(df) == 0) {
        0
      } else {
        df
      }
      df1 <- if (is.na(df$alicuota)) {
        0
      } else {
        df$alicuota
      }
      
      return(df1)
    })
   
     dd_aso <- reactive({
      req(r$map_data > 0) 
      df <- filteredData() %>%
        filter(CUIT == r$map_data) %>%
        select(alicuota_mediana)
     if (nrow(df) == 0) {
        NULL
      } else {
        return(df$alicuota_mediana)
      }
    })
  
    
    dd_enfq1 <- reactive({
     req(r$map_data > 0) 
      df <- filteredData() %>%
        filter(Trabajadores < 101) %>%
        filter(Aseguradora %in% amigas) %>%
        filter(CUIT == r$map_data) %>%
        select(LS.20930, LS.33033, LS.52507, LS.9999999, Remuneracion, Canttrabajadores.Up) %>%
        mutate(SP_Mercado = round(Remuneracion / Canttrabajadores.Up, 0)) %>%
        select(LS.20930, LS.33033, LS.52507, LS.9999999, SP_Mercado)
      if (nrow(df) == 0) {
       return(NULL)
      } else {
      print("estoy aca")
      df <- df %>% as.data.frame()
      df2 <- data.table::transpose(df)
      # get row and colnames in order
      rownames(df2) <- colnames(df)
      colnames(df2) <- "Tarifa Enfoque"
      return(df2)
      }
      
    })
   
    ## precio referencia mercado ----
    output$precio_m <- renderValueBox({
      req(r$map_data > 0) 
      if (!isTruthy(dd_aso())) {
        df <- ""
        valueBox(
          df,
          "Precio de Referencia",
          color = "olive",
          width = 8
        )
      } else {
        df2 <- paste(prettyNum(round(dd_aso(), 2), big.mark = ",", scientific = FALSE), "%")
        valueBox(
          formatC(df2, format = "d", big.mark = ","),
          "Precio de Referencia",
          icon = icon("check", lib = "glyphicon"),
          color = "olive",
          width = 8
        )
      }
    })
    
    
   
    ## indicadores de enfoque tecnico ----
    
    output$table_enfoque <- DT::renderDataTable(
 
     dd_enfq1(),
      options = list(
        paging = FALSE,
        searching = FALSE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = FALSE,
        bInfo = FALSE,
        dom = "Bfrtip",
        # buttons = c('excel'),
        scrollX = FALSE,
        class = "display"
      )
    )


    return(filteredData)
  })
}

