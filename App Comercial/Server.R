function(input, output, session) {
 
  ## logica de filtros ----
  filteredData <- reactive({data_vigencias %>%
      filter(Aseguradora %in% input$Aseguradora)  %>%
      filter(CIIU.V1.DESC %in%  input$ge) %>% 
      filter(Segmento %in%  input$ge0) %>% 
      filter(admin.name1 %in%  input$ge2) %>% 
      filter(CIIU.V6.DESC %in%  input$ge1)
  })
  
  
  outVar2 = reactive({
    if (is.null(input$Aseguradora)) {
      mydata = "51 - PROVINCIA ART"
    }
    else {
      mydata = input$Aseguradora
    }
    return(mydata)
  })
  
  
  elegir_Segmento <- reactive({ 
    x <- data_vigencias %>% 
      filter (Aseguradora %in% outVar2()) %>% 
      select (Segmento) 
    return(x)
  })
  
  observe({
    updatePickerInput(session, "ge0",
                      choices =   unique(elegir_Segmento()),
                      selected = "Mas de 100"
    )
  })
  
  outVar1 = reactive({
    if (is.null(input$ge0)) {
      mydata = "Mas de 100"
    }
    else {
      mydata = input$ge0
    }
    return(mydata)
  })
  
  elegir_ciiu <- reactive({data_vigencias %>% 
      filter (Aseguradora %in% outVar2() ) %>% 
      filter (Segmento %in% outVar1()) %>% 
      select( CIIU.V1.DESC)
  })
  
  observe({
    updatePickerInput(session, "ge",
                      choices =   unique(elegir_ciiu()$CIIU.V1.DESC),
                      selected = unique(elegir_ciiu()$CIIU.V1.DESC)
                      
    ) })
  
  
  outVar3 = reactive({
    if (is.null(input$ge)) {
      mydata = "COMERCIO MAYORISTA Y MINORISTA"
    }
    else {
      mydata = input$ge
    }
    return(mydata)
  })
  
  elegir_ciiu_ <- reactive({data_vigencias %>% 
      filter (Aseguradora %in% outVar2() ) %>% 
      filter (Segmento %in% outVar1()) %>% 
      filter (CIIU.V1.DESC %in% outVar3()) %>% 
      select( CIIU.V6.DESC)
  })
  
  observe({
    updatePickerInput(session, "ge1",
                      choices =   unique(elegir_ciiu_()$CIIU.V6.DESC),
                      selected = unique(elegir_ciiu_()$CIIU.V6.DESC)
                      
    )})
  
  outVar4 = reactive({
    if (is.null(input$ge1)) {
      mydata = "Venta de productos en general. Supermercados. Autoservicios."
    }
    else {
      mydata = input$ge1
    }
    return(mydata)
  })
  
  elegir_pcia <- reactive({data_vigencias %>% 
      filter (Aseguradora %in% outVar2() ) %>% 
      filter (Segmento %in% outVar1()) %>% 
      filter (CIIU.V1.DESC%in% outVar3()) %>% 
      filter (CIIU.V6.DESC %in% outVar4()) %>% 
      select( admin.name1)
  })
  
  observe({
    updatePickerInput(session, "ge2",
                      choices =   unique(elegir_pcia()$admin.name1),
                      selected = unique(elegir_pcia()$admin.name1)                  
    )
  })
  ## mapa ----
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       color = ~color, 
                 labelOptions = labelOptions(textsize = "12px"),
                 #popup = ~popup_text,
                 layerId = ~CUIT,
                 label =  ~CUIT
                 )
  })
  
  
  ## tabla ----
    output$tabla <- renderDataTable(
    server=FALSE,
    filteredData() %>% 
      select (CUIT, Trabajadores,Aseguradora,Provincia,CIIU.V1.DESC,CIIU.V6.DESC,Cliente, Contactado,'Fecha_Cotizacion', Alicuota.Variable.Cotizacion) %>% 
      rename ('CIIU V1' = CIIU.V1.DESC, 'CIIU V6' = CIIU.V6.DESC),
    selection = 'none', 
    editable = TRUE, 
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      bInfo = FALSE,
      dom = 'Bfrtip',
      buttons = c('excel'),
      scrollX = TRUE,
      class = "display"
    )
  )
  
    ## logica de guardado y  visualizacion de datos una vez clickeado el mapa----
    
    rResult <- reactiveValues(df = 0, df1 = 0)
   
    data_save1 <- reactive({
      data = read.xlsx('nuevos datos.xlsx')
      return(data)})
   
    
    observeEvent(input$map_marker_click,{
      
      dd <- reactive({df <- filteredData() %>% filter (CUIT == input$map_marker_click$id) %>% select(alicuota)
                      df <- if (length(df) ==0) {0} else{df}
                      df1 <- if (is.na(df$alicuota)) {0} else { df$alicuota}
                       
                      return(df1)})
    
       output$banking.df_data <- renderDataTable(
        data_save(),
        selection = 'none', 
        editable = TRUE, 
        rownames = FALSE,
        extensions = 'Buttons',
        
        options = list(
          paging = FALSE,
          searching = FALSE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          bInfo = FALSE,
          dom = 'Bfrtip',
          buttons = c('excel'),
          
          class = "display"
        )
      )
       ## precio referencia mercado ----
       output$precio_m <- renderValueBox({
         if ( dd() == 0) { 
           df <- ""
           valueBox(
             df
             ,'Precio de Referencia Mercado'
             #,icon = icon("check",lib='glyphicon')
             ,color = "olive",
             width = 8)
         }else{
           df2 <-paste(prettyNum(round(dd()*100,2),big.mark=",",scientific=FALSE),"%") 
           valueBox(
             formatC(df2, format="d", big.mark=',')
             ,'Precio de Referencia Mercado'
             ,icon = icon("check",lib='glyphicon')
             ,color = "olive",
             width = 8)}
       })
       rResult$df1 <- input$map_marker_click$id
    }
    )
    
    x <- reactiveValues(
         df = 0
       )
       
    observe({
         x$df <- data_save()
         
       })
       
    
    observeEvent(input$banking.df_data_cell_edit, {
      info = input$banking.df_data_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      x$df[i, j+1]  <<- DT::coerceValue(v, x$df[i, j+1]) # el usuario edita el dataframe y lo guardo en x$df
       })
    
    data_save <- reactive({ 
      data = data_save1() %>% 
        filter (CUIT == rResult$df1) 
      data1 = data_vigencias %>% 
        select(CUIT,Cliente,Contactado,'Fecha_Cotizacion', Alicuota.Variable.Cotizacion, Alicuota.Competencia) %>% 
        mutate (
          obsevaciones = ""
        ) %>% 
        filter (CUIT == rResult$df1)
      if(z$z == 0){
      if (dim(data)[1] == 0) {
        data2 = data1
      }else{
        data2 = data
      }
      return(data2)
      }
      else{
        data = view_fun()%>% 
          filter (CUIT == rResult$df1) 
        
        if (dim(data)[1] == 0) {
          data2 = data1
        }else{
          data2 = data        }
        return(data2)
      }
      
      })
    
    z <- reactiveValues(
      z = 0
    )
    observeEvent(input$Switch.1,{
      if (input$Switch.1 == 1) 
      {insertUI(selector = "#saveBtn",
                where = "afterEnd",
                ui = tags$audio(src = "gold_sack.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")
      )}
      print(input$Switch.1)
    })
    
    
    observeEvent(input$saveBtn,{
       insertUI(selector = "#saveBtn",
                where = "afterEnd",
                ui = tags$audio(src = "Collect_Point_00.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")
       )
      a <- read.xlsx('nuevos datos.xlsx')
      b <- rbind(a,x$df)
      b <- b %>% group_by(CUIT) %>% filter(duplicated(CUIT) | n()==1)
      print(x$df)
      print(a)
      write.xlsx(b,'nuevos datos.xlsx')
      z$z = 1 
      shinyalert(title = "Guardado!", type = "success")
    })
    view_fun<-eventReactive(input$saveBtn,{
      data = read.xlsx('nuevos datos.xlsx')
     
      return(data)
    }
    )
  }

