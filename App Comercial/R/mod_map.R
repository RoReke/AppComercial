mapUI <- function(id) {
  ns <- NS(id)
      tabPanel("Mapa", icon = icon("globe"), "", width = 400, box(
        width = NULL, solidHeader = TRUE,
        leafletOutput(ns("map"), height = 700)
      ))
}

mapServer <- function(id, df, r) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      if (nrow(df()) > 0) {
      leaflet(df(), options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$Esri.WorldTopoMap,
          options = providerTileOptions(
            updateWhenZooming = FALSE,
            updateWhenIdle = TRUE
          )
        ) %>%
        addCircleMarkers(~longitude, ~latitude,
          color = ~color,
          labelOptions = labelOptions(textsize = "12px"),
       #   popup = ~Trabajadores,
          layerId = ~CUIT,
          label = ~label
        )
      }
    })
    cuit_mapa <- reactiveValues(
      x = NULL
    )
    rResult <- reactiveValues(df = 0, df1 = 0)
    
    observeEvent(input$map_marker_click, {
      r$map = 1 
      r$map_data = input$map_marker_click$id
      #print(r$map)
      #print(input$map_marker_click$id)
    })
  })
}

# shinyApp(ui = fluidPage(mapUI("map")) ,
#          server = server <- function(input, output, session) {
#            mapServer("map", df = df_vig )})
