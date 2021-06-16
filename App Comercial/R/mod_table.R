tableUI <- function(id) {
  ns <- NS(id)
      tabPanel("Tabla",
             DT::dataTableOutput(ns("tabla"))
  )
}

tableServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ## tabla ----
    output$tabla <- DT::renderDataTable(
      server = FALSE,
      df() %>%
        select(
          CUIT,
          Trabajadores,
          Aseguradora,
          Provincia,
          CIIU.V1.DESC,
          CIIU.V6.DESC,
          Cliente,
          Contactado,
          "Fecha_Cotizacion",
          Alicuota.Variable.Cotizacion
        ), # %>%
      #  rename ('CIIU V1' = CIIU.V1.DESC, 'CIIU V6' = CIIU.V6.DESC),
      selection = "none",
      editable = FALSE,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        bInfo = FALSE,
        dom = "Bfrtip",
        buttons = c("excel"),
        scrollX = TRUE,
        class = "display"
      )
    )
  })
}
