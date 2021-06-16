function(input, output, session) {

  ## conexiones entre modulos a través de reactive values
  r <- reactiveValues(map = 0, session = session, map_data = NULL)
  new_data <- reactiveValues(
    # df = loadData() # metodo deprecado
    df = read.xlsx("primeras observaciones.xlsx")
  )

  filteredData <- dataServer("data", data_vigencias, new_data = new_data, r = r)
  mapServer("map", df = filteredData, r = r)
  tableServer("table", df = filteredData)
  customerServer("cliente", df = filteredData, r = r, new_data = new_data, USER = USER)
  marketServer("market")
  curvapreciosServer("curva", r = r)
  login <- FALSE
  user <- NULL
  USER <- reactiveValues(login = login, user = user)

  loginServer("login", USER = USER)


  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE) {
      sidebarMenu(
        menuItem("Search New Customer", tabName = "dashboard", icon = icon("check", lib = "glyphicon")),
        menuItem("Market Share", tabName = "second", icon = icon("chart-bar")),
        menuItem("Market Pricing", tabName = "third", icon = icon("file-invoice-dollar"))
      )
    }
  })

  output$body <- renderUI({
    if (USER$login == TRUE) {
      tabItems(
        tabItem(
          tabName = "dashboard", class = "active",
          fluidRow(
            dataUI("data"),
            column(
              width = 8,
              tabsetPanel(
                id = "inTabset",
                mapUI("map"),
                customerUI("cliente"),
                tableUI("table")
              )
            )
          )
        ),
        tabItem(
          tabName = "second",
          # h2("This is second tab"),
          marketUI("market")
        ),
        tabItem(
          tabName = "third",
          curvapreciosID("curva")
        )
      )
    }
    else {
      logiUi("login")
    }
  })

  ## Control de la cantidad de usuarios
  onSessionStart <- isolate({
    users$count <- users$count + 1
  })

  onSessionEnded(function() {
    isolate({
      users$count <- users$count - 1
    })
  })

  output$text <- renderUI({
    h6(paste0("There are ", users$count, " user(s) connected to this app"))
  })
}

# rsconnect::deployApp("c:/users/jrr/desktop/App_BI - V2")
