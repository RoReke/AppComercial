

# Main login screen

header <- dashboardHeader(title = "R_BIapp", uiOutput("text"), uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) # ,width = 100
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"), width = 800)
ui <- dashboardPage(header, sidebar, body, skin = "blue")
