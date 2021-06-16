loginServer <- function(id, USER) {
  moduleServer(id, function(input, output, session) {
    
    observe({ 
      USER$user <- input$userName
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            if(length(which(credentials$username_id==Username))==1) { 
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              if(pasverify & users$count <= limite_de_usuarios) {
                USER$login <- TRUE
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } 
        }
      }    
    })
    
      output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                          font-weight: bold; margin:5px; padding: 10px;")
      })
  })
}



logiUi <- function(id) { 
div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput(NS(id,"userName"), placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput(NS(id,"passwd"), placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton(NS(id,"login"), "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = NS(id,"nomatch"),
                           tags$p("Usuario o Password incorrecto o exceso de usuarios!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                   ))
)
}

credentials = data.frame(
  username_id = c("Calamante", "Bona", "Spagnoli", "Esperon","admin" ,"myuser1"),
  passod   = sapply(c("Asociart01", "Asociart01","Asociart01","Asociart01","admin","mypass1"),password_store),
  permission  = c("basic","basic","basic","basic","basic", "advanced"), 
  stringsAsFactors = F
)

# shinyApp(server = function(input, output, session){ ## debuguiando. 
#   loginServer("r") }, 
#   ui =  logiUi("r")
#            )