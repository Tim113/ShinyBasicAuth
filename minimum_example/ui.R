## ui.R ##
shiny::shinyUI(
  shinydashboard::dashboardPage(
    title = "ShinyBasicAuth",
    
    shinydashboard::dashboardHeader(
      title = "ShinyBasicAuth"),
    
    shinydashboard::dashboardSidebar(
      
      ### Your sidebar deffined in server_post_auth ### 
      shiny::uiOutput("sidebar"),
      
      # Requiered for ShinyBasicAuth
      shiny::uiOutput("auth_sidebar")
    ),
    
    shinydashboard::dashboardBody(
      # Requiered for ShinyBasicAuth
      shiny::tags$head(
        # Import the Java script for the pop-up error message box
        # This is needed if you want any messages to users
        shiny::tags$script(src = "message-handler.js")),
      
      shiny::uiOutput("auth_body"),
      
      
      ### Your body Start ###
      shiny::uiOutput("body")
      ### Your body End ###
      
    ))
)
