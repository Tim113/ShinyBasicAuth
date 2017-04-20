library(shiny)
library(shinydashboard)
library(ShinyBasicAuth)

# The following line must have been run:
ShinyBasicAuth::create_auth_tables(auth_config_path = "./auth_conf.yaml")

### Call the sheppey auth server
ShinyBasicAuth::auth_server(
  server      = server_post_auth,
  config_path = "./auth_conf.yaml")



### Server funciton to run when the user is logged in
server_post_auth = function(input, output, session, auth) {
  
  # Tranditional Server functions
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # Render the ui the ui 
  output$body = renderUI({
    shiny::conditionalPanel(  # You must use condtional panels for the tabs
      condition = "input.authMenuItems == 'histogram'",
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(plotOutput("plot1", height = 250)),
        
        box(
          title = "Controls",
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      ))
  })
  
  output$sidebar = renderUI({
    shinydashboard::sidebarMenu(
      id = "authMenuItems",     # Must be called this
      shinydashboard::menuItem(text     = "A Histogram",
                               tabName  = "histogram"),
      
      shinydashboard::menuItem(text     = "Settings",
                               tabName  = "settings"),
      
      shinydashboard::menuItem(text     = "Admin",
                               tabName  = "admin")
    )
  })
}
