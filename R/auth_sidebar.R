#' Function that creates a sidebar with the id authMenuItems and ShinyBasicAuth tabs automaticly
#' This should be used like shinydashboard::sidebarMenu exept the id cannot be changed
#'
#' @import data.table
#' @export
authSidebarMenu = function(auth, ...) {
  shinydashboard::sidebarMenu(
    id = "authMenuItems",

    ...,

    shinydashboard::menuItem(text     = "Settings",
                             tabName  = "settings"),

    if (auth$dt_user[, admin]) {
      shinydashboard::menuItem(text     = "Admin",
                               tabName  = "admin")
    }
  )
}

#' Create the sidbar for the dashboard, diffrent dependent upon the status of the app
#'
#' @param status What is the status of the app and as such which of the sidebars should be shown
#'
#' @import data.table
#' @import magrittr
auth_sidebar = function(input, output, session, status){
  #################### Explanation
  # There are three possible states for the sidebar
  # 1. No-login attempt ("start")
  # 3. User logged in ("logged-in")
  # 2. Login Failed     ("failed")
  # Each of these will have their own side bar, and thus will need diffrent deffintions
  # When this fucntion is called the state argument must be one of the three listed above
  # so that the funciton calls the rifht one

  # Insure that staus is on of the valid statu's for the list
  if (!(status %in% c("start", "logged-in", "failed"))) {
    stop("Agrument staus must be a member of {start, logged-in, failed}")
  }

  #################### Create Sidebar   ################
  # If status is start show the logon sidebar
  if (status == "start") {
    ## Render the Sidbar Meneu
    output$auth_sidebar = shiny::renderUI({
      # The inital sidebar menue
      shinydashboard::sidebarMenu(
        ### Login to the app
        shiny::textInput(inputId = "user",
                         label   = "Employee ID:",
                         value   = ""),
        shiny::passwordInput(inputId = "password",
                             label   = "Password:",
                             value   = ""),

        shiny::HTML("<p> <br/> </p>"),

        shiny::actionButton(inputId = "login",
                            label   = "Login",
                            width   = "100%",
                            icon    = shiny::icon("sign-in")))
    })

  } else if (status == "failed") {
    ## Render the Sidbar Meneu after a failed logon

    # Get the user_id and save it
    user_id = input$user

    # Render
    output$auth_sidebar = shiny::renderUI({
      # The inital sidebar menue
      shinydashboard::sidebarMenu(
        ### Login to the app
        shiny::textInput(inputId = "user",
                         label   = "Employee ID:",
                         value   = user_id),
        shiny::passwordInput(inputId = "password",
                             label   = "Password:",
                             value   = NULL),

        shiny::HTML("<p> <br/> </p>"),

        shiny::actionButton(inputId = "login",
                            label   = "Login",
                            width   = "100%",
                            icon    = shiny::icon("sign-in")),

        # Dispaly error message
        shinydashboard::valueBox(value    = "",
                                 subtitle = "Username or password incorrect.",
                                 icon     = NULL,
                                 width    = "100%",
                                 color    = "yellow"))
    })

  } else if (status == "logged-in") {

    output$auth_sidebar = shiny::renderUI({
      # shinydashboard::sidebarMenu(
      #
      #   shinydashboard::menuItem(text     = "Settings",
      #                            tabName  = "settings"),
      #
      #   shinydashboard::menuItem(text     = "Admin",
      #                            tabName  = "admin")
      # )
    })

  }
}
