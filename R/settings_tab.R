#' Create the settings tab
#' This function needs to be able to trigger the fech of a new dt_user
#'
#' @param dt_user Data table identifying the current user
#'
#' @export
settings_tab = function(input, output, session, auth){
  ### Render the summary page
  # This must be reset then the "reset" button is pressed
  shiny::observeEvent(
    eventExpr   = input$reset,
    ignoreNULL  = FALSE,
    handlerExpr = {
      output$settings = render_settings_page(input, output, session, auth,
                                             permissions = "user")
    })

  ### Save changes to the users details
  shiny::observeEvent(
    eventExpr   = input$save,
    handlerExpr = {
      save_user_details(input, output, session, auth, permissions = "user")
    })

  ### Change password
  shiny::observeEvent(
    eventExpr   = input$password_change,
    handlerExpr = {

      ShinyBasicAuth::password_change_manager(
        input, output, session, auth,
        admin        = FALSE,
        user_id      = auth$dt_user[, users_id],
        old_password = auth$dt_user[, password])
    })

}

#' Save the current user datailes to the db
save_user_details = function(input, output, session, auth, permissions = "user"){

  # Find all of the rows that are spesificly user changable
  if (permissions == "admin") {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$admin_changeable))
  } else {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$user_changeable))
  }

  # Extract just the user chanable columns
  user_changeable_columns = setdiff(
    x = names(auth$table_cofig[cond]),
    y = c("password", "admin", "moderator"))

  # // TODO change this to something less awfull
  for (col_name in user_changeable_columns) {
    # Make nice injection proof queury
    sql_save_user_details =
      paste0(
        "UPDATE Users SET ",
        col_name, " = ?value ",
        " WHERE ",
        " user_id = ?user_id;"
      )

    query_save_user_details =
      DBI::sqlInterpolate(auth$con, sql_save_user_details,
                          value   = input[[col_name]],
                          user_id = auth$dt_user[, user_id])

    # Send the qeruy
    DBI::dbGetQuery(auth$con, query_save_user_details)
  }

  # Tell the user the save has been sucessfull
  session$sendCustomMessage(
    type    = 'testmessage',
    message = "Save changes to the datatbase")

}

#' Function creating the ui for a settings page for a given user table
#'
#' @param permissions The level of permision the accessing user has
#'
#' @import data.table
render_settings_page = function(input, output, session, auth,
                                permissions = "user"){
  ns = session$ns

  # Find all of the rows that are spesificly user changable
  if (permissions == "admin") {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$admin_changeable))
  } else {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$user_changeable))
  }

  # Extract just the user chanable columns
  user_changeable_columns = setdiff(
    x = names(auth$table_cofig[cond]),
    y = c("password", "admin", "moderator"))

  shiny::renderUI({
    shiny::conditionalPanel(
      condition = "input.navtabs == 'settings'",

      shiny::fluidPage(

        page_tile("Settings"),

        # Verable boxes
        shiny::fluidRow(
          lapply(
            X = user_changeable_columns,
            FUN     = render_settings_box,
            input   = input,
            output  = output,
            session = session,
            auth    = auth)),

        shiny::fluidRow(
          shinydashboard::box(width = 4,
                              shiny::actionButton(
                                inputId = ns("password_change"),
                                label   = "Change Password",
                                width   = "100%"
                              )),
          shinydashboard::box(width = 4,
                              shiny::actionButton(
                                inputId = ns("save"),
                                label   = "Save Changes",
                                width   = "100%"
                              )),
          shinydashboard::box(width = 4,
                              shiny::actionButton(
                                inputId = ns("reset"),
                                label   = "Reset",
                                width   = "100%"
                              ))
        )

        ))
    })

}



#' @import data.table
render_settings_box = function(input, output, session, auth,
                               column_name) {

  ns = session$ns

  # Find the type of verable we need to genorate a box for
  type = auth$table_cofig[[column_name]]$type

  # Check for special case
  if (column_name %in% c("moderator", "admin")) {
    ui = shinydashboard::box(width = 4,
                             title = auth$table_cofig[[column_name]]$human_name,
                             shiny::checkboxInput(
                               inputId = ns(column_name),
                               label   = NULL,
                               value   = as.logical(auth$dt_user[, ..column_name])
                               ))

  } else if (column_name %in% c("users_moderator")) {
    # Get list of moderators
    moderators_list = c(1, 2, 3)

    ui = shinydashboard::box(width = 4,
                             title = auth$table_cofig[[column_name]]$human_name,
                             shiny::selectInput(
                               inputId  = ns(column_name),
                               label    = NULL,
                               choices  = moderators_list,
                               selected = auth$dt_user[, ..column_name]))

  } else if (type %in% c("character", "intiger", "decimal")) {
    ui = shinydashboard::box(width = 4,
                             title = auth$table_cofig[[column_name]]$human_name,
                             shiny::textInput(
                               inputId = ns(column_name),
                               label   = NULL,
                               value   = auth$dt_user[, ..column_name]))

  } else if (type == "logical") {
    ui = shinydashboard::box(width = 4,
                             title = auth$table_cofig[[column_name]]$human_name,
                             shiny::checkboxInput(
                               inputId = ns(column_name),
                               label   = NULL,
                               value   = auth$dt_user[, ..column_name]))

  } else if (type == "categorical") {
    # Get the catagorys from auth
    catagorys = auth$table_cofig[[column_name]]$categories

    ui = shinydashboard::box(width = 4,
                             title = auth$table_cofig[[column_name]]$human_name,
                             shiny::selectInput(
                               inputId  = ns(column_name),
                               label    = NULL,
                               choices  = catagorys,
                               selected = auth$dt_user[, ..column_name]))
  } else {
    stop("The coloumn ", column_name, " is said to to be of type ", type,
         ".  This is not a valid column type.")
  }

  return(ui)
}

page_tile = function(title){
  shiny::fluidRow(
    shinydashboard::box(width  = 12,
                        height = "60px",
                        title  = shiny::HTML(paste0("<p style='font-size:20px'>",
                                                    title,
                                                    "</p>"))
    ))
}
