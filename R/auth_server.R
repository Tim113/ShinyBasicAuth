#' Manage the logon, and the changes of the UI based upon
#'
#' @param con Database connection or pool
#' @param server Shiny Server function to run when a user is logged on, must take aut as argument
#' @param auth_config File path to auth_config file
#'
#' @import data.table
#' @import magrittr
#' @export
auth_server = function(con,
                       server,
                       config_path) {

  # Make regular shiny server
  shiny::shinyServer(function(input, output, session) {

    ### Create the sidebar, with no-user logged in
    auth_sidebar(input, output, session,
                 status = "start")

    ###### When logon is pressed
    shiny::observeEvent(input$login, {
      # Check to see if the password is correct
      # If there is a sucessfull logon then this will be a datatable of the users information
      # if not it will return null

      # Create auth object to pass to logged_on_server
      auth = make_auth_object(con, config_path)

      # loggedin_user_id shoudl be treated as the sacrosanct identifyer of the logged in user
      loggedin_user_id = auth_check(input, output, session, auth)

      # Get the username used to check the loginstatus via
      inputed_user = input$user

      # Rest the password field, this is not a good solution to this problem
      auth_sidebar(
        input, output, session,
        status       = "start")

      # Check to see if the autentifaction was sucessfull
      if (is.null(loggedin_user_id)) {
        ###  The password is incorrect show an error
        auth_sidebar(
          input, output, session,
          status = "failed")
        return()
      } else if (inputed_user == loggedin_user_id) {

        # Add uer_id to auth object
        auth$user_id = loggedin_user_id

        # Get the user from the db
        auth$dt_user = get_dt_user(auth)

        # Create the sidebar
        auth_sidebar(
          input, output, session,
          status = "logged-in")

        ### Render the settings tab
        settings_tab(input, output, session, auth)

        ### Render the admin tab
        admin_tab(input, output, session, auth)

        ### Run the server code
        server(input, output, session, auth)


      } else {
        stop("There has been quite a major error in the auth")
      }
    })

    # # If logout reset the sidbar, and remove the body
    # shiny::observeEvent(input$logout, {
    #
    #   ### Create the sidebar, with no-user logged in
    #   create_dashboard_sidebar(input, output, session,
    #                            competencies,
    #                            status       = "blank")
    #
    #   # Remove any dashoard body there has been
    #   output$body = shiny::h
    # })
  })
}


get_dt_user = function(auth) {
  # The password is correct so get the rest of the user information from the db
  # and return it to the calling funciton
  sql_user_info   = paste0("SELECT * FROM Users WHERE user_id = ?user_id;")
  query_user_info = DBI::sqlInterpolate(auth$con, sql_user_info ,
                                        user_id = auth$user_id)

  # Retreve the query from the db
  suppressWarnings({
    dt_user =
      DBI::dbGetQuery(auth$con, query_user_info) %>%
      data.table::setDT(.)
  })

  # Make the admin column logical
  dt_user = dt_user[, admin := as.logical(admin)]

  # Make the moderator column logical
  dt_user = dt_user[, moderator := as.logical(moderator)]

  return(dt_user)
}