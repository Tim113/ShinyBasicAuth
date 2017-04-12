#' Manage the logon, and the changes of the UI based upon
#'
#' @param con Database connection or pool
#'
#' @import data.table
#' @import magrittr
#' @export
auth_server = function(con,
                       logged_on_server,
                       user_id_col,
                       password_col,
                       admin_col,
                       date_created_col,
                       date_password_changed_col,
                       user_table) {

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
      auth = list(
        con                       = con,
        user_id                   = NULL,
        user_table                = user_table,
        user_id_col               = user_id_col,
        password_col              = password_col,
        admin_col                 = admin_col,
        date_created_col          = date_created_col,
        date_password_changed_col = date_password_changed_col
      )

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

        # Create the sidebar
        auth_sidebar(
          input, output, session,
          status = "logged-in")

        ### Run the server code
        logged_on_server(input, output, session, auth)


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
