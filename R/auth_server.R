#' Manage the logon, and the changes of the UI based upon
#'
#' @param con Database connection or pool
#'
#' @import data.table
#' @import magrittr
#' @export
auth_server = function(con,
                               logged_on_server,
                               user_id_col_name,
                               password_col_name,
                               user_table) {

  # Make regular shiny server
  shiny::shinyServer(function(input, output, session) {


    ### Create the sidebar, with no-user logged in
    sheppy_auth_sidebar(input, output, session,
                        status = "start")

    ###### When logon is pressed
    shiny::observeEvent(input$login, {
      # Check to see if the password is correct
      # If there is a sucessfull logon then this will be a datatable of the users information
      # if not it will return null

      # loggedin_user_id shoudl be treated as the sacrosanct identifyer of the logged in user
      loggedin_user_id = sheppey_auth(input, output, session, con,
                                      user_id_col_name,
                                      password_col_name,
                                      user_table)

      # Get the username used to check the loginstatus via
      inputed_user = input$user

      # Rest the password field, this is not a good solution to this problem
      sheppy_auth_sidebar(
        input, output, session,
        status       = "start")

      # Check to see if the autentifaction was sucessfull
      if (is.null(loggedin_user_id)) {
        ###  The password is incorrect show an error
        sheppy_auth_sidebar(
          input, output, session,
          status = "failed")
        return()
      } else if (inputed_user == loggedin_user_id) {

        ### Run the server code
        logged_on_server(input, output, session, con, loggedin_user_id)


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
