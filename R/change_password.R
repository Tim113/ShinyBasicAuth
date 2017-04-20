#' This function creates and manages the life cycle of a uniqe password_change_modal that
#' allos the user to change there password.  Or an andmin to change the user passwords for
#' any user.
#'
#' @param admin Is true if this is an admin changin the datils for another user
#'
#' @export
password_change_manager = function(input, output, session, auth,
                                   admin        = FALSE,
                                   user_id      = NULL,
                                   old_password = NULL){
  # If user_id has not been given then take it form auth object
  user_id = auth$user_id

  # If not acting as admin then there must be an old_password
  if (!admin & is.null(old_password)) {
    stop("If not acting as admin then users old password must be given.")
  }

  # Create time stamp for this instance
  time_stamp = Sys.time() %>%
    gsub('[[:punct:], [:space:]]', '', .)

  # Make the password modal
  password_modal = password_change_modal(input, output, session,
                                         admin,
                                         user_id,
                                         time_stamp)

  # Show the modal containg password change options
  shiny::showModal(password_modal)

  ################ listen for password buttons
  # There are two diffrent password modals that can be shown depening on the context
  # that this funcition is called from, they are disigised via the button id they use
  # for saving
  ### Listen for the password change button
  shiny::observeEvent(
    eventExpr = input[[paste0("check_and_change_password", time_stamp)]],
    handlerExpr = {

      # Check the old password given matches the one in the db
      old_password_correct =
        sodium::password_verify(password =  input$old_password,
                                hash     = old_password)

      # The two new passwords given match
      new_passwords_match =
        input$new_password_main == input$new_password_confirm

      # New password vaild
      # // TODO Check htr password is storng
      password_strong_enough = TRUE

      if (old_password_correct &
          new_passwords_match &
          input$password_change_confirm &
          password_strong_enough) {
        # Save the new password to the db
        save_new_password(session, auth,
                          user_id,
                          sodium::password_store(input$new_password_main))

        # Close the modal dialog box
        shiny::removeModal()

        # Remake the modal to remove the passwords from memory
        password_modal = password_change_modal(input, output, session,
                                               admin,
                                               user_id,
                                               time_stamp)
        gc()
      } else {
        ### Find the error then dispaly message to user explaing the problem
        if (!old_password_correct) {
          session$sendCustomMessage(
            type    = 'testmessage',
            message = "Old password not given correctly.")
        } else if (!new_passwords_match) {
          session$sendCustomMessage(
            type    = 'testmessage',
            message = "New passwords do not match.")
        } else {
          session$sendCustomMessage(
            type    = 'testmessage',
            message = "Please check Confirm Password Change.")
        }
      }
    })


  ### Listen for the password reset button
  shiny::observeEvent(
    eventExpr = input[[paste0("reset_password", time_stamp)]],
    handlerExpr = {



      # Check that the coconformfurm password change box has been ticked
      if (input$password_change_confirm) {
        # Save the new password to the db
        save_new_password(session, auth,
                          user_id,
                          sodium::password_store(input$new_password_main))

        # Remake the modal to remove the passwords from memory
        password_modal = password_change_modal(input, output, session,
                                               admin,
                                               user_id,
                                               time_stamp)
        gc()

        # Close the modal dialog box
        shiny::removeModal()
      } else {
        ### Error message
        session$sendCustomMessage(
          type    = 'testmessage',
          message = "Please check Confirm Password Reset.")
      }
    })
}

#' Save a new password to the db
save_new_password = function(session, auth,
                             user_id,
                             new_password) {

  sql_update_password =
    paste0(
      "UPDATE Users",
      " SET password = ?password, ",
      " last_password_change = NOW() ",
      " WHERE ",
      "user_id = ?user_id;"
    )

  query_update_password =
    DBI::sqlInterpolate(auth$con, sql_update_password,
                        password = new_password,
                        user_id  = auth$user_id)

  DBI::dbGetQuery(auth$con, query_update_password)#

  # Let the user know the password has been saved to the db
  session$sendCustomMessage(
    type    = 'testmessage',
    message = "New password Saved")

}

#' Password Reset/Change modal ui box
#' The UI shown depends on if this is for the logged inuser or an admin
#' changeing the passowrd of anothe user
#'
#' @param time_stamp Creates a uniqe box each time the box is opends
#'
#' @import data.table
password_change_modal = function(input, output, session,
                                 admin,
                                 user_id,
                                 time_stamp){

  # Check to see if this is a password change for the current user
  if (!admin) {

    #  The user to change the password is the active user
    shiny::modalDialog(
      title     = "Change Password",
      size      = "s",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = shiny::tagList(
        shiny::actionButton(inputId =
                              paste0("check_and_change_password",
                                     time_stamp),
                            label   = "Change Password"),
        shiny::modalButton("Close")
      ),

      # Body of Modal Window
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::passwordInput(
            inputId = "old_password",
            label   = "Old Password",
            width   = "100%"),
          shiny::passwordInput(
            inputId = "new_password_main",
            label   = "New Password",
            width   = "100%"),
          shiny::passwordInput(
            inputId = "new_password_confirm",
            label   = "Confirm New Password",
            width   = "100%"),
          shiny::checkboxInput(
            inputId = "password_change_confirm",
            label   = "Confirm Password Change",
            value   = FALSE,
            width   = "100%")
        )
      )
    )
  } else {

    reset_password = bcrypt::gensalt() %>%
      substr(start = 8, stop = 18)


    # The user to change password for is not the active user
    shiny::modalDialog(
      title     = "Reset Password",
      size      = "s",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = shiny::tagList(
        shiny::actionButton(inputId =
                              paste0("reset_password",
                                     time_stamp),
                            label   = "Reset Password"),
        shiny::modalButton("Close")
      ),

      # Body of Modal Window
      shiny::fluidPage(
      shiny::fluidRow(
        shiny::textInput(
          inputId = "new_password_main",
          label   = "Make Note of New Tempory Password",
          value   = reset_password),
        shiny::checkboxInput(
          inputId = "password_change_confirm",
          label   = "Confirm Password Reset",
          value   = FALSE,
          width   = "100%")
      )
    ))
  }

}
