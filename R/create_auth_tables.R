#' Function to create tables for ShinyBasicAuth, this funciton will also chec that the
#' auth.config.yaml file contanis the requite information
#'
#' @import data.table
#' @import magrittr
#' @export
create_auth_tables = function(auth_config) {

  #################### Basic Checks ##########
  # Check that auth_config has all of the requiered columns
  required_columns = c(
    "user_id",
    "password",
    "date_created",
    "last_password_change",
    "admin",
    "moderator"
  )

  # Stop the funciton if not all of the columns are present
  if (!all(required_columns %in% names(auth_config$table_cofig))) {
    # Find all of the columsn that are not included to give a nice error message
    missing_columns =
      required_columns[!(required_columns %in% names(auth_config$table_cofig))]
    stop(paste0("Requiered column: ", as.character(missing_columns),
                " is not present.  "))
  }

  # Check that the use_moderatior entary exist iff use_moderatior: TRUE
  if (auth_config$table_cofig$moderator$use_moderatior &
      !("users_moderator" %in% names(auth_config$table_cofig))) {
    stop("use_moderatior: TRUE but there is no users_moderator in the config file")
  } else if (!auth_config$table_cofig$moderator$use_moderatior &
              "users_moderator" %in% names(auth_config$table_cofig)) {
    stop("use_moderatior: FALSE but there is a users_moderator in the config file.  ",
         "You have probably made a mistake.")
  }

  # Check that a valid first user has been given
  if (is.null(auth_config$first_user)) {
    stop("No first user has been given for table creation.")
  } else if (is.null(auth_config$first_user$user_id)) {
    stop("No user id has been given for the first user.")
  } else if (is.null(auth_config$first_user$password)) {
    stop("No password has been given for the first user.")
  }

  #################### Requiered Columns ##########
  ########## User_id
  # Set the type of column for user id
  if (auth_config$table_cofig$user_id$type == "integer") {

    model_Users = data.table::data.table(user_id = integer())

  } else if (auth_config$table_cofig$user_id$type == "numeric") {

    model_Users = data.table::data.table(user_id = numeric())

  } else if (auth_config$table_cofig$user_id$type == "character") {

    model_Users = data.table::data.table(user_id = character())

  } else {
    # The user_id column is not of a valid form
    stop(paste0(
      "The type fo the user_id column has been given as ",
      auth_config$table_cofig$user_id$type,
      ".  Howerver user_id must be one of integer, numeric, character"))
  }

  ########## Password
  # make the password column
  model_Users[, password := character()]

  ########## Date Created
  # make the date_created column
  model_Users[, date_created := as.Date(character())]

  ########## Last Password Change
  # make the last_password_change column
  model_Users[, last_password_change := as.Date(character())]

  ########## Admin
  # make the admin column
  model_Users[, admin := logical()]

  ########## moderator
  # Only run this section if use_moderatior: TRUE
  if (auth_config$table_cofig$moderator$use_moderatior) {
    ### Moderator
    # make the moderator column
    model_Users[, moderator := logical()]

    ### Users Moderatior
    # make the users_moderator column
    model_Users[, users_moderator := character()]

    # They type of column for users_modeator must be the same as user_id
    if (auth_config$table_cofig$user_id$type == "integer") {

      model_Users[, users_moderator := integer()]

    } else if (auth_config$table_cofig$user_id$type == "numeric") {

      model_Users[, users_moderator := numeric()]

    } else if (auth_config$table_cofig$user_id$type == "character") {

      model_Users[, users_moderator := character()]

    } else {
      # The user_id column is not of a valid form
      stop(paste0(
        "The type fo the user_id column has been given as ",
        auth_config$table_cofig$user_id$type,
        ".  Howerver user_id must be one of integer, numeric, character"))
    }
  }

  #################### Additonal Columns ##########
  # Find all of the addional columns that the user has created
  additonal_columns =
    names(auth_config$table_cofig)[!(names(auth_config$table_cofig) %in%
                                         c(required_columns, "users_moderator"))]


  # Now we have a list of all of the additonal columns we must add each of them in tern
  # to the model table
  for (col_name in additonal_columns) {
    # They type of column for users_modeator must be the same as user_id
    if (auth_config$table_cofig[[col_name]]$type == "integer") {

      model_Users[, (col_name) := integer()]

    } else if (auth_config$table_cofig[[col_name]]$type == "numeric") {

      model_Users[, (col_name) := numeric()]

    } else if (auth_config$table_cofig[[col_name]]$type == "character") {

      model_Users[, (col_name) := character()]

    } else if (auth_config$table_cofig[[col_name]]$type == "datetime") {

      model_Users[, (col_name) := as.Date(character())]

    } else if (auth_config$table_cofig[[col_name]]$type == "logical") {

      model_Users[, (col_name) := logical()]

    } else if (auth_config$table_cofig[[col_name]]$type == "catagorical") {

      # Check that the leves of the catagorcial verable have been set
      if (is.null(auth_config$table_cofig[[col_name]]$categories)) {
        stop(paste0(
          "There are no categories set in the config file for column: ",
          col_name,
          "."
        ))
      } else {
        model_Users[, (col_name) := character()]
      }

    }
  }

  #################### Create Table ##########
  # Set key for the table
  data.table::setkeyv(model_Users, "user_id")

  # Create the Query
  create_query_Users = dbUpdateTable::create(
    verbose = FALSE,
    model   = model_Users)

  # Replace the NA for date with just date
  create_query_Users = gsub("NA", "datetime",  create_query_Users)

  # Connect to the db
  con = RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname   = auth_config$users_table$db_name,
    username = auth_config$users_table$username,
    password = auth_config$users_table$password,
    host     = auth_config$users_table$host,
    port     = auth_config$users_table$port)

  # Make the tables
  DBI::dbGetQuery(con, create_query_Users)

  #################### Make first user ##########
  # Set the name
  dt_first_user = data.table::data.table(
    user_id = auth_config$first_user$user_id)

  # Set the password
  dt_first_user[, password := sodium::password_store(auth_config$first_user$password)]

  # Set as admin
  dt_first_user[, admin := 1]

  dbUpdateTable::dbUpdateTable(con = con, name = "Users", dt = dt_first_user)

  # Kill the connection
  RMySQL::dbDisconnect(con)

}
