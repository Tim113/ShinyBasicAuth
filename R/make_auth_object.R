#' Make the auth object from the auth_config file and the connection
#'
#' @import data.table
#' @import magrittr
make_auth_object = function(pool_auth, pool_data, auth_config) {
  # Extract the table names, or set defults in the auth object from auth_config

  auth = list(
    pool_auth   = pool_auth,
    pool_data   = pool_data,
    user_id     = NULL,
    dt_user     = NULL,
    table_cofig = auth_config$table_cofig
  )

  return(auth)
}

# auth_config = yaml::yaml.load_file("/media/sf_Documents/FA Tool/ShinyBasicAuth/.auth_config.yaml")
