#' @export
text_with_linebreaks = function(text) {
  # Replace \n line breaks with the <br/> tags
  shiny::HTML(gsub(
    pattern = "\n",
    replacement = "<br/>",
    x = (text)))
}


#' @export
page_tile = function(title, text = NULL, main = TRUE){
  shiny::fluidRow(
    shinydashboard::box(
      width  = 12,
      title = NULL,
      if (main) {
        shiny::h2(title)
      } else {
        shiny::h3(title)
      },
      shiny::p(text)
    ))
}


#' Defult syling for what was a slect input that another user has changed and saved
fixed_shiny_input = function(label = NULL, text = NULL) {
  shiny::HTML(paste0(
    "<h4>",
    label,
    "</h4>",
    "<p>",
    text,
    "<p>"
  ))
}

# Creates row of header boxes that line up with the boxes in resuts_row
header_row = function(col_names) {
  # Check that no more than 11 values have been given to values
  if (length(col_names) > 5) {
    stop("values cannot have more than 5 elements, this is a limitation in the number of ",
         " available colours defined in the css.")
  }

  # Find the width of each box
  width = floor(12/(length(col_names)))

  # The width of the row must be 12 in to
  row_name_with = 12 - (width * (length(col_names) - 1))

  shiny::fluidRow(
    shinydashboard::box(
      width = row_name_with,
      shiny::h4(col_names[1])),

    lapply(
      X = 2:length(col_names),
      FUN = function(x) {
        shinydashboard::box(
          width = width,
          shiny::h4(col_names[x]))
      })
  )
}


resuts_row = function(row_name, values) {
  values = as.numeric(values)
  values[is.na(values)] = 0

  # Check that no more than 11 values have been given to values
  if (length(values) > 5) {
    stop("values cannot have more than 5 elements, this is a limitation in the number of ",
         " available colours defined in the css.")
  }

  # Find the width of each box
  width = floor(12/(length(values) + 1))

  # The width of the row must be 12 in to
  row_name_with = 12 - (width * length(values))

  # Set a vector of colours
  colours = c("light-blue", "green", "aqua", "yellow", "red")

  shiny::fluidRow(
    shinydashboard::box(
      width = row_name_with,
      height = "40px",
      shiny::h5(row_name)),

    lapply(
      X = 1:length(values),
      FUN = function(x) {
        shinydashboard::box(
          width = width,
          percent_bar(value = values[x],
                      color = colours[x]))
      })
  )
}

#' Box of 40px hight that shows value as a percent
#'
#' @export
percent_bar = function(value, color = "aqua"){

  if (is.na(value)) {value = 0}

  # Check value
  if (value > 1 | value < 0) {
    stop(paste0(
      "value has been given as ", value, ".  However value must be in [0, 1]."
    ))}

  # Multiply value by 100 so that is looks like a 'percent'
  value = 100 * value

  shinydashboard:::validateColor(color)
  shiny::tags$div(
    # h5(text),
    div(
      class = "progress s",
      # tags$p(class = "pull-right", scales::percent(value), position = "absolute"),
      div(
        class = paste0("progress-bar progress-bar-", color),
        style = paste0("width: ", value, "%"),
        role  = "progressbar",
        `aria-valuenow` = value, `aria-valuemin` = "0", `aria-valuemax` = "100",
        span(class = "sr-only", paste0(value, "% complete")))))
}
