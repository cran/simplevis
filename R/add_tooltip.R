#' Add a tooltip column
#'
#' @description Add a tooltip column of united variable names and values.
#' @param data A data frame or tibble.
#' @param ... Arguments passed to select (i.e unquoted variables, tidyselect helpers etc). If no arguments provided, uses all columns.
#' @param titles A function to format the variable names, including in rlang lambda format.
#' @param name The name of the column created. Defaults to "tooltip".
#'
#' @return A data frame or tibble with a column of text
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' iris %>%
#'   add_tooltip() %>%
#'   head(1)
#'
#'  iris %>%
#'   add_tooltip(tidyselect::contains("Sepal"), Species) %>%
#'   head(1)
#'
#' p <- iris %>%
#'   dplyr::mutate(Species = stringr::str_to_sentence(Species)) |>
#'   add_tooltip(tidyselect::contains("Sepal"), Species) |>
#'   ggplot() +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, col = Species, text = tooltip))
#'
#' p
#'
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   plotly::ggplotly(p, tooltip = "text")
#' }
add_tooltip <- function(data,
                        ...,
                        titles = snakecase::to_sentence_case,
                        name = "tooltip") {

  if (class(data)[1] == "sf") {
    temp_data <- data %>%
      dplyr::ungroup() %>%
      sf::st_drop_geometry()
  } else {
    temp_data <- data %>%
      dplyr::ungroup()
  }

  if (...length() != 0) {
    temp_data <- temp_data %>%
      dplyr::select(...)
  }

  if (!rlang::is_null(titles)) {
    temp_data <- temp_data %>%
      dplyr::rename_with(titles)
  }

  temp_data <- temp_data %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ paste0(dplyr::cur_column(), ": ", .x),
      .names = ".unite_{.col}"
    )) %>%
    tidyr::unite(!!(name), tidyselect::starts_with(".unite_"), sep = "<br>") %>%
    dplyr::select(!!(name))

  data %>%
    dplyr::bind_cols(temp_data)
}
