govuk_colours <- c(
  "blue" = "#1d70b8",
  "turquoise" = "#28a197",
  "green" = "#00703c",
  "light-green" = "#85994b",
  "yellow" = "#ffdd00",
  "orange" = "#f47738",
  "red" = "#d4351c",
  "bright-purple" = "#912b88",
  "black" = "#0b0c0c"
)

default_colour <- govuk_colours[[1]]

govuk_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(govuk_colours)
  }

  govuk_colours[cols]
}

govuk_pal <- function() {
  function(n) {
    unname(govuk_cols(1:n))
  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{discrete_scale}}
#' @rdname scale_color_govuk
#' @export
#' @importFrom ggplot2 discrete_scale
scale_color_govuk <- function(...) {
  ggplot2::discrete_scale("colour", "govuk", govuk_pal(), ...)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{discrete_scale}}
#' @rdname scale_fill_govuk
#' @export
#' @importFrom ggplot2 discrete_scale
scale_fill_govuk <- function(...) {
  ggplot2::discrete_scale("fill", "govuk", govuk_pal(), ...)
}
