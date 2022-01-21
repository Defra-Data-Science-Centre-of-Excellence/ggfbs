add_error_bars <- function(data, values, ci) {
  ggplot2::geom_errorbar(
    ggplot2::aes(ymax = mean + confidence_interval, ymin = mean - confidence_interval),
    width = 0.2,
    position = ggplot2::position_dodge(0.75)
  )
}

set_label_position <- function(max_value, position_scaler = 0.025) {
  pos <- position_scaler * 10 ^ floor(log(max_value, 10))
  pos
}

format_label_continuous <- function(label_format, ...) {
  switch(label_format,
    "identity" = scales::label_number(...),
    "comma" = scales::label_comma(...),
    "percent" = scales::label_percent(..., accuracy = 1),
    "abbreviated" = scales::label_number_si(...),
    "scientific" = scales::label_scientific(...),
    stop(paste0("Unknown label format: ", label_format))
  )
}


check_aesthetic <- function(aesthetic) {
  aesthetic_names <- names(aesthetic)
  list(
    x = "x" %in% aesthetic_names,
    y = "y" %in% aesthetic_names,
    fill = "fill" %in% aesthetic_names,
    colour = any(c("colour", "color") %in% aesthetic_names),
    ymax  = "ymax" %in% aesthetic_names,
    ymin = "ymin" %in% aesthetic_names,
    xmax = "xmax" %in% aesthetic_names,
    xmin = "xmin" %in% aesthetic_names,
    label = "label" %in% aesthetic_names
  )
}

# Chart Templates ----

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param aesthetic PARAM_DESCRIPTION
#' @param error PARAM_DESCRIPTION, Default: NULL
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param value_name PARAM_DESCRIPTION, Default: NULL
#' @param horizontal PARAM_DESCRIPTION, Default: FALSE
#' @param label_bars PARAM_DESCRIPTION, Default: FALSE
#' @param label_position PARAM_DESCRIPTION, Default: 'top'
#' @param continuous_format PARAM_DESCRIPTION, Default: 'comma'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#' \code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{aes}}
#' @rdname fbs_barplot
#' @export
#' @importFrom ggplot2 ggplot geom_bar position_dodge geom_hline geom_text aes geom_errorbar labs coord_flip scale_y_continuous expansion ylab scale_x_discrete
#' @importFrom rlang as_name
#' @importFrom scales pretty_breaks label_wrap
fbs_barplot <- function(
  data,
  aesthetic,
  error = NULL,
  title = NULL,
  value_name = NULL,
  horizontal = FALSE,
  label_bars = FALSE,
  label_position = "top",
  continuous_format = "comma"
) {

  plot_type <- "bar"
  aes_spec <- check_aesthetic(aesthetic)

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    geom_fbsbar() +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  if (label_bars) {
    max_value <- max(to_plot[[values]])

    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        label = .data[[values]],
        x = .data[[categories]],
        y = .data[[values]] + set_label_position(max_value)
      ),
      position = ggplot2::position_dodge(0.9),
      vjust = 0
    )
  }

  # Add error bars if needed
  if (!is.null(error)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymax = .data[[rlang::as_name(aesthetic[["y"]])]] + .data[[error]],
          ymin = .data[[rlang::as_name(aesthetic[["y"]])]] - .data[[error]]
        ),
        width = 0.2,
        position = ggplot2::position_dodge(0.75),
        size = 0.1,
        colour = "black"
      )
  }

  # Add labels
  p <- p +
    ggplot2::labs(title = title)

  # Add scales
  if (horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format),
        expand = ggplot2::expansion(mult = c(0, 0.2))
      ) +
      ggplot2::ylab(value_name)
  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(10),
        labels =  format_label_continuous(continuous_format)
      ) +
      ggplot2::labs(subtitle = value_name)
  }

  # Add scale if fill passed into aesthetic
  if (aes_spec$fill) {
    p <- p +
      scale_fill_govuk()
  }

  # Add default styling
  p <- p +
    theme_fbs()

  # Adjust for horizontal
  if (horizontal) {
    p <- p +
      theme_fbs_h()
  }

  p
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param aesthetic PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param value_name PARAM_DESCRIPTION, Default: NULL
#' @param horizontal PARAM_DESCRIPTION, Default: FALSE
#' @param continuous_format PARAM_DESCRIPTION, Default: 'comma'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#' \code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{aes}}
#' @rdname fbs_stackplot
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_hline labs scale_x_discrete scale_y_continuous
#' @importFrom scales label_wrap pretty_breaks
fbs_stackplot <- function(
  data,
  aesthetic,
  title = NULL,
  value_name = NULL,
  horizontal = FALSE,
  continuous_format = "comma"
) {

  plot_type <- "stackbar"

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = 0.75) +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title)

  # Add scales
  if (horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ylab(value_name)

  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format)
      ) +
      ggplot2::labs(subtitle = value_name)

  }

  p <- p +
    scale_fill_govuk()

  # Add default styling
  p <- p +
    theme_fbs()

  # Adjust for horizontal
  if (horizontal) {
    p <- p +
      theme_fbs_h()
  }

  p
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param aesthetic PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param value_name PARAM_DESCRIPTION, Default: NULL
#' @param horizontal PARAM_DESCRIPTION, Default: TRUE
#' @param continuous_format PARAM_DESCRIPTION, Default: 'percent'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#' \code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{aes}}
#' @rdname fbs_distribution_plot
#' @export
fbs_distribution_plot <- function(
  data,
  aesthetic,
  title = NULL,
  value_name = NULL,
  horizontal = FALSE,
  continuous_format = "percent"
) {

  plot_type <- "distbar"

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_fill(), width = 0.75) +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title) +
    ggplot2::ylab(label = value_name)

  # Add scales
  if (horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      )
  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(6),
        labels = format_label_continuous(continuous_format)
      )
  }

  p <- p +
    scale_fill_govuk()

  # Add default styling
  p <- p +
    theme_fbs()

  # Adjust for horizontal
  if (horizontal) {
    p <- p +
      theme_fbs_h()
  }

  p
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param aesthetic PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param value_name PARAM_DESCRIPTION, Default: NULL
#' @param include_legend PARAM_DESCRIPTION, Default: TRUE
#' @param continuous_format PARAM_DESCRIPTION, Default: 'comma'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#' \code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{aes}}
#' @rdname fbs_lineplot
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_hline labs scale_x_discrete scale_y_continuous
#' @importFrom scales label_wrap pretty_breaks
fbs_lineplot <- function(
  data,
  aesthetic,
  title = NULL,
  value_name = NULL,
  include_legend = TRUE,
  continuous_format = "comma"
) {

  plot_type <- "line"

  to_plot <- data

  # Primary geom
  p <- to_plot %>%
    ggplot2::ggplot(aesthetic) +
    # Add bar plot
    ggplot2::geom_line(size = 1.5, stat = "identity") +
    # Add line at 0
    ggplot2::geom_hline(yintercept = 0)

  # Add labels
  p <- p +
    ggplot2::labs(title = title, subtitle = value_name)

  # Add scales
  p <- p +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(10)) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(6),
      labels = format_label_continuous(continuous_format)
    )


  p <- p +
    scale_color_govuk()

  # Add default styling
  p <- p +
    theme_fbs() +
    theme_fbs_line(include_legend = include_legend)

  p
}
