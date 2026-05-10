# eolas_dataset S3 class — a data frame with name/source metadata attached

new_eolas_dataset <- function(df, name, source = NULL) {
  structure(df,
    eolas_name = name,
    eolas_source = source,
    class     = c("eolas_dataset", class(df))
  )
}

#' @export
print.eolas_dataset <- function(x, ...) {
  name   <- attr(x, "eolas_name")
  source <- attr(x, "eolas_source")
  header <- paste0("# eolas_dataset: ", name)
  if (!is.null(source) && nchar(source) > 0)
    header <- paste0(header, " [", source, "]")
  cat(header, "\n")
  cat(sprintf("# %d rows\n", nrow(x)))
  class(x) <- setdiff(class(x), "eolas_dataset")
  print(x, ...)
  invisible(x)
}


#' Quick line plot for a eolas_dataset
#'
#' A thin ggplot2 wrapper that returns a `ggplot` object — add further layers
#' with `+` as usual.
#'
#' @param x An `eolas_dataset` returned by any `eolas_get_*()` function.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df <- eolas_get_statsnz("nz_cpi", start = "2015-01-01")
#' eolas_plot(df)
#' }
eolas_plot <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required. Install with install.packages('ggplot2')", call. = FALSE)

  name   <- attr(x, "eolas_name")   %||% "Dataset"
  source <- attr(x, "eolas_source") %||% ""

  date_col  <- if ("date"  %in% names(x)) "date"  else names(x)[1]
  value_col <- if ("value" %in% names(x)) "value" else names(x)[2]

  caption <- if (nchar(source) > 0)
    paste0("Source: ", source, " · eolas.fyi")
  else
    "eolas.fyi"

  ggplot2::ggplot(x, ggplot2::aes(x = .data[[date_col]], y = .data[[value_col]])) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 0.8) +
    ggplot2::labs(
      title   = name,
      x       = NULL,
      y       = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = "#9ca3af", size = 9)
    )
}
