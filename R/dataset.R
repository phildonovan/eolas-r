# eolas_dataset S3 class -- a data frame with name/source metadata attached

new_eolas_dataset <- function(df, name, source = NULL, meta_info = NULL) {
  if (!inherits(df, "tbl_df")) {
    df <- tibble::as_tibble(df)
  }
  structure(
    .eolas_attach_dataset_meta(df, name = name, source = source, meta_info = meta_info),
    class = c("eolas_dataset", class(df))
  )
}

#' @export
print.eolas_dataset <- function(x, ...) {
  name   <- attr(x, "eolas_name")
  source <- attr(x, "eolas_source")
  label  <- paste0("eolas_dataset: ", name)
  if (!is.null(source) && nchar(source) > 0) {
    label <- paste0(label, " [", source, "]")
  }
  cli::cli_h1(label)
  .eolas_print_meta_subtitle(x)
  cli::cli_text("{.emph {nrow(x)} row{?s}}")
  class(x) <- setdiff(class(x), "eolas_dataset")
  print(x, ...)
  invisible(x)
}


# `eolas_plot()` was removed in v1.3.0. It silently mis-rendered datasets
# with multiple series per date -- it auto-picked `value` as the y column
# and drew a single line, which produced zigzag traces wherever the
# response had a dimension column (e.g. measure, frequency, age band).
# Rather than ship a helper that needs to know each dataset's shape, we
# leave plotting to the caller -- ggplot2 / plotly know better than we do
# once you have a tidy data frame. See README for one-liners.
