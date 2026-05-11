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


# `eolas_plot()` was removed in v1.3.0. It silently mis-rendered datasets
# with multiple series per date — it auto-picked `value` as the y column
# and drew a single line, which produced zigzag traces wherever the
# response had a dimension column (e.g. measure, frequency, age band).
# Rather than ship a helper that needs to know each dataset's shape, we
# leave plotting to the caller — ggplot2 / plotly know better than we do
# once you have a tidy data frame. See README for one-liners.
