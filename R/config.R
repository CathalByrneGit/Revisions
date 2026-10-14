#' Read tables.yml.
#'
#' `publish` (optional) lists the statistic codes to put on the site. Every
#' statistic is still recorded in the DoltLite history; this only trims what
#' is published, e.g. dropping percentage-change series that are derived from
#' an index and would duplicate its revisions. Stored comma-separated, "" = all.
read_tables <- function(path = "tables.yml") {
  cfg <- yaml::read_yaml(path)$tables
  data.frame(
    table_id = vapply(cfg, function(x) x$id, ""),
    title = vapply(cfg, function(x) x$title %||% NA_character_, ""),
    why = vapply(cfg, function(x) trimws(x$why %||% NA_character_), ""),
    publish = vapply(cfg, function(x) paste(unlist(x$publish), collapse = ","), ""),
    stringsAsFactors = FALSE
  )
}

#' Keep only configured tables, and only the configured statistics of each.
filter_published <- function(history, tables_cfg) {
  if (is.null(tables_cfg)) return(history)
  keep_series <- history$series[history$series$table_id %in% tables_cfg$table_id, ]
  if (!is.null(tables_cfg$publish)) {
    allowed <- strsplit(tables_cfg$publish, ",", fixed = TRUE)
    names(allowed) <- tables_cfg$table_id
    ok <- mapply(function(tid, stat) {
      a <- allowed[[tid]]
      !length(a) || stat %in% a
    }, keep_series$table_id, keep_series$statistic)
    keep_series <- keep_series[ok, ]
  }
  key <- paste(keep_series$table_id, keep_series$series_key)
  by_series <- function(df) df[paste(df$table_id, df$series_key) %in% key, , drop = FALSE]
  by_table <- function(df) df[df$table_id %in% tables_cfg$table_id, , drop = FALSE]
  history$series <- keep_series
  history$observations <- by_series(history$observations)
  history$changes <- by_series(history$changes)
  history$releases <- by_table(history$releases)
  history$periods <- by_table(history$periods)
  history
}
