read_tables <- function(path = "tables.yml") {
  cfg <- yaml::read_yaml(path)$tables
  data.frame(
    table_id = vapply(cfg, function(x) x$id, ""),
    title = vapply(cfg, function(x) x$title %||% NA_character_, ""),
    why = vapply(cfg, function(x) trimws(x$why %||% NA_character_), ""),
    stringsAsFactors = FALSE
  )
}
