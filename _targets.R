# Revisions: track how CSO statistics change between releases.
#
#   tables.yml  ->  fetch (PxStat JSON-stat)  ->  parse
#               ->  commit each new release to data/cso.doltlite (DoltLite)
#               ->  read the full history and classify every change
#               ->  export site/revisions.db (plain SQLite) for Datasette Lite
#
# Run with targets::tar_make(). Set REVISIONS_SOURCE=<dir> to read JSON files
# from a directory instead of the CSO API (used by the tests).

library(targets)

tar_option_set(
  packages = c("DBI", "doltliter", "jsonlite", "yaml"),
  # Every branch writes to the same DoltLite file, so keep it sequential.
  controller = NULL
)

tar_source("R")

list(
  tar_target(tables_file, "tables.yml", format = "file"),
  tar_target(tables, read_tables(tables_file)),
  tar_target(table_id, tables$table_id),

  # Always re-download; targets compares the file's content, so an unchanged
  # table stops here and nothing downstream reruns.
  tar_target(
    raw, fetch_pxstat(table_id),
    pattern = map(table_id), format = "file", cue = tar_cue("always")
  ),
  tar_target(parsed, parse_jsonstat(raw), pattern = map(raw), iteration = "list"),
  tar_target(snapshot, commit_release(parsed, dolt_path()), pattern = map(parsed)),

  tar_target(history, read_history(dolt_path(), snapshot)),
  tar_target(
    published_db,
    export_sqlite(history, "site/revisions.db", tables),
    format = "file"
  ),
  tar_target(
    datasette_metadata,
    write_datasette_metadata("site/metadata.json", tables),
    format = "file"
  )
)
