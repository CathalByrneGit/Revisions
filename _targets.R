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
  packages = c("DBI", "doltliter", "jsonlite", "yaml")
)

tar_source("R")

list(
  tar_target(
    tables_file, "tables.yml",
    format = "file",
    description = "The list of CSO tables to track, edited by hand"
  ),
  tar_target(
    tables, read_tables(tables_file),
    description = "Tracked tables with their titles and why each is interesting"
  ),
  tar_target(
    table_id, tables$table_id,
    description = "PxStat matrix codes, one branch per table downstream"
  ),

  # Always re-download; targets compares the file's content, so an unchanged
  # table stops here and nothing downstream reruns.
  tar_target(
    raw, fetch_pxstat(table_id),
    pattern = map(table_id), format = "file", cue = tar_cue("always"),
    description = "Latest JSON-stat download per table, re-fetched on every run"
  ),
  tar_target(
    parsed, parse_jsonstat(raw),
    pattern = map(raw), iteration = "list",
    description = "Each table split into series, periods and observations"
  ),
  # Every branch writes to the same DoltLite file, so this runs sequentially
  # (the default; no parallel workers are configured).
  tar_target(
    snapshot, commit_release(parsed, dolt_path()),
    pattern = map(parsed),
    description = "One DoltLite commit and tag per new CSO release; repeats are skipped"
  ),

  tar_target(
    history, read_history(dolt_path(), snapshot),
    description = "Every change in the DoltLite history, matched to the release that made it"
  ),
  tar_target(
    published_db,
    export_sqlite(history, "site/revisions.db", tables),
    format = "file",
    description = "Plain SQLite database for Datasette Lite and the landing page"
  ),
  tar_target(
    datasette_metadata,
    write_datasette_metadata("site/metadata.json", tables),
    format = "file",
    description = "Datasette titles, column notes and canned queries"
  )
)
