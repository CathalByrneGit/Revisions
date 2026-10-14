# The DoltLite history store: one commit (and one tag) per CSO release.

dolt_path <- function() Sys.getenv("REVISIONS_DOLT", "data/cso.doltlite")

dolt_open <- function(db) {
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(doltliter::Doltlite(), db)
  doltliter::dolt_config(
    con,
    user.name = Sys.getenv("REVISIONS_AUTHOR", "revisions-bot"),
    user.email = Sys.getenv("REVISIONS_EMAIL", "revisions-bot@users.noreply.github.com")
  )
  dolt_ensure_schema(con)
  con
}

dolt_ensure_schema <- function(con) {
  ddl <- c(
    "CREATE TABLE IF NOT EXISTS observations (
       table_id TEXT NOT NULL, series_key TEXT NOT NULL, period TEXT NOT NULL,
       value REAL,
       PRIMARY KEY (table_id, series_key, period))",
    "CREATE TABLE IF NOT EXISTS series (
       table_id TEXT NOT NULL, series_key TEXT NOT NULL,
       statistic TEXT, statistic_label TEXT, unit TEXT, dims TEXT, label TEXT,
       PRIMARY KEY (table_id, series_key))",
    "CREATE TABLE IF NOT EXISTS periods (
       table_id TEXT NOT NULL, period TEXT NOT NULL,
       period_label TEXT, period_start TEXT,
       PRIMARY KEY (table_id, period))",
    "CREATE TABLE IF NOT EXISTS releases (
       table_id TEXT NOT NULL, release TEXT NOT NULL,
       title TEXT, fetched_at TEXT, tag TEXT,
       n_series INTEGER, n_periods INTEGER, n_obs INTEGER,
       PRIMARY KEY (table_id, release))"
  )
  for (sql in ddl) DBI::dbExecute(con, sql)
  invisible(con)
}

release_tag <- function(table_id, release) {
  paste0(table_id, "/", gsub("[^0-9A-Za-z_.-]", "", release))
}

#' Record one parsed table as a release.
#'
#' Replaces that table's rows and commits. Because DoltLite diffs by primary
#' key, deleting and re-inserting unchanged rows produces no change, so only
#' real revisions and new periods show up in the history.
#'
#' Idempotent: a release CSO has already published (same `updated` stamp) is
#' skipped, which makes re-running the pipeline safe.
commit_release <- function(parsed, db = dolt_path()) {
  con <- dolt_open(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  meta <- parsed$meta
  tid <- meta$table_id
  release <- meta$updated
  if (is.na(release) || !nzchar(release)) {
    # No timestamp from CSO: fall back to a hash of the content.
    release <- paste0("content-", substr(digest::digest(parsed$obs), 1, 12))
  }
  tag <- release_tag(tid, release)

  seen <- DBI::dbGetQuery(
    con, "SELECT 1 FROM releases WHERE table_id = ? AND release = ?",
    params = list(tid, release)
  )
  if (nrow(seen)) {
    return(data.frame(table_id = tid, release = release, tag = tag,
                      status = "already recorded",
                      head = doltliter::dolt_hashof(con)))
  }

  DBI::dbWithTransaction(con, {
    for (t in c("observations", "series", "periods")) {
      DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE table_id = ?", t), params = list(tid))
    }
    DBI::dbAppendTable(con, "observations", parsed$obs)
    DBI::dbAppendTable(con, "series", parsed$series)
    DBI::dbAppendTable(con, "periods", parsed$periods)
    DBI::dbAppendTable(con, "releases", data.frame(
      table_id = tid, release = release, title = meta$title,
      fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      tag = tag, n_series = meta$n_series, n_periods = meta$n_periods,
      n_obs = meta$n_obs
    ))
  })

  doltliter::dolt_commit(con, sprintf("%s: CSO release %s", tid, release))
  doltliter::dolt_tag(con, tag)
  data.frame(table_id = tid, release = release, tag = tag, status = "committed",
             head = doltliter::dolt_hashof(con))
}

#' Read everything needed for the published database out of DoltLite.
#'
#' The core is `dolt_diff_observations` over the whole history: every row that
#' was added, changed or removed, and the commit that did it. Each commit is one
#' release of one table, so joining commits to release tags tells us which CSO
#' release revised which value.
read_history <- function(db = dolt_path(), ...) {
  con <- dolt_open(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  releases <- DBI::dbReadTable(con, "releases")
  tags <- doltliter::dolt_tags(con)[, c("tag_name", "tag_hash")]
  releases <- merge(releases, tags, by.x = "tag", by.y = "tag_name", all.x = TRUE)
  names(releases)[names(releases) == "tag_hash"] <- "commit_hash"
  releases <- releases[order(releases$table_id, releases$release), ]

  diff <- doltliter::dolt_table_diff(con, "observations")
  changes <- data.frame(
    table_id = ifelse(is.na(diff$to_table_id), diff$from_table_id, diff$to_table_id),
    series_key = ifelse(is.na(diff$to_series_key), diff$from_series_key, diff$to_series_key),
    period = ifelse(is.na(diff$to_period), diff$from_period, diff$to_period),
    old_value = diff$from_value,
    new_value = diff$to_value,
    diff_type = diff$diff_type,
    commit_hash = diff$to_commit,
    stringsAsFactors = FALSE
  )
  changes <- merge(changes, releases[, c("table_id", "commit_hash", "release")],
                   by = c("table_id", "commit_hash"), all.x = TRUE)

  list(
    releases = releases,
    changes = changes,
    observations = DBI::dbReadTable(con, "observations"),
    series = DBI::dbReadTable(con, "series"),
    periods = DBI::dbReadTable(con, "periods"),
    head = doltliter::dolt_hashof(con)
  )
}
