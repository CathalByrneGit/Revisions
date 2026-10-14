# Build the plain SQLite database that Datasette (Lite) serves.
#
# DoltLite files use a prolly-tree storage engine that stock SQLite cannot read,
# so the published database is a derived, ordinary SQLite file.

#' Classify every change in the history and attach labels.
derive_revisions <- function(history) {
  rel <- history$releases
  first_release <- tapply(rel$release, rel$table_id, min)

  ch <- history$changes
  ch$is_baseline <- ch$release == first_release[ch$table_id]
  ch$change_type <- ifelse(
    ch$diff_type == "modified", "revision",
    ifelse(ch$diff_type == "removed", "removed",
           ifelse(ch$is_baseline, "baseline", "first_print"))
  )
  ch$value <- ifelse(ch$diff_type == "removed", ch$old_value, ch$new_value)
  # A value that only went from missing to missing isn't a revision.
  same <- ch$change_type == "revision" &
    ((is.na(ch$old_value) & is.na(ch$new_value)) |
       (!is.na(ch$old_value) & !is.na(ch$new_value) & ch$old_value == ch$new_value))
  ch <- ch[!same, ]

  ch <- ch[order(ch$table_id, ch$series_key, ch$period, ch$release), ]
  vintages <- ch[, c("table_id", "series_key", "period", "release", "change_type",
                     "value", "old_value")]

  lab <- history$series[, c("table_id", "series_key", "label", "statistic_label", "dims", "unit")]
  per <- history$periods[, c("table_id", "period", "period_label", "period_start")]

  revisions <- ch[ch$change_type == "revision", c("table_id", "series_key", "period",
                                                    "release", "old_value", "new_value")]
  revisions$change <- revisions$new_value - revisions$old_value
  revisions$pct_change <- ifelse(
    is.na(revisions$old_value) | revisions$old_value == 0, NA_real_,
    round(100 * revisions$change / abs(revisions$old_value), 3)
  )
  revisions$change <- round(revisions$change, 6)
  revisions <- merge(revisions, lab, by = c("table_id", "series_key"), all.x = TRUE)
  revisions <- merge(revisions, per, by = c("table_id", "period"), all.x = TRUE)
  revisions <- revisions[order(revisions$release, -abs(revisions$change)), ]

  # First published value versus latest, for periods first printed while we
  # were watching (values already in the baseline snapshot have no first print).
  fp <- ch[ch$change_type == "first_print", c("table_id", "series_key", "period", "release", "value")]
  names(fp)[4:5] <- c("first_release", "first_value")
  n_rev <- if (nrow(revisions)) {
    aggregate(
      list(n_revisions = rep(1L, nrow(revisions))),
      revisions[, c("table_id", "series_key", "period")], sum
    )
  } else {
    data.frame(table_id = character(), series_key = character(),
               period = character(), n_revisions = integer())
  }
  latest <- history$observations
  names(latest)[names(latest) == "value"] <- "latest_value"
  fvl <- merge(fp, latest, by = c("table_id", "series_key", "period"), all.x = TRUE)
  fvl <- merge(fvl, n_rev, by = c("table_id", "series_key", "period"), all.x = TRUE)
  fvl$n_revisions[is.na(fvl$n_revisions)] <- 0L
  fvl$total_revision <- round(fvl$latest_value - fvl$first_value, 6)
  fvl <- merge(fvl, lab, by = c("table_id", "series_key"), all.x = TRUE)
  fvl <- merge(fvl, per, by = c("table_id", "period"), all.x = TRUE)

  counts <- function(type) {
    x <- ch[ch$change_type == type, ]
    if (!nrow(x)) return(setNames(integer(0), character(0)))
    tapply(rep(1L, nrow(x)), paste(x$table_id, x$release), sum)
  }
  key <- paste(rel$table_id, rel$release)
  pick <- function(v) { out <- v[key]; out[is.na(out)] <- 0L; as.integer(out) }
  releases <- data.frame(
    table_id = rel$table_id, release = rel$release, title = rel$title,
    tag = rel$tag, commit_hash = rel$commit_hash, fetched_at = rel$fetched_at,
    is_baseline = as.integer(rel$release == first_release[rel$table_id]),
    n_new_periods = pick(counts("first_print")),
    n_revised = pick(counts("revision")),
    n_removed = pick(counts("removed")),
    n_obs = rel$n_obs
  )

  list(releases = releases, vintages = vintages, revisions = revisions,
       first_vs_latest = fvl)
}

export_sqlite <- function(history, path = "site/revisions.db", tables_cfg = NULL) {
  d <- derive_revisions(history)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ddl <- c(
    "CREATE TABLE cso_tables (table_id TEXT PRIMARY KEY, title TEXT, why TEXT,
       source_url TEXT, latest_release TEXT, n_releases INTEGER,
       n_series INTEGER, n_revisions INTEGER)",
    "CREATE TABLE releases (table_id TEXT REFERENCES cso_tables(table_id),
       release TEXT, title TEXT, tag TEXT, commit_hash TEXT, fetched_at TEXT,
       is_baseline INTEGER, n_new_periods INTEGER, n_revised INTEGER,
       n_removed INTEGER, n_obs INTEGER, PRIMARY KEY (table_id, release))",
    "CREATE TABLE series (table_id TEXT REFERENCES cso_tables(table_id),
       series_key TEXT, statistic TEXT, statistic_label TEXT, unit TEXT,
       dims TEXT, label TEXT, PRIMARY KEY (table_id, series_key))",
    "CREATE TABLE periods (table_id TEXT REFERENCES cso_tables(table_id),
       period TEXT, period_label TEXT, period_start TEXT,
       PRIMARY KEY (table_id, period))",
    "CREATE TABLE observations (table_id TEXT REFERENCES cso_tables(table_id),
       series_key TEXT, period TEXT, value REAL,
       PRIMARY KEY (table_id, series_key, period))",
    "CREATE TABLE vintages (table_id TEXT REFERENCES cso_tables(table_id),
       series_key TEXT, period TEXT, release TEXT, change_type TEXT,
       value REAL, old_value REAL,
       PRIMARY KEY (table_id, series_key, period, release))",
    "CREATE TABLE revisions (table_id TEXT REFERENCES cso_tables(table_id),
       series_key TEXT, period TEXT, release TEXT, old_value REAL,
       new_value REAL, change REAL, pct_change REAL, label TEXT,
       statistic_label TEXT, dims TEXT, unit TEXT, period_label TEXT,
       period_start TEXT, PRIMARY KEY (table_id, series_key, period, release))",
    "CREATE TABLE first_vs_latest (table_id TEXT REFERENCES cso_tables(table_id),
       series_key TEXT, period TEXT, first_release TEXT, first_value REAL,
       latest_value REAL, n_revisions INTEGER, total_revision REAL, label TEXT,
       statistic_label TEXT, dims TEXT, unit TEXT, period_label TEXT,
       period_start TEXT, PRIMARY KEY (table_id, series_key, period))"
  )
  for (sql in ddl) DBI::dbExecute(con, sql)

  cfg <- tables_cfg %||% data.frame(table_id = character(), title = character(), why = character())
  tids <- unique(d$releases$table_id)
  tbl <- data.frame(
    table_id = tids,
    title = vapply(tids, function(t) {
      x <- cfg$title[cfg$table_id == t]
      if (length(x) && !is.na(x[1])) x[1] else d$releases$title[d$releases$table_id == t][1]
    }, ""),
    why = vapply(tids, function(t) { x <- cfg$why[cfg$table_id == t]; if (length(x)) x[1] else NA_character_ }, ""),
    source_url = sprintf("https://data.cso.ie/table/%s", tids),
    latest_release = vapply(tids, function(t) max(d$releases$release[d$releases$table_id == t]), ""),
    n_releases = vapply(tids, function(t) sum(d$releases$table_id == t), 0L),
    n_series = vapply(tids, function(t) sum(history$series$table_id == t), 0L),
    n_revisions = vapply(tids, function(t) sum(d$revisions$table_id == t), 0L),
    row.names = NULL
  )

  DBI::dbAppendTable(con, "cso_tables", tbl)
  DBI::dbAppendTable(con, "releases", d$releases)
  DBI::dbAppendTable(con, "series", history$series)
  DBI::dbAppendTable(con, "periods", history$periods)
  DBI::dbAppendTable(con, "observations", history$observations)
  DBI::dbAppendTable(con, "vintages", d$vintages)
  cols <- function(table) DBI::dbListFields(con, table)
  DBI::dbAppendTable(con, "revisions", d$revisions[, cols("revisions")])
  DBI::dbAppendTable(con, "first_vs_latest", d$first_vs_latest[, cols("first_vs_latest")])

  DBI::dbExecute(con, "CREATE INDEX idx_revisions_release ON revisions (release)")
  DBI::dbExecute(con, "CREATE INDEX idx_vintages_series ON vintages (table_id, series_key, period)")
  DBI::dbExecute(con, "
    CREATE VIEW series_revision_summary AS
    SELECT r.table_id, r.series_key, s.label, s.unit,
           COUNT(*) AS n_revisions,
           COUNT(DISTINCT r.period) AS n_periods_revised,
           ROUND(AVG(ABS(r.change)), 4) AS mean_abs_change,
           ROUND(MAX(ABS(r.change)), 4) AS max_abs_change
    FROM revisions r JOIN series s USING (table_id, series_key)
    GROUP BY r.table_id, r.series_key
    ORDER BY n_revisions DESC")
  DBI::dbExecute(con, "
    CREATE VIEW observations_labelled AS
    SELECT o.table_id, s.label, o.period, p.period_start, o.value, s.unit, o.series_key
    FROM observations o
    JOIN series s USING (table_id, series_key)
    JOIN periods p USING (table_id, period)")
  DBI::dbExecute(con, "VACUUM")
  DBI::dbDisconnect(con)
  on.exit()

  file.copy(tmp, path, overwrite = TRUE)
  write_summary(d, tbl, file.path(dirname(path), "summary.json"), history$head)
  path
}

#' A small JSON summary the static landing page renders without loading the db.
write_summary <- function(d, tbl, path, head) {
  latest_revs <- lapply(tbl$table_id, function(t) {
    r <- d$revisions[d$revisions$table_id == t, ]
    if (!nrow(r)) return(list())
    last <- max(r$release)
    r <- r[r$release == last, ]
    r <- r[order(-abs(r$change)), ]
    r <- head(r, 10)
    lapply(seq_len(nrow(r)), function(i) as.list(r[i, c(
      "label", "period_label", "release", "old_value", "new_value", "change",
      "pct_change", "unit")]))
  })
  names(latest_revs) <- tbl$table_id
  out <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    dolt_head = head,
    tables = lapply(seq_len(nrow(tbl)), function(i) as.list(tbl[i, ])),
    releases = lapply(seq_len(nrow(d$releases)), function(i) as.list(d$releases[i, c(
      "table_id", "release", "is_baseline", "n_new_periods", "n_revised", "n_removed")])),
    latest_revisions = latest_revs
  )
  jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA)
  path
}
