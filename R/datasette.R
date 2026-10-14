# Datasette metadata: titles, column descriptions and canned queries.

write_datasette_metadata <- function(path = "site/metadata.json", tables = NULL) {
  q <- function(title, sql, description = NULL) {
    out <- list(title = title, sql = trimws(sql))
    if (!is.null(description)) out$description <- description
    out
  }
  md <- list(
    title = "Revisions: how CSO statistics change after publication",
    description_html = paste(
      "<p>Every time the Central Statistics Office republishes a tracked table,",
      "a snapshot is committed to a <a href='https://github.com/dolthub/doltlite'>DoltLite</a>",
      "database. This site is derived from that history.</p>",
      "<p>Start with <code>revisions</code>, or one of the queries below.",
      "Independent project, not affiliated with the CSO.</p>"
    ),
    source = "Central Statistics Office, Ireland (PxStat)",
    source_url = "https://data.cso.ie",
    license = "CC BY 4.0 (source data)",
    license_url = "https://creativecommons.org/licenses/by/4.0/",
    databases = list(revisions = list(
      tables = list(
        cso_tables = list(description = "The CSO tables being tracked."),
        releases = list(description = paste(
          "One row per CSO release we observed. The first is the baseline:",
          "everything already published when tracking started.")),
        revisions = list(
          description = "Every published value that changed between two releases.",
          sort_desc = "release",
          columns = list(
            old_value = "Value before this release",
            new_value = "Value in this release",
            change = "new_value - old_value",
            pct_change = "Change as a percentage of the old value"
          )
        ),
        vintages = list(description = paste(
          "Every value each period has held: baseline, first print, revisions,",
          "removals. One row per change, not per release.")),
        first_vs_latest = list(description = paste(
          "For periods first published while tracking: the first printed",
          "value against the latest.")),
        observations = list(description = "Latest published values."),
        series = list(description = "One row per series (every combination of the non-time dimensions)."),
        periods = list(description = "Time periods with their start dates.")
      ),
      queries = list(
        latest_release_revisions = q(
          "Biggest revisions in each table's latest release",
          "
          select r.table_id, r.label, r.period_label, r.old_value, r.new_value,
                 r.change, r.pct_change, r.unit, r.release
          from revisions r
          join (select table_id, max(release) as release from releases group by table_id) l
            using (table_id, release)
          order by abs(r.change) desc
          limit 200"
        ),
        value_history = q(
          "Every value a period has held",
          "
          select v.release, v.change_type, v.old_value, v.value, s.label, p.period_label
          from vintages v
          join series s using (table_id, series_key)
          join periods p using (table_id, period)
          where v.table_id = :table_id and v.period = :period
            and s.label like '%' || :series_label || '%'
          order by s.label, v.release",
          "Example: table_id MUM01, period 2026M08, series_label Rate"
        ),
        most_revised_series = q(
          "Which series get revised most",
          "select * from series_revision_summary limit 100"
        ),
        first_print_vs_latest = q(
          "How far have first prints moved?",
          "
          select table_id, label, period_label, first_release, first_value,
                 latest_value, total_revision, n_revisions, unit
          from first_vs_latest
          where n_revisions > 0
          order by abs(total_revision) desc
          limit 200"
        ),
        release_log = q(
          "Release log",
          "
          select table_id, release, is_baseline, n_new_periods, n_revised,
                 n_removed, tag, commit_hash
          from releases order by release desc"
        )
      )
    ))
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(md, path, auto_unbox = TRUE, pretty = TRUE)
  path
}
