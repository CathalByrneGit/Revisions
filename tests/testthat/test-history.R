record <- function(db, ...) {
  for (r in c(...)) commit_release(parse_jsonstat(fixture(r)), db)
}

test_that("a release is committed and tagged once", {
  db <- tempfile(fileext = ".doltlite")
  a <- commit_release(parse_jsonstat(fixture("release1")), db)
  b <- commit_release(parse_jsonstat(fixture("release1")), db)
  expect_equal(a$status, "committed")
  expect_equal(b$status, "already recorded")
  expect_equal(a$head, b$head)
  expect_equal(a$tag, "MUM01/2026-07-02T110000Z")
})

test_that("a failed fetch is reported and skipped, not fatal", {
  withr::local_envvar(REVISIONS_SOURCE = file.path(root, "tests", "fixtures", "release1"))
  dir <- tempfile()
  path <- fetch_pxstat("NOPE01", dir)
  p <- parse_jsonstat(path)
  expect_true(fetch_failed(p))
  db <- tempfile(fileext = ".doltlite")
  s <- commit_release(p, db)
  expect_match(s$status, "^fetch failed")
  expect_false(file.exists(db))
})

test_that("revisions, first prints and baseline are classified", {
  db <- tempfile(fileext = ".doltlite")
  record(db, "release1", "release2", "release3")
  h <- read_history(db)
  expect_equal(nrow(h$releases), 3)
  expect_false(anyNA(h$releases$commit_hash))

  d <- derive_revisions(h)
  expect_equal(d$releases$n_revised, c(0L, 6L, 0L))
  expect_equal(d$releases$n_new_periods, c(0L, 12L, 0L))
  expect_equal(d$releases$is_baseline, c(1L, 0L, 0L))
  expect_equal(sum(d$vintages$change_type == "baseline"), 12 * 18)

  r <- d$revisions[d$revisions$series_key == "MUM01C01|315|-" & d$revisions$period == "2026M06", ]
  expect_equal(r$old_value, 130.8)
  expect_equal(r$new_value, 135.1)
  expect_equal(r$change, 4.3)
  expect_equal(r$release, "2026-08-05T11:00:00Z")
})

test_that("the exported SQLite opens with stock SQLite", {
  db <- tempfile(fileext = ".doltlite")
  record(db, "release1", "release2")
  out <- file.path(tempfile(), "revisions.db")
  export_sqlite(read_history(db), out, read_tables(file.path(root, "tables.yml")))
  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con))
  expect_setequal(
    DBI::dbListTables(con),
    c("cso_tables", "releases", "series", "periods", "observations", "vintages",
      "revisions", "first_vs_latest", "series_revision_summary", "observations_labelled")
  )
  expect_equal(DBI::dbGetQuery(con, "select count(*) n from revisions")$n, 6)
  expect_equal(DBI::dbGetQuery(con, "select title from cso_tables")$title, "Monthly unemployment")
  expect_true(file.exists(file.path(dirname(out), "summary.json")))
})
