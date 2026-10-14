test_that("JSON-stat is parsed into series, periods and observations", {
  p <- parse_jsonstat(fixture("release1"))
  expect_equal(p$meta$table_id, "MUM01")
  expect_equal(p$meta$updated, "2026-07-02T11:00:00Z")
  expect_equal(nrow(p$series), 2 * 2 * 3)
  expect_equal(nrow(p$periods), 18)
  expect_equal(nrow(p$obs), 12 * 18)
  expect_false(any(duplicated(p$obs[c("series_key", "period")])))

  # Row-major order: check one value against the generator's formula.
  v <- p$obs$value[p$obs$series_key == "MUM01C01|315|-" & p$obs$period == "2025M01"]
  expect_equal(v, 120.6)
  s <- p$series[p$series$series_key == "MUM01C02|316|2", ]
  expect_equal(s$unit, "%")
  expect_match(s$label, "Rate \\(Age Group: 15 - 24 years; Sex: Female\\)")
})

test_that("sparse JSON-stat values and object indexes are handled", {
  js <- jsonlite::read_json(fixture("release1"), simplifyVector = FALSE)
  idx <- js$dimension$C02199V02655$category$index
  js$dimension$C02199V02655$category$index <- setNames(as.list(seq_along(idx) - 1L), unlist(idx))
  dense <- js$value
  js$value <- setNames(dense[c(1, 5)], c("0", "4"))
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(js, path, auto_unbox = TRUE, digits = NA)
  p <- parse_jsonstat(path, "MUM01")
  expect_equal(sum(!is.na(p$obs$value)), 2)
  expect_equal(p$obs$value[5], dense[[5]])
})

test_that("period codes become start dates", {
  expect_equal(
    as.character(period_start(c("2024M03", "2024Q3", "20243", "2024", "202411", "2024W01"))),
    c("2024-03-01", "2024-07-01", "2024-07-01", "2024-01-01", "2024-11-01", NA)
  )
})
