# Generates small, SYNTHETIC JSON-stat 2.0 files shaped like the CSO's MUM01
# table, in three "releases":
#
#   release1  baseline: Jan 2025 - Jun 2026
#   release2  adds Jul 2026 and revises the last three months of the SA series
#   release3  same numbers as release2, newer timestamp (a no-change release)
#
# The values are made up. Run from the repo root:
#   Rscript tests/fixtures/make_fixtures.R

make_release <- function(months, updated, revise = list()) {
  stats <- c(MUM01C01 = "Seasonally Adjusted Monthly Unemployment",
             MUM01C02 = "Seasonally Adjusted Monthly Unemployment Rate")
  units <- list(MUM01C01 = list(label = "Thousand", decimals = 1),
                MUM01C02 = list(label = "%", decimals = 1))
  ages <- c("315" = "15 - 74 years", "316" = "15 - 24 years")
  sexes <- c("-" = "Both sexes", "1" = "Male", "2" = "Female")

  base <- function(stat, age, sex, i) {
    level <- if (stat == "MUM01C01") 120 + 0.6 * i else 4.2 + 0.03 * i
    if (age == "316") level <- level * (if (stat == "MUM01C01") 0.3 else 2.6)
    if (sex == "1") level <- level * (if (stat == "MUM01C01") 0.54 else 1.04)
    if (sex == "2") level <- level * (if (stat == "MUM01C01") 0.46 else 0.96)
    round(level, 1)
  }
  vals <- c()
  for (s in names(stats)) for (m in seq_along(months)) for (a in names(ages)) for (x in names(sexes)) {
    v <- base(s, a, x, m)
    key <- paste(s, months[m], a, x)
    if (!is.null(revise[[key]])) v <- revise[[key]]
    vals <- c(vals, v)
  }
  list(
    version = "2.0",
    class = "dataset",
    label = "Monthly Unemployment (SYNTHETIC TEST DATA)",
    source = "Test fixture shaped like CSO PxStat",
    updated = updated,
    id = list("STATISTIC", "TLIST(M1)", "C02076V02508", "C02199V02655"),
    size = list(length(stats), length(months), length(ages), length(sexes)),
    role = list(time = list("TLIST(M1)"), metric = list("STATISTIC")),
    dimension = list(
      STATISTIC = list(label = "Statistic", category = list(
        index = as.list(names(stats)), label = as.list(stats), unit = units)),
      `TLIST(M1)` = list(label = "Month", category = list(
        index = as.list(months),
        label = setNames(as.list(format(as.Date(paste0(sub("M", "-", months), "-01")), "%Y %B")), months))),
      C02076V02508 = list(label = "Age Group", category = list(
        index = as.list(names(ages)), label = as.list(ages))),
      C02199V02655 = list(label = "Sex", category = list(
        index = as.list(names(sexes)), label = as.list(sexes)))
    ),
    value = as.list(vals)
  )
}

months_to <- function(end) {
  d <- seq(as.Date("2025-01-01"), as.Date(end), by = "month")
  format(d, "%YM%m")
}

write_release <- function(x, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, file.path(dir, "MUM01.json"), auto_unbox = TRUE,
                       digits = NA, pretty = FALSE)
}

out <- "tests/fixtures"
r1 <- make_release(months_to("2026-06-01"), "2026-07-02T11:00:00Z")
write_release(r1, file.path(out, "release1"))

revise <- list(
  "MUM01C01 2026M04 315 -" = 132.0,
  "MUM01C01 2026M05 315 -" = 133.4,
  "MUM01C01 2026M06 315 -" = 135.1,
  "MUM01C02 2026M05 315 -" = 4.8,
  "MUM01C02 2026M06 315 -" = 4.9,
  "MUM01C02 2026M06 316 -" = 12.1
)
r2 <- make_release(months_to("2026-07-01"), "2026-08-05T11:00:00Z", revise)
write_release(r2, file.path(out, "release2"))

r3 <- r2
r3$updated <- "2026-08-12T11:00:00Z"
write_release(r3, file.path(out, "release3"))
