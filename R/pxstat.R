# Fetching and parsing CSO PxStat tables (JSON-stat 2.0).

pxstat_url <- function(table_id) {
  sprintf(
    "https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/%s/JSON-stat/2.0/en",
    table_id
  )
}

#' Download one PxStat table to `raw_dir/<table_id>.json`.
#'
#' If the environment variable REVISIONS_SOURCE names a directory, the file is
#' copied from `<REVISIONS_SOURCE>/<table_id>.json` instead. That is how the
#' tests and local runs work without network access.
#'
#' Returns the path, so targets can track the file by its content: a fetch that
#' returns the same bytes as last time leaves everything downstream untouched.
fetch_pxstat <- function(table_id, raw_dir = "data/raw") {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(raw_dir, paste0(table_id, ".json"))

  source_dir <- Sys.getenv("REVISIONS_SOURCE")
  if (nzchar(source_dir)) {
    src <- file.path(source_dir, paste0(table_id, ".json"))
    if (file.exists(src)) {
      file.copy(src, dest, overwrite = TRUE)
    } else {
      jsonlite::write_json(
        list(class = "error", table_id = table_id,
             error = paste("no fixture in", source_dir)),
        dest, auto_unbox = TRUE
      )
    }
    return(dest)
  }

  tmp <- tempfile(fileext = ".json")
  ok <- tryCatch({
    req <- httr2::request(pxstat_url(table_id)) |>
      httr2::req_user_agent("Revisions (https://github.com/CathalByrneGit/Revisions)") |>
      httr2::req_retry(max_tries = 4, backoff = function(i) 5 * i) |>
      httr2::req_timeout(300)
    httr2::req_perform(req, path = tmp)
    js <- jsonlite::read_json(tmp, simplifyVector = FALSE)
    if (!identical(js$class, "dataset")) stop("response is not a JSON-stat dataset")
    TRUE
  }, error = function(e) {
    # One unreachable or mistyped table must not stop the others: write a
    # marker the later steps recognise and skip.
    warning(table_id, ": fetch failed: ", conditionMessage(e), call. = FALSE)
    jsonlite::write_json(
      list(class = "error", table_id = table_id, error = conditionMessage(e)),
      tmp, auto_unbox = TRUE
    )
    FALSE
  })
  file.copy(tmp, dest, overwrite = TRUE)
  dest
}

fetch_failed <- function(parsed) !is.null(parsed$error)

# JSON-stat allows a category index to be an array of codes or an object of
# code -> position. Return codes in position order.
jsonstat_codes <- function(index) {
  if (is.null(names(index))) return(unlist(index, use.names = FALSE))
  pos <- unlist(index)
  names(pos)[order(pos)]
}

#' Parse a JSON-stat 2.0 dataset into three tidy pieces.
#'
#' * `meta`: one row describing this version of the table (its `updated` stamp
#'   is what identifies a CSO release).
#' * `series`: one row per series, i.e. per combination of every non-time
#'   dimension, with readable labels.
#' * `obs`: one row per series and period, with the value.
#'
#' `series_key` joins the non-time dimension codes with "|" so that it stays
#' stable across releases even if CSO tweaks a label.
parse_jsonstat <- function(path, table_id = sub("\\.json$", "", basename(path))) {
  js <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (identical(js$class, "error")) {
    return(list(table_id = table_id, error = js$error))
  }
  ids <- unlist(js$id)
  sizes <- unlist(js$size)
  time_id <- unlist(js$role$time)
  if (length(time_id) != 1) stop(table_id, ": expected exactly one time dimension")
  metric_id <- unlist(js$role$metric)
  if (is.null(metric_id)) metric_id <- ids[1]

  codes <- lapply(ids, function(d) jsonstat_codes(js$dimension[[d]]$category$index))
  names(codes) <- ids
  stopifnot(identical(unname(lengths(codes)), as.integer(sizes)))

  # Values are stored row-major: the last dimension varies fastest.
  n <- prod(sizes)
  values <- rep(NA_real_, n)
  if (is.null(names(js$value))) {
    v <- js$value
    values[seq_along(v)] <- vapply(v, function(x) if (is.null(x)) NA_real_ else as.numeric(x), 0)
  } else {
    pos <- as.integer(names(js$value)) + 1L
    values[pos] <- vapply(js$value, function(x) if (is.null(x)) NA_real_ else as.numeric(x), 0)
  }
  grid <- expand.grid(rev(codes), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- grid[, rev(seq_along(ids)), drop = FALSE]
  names(grid) <- ids

  label_of <- function(d, code) {
    lab <- js$dimension[[d]]$category$label
    out <- vapply(code, function(k) if (is.null(lab[[k]])) k else lab[[k]], "")
    unname(out)
  }
  unit_of <- function(code) {
    u <- js$dimension[[metric_id]]$category$unit
    vapply(code, function(k) {
      x <- u[[k]]
      if (is.null(x)) NA_character_ else paste(c(x$label, x$symbol)[1], collapse = "")
    }, "", USE.NAMES = FALSE)
  }

  other <- setdiff(ids, time_id)
  series_key <- do.call(paste, c(unname(grid[other]), sep = "|"))

  obs <- data.frame(
    table_id = table_id,
    series_key = series_key,
    period = grid[[time_id]],
    value = values,
    stringsAsFactors = FALSE
  )

  series <- unique(grid[other])
  dims_other <- setdiff(other, metric_id)
  series_df <- data.frame(
    table_id = table_id,
    series_key = do.call(paste, c(unname(series[other]), sep = "|")),
    statistic = series[[metric_id]],
    statistic_label = label_of(metric_id, series[[metric_id]]),
    unit = unit_of(series[[metric_id]]),
    stringsAsFactors = FALSE
  )
  series_df$dims <- if (length(dims_other)) {
    parts <- lapply(dims_other, function(d) {
      paste0(js$dimension[[d]]$label, ": ", label_of(d, series[[d]]))
    })
    do.call(paste, c(parts, sep = "; "))
  } else ""
  series_df$label <- ifelse(
    nzchar(series_df$dims),
    paste0(series_df$statistic_label, " (", series_df$dims, ")"),
    series_df$statistic_label
  )
  rownames(series_df) <- NULL

  periods <- unique(grid[[time_id]])
  period_df <- data.frame(
    table_id = table_id,
    period = periods,
    period_label = label_of(time_id, periods),
    period_start = as.character(period_start(periods)),
    stringsAsFactors = FALSE
  )

  meta <- data.frame(
    table_id = table_id,
    title = js$label %||% table_id,
    updated = js$updated %||% NA_character_,
    n_series = nrow(series_df),
    n_periods = length(periods),
    n_obs = nrow(obs),
    stringsAsFactors = FALSE
  )

  list(meta = meta, series = series_df, periods = period_df, obs = obs)
}

#' Turn PxStat time codes into the first day of the period.
#' Handles 2024M03, 2024Q1, 20241 (quarterly), 2024 and 202403 (monthly).
period_start <- function(code) {
  out <- rep(as.Date(NA), length(code))
  m <- regmatches(code, regexec("^(\\d{4})M(\\d{2})$", code))
  q <- regmatches(code, regexec("^(\\d{4})Q([1-4])$", code))
  q5 <- grepl("^\\d{4}[1-4]$", code)   # PxStat quarterly codes: 20262 = 2026 Q2
  y <- grepl("^\\d{4}$", code)
  m6 <- grepl("^\\d{6}$", code)
  for (i in seq_along(code)) {
    if (length(m[[i]]) == 3) {
      out[i] <- as.Date(sprintf("%s-%s-01", m[[i]][2], m[[i]][3]))
    } else if (length(q[[i]]) == 3) {
      out[i] <- as.Date(sprintf("%s-%02d-01", q[[i]][2], (as.integer(q[[i]][3]) - 1L) * 3L + 1L))
    } else if (q5[i]) {
      out[i] <- as.Date(sprintf("%s-%02d-01", substr(code[i], 1, 4), (as.integer(substr(code[i], 5, 5)) - 1L) * 3L + 1L))
    } else if (y[i]) {
      out[i] <- as.Date(sprintf("%s-01-01", code[i]))
    } else if (m6[i]) {
      out[i] <- as.Date(sprintf("%s-%s-01", substr(code[i], 1, 4), substr(code[i], 5, 6)))
    }
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a
