#' Match messy state names to standard state information
#'
#' `state_match()` takes free-text US state names or abbreviations and returns
#' the standard postal abbreviation, full name and FIPS code, tolerating
#' common abbreviations and small misspellings.
#'
#' @details
#' Matching is attempted in this order:
#'
#' 1. Postal abbreviation (`"az"`).
#' 2. Exact full name or known alias after normalisation (case, punctuation,
#'    whitespace, "N."/"S."/"W." prefixes). Aliases include traditional
#'    abbreviations such as `"Calif"`, `"Ariz"`, `"Penn"`, `"Tenn"` and
#'    `"Wash"`.
#' 3. Unique prefix of a state name, at least `min_prefix` letters long:
#'    `"cal"` is `CA`, `"tex"` is `TX`, `"N. Car"` is `NC`.
#' 4. Fuzzy match using a transposition-aware edit distance, for misspellings.
#'
#' Matching is deliberately conservative. A prefix or fuzzy value is only
#' matched when exactly one state fits; if several do (for example `"Mis"`
#' `"New"` or `"Nor"`) the result is `NA` with `method = "ambiguous"`. Words that match no state, like `"Carolina"`, return `NA` with `method = "none"`.
#' Inputs shorter than four characters are never fuzzy matched, and two-letter
#' abbreviations are never "corrected".
#'
#' @param x A character vector (or factor) of state names or abbreviations.
#' @param max_dist Maximum edit distance allowed in a fuzzy match, as a
#'   fraction of the candidate name length (rounded down). The default `0.25`
#'   allows 1 edit for names up to 7 letters and 2 edits for longer names.
#'   Set to `0` or use `fuzzy = FALSE` to turn fuzzy matching off.
#' @param fuzzy Logical. Allow fuzzy matching? Default `TRUE`.
#' @param prefix Logical. Allow unique-prefix matching (`"cal"`, `"tex"`)?
#'   Default `TRUE`.
#' @param min_prefix Minimum number of letters for a prefix match. Default `3`.
#' @param territories Logical. Also recognise Puerto Rico, Guam, the US Virgin
#'   Islands, American Samoa and the Northern Mariana Islands? Default `FALSE`.
#'
#' @return `state_match()` returns a data frame with one row per element of `x`
#'   and columns `input`, `abb`, `name`, `fips`, `method` (one of
#'   `"abbreviation"`, `"exact"`, `"prefix"`, `"fuzzy"`, `"ambiguous"`,
#'   `"none"`, or `NA`
#'   for missing input) and `distance` (edit distance for fuzzy matches, else
#'   `0` or `NA`).
#'
#' @examples
#' state_match(c("Arizona", "AZ", "arizonna", "N. Dakota", "Carolina", NA))
#' state_abb(c("Tenessee", "Pennsylvnaia", "Wisconsni", "West Virgina"))
#' state_abb(c("cal", "TEX", "Calif.", "Penn", "Mis"))
#' state_name("tx")
#' state_fips("Washington D.C.")
#' @name state_match
NULL

#' @rdname state_match
#' @export
state_match <- function(x, max_dist = 0.25, fuzzy = TRUE, prefix = TRUE,
                        min_prefix = 3, territories = FALSE) {
  stopifnot(is.numeric(max_dist), length(max_dist) == 1L, max_dist >= 0,
            is.logical(fuzzy), length(fuzzy) == 1L, !is.na(fuzzy),
            is.logical(prefix), length(prefix) == 1L, !is.na(prefix),
            is.numeric(min_prefix), length(min_prefix) == 1L,
            is.logical(territories), length(territories) == 1L)

  input <- as.character(x)
  tbl <- if (isTRUE(territories)) rbind(.states, .territories) else .states

  keys <- .normalize(input)
  uniq <- unique(keys[!is.na(keys)])

  res <- lapply(uniq, .match_one, tbl = tbl, max_dist = max_dist,
                fuzzy = fuzzy, prefix = prefix, min_prefix = min_prefix)
  u_abb    <- vapply(res, function(r) r$abb, character(1))
  u_method <- vapply(res, function(r) r$method, character(1))
  u_dist   <- vapply(res, function(r) r$distance, numeric(1))

  idx <- match(keys, uniq)
  abb <- u_abb[idx]
  m   <- match(abb, tbl$abb)

  data.frame(
    input    = input,
    abb      = abb,
    name     = tbl$name[m],
    fips     = tbl$fips[m],
    method   = ifelse(is.na(keys), NA_character_, u_method[idx]),
    distance = ifelse(is.na(keys), NA_real_, u_dist[idx]),
    stringsAsFactors = FALSE
  )
}

#' @rdname state_match
#' @param ... Arguments passed on to `state_match()`.
#' @return `state_abb()`, `state_name()` and `state_fips()` return a character
#'   vector the same length as `x`, with `NA` where no match was found.
#' @export
state_abb <- function(x, ...) state_match(x, ...)$abb

#' @rdname state_match
#' @export
state_name <- function(x, ...) state_match(x, ...)$name

#' @rdname state_match
#' @export
state_fips <- function(x, ...) state_match(x, ...)$fips

# ---- internals --------------------------------------------------------------

# Lower-case, strip punctuation/extra whitespace, expand N./S./W. prefixes.
.normalize <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[.']", "", x)
  x <- gsub("[^[:alpha:] ]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)
  x <- sub("^(n|no|nth) (?=\\S)", "north ", x, perl = TRUE)
  x <- sub("^(s|so|sth) (?=\\S)", "south ", x, perl = TRUE)
  x <- sub("^w (?=\\S)", "west ", x, perl = TRUE)
  x
}

# Optimal-string-alignment distance (Levenshtein + adjacent transposition).
.osa <- function(a, b) {
  x <- strsplit(a, "", fixed = TRUE)[[1L]]
  y <- strsplit(b, "", fixed = TRUE)[[1L]]
  la <- length(x)
  lb <- length(y)
  d <- matrix(0L, la + 1L, lb + 1L)
  d[, 1L] <- 0:la
  d[1L, ] <- 0:lb
  for (i in seq_len(la)) {
    for (j in seq_len(lb)) {
      v <- min(d[i, j + 1L] + 1L, d[i + 1L, j] + 1L,
               d[i, j] + as.integer(x[i] != y[j]))
      if (i > 1L && j > 1L && x[i] == y[j - 1L] && x[i - 1L] == y[j]) {
        v <- min(v, d[i - 1L, j - 1L] + 1L)
      }
      d[i + 1L, j + 1L] <- v
    }
  }
  d[la + 1L, lb + 1L]
}

.result <- function(abb, method, distance) {
  list(abb = abb, method = method, distance = distance)
}

.match_one <- function(key, tbl, max_dist, fuzzy, prefix, min_prefix) {
  if (!nzchar(key)) return(.result(NA_character_, "none", NA_real_))

  # 1. postal abbreviation
  hit <- match(toupper(key), tbl$abb)
  if (!is.na(hit)) return(.result(tbl$abb[hit], "abbreviation", 0))

  # 2. exact full name or alias
  cand_key <- c(tolower(tbl$name), .aliases$alias)
  cand_abb <- c(tbl$abb, .aliases$abb)
  keep <- cand_abb %in% tbl$abb          # drop aliases for excluded states
  cand_key <- cand_key[keep]
  cand_key <- .normalize(cand_key)
  cand_abb <- cand_abb[keep]
  hit <- match(key, cand_key)
  if (!is.na(hit)) return(.result(cand_abb[hit], "exact", 0))

  # 3. unique prefix of a state name ("cal", "tex", "n car")
  if (prefix && nchar(key) >= min_prefix) {
    full <- .normalize(tbl$name)
    pre <- which(startsWith(full, key))
    if (length(pre) == 1L) return(.result(tbl$abb[pre], "prefix", 0))
    if (length(pre) > 1L) return(.result(NA_character_, "ambiguous", NA_real_))
  }

  # 4. fuzzy
  if (!fuzzy || max_dist <= 0 || nchar(key) < 4L) {
    return(.result(NA_character_, "none", NA_real_))
  }
  allowed <- floor(max_dist * nchar(cand_key))
  near <- which(abs(nchar(cand_key) - nchar(key)) <= allowed)
  if (!length(near)) return(.result(NA_character_, "none", NA_real_))
  dist <- vapply(near, function(i) .osa(key, cand_key[i]), numeric(1))
  ok <- dist <= allowed[near]
  if (!any(ok)) return(.result(NA_character_, "none", NA_real_))
  near <- near[ok]
  dist <- dist[ok]
  best <- near[dist == min(dist)]
  best_abb <- unique(cand_abb[best])
  if (length(best_abb) > 1L) {
    return(.result(NA_character_, "ambiguous", min(dist)))
  }
  .result(best_abb, "fuzzy", min(dist))
}
