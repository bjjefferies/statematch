test_that("abbreviations and exact names match, case-insensitively", {
  expect_equal(state_abb(c("AZ", "az", "Arizona", "ARIZONA", " arizona ")),
               rep("AZ", 5))
  expect_equal(state_name("tx"), "Texas")
  expect_equal(state_fips("Washington D.C."), "11")
})

test_that("every state name and abbreviation round-trips", {
  expect_equal(state_abb(statematch:::.states$name), statematch:::.states$abb)
  expect_equal(state_abb(statematch:::.states$abb), statematch:::.states$abb)
  expect_equal(abbr2state(statematch:::.states$abb), statematch:::.states$name)
})

test_that("small misspellings are caught", {
  typos <- c(Tenessee = "TN", Pennsylvnaia = "PA", Wisconsni = "WI",
             `West Virgina` = "WV", Missisipi = "MS", Arizonna = "AZ",
             Kanasas = "KS", Conecticut = "CT", Virgina = "VA",
             `New Hampshir` = "NH", Oklahmoa = "OK")
  expect_equal(unname(state_abb(names(typos))), unname(typos))
  expect_equal(state_match("Tenessee")$method, "fuzzy")
})

test_that("similar states are not confused", {
  expect_equal(state_abb(c("Kansas", "Arkansas", "Virginia", "West Virginia")),
               c("KS", "AR", "VA", "WV"))
})

test_that("ambiguous or hopeless input returns NA", {
  expect_true(is.na(state_abb("Carolina")))
  expect_true(is.na(state_abb("Dakota")))
  expect_true(is.na(state_abb("Narnia")))
  expect_true(is.na(state_abb("")))
  expect_true(is.na(state_abb(NA)))
  expect_true(is.na(state_abb("Mis")))
  expect_equal(state_match("Mis")$method, "ambiguous")
  expect_equal(state_match("New")$method, "none")
})

test_that("common 3-letter and traditional abbreviations match", {
  x <- c(cal = "CA", TEX = "TX", Calif. = "CA", Ala = "AL", Ariz = "AZ",
         Penn = "PA", Tenn = "TN", Wash = "WA", Miss = "MS", Ore = "OR",
         Mar = "MD", Wis = "WI", Ken = "KY", Okl = "OK", `N Car` = "NC",
         `So. Dak` = "SD", Neb = "NE", Mon = "MT")
  expect_equal(unname(state_abb(names(x))), unname(x))
  expect_equal(state_match("tex")$method, "exact")
  expect_equal(state_match("ida")$method, "prefix")
  expect_equal(state_abb("ida"), "ID")
  expect_true(is.na(state_abb("ida", prefix = FALSE, fuzzy = FALSE)))
})

test_that("regressions: partial words and abbreviation lookalikes don't match", {
  # "west" is the first word of West Virginia, not a state
  expect_true(is.na(state_abb("west")))
  expect_true(is.na(state_abb(c("north", "south", "new", "rhode"))[1]))
  # "Kano" is 1 edit from the abbreviation "kans", but not a Kansas typo
  expect_true(is.na(state_abb("Kano")))
  expect_equal(state_abb(c("Kansa", "Kanasas", "W Virginia")),
               c("KS", "KS", "WV"))
})

test_that("prefix shorthand and punctuation are handled", {
  expect_equal(state_abb(c("N. Dakota", "S Carolina", "W. Virginia", "N.Y.")),
               c("ND", "SC", "WV", "NY"))
})

test_that("fuzzy = FALSE disables typo correction", {
  expect_true(is.na(state_abb("Tenessee", fuzzy = FALSE)))
  expect_equal(state_abb("Tennessee", fuzzy = FALSE), "TN")
})

test_that("territories are opt-in", {
  expect_true(is.na(state_abb("Puerto Rico")))
  expect_equal(state_abb("Puerto Rico", territories = TRUE), "PR")
})

test_that("factors, length and order are preserved", {
  x <- factor(c("az", "tx", "az"))
  expect_equal(state_abb(x), c("AZ", "TX", "AZ"))
  expect_equal(nrow(state_match(character(0))), 0L)
})

test_that("abbr2state is strict", {
  expect_equal(abbr2state(c("az", " TX ", "ZZ", NA)),
               c("Arizona", "Texas", NA, NA))
  expect_true(is.na(abbr2state("PR")))
  expect_equal(abbr2state("PR", territories = TRUE), "Puerto Rico")
})
