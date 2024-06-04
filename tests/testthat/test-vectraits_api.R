# Sean Sorek 10/1/23

test_that("asking for ID works", {
  f <- file()
  lines <- c(NA, -1, 0, 1, 2, 100, 101, 1000)
  coll <- paste(lines, collapse = "\n")
  write(coll, f)

  withr::local_options(list(ask_dataset_ID.con = f))

  suppressWarnings(expect_error(ask_for_ID(100), "is invalid or is out of range")) # NA
  expect_error(ask_for_ID(100), regexp = "is invalid or is out of range") #-1
  expect_error(ask_for_ID(100), regexp = "is invalid or is out of range") # 0
  expect_equal(ask_for_ID(100), 1)
  expect_equal(ask_for_ID(100), 2)
  expect_equal(ask_for_ID(100), 100)
  expect_error(ask_for_ID(100), regexp = "is invalid or is out of range") # 101
  expect_error(ask_for_ID(100), regexp = "is invalid or is out of range") # 1000

  close(f)
})

test_that("fetching datasets works", {
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  skip_on_cran()
  skip_if_not(is_VecTraits_alive(), message = "VecTraits database is not available right now. API cannot be tested.")


  expect_error(get_VB_dataset(c(1, 2)), regexp = "If pulling multiple datasets")
  load(testthat::test_path("example_data", "VT_1.rda"))
  expect_equal(get_VB_dataset(1), VT_1)

  expect_error(get_VB_dataset(-1, check_interactive = F), regexp = "is invalid or is out of range")
  expect_error(get_VB_dataset(NA, check_interactive = F), regexp = "is invalid or is out of range")
  expect_error(get_VB_dataset(NA_integer_, check_interactive = F), regexp = "is invalid or is out of range")
  expect_error(get_VB_dataset(NA_character_, check_interactive = F), regexp = "is invalid or is out of range")
  expect_error(get_VB_dataset(NA_real_, check_interactive = F), regexp = "is invalid or is out of range")
  expect_error(get_VB_dataset(NaN, check_interactive = F), regexp = "is invalid or is out of range")
  httr::set_config(httr::config(ssl_verifypeer = TRUE))
})

test_that("bad website access is caught", {
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  skip_on_cran()
  # I know for a fact this page will never have anything on it :)
  expect_error(get_web_data("https://seansorek.github.io/broken"), regexp = "Data fetch failed.")
  httr::set_config(httr::config(ssl_verifypeer = TRUE))
})

test_that("fetching multiple datasets works", {
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  skip_on_cran()
  skip_if_not(is_VecTraits_alive(), message = "VecTraits database is not available right now. API cannot be tested.")

  expect_error(get_VB_datasets(NA), regexp = "is invalid or is out of range")
  expect_error(get_VB_datasets(c()), regexp = "must have at least length 1")
  expect_error(get_VB_datasets(c(1, NA)), regexp = "is invalid or is out of range")
  expect_error(get_VB_datasets(c(1, -1)), regexp = "is invalid or is out of range")
  expect_error(get_VB_datasets(c(-1, 1)), regexp = "is invalid or is out of range")
  expect_error(get_VB_datasets(1:12, T), regexp = "Attempt to retreive too many datasets")
  expect_warning(get_VB_datasets(1:12, F), regexp = "Pulling a large number of datasets may take a while")

  load(testthat::test_path("example_data", "VT_1.rda"))
  load(testthat::test_path("example_data", "VT_2.rda"))

  expect_equal(get_VB_datasets(1)[[1]], VT_1)
  expect_equal(get_VB_datasets(2)[[1]], VT_2)

  ds <- get_VB_datasets(c(1, 2))
  expect_equal(ds[[1]], VT_1)
  expect_equal(ds[[2]], VT_2)
  httr::set_config(httr::config(ssl_verifypeer = TRUE))
})

test_that("Dataset searching works",{
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  skip_on_cran()
  skip_if_not(is_VecTraits_alive(), message = "VecTraits database is not available right now. API cannot be tested.")

  expect_error(find_VB_datasets(3), regexp = "Invalid keyword")
  expect_error(find_VB_datasets(character()), regexp = "Keyword must be provided")


  expect_output(amer <- find_VB_datasets("Americanum", F), regexp = "datasets found")
  expect_error(find_VB_datasets("sojbgsidkhgbf"), regexp = "Data fetch failed")
  expect_gte(length(amer), 4) #so the test doesn't fail when more lone star datasets are added
  httr::set_config(httr::config(ssl_verifypeer = TRUE))
})
