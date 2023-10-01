test_that("asking for ID works", {
  f <- file()
  lines <- c(NA,-1,0,1,2,100,101,1000,"are you over twenty-one?")
  coll <- paste(lines, collapse = "\n")
  write(coll,f)

  withr::local_options(list(ask_dataset_ID.con = f))

  suppressWarnings(expect_error(ask_for_ID(100), "is invalid or is out of range")) #NA
  expect_error(ask_for_ID(100), "is invalid or is out of range") #-1
  expect_error(ask_for_ID(100), "is invalid or is out of range") #0
  expect_equal(ask_for_ID(100), 1)
  expect_equal(ask_for_ID(100), 2)
  expect_equal(ask_for_ID(100), 100)
  expect_error(ask_for_ID(100), "is invalid or is out of range") #101
  expect_error(ask_for_ID(100), "is invalid or is out of range") #1000

  close(f)
})

test_that("fetching datasets works", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(is_VecTraits_alive(), message = "VecTraits database is not available right now. API cannot be tested.")

  expect_error(get_dataset(c(1,2)), regexp = "If pulling multiple datasets")
  load(testthat::test_path("example_data", "VT_1.rda"))
  expect_equal(get_dataset(1), VT_1)

  expect_error(get_dataset(-1, check_interactive = F), regexp = "is invalid or is out of range")
  })

test_that("bad website access is caught", {
  skip_if_offline()
  skip_on_cran()
  #I know for a fact this page will never have anything on it :)
  expect_error(get_web_data("https://seansorek.github.io/broken"), regexp = "Data fetch failed.")

})
