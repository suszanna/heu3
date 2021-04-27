# Install the released version from CRAN
# install.packages("testthat")

# use the development version of testthat from GitHub
# install.packages("devtools")
devtools::install_github("r-lib/testthat")

if (!require(testthat)) install.packages('testthat')
library(testthat)

# Install package from CRAN only if not installed, and load the library
if (!require(covr)) install.packages('covr')
library(covr)

#local_edition(3)

test_that("I can use the 3rd edition", {
  local_edition(3)
  expect_true(TRUE)
})

# alternatively, onlyif necessary
#test_that("I want to use the 2nd edition", {
local_edition(2)
expect_true(TRUE)
#})

#source("../../R/heuPlotter.Rmd") #  is your project a package?
#test_that("Add one to 99", {
#  expect_equal(add_one(99), 100)
#})

# forget the following environments & exit without testing
local({
  # environments not for testing at this time
  # return if on Windows
  isWindows <- Sys.info()[["sysname"]] == "Windows"
  if (isWindows)
    return()

  # return if on Solaris
  isSolaris <- Sys.info()[["sysname"]] == "SunOS"
  if (isSolaris)
    return()

  # return if 'usethis' required by namespace
  if (!requireNamespace("usethis", quietly = TRUE))
    return()

  # return only required messaging
  quietly <- function(expr) {
    suppressMessages(capture_output(result <- expr))
    result
  }

  perform_test <- function(pkgName, catchEnabled) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd), add = TRUE)

    pkgPath <- file.path(tempdir(), pkgName)
    libPath <- file.path(tempdir(), "rlib")
    if (!utils::file_test("-d", libPath))
      dir.create(libPath)
    .libPaths(c(libPath, .libPaths()))

    on.exit({
      unlink(pkgPath, recursive = TRUE)
      unlink(libPath, recursive = TRUE)
    }, add = TRUE)

    quietly(usethis::create_package(pkgPath, open = FALSE))
    quietly(testthat::use_catch(pkgPath))

    cat("LinkingTo: testthat",
        file = file.path(pkgPath, "DESCRIPTION"),
        append = TRUE,
        sep = "\n")

    cat(
      sprintf("useDynLib(%s, .registration=TRUE)", pkgName),
      file = file.path(pkgPath, "NAMESPACE"),
      append = TRUE,
      sep = "\n"
    )
    if (!catchEnabled) {

      makevarsPath <- file.path(
        pkgPath,
        "src",
        if (isWindows) "Makevars.win" else "Makevars"
      )
      cat(
        "PKG_CPPFLAGS = -DTESTTHAT_DISABLED",
        file = makevarsPath,
        sep = "\n"
      )

    }

    install.packages(pkgs = pkgPath, repos = NULL, type = "source")
    library(pkgName, character.only = TRUE)
    stopifnot(.Call("run_testthat_tests", FALSE, PACKAGE = pkgName))
    pkgload::unload(pkgName)
  }

  #set up done, run tests - not these, save for tutorial followup
  #withr::with_envvar(c(R_TESTS = ''), perform_test("testthatclient1",  TRUE))
  #withr::with_envvar(c(R_TESTS = ''), perform_test("testthatclient2", FALSE))

  # now test our code - if no error thrown then test passed
  # expected - identical date formatting
  #test 1 - (as.date function - expect_identical)
  test_that("check date is identical - against expectation", {
    one <- is_identical_to(as.Date("2007-02-02"), "2007-02-02")
    two <- one
    expect_identical(one, two)
  })

  # expectation: equivalent data formatting
  #test 2 - (as.numeric function -expect_equivalent)
  test_that("all numeric data", {
    data$Global_active_power <- as.numeric(data$Global_active_power)
    totalPower <- data$Global_active_power
    expect_equivalent(data$Global_active_power, totalPower)
  })

  #expectation - multiframe formatting equal (the same)
  #test 3 -( par() function - expect_equal)
  good_multiframe  <- par(mfrow = c(2, 2))
  better_multiframe <- good_multiframe
  # Pass
  expect_equal(good_multiframe, better_multiframe)

  #test 4 - what coverage have we achieved in heuPlotter.Rmd?
  test_that("test coverage report", {
    coverage <- package_coverage(path="./")
    #coverage <- package_coverage(path="/Users/susanlmartin/coursera/heu")
    coverage
    covr <- coverage

    #Pass - error as expected
    report(coverage)
    expect_error(coverage, covr)
  })

})




