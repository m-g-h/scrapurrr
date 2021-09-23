test_that("return_named_list() works", {

  # Function that uses return_named_list()
  func <- function(a, b) {
    # objects prefixed with a dot get ignored
    .not_include = b
    x = a
    y = NULL
    return_named_list()
  }

  expect_equal(func(1, 2),
               list("x" = 1,
                    "y" = NA))

  # Should also work within do_scrape()
  expect_equal(do_scrape(func, 1:3, 1:3,print_status_message = F),
               tibble(x = 1:3,
                      y = NA,
                      id = x))

})
