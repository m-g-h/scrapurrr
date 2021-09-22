test_that("messagefun works", {
  expect_message(messagefun(1, 1000, "object"),
                 regexp = "object")
})

test_that("try-and-timeout works",{

  expect_equal(try_and_timeout(mean, 1:100),
               50.5)

  expect_warning(try_and_timeout(Sys.sleep, 0.2, timeout = 0.1),
                 "reached elapsed time limit")

  expect_warning(try_and_timeout(mean, "error"),
               ".")

})

