test_that("messagefun works", {
  expect_message(messagefun(1, 1000, "object"),
                 regexp = "object")
})


