
# return_result_or_message() ----------------------------------------------

test_that("return_result_or_message() returns function",{

  result = mean %>%
    return_result_or_message()

  expect_true(is.function(result))

})

test_that("return_result_or_message() function returns result or message",{

  returnfun = function(x){
    if(x == 2){
      ret = NULL
      error = list("message" = "error")
    } else {
      ret = x
      error = NULL
    }

    return(list("result" = ret,
                "error" = error))
  }

  testfun = returnfun %>%
    return_result_or_message()

  expect_equal(testfun(1),
               1)

  expect_equal(testfun(2),
               list("error" = c("message" = "error")))

})



# with_progress() ---------------------------------------------------------

test_that("with_progress()",{

  func = with_progress(mean)

  expect_equal(class(func),
               "function")

})

# scrapurrr() with map() -------------------------------------------------------

test_that("scrapurrr() with map() returns result and error list",{

  return_x = function(x){
    x
  }

  result = scrapurrr(1:3,
                     scrapefun = return_x,
                     map_fun = purrr::map)

  expect_equal(object = result,
               expected = as.list(1:3)
  )

})

test_that("scrapurrr() with map() returns error message as string",{

  error_on_2 = function(x){
    if(x == 2){
      stop()
    } else {
      x
    }
  }

  result = scrapurrr(1:3,
                     scrapefun = error_on_2,
                     map_fun = purrr::map)

  expect_equal(result[[2]],
               list("error" = c("message" = "Request failed after 3 attempts"))
  )
})

# scrapurrr() with map_dfr() ---------------------------------------------------

test_that("map_scrape_dfr() returns a tibble",{

  error_on_2 = function(x){
    if(x == 2){
      stop()
    } else {
      list("res" = x)
    }
  }

  result = scrapurrr(1:3,
                     scrapefun = error_on_2,
                     map_fun = purrr::map_dfr)

  expected = tibble::tibble(res = c(1, NA, 3),
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})

# scrapurrr() with map2() ------------------------------------------------------

test_that("scrapurrr() with map2() returns result and error list",{

  return_xy = function(x, y){
    list("x" = x, "y" = y)
  }

  result = scrapurrr(1:3, 1:3,
                     scrapefun = return_xy,
                     map_fun = purrr::map2)

  expect_equal(object = result,
               expected = list(list("x" = 1,
                                    "y" = 1),
                               list("x" = 2,
                                    "y" = 2),
                               list("x" = 3,
                                    "y" = 3))
  )

})

test_that("scrapurrr() with map2() returns error message as string",{

  error_on_2_xy = function(x, y){
    if(x == 2){
      stop()
    } else {
      list("x" = x, "y" = y)
    }
  }

  result = scrapurrr(1:3, 1:3,
                     scrapefun = error_on_2_xy,
                     map_fun = purrr::map2)

  expect_equal(result[[2]],
               list("error" = c("message" = "Request failed after 3 attempts"))
  )
})
