
# add_sleep_time() --------------------------------------------------------

test_that("add_sleeping_time() returns a function", {
  expect_equal(mean %>%
                 add_sleep_time() %>%
                 class(),
               "function")

})
test_that("add_sleep_time() adds to execution time",{

  sleep_time = 0.5

  with_sleep = add_sleep_time(mean, sleep_time)

  start_time = Sys.time()
  mean(1:10)
  time_without_sleep = difftime(Sys.time(), start_time,
                                units = "secs")

  start_time = Sys.time()
  with_sleep(1:10)
  time_with_sleep = difftime(Sys.time(), start_time,
                                units = "secs")

  difference = time_with_sleep - time_without_sleep

  expect_equal(as.numeric(difference),
               sleep_time,
               tolerance = 0.1)


})

test_that("add_sleep_time() works with two arguments",{

  add_two_numbers = function(x, y){
    x + y
  }

  with_sleep = add_two_numbers %>%
    add_sleep_time()

  expect_equal(with_sleep(1, 2),
               3)

})
