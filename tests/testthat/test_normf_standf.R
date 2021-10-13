test_that('Length of returned vector is same as input vector', {

  expect_equal(length(normf(c(1, 2, 3), 1, 10)), 3)
  expect_equal(length(normf(1:100, -5, 5)), 100)

  expect_equal(length(standf(c(1, 2, 3))), 3)
  expect_equal(length(standf(1:100)), 100)

})

test_that('Numeric vector needs to be longer than 1 element', {

  expect_error(normf(10, 10, 10), 'longer than 1')
  expect_error(standf(10), 'longer than 1')

})

test_that('newMax should be larger than newMin', {

  expect_warning(normf(c(10, 11, 12, 13), 20, 10), 'is larger than or equal to')
  expect_warning(normf(c(10, 11, 12, 13), 5, 5), 'is larger than or equal to')

})

test_that('Return value is correct', {

  test_vec = sample(-100:100, 15, replace = FALSE)
  test_mean = mean(test_vec)
  test_sd = stats::sd(test_vec)

  standf_ans = (test_vec - test_mean) / test_sd

  newMin = sample(1:50, 1)
  newMax = sample(100:150, 1)

  normf_ans = (newMax - newMin) * ((test_vec - min(test_vec)) / (max(test_vec) - min(test_vec))) + newMin

  expect_equal(standf(test_vec), standf_ans)
  expect_equal(normf(test_vec, newMin, newMax), normf_ans)

})

test_that('Functions handle NA values correctly', {

  expect_error(normf(NA, 10, 20), 'longer than 1')
  expect_error(standf(NA), 'longer than 1')

  expect_error(normf(c(1, 2, 3), NA, 20), 'missing value')
  expect_error(normf(c(1, 2, 3), 10, NA), 'missing value')
  expect_error(normf(c(1, 2, 3), NA, NA), 'missing value')

  expect_equal(normf(c(NA, 10, 20), 0, 10), as.numeric(c(NA, NA, NA)))
  expect_equal(normf(c(1, NA, NA), 0, 10), as.numeric(c(NA, NA, NA)))

  expect_equal(standf(c(NA, 10, 20)), as.numeric(c(NA, NA, NA)))
  expect_equal(standf(c(1, NA, NA)), as.numeric(c(NA, NA, NA)))

  expect_error(normf(c(1, NA, NA), 10, 20, na.rm = TRUE), 'more than 1 non-missing values')
  expect_error(standf(c(1, NA, NA), na.rm = TRUE), 'more than 1 non-missing values')

  test_vec = c(NA, 10, -5, 2, -87, NA)
  test_mean = mean(test_vec, na.rm = TRUE)
  test_sd = sd(test_vec, na.rm = TRUE)
  newMin = sample(-100:-50, 1)
  newMax = sample(0:40, 1)

  normf_ans = (newMax - newMin) * ((test_vec - min(test_vec, na.rm = TRUE)) / (max(test_vec, na.rm = TRUE) -
                                                                                 min(test_vec, na.rm = TRUE))) +
    newMin
  standf_ans = (test_vec - test_mean) / test_sd

  expect_equal(normf(test_vec, newMin, newMax, na.rm = TRUE), normf_ans)
  expect_equal(standf(test_vec, na.rm = TRUE), standf_ans)


})
