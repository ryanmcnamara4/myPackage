test_that('Function splits single strings correctly', {

  test_s = 'This is a sentence.'

  expect_equal(class(split_n_characters(test_s)), 'list')
  expect_equal(length(split_n_characters(test_s)), 1)

  expect_equal(split_n_characters(test_s)[[1]], c('Thi', 's i', 's a', ' se', 'nte', 'nce', '.'))

  expect_equal(split_n_characters(test_s, n = 1)[[1]], unlist(strsplit(test_s, '')))

  expect_equal(split_n_characters(test_s, n = 6)[[1]], c('This i', 's a se', 'ntence', '.'))


})

test_that('Function splits a character vector of multiple strings correctly', {

  test_s = c('ACTGACGACTGG', 'CAGGGGTGACG', 'GG')

  expect_equal(class(split_n_characters(test_s)), 'list')
  expect_equal(length(split_n_characters(test_s)), length(test_s))

  expect_equal(split_n_characters(test_s)[[1]], c('ACT', 'GAC', 'GAC', 'TGG'))
  expect_equal(split_n_characters(test_s)[[2]], c('CAG', 'GGG', 'TGA', 'CG'))
  expect_equal(split_n_characters(test_s)[[3]], 'GG')

})

test_that('Strange conditions give appropriate result', {

  expect_equal(split_n_characters('')[[1]], character())
  expect_equal(split_n_characters(100, n = 2)[[1]], c('10', '0'))
  expect_true(is.na(split_n_characters(NA, 100)[[1]]))
  expect_equal(split_n_characters('ACTGAC', n = 0)[[1]], c('A', 'C', 'T', 'G', 'A', 'C'))
  expect_equal(split_n_characters('ACTGAC', n = -45)[[1]], c('A', 'C', 'T', 'G', 'A', 'C'))

})
