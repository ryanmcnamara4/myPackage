#######################################################################################################
# nucleotide_diff

test_that('Appropriate errors are thrown with nucleotide_diff', {

  expect_error(nucleotide_diff('AGT', 'AGA'),
               'Choose either "num" or "position" for the output.')
  expect_error(nucleotide_diff('ACG', 'ACG', output = 'chicken'),
               'Choose either "num" or "position" for the output.')
  expect_error(nucleotide_diff(c('ACG', 'TGG'), 'AGC', output = 'num'),
               'should only be one sequence.')
  expect_error(nucleotide_diff('AGG', c('AGG', 'CAGT'), output = 'position'),
               'need to have the same number of characters.')

})

test_that('The "num" output functionality works correctly', {

  expect_equal(nucleotide_diff('', '', output = 'num'), 0)
  expect_equal(nucleotide_diff('ACG', 'ACG', output = 'num'), 0)
  expect_equal(nucleotide_diff('ACGGTG', c('ACGGTG', 'AGTGTA'), output = 'num'), c(0, 3))
  expect_equal(nucleotide_diff('gttctg', c('GTtcTA', 'gtaagg'), output = 'num'), c(1, 3))
  expect_equal(nucleotide_diff('gttctg', c('GTtcTA', 'gtaagg'), output = 'num', ignore_case = FALSE),
               c(4, 3))

})

test_that('The "position" output functionality works correctly', {

  expect_equal(nucleotide_diff('', '', output = 'position'), list(integer()))
  expect_equal(nucleotide_diff('ACG', 'ACG', output = 'position'), list(integer()))
  expect_equal(nucleotide_diff('ACGGTG', c('ACGGTG', 'AGTGTA'), output = 'position'),
               list(integer(), c(2, 3, 6)))
  expect_equal(nucleotide_diff('gttctg', c('GTtcTA', 'gtaagg'), output = 'position'),
               list(6, c(3, 4, 5)))
  expect_equal(nucleotide_diff('gttctg', c('GTtcTA', 'gtaagg'), output = 'position', ignore_case = FALSE),
               list(c(1, 2, 5, 6), c(3, 4, 5)))

})

#######################################################################################################
# codon_diff

test_that('Appropriate errors are thrown with codon_diff', {

  w = testthat::capture_warnings(codon_diff('AA', 'GT', output = 'num', diff_type = 'all', seq_type = 'DNA'))
  expect_match(w, "not divisible by 3.", all = FALSE)
  expect_match(w, "not divisible by 3.", all = FALSE)

  expect_error(codon_diff('CTG', 'GTG', diff_type = 'all', seq_type = 'DNA'),
               'Choose either "num" or "position" for the output.')
  expect_error(codon_diff('CTG', 'GTG', diff_type = 'all', output = 'chicken', seq_type = 'DNA'),
               'Choose either "num" or "position" for the output.')
  expect_error(codon_diff('CTG', 'GTG', output = 'num', seq_type = 'DNA'),
               'Choose "synonymous", "non-synonymous", or "all" for the "diff_type".')
  expect_error(codon_diff('CTG', 'GTG', output = 'position', diff_type = 'any', seq_type = 'DNA'),
               'Choose "synonymous", "non-synonymous", or "all" for the "diff_type".')
  expect_error(codon_diff(c('CAT', 'GAC'), 'GAC', output = 'num', diff_type = 'all', seq_type = 'DNA'),
               'should only be one sequence.')
  expect_error(codon_diff('CATGAA', c('CATGAA', 'GTA'), output = 'num', diff_type = 'all', seq_type = 'DNA'),
               'need to have the same number of characters.')
  expect_error(codon_diff('CTG', 'GTG', diff_type = 'all', output = 'num'),
               '"seq_type" should be either "DNA" or "RNA".')
  expect_error(codon_diff('CTG', 'GTG', diff_type = 'all', output = 'position', seq_type = 'none'),
               '"seq_type" should be either "DNA" or "RNA".')

})

test_that('The "num" output functionality works correctly', {

  expect_equal(codon_diff('', '', output = 'num', diff_type = 'all', seq_type = 'DNA'), 0)
  expect_equal(codon_diff('GACTAG', 'GATTAG', output = 'num', diff_type = 'all', seq_type = 'DNA'),
               1)
  expect_equal(codon_diff('AGAATACGT', c('AAAATACCC', 'AGAATACGT'), output = 'num', diff_type = 'all', seq_type = 'DNA'),
               c(2, 0))
  expect_equal(codon_diff('AGAATACGT', c('AGGATACGG', 'GGGATACGT'), output = 'num', diff_type = 'synonymous', seq_type = 'DNA'),
               c(2, 0))
  expect_equal(codon_diff('AGAAUACGU', c('UUUUGUCGU', 'AGAUUACGU'), output = 'num', diff_type = 'non-synonymous', seq_type = 'RNA'),
               c(2, 1))

  expect_equal(codon_diff('agaauacgu', c('AAAAuaCCC', 'agAauACGu'), output = 'num', diff_type = 'all', seq_type = 'RNA', ignore_case = TRUE),
               c(2, 0))
  expect_equal(codon_diff('agaatacgt', c('AAAAtaCCC', 'agAatACGt'), output = 'num', diff_type = 'all', seq_type = 'DNA', ignore_case = FALSE),
               c(3, 3))

})

test_that('The "position" output functionality works correctly', {

  expect_equal(codon_diff('', '', output = 'position', diff_type = 'all', seq_type = 'DNA'), list(integer()))
  expect_equal(codon_diff('GACTAG', 'GATTAG', output = 'position', diff_type = 'all', seq_type = 'DNA'),
               list(1))
  expect_equal(codon_diff('AGAATACGT', c('AAAATACCC', 'AGAATACGT'), output = 'position', diff_type = 'all', seq_type = 'DNA'),
               list(c(1, 3), integer()))
  expect_equal(codon_diff('AGAAUACGU', c('AGGAUACGG', 'GGGAUACGU'), output = 'position', diff_type = 'synonymous', seq_type = 'RNA'),
               list(c(1, 3), integer()))
  expect_equal(codon_diff('AGAATACGT', c('TTTTGTCGT', 'AGATTACGT'), output = 'position', diff_type = 'non-synonymous', seq_type = 'DNA'),
               list(c(1, 2), 2))

  expect_equal(codon_diff('agaatacgt', c('AAAAtaCCC', 'agAatACGt'), output = 'position', diff_type = 'all', seq_type = 'DNA', ignore_case = TRUE),
               list(c(1, 3), integer()))
  expect_equal(codon_diff('agaauacgu', c('AAAAuaCCC', 'agAauACGu'), output = 'position', diff_type = 'all', seq_type = 'RNA', ignore_case = FALSE),
               list(c(1, 2, 3), c(1, 2, 3)))

})
