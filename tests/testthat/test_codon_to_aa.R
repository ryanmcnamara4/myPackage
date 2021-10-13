test_that('Function returns the correct values', {

  expect_equal(codon_to_aa('AGA'), 'Arg')
  expect_equal(codon_to_aa(c('CAT', 'ATG', 'TTT', 'AGT', 'TAG')), c('His', 'Met', 'Phe', 'Ser', 'Stp'))
  expect_equal(codon_to_aa('TAD'), character())
  expect_error(codon_to_aa())

  aa_ans = c()
  for (c in ALL_CODONS) {
    aa_ans = append(aa_ans, names(SYNONYMOUS_CODONS)[grep(c, SYNONYMOUS_CODONS)])
  }

  expect_equal(codon_to_aa(ALL_CODONS), aa_ans)

})
