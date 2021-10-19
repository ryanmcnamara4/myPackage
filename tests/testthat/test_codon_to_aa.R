test_that('Function returns the correct values for DNA seqs', {

  expect_equal(codon_to_aa('AGA', seq_type = 'DNA'), 'Arg')
  expect_equal(codon_to_aa(c('CAT', 'ATG', 'TTT', 'AGT', 'TAG'), seq_type = 'DNA'), c('His', 'Met', 'Phe', 'Ser', 'Stp'))
  expect_equal(codon_to_aa('TAD', seq_type = 'DNA'), character())
  expect_error(codon_to_aa(seq_type = 'DNA'))

  aa_ans = c()
  for (c in ALL_CODONS_DNA) {
    aa_ans = append(aa_ans, names(SYNONYMOUS_CODONS_DNA)[grep(c, SYNONYMOUS_CODONS_DNA)])
  }

  expect_equal(codon_to_aa(ALL_CODONS_DNA, seq_type = 'DNA'), aa_ans)

})

test_that('Function returns the correct values for RNA seqs', {

  expect_equal(codon_to_aa('UUC', seq_type = 'RNA'), 'Phe')
  expect_equal(codon_to_aa(c('CAU', 'AUG', 'UUU', 'AGU', 'UAG'), seq_type = 'RNA'), c('His', 'Met', 'Phe', 'Ser', 'Stp'))
  expect_equal(codon_to_aa('UAD', seq_type = 'RNA'), character())
  expect_error(codon_to_aa(seq_type = 'RNA'))

  aa_ans = c()
  for (c in ALL_CODONS_RNA) {
    aa_ans = append(aa_ans, names(SYNONYMOUS_CODONS_RNA)[grep(c, SYNONYMOUS_CODONS_RNA)])
  }

  expect_equal(codon_to_aa(ALL_CODONS_RNA, seq_type = 'RNA'), aa_ans)

})

test_that('Function throws appropriate errors', {

  expect_error(codon_to_aa('CAG'), '"seq_type" should be either "DNA" or "RNA".')
  expect_error(codon_to_aa('CAG', seq_type = 'none'), '"seq_type" should be either "DNA" or "RNA".')
  expect_error(codon_to_aa('CAU', seq_type = 'DNA'), '"U" should not be in a DNA sequence.')
  expect_error(codon_to_aa(c('CAT', 'GAC', 'ATG', 'UAA'), seq_type = 'DNA'), '"U" should not be in a DNA sequence.')
  expect_error(codon_to_aa('CAT', seq_type = 'RNA'), '"T" should not be in a RNA sequence.')
  expect_error(codon_to_aa(c('CAU', 'GAC', 'ATG', 'UAA'), seq_type = 'RNA'), '"T" should not be in a RNA sequence.')

})
