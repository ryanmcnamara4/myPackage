test_that('Function throws the correct warnings and errors', {

  expect_error(get_rscu(10, seq_type = 'DNA', as.data.frame = TRUE),
               'needs to be a character vector, a XString, or a XStringSet')
  expect_warning(get_rscu('ACGT', seq_type = 'DNA', as.data.frame = TRUE),
                 'Not all sequences are divisible by 3.')
  expect_error(get_rscu('ACGPE', seq_type = 'DNA', as.data.frame = FALSE),
               'not in lookup table')
  expect_error(get_rscu('ACT', as.data.frame = TRUE),
               '"seq_type" should be either "DNA" or "RNA".')
  expect_error(get_rscu('ACT', seq_type = 'none'),
               '"seq_type" should be either "DNA" or "RNA".')

})

test_that('Function handles different seq inputs correctly for single seq', {

  seq = 'AAAAAG'
  ans = get_rscu(seq, seq_type = 'DNA', as.data.frame = TRUE)

  indexes = which(colnames(ans) %in% c('AAA', 'AAG'))

  expect_equal(as.numeric(ans[indexes]), c(1, 1))
  expect_true(all(is.nan(as.numeric(ans[-indexes]))))

  seq_dna = Biostrings::DNAString(seq)
  ans = get_rscu(seq_dna, seq_type = 'DNA', as.data.frame = TRUE)

  expect_equal(as.numeric(ans[indexes]), c(1, 1))
  expect_true(all(is.nan(as.numeric(ans[-indexes]))))

  seq_dna_set = Biostrings::DNAStringSet(seq)
  ans = get_rscu(seq_dna_set, seq_type = 'DNA', as.data.frame = TRUE)

  expect_equal(as.numeric(ans[indexes]), c(1, 1))
  expect_true(all(is.nan(as.numeric(ans[-indexes]))))

  seq_rna = Biostrings::RNAString(seq)
  ans = get_rscu(seq_rna, seq_type = 'RNA', as.data.frame = TRUE)

  expect_equal(as.numeric(ans[indexes]), c(1, 1))
  expect_true(all(is.nan(as.numeric(ans[-indexes]))))

  seq_rna_set = Biostrings::RNAStringSet(seq)
  ans = get_rscu(seq_rna_set, seq_type = 'RNA', as.data.frame = TRUE)

  expect_equal(as.numeric(ans[indexes]), c(1, 1))
  expect_true(all(is.nan(as.numeric(ans[-indexes]))))

})

test_that('Function handles different seq inputs for multiple seqs', {

  file = system.file('extdata', 'human_viruses.xls', package = 'myPackage')
  data = readxl::read_excel(file, sheet = 'Codons')
  data = data[1:10, ]

  amino_acids = codon_to_aa(ALL_CODONS_DNA, seq_type = 'DNA')

  rscu = c()
  for (c in seq_along(ALL_CODONS_DNA)) {

    codon = ALL_CODONS_DNA[c]
    aa = amino_acids[c]

    n = length(SYNONYMOUS_CODONS_DNA[[aa]])

    if (n == 1) {
      temp_rscu = n * data[[codon]] / data[SYNONYMOUS_CODONS_DNA[[aa]]]
    } else {
      temp_rscu = n * data[[codon]] / rowSums(data[SYNONYMOUS_CODONS_DNA[[aa]]])
    }

    rscu = cbind(rscu, temp_rscu)

  }
  names(rscu) = ALL_CODONS_DNA

  seqs = data$coding
  expect_equal(get_rscu(seqs, seq_type = 'DNA', as.data.frame = TRUE), rscu)

  seqs_dna_set = Biostrings::DNAStringSet(seqs)
  expect_equal(get_rscu(seqs_dna_set, seq_type = 'DNA', as.data.frame = TRUE), rscu)

  seqs = stringr::str_replace_all(seqs, 'T', 'U')
  names(rscu) = stringr::str_replace_all(names(rscu), 'T', 'U')
  seqs_rna_set = Biostrings::RNAStringSet(seqs)
  expect_equal(get_rscu(seqs_rna_set, seq_type = 'RNA', as.data.frame = TRUE), rscu)

})
