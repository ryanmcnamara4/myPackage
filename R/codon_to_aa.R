#' Convert codons to amino acids
#'
#' Takes a character vector of codons and converts them to their amino acids.
#' Useful for creating an amino acid column from a column of codons, in order to
#' facet a plot for example.
#'
#' @param x Character vector of three nucleotide codons
#' @param seq_type Character value of either "DNA" or "RNA" depending on whether
#'     "T" or "U" is used in the sequence.
#'
#' @return Character vector of amino acids
#' @export
#'
#' @examples
#' codon_to_aa(c('CAT', 'GAC', 'TTT', 'ATG'), seq_type = 'DNA')
#' codon_to_aa('GAT', seq_type = 'DNA')
#' codon_to_aa(c('CAU', 'GAC', 'UUU', 'AUG'), seq_type = 'RNA')
#' codon_to_aa('GAU', seq_type = 'RNA')
codon_to_aa = function(x, seq_type = c('DNA', 'RNA')) {

  if (length(seq_type) > 1) {
    stop('"seq_type" should be either "DNA" or "RNA".')
  }

  if (seq_type == 'DNA') {
    SYNONYMOUS_CODONS = SYNONYMOUS_CODONS_DNA
    if (any(stringr::str_detect(x, 'U'))) {
      stop('"U" should not be in a DNA sequence.')
    }
  } else if (seq_type == 'RNA') {
    SYNONYMOUS_CODONS = SYNONYMOUS_CODONS_RNA
    if (any(stringr::str_detect(x, 'T'))) {
      stop('"T" should not be in a RNA sequence.')
    }
  } else {
    stop('"seq_type" should be either "DNA" or "RNA".')
  }

  aa = c()
  for (c in x) {

    aa = append(aa, names(SYNONYMOUS_CODONS)[grep(c, SYNONYMOUS_CODONS)])

  }

  return(aa)

}
