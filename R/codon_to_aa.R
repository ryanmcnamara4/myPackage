#' Convert codons to amino acids
#'
#' Takes a character vector of codons and converts them to their amino acids.
#' Useful for creating an amino acid column from a column of codons, in order to
#' facet a plot for example.
#'
#' @param x Character vector of three nucleotide codons
#'
#' @return Character vector of amino acids
#' @export
#'
#' @examples
#' codon_to_aa(c('CAT', 'GAC', 'TTT', 'ATG'))
#' codon_to_aa('GAT')
codon_to_aa = function(x) {

  aa = c()
  for (c in x) {

    aa = append(aa, names(SYNONYMOUS_CODONS)[grep(c, SYNONYMOUS_CODONS)])

  }

  return(aa)

}
