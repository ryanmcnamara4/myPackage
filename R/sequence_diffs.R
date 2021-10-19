#' Find single character differences in sequences
#'
#' Function takes a baseline sequence and any number of sequences to compare to
#' and determines the number or the positions of single character differences.
#' Note that this function can be used with amino acid sequences
#' (1 letter abbreviation) as well.
#'
#'
#' @param baseline_seq Character vector of length one that all other sequences
#'     are being compared to.
#' @param compare_seqs Character vector of any length that will be compared to
#'     the baseline seq.
#' @param output Either "num" (return the number of differences) or "position"
#'     (return the positions of the differences).
#' @param ignore_case Logical value (TRUE / FALSE) designating whether the
#'     function should ignore case. Defaults to TRUE.
#'
#' @return Either a list of numeric vectors describing the positions of
#'     differences or a numeric vector of the number of differences.
#' @export
#'
#' @examples
#' nucleotide_diff('AGTGGG', 'CGTGAG', output = 'num')
#'
#' seq1 = 'ACTGACTGG'
#' seq2 = c('ACGGGCCGG', 'ACTGACTGG', 'GCTCAGTGG')
#' nucleotide_diff(seq1, seq2, output = 'num', ignore_case = TRUE)
#' nucleotide_diff(seq1, seq2, output = 'position', ignore_case = FALSE)
#'
#' \dontrun{
#' nucleotide_diff(seq1, seq2)
#' nucleotide_diff('AGTCGA', 'TTA')
#' }
nucleotide_diff = function(baseline_seq, compare_seqs, output = c('num', 'position'), ignore_case = TRUE) {

  if (length(output) > 1) stop('Choose either "num" or "position" for the output.')
  if (!(output %in% c('num', 'position'))) stop('Choose either "num" or "position" for the output.')
  if (length(baseline_seq) > 1) stop('"baseline_seq" should only be one sequence.')
  if (!all(nchar(baseline_seq) == nchar(compare_seqs))) stop('All sequences being compared need to have the same number of characters.')

  if (ignore_case) {

    baseline_seq = toupper(baseline_seq)
    compare_seqs = toupper(compare_seqs)

  }

  baseline_seq = unlist(strsplit(baseline_seq, ''))
  compare_seqs = strsplit(compare_seqs, '')

  position_diffs = lapply(compare_seqs, function(x) which(x != baseline_seq))
  num_diffs = sapply(position_diffs, length)

  if (output == 'num') {

    return(num_diffs)

  } else {

    return(position_diffs)

  }

}

#' Find codon differences between sequences
#'
#' Function takes a baseline sequence and any number of sequences to compare to
#' and determines the number or the positions of the codon (3 characters)
#' differences.
#'
#' @param baseline_seq Character vector of length one that all other sequences
#'     are being compared to.
#' @param compare_seqs Character vector of any length that will be compared to
#'     the baseline sequence.
#' @param diff_type Either "synonymous", "non-synonymous", or "all".
#'     \itemize{
#'         \item "all" : find all the codon differences
#'         \item "non-synonymous" : find all of the non-synonymous codon differences
#'         \item "synonymous" : find all of the synonymous codon differences
#'     }
#' @param output Either "num" (return the number of differences) or "position"
#'     (return the positions of the differences).
#' @param ignore_case Logical value (TRUE / FALSE) designating whether the
#'     function should ignore case. Defaults to TRUE.
#' @param seq_type Character value of "DNA" or "RNA" describing whether the sequences
#'     use "T" or "U".
#'
#' @return Either a list of numeric vectors describing the positions of the
#'     differences or a numeric vector of the number of differences.
#' @export
#'
#' @examples
#' codon_diff('AGTGGG', 'CGTGAG', output = 'num', diff_type = 'all', seq_type = 'DNA')
#'
#' seq1 = 'ACTGACTGG'
#' seq2 = c('ACGGGCCGG', 'ACTGACTGG', 'GCTCAGTGG')
#' codon_diff(seq1, seq2, output = 'num', diff_type = 'synonymous',
#'     seq_type = 'DNA', ignore_case = TRUE)
#' codon_diff(seq1, seq2, output = 'position', diff_type = 'non-synonymous',
#'     seq_type = 'DNA', ignore_case = FALSE)
#'
#' seq1 = 'ACUGACUGG'
#' seq2 = c('ACGGGCCGG', 'ACUGACUGG', 'GCUCAGUGG')
#' codon_diff(seq1, seq2, output = 'num', diff_type = 'synonymous',
#'     seq_type = 'RNA', ignore_case = TRUE)
#' codon_diff(seq1, seq2, output = 'position', diff_type = 'non-synonymous',
#'     seq_type = 'RNA', ignore_case = FALSE)
#'
#' \dontrun{
#' codon_diff(seq1, seq2)
#' codon_diff('AGTCGA', 'TTA')
#' }
codon_diff = function(baseline_seq, compare_seqs, diff_type = c('synonymous', 'non-synonymous', 'all'), output = c('num', 'position'),
                      seq_type = c('DNA', 'RNA'), ignore_case = TRUE) {

  if (length(baseline_seq) > 1) stop('"baseline_seq" should only be one sequence.')
  if (length(seq_type) > 1) stop('"seq_type" should be either "DNA" or "RNA".')
  if (nchar(baseline_seq) %% 3 != 0) warning('"baseline_seq" is not divisible by 3.')
  if (!all(nchar(compare_seqs) %% 3 == 0)) warning('One or more "compare_seqs" are not divisible by 3.')
  if (length(output) > 1) stop('Choose either "num" or "position" for the output.')
  if (!(output %in% c('num', 'position'))) stop('Choose either "num" or "position" for the output.')
  if (length(diff_type) > 1) stop('Choose "synonymous", "non-synonymous", or "all" for the "diff_type".')
  if (!(diff_type %in% c('synonymous', 'non-synonymous', 'all'))) stop('Choose "synonymous", "non-synonymous", or "all" for the "diff_type".')
  if (!all(nchar(baseline_seq) == nchar(compare_seqs))) stop('All sequences being compared need to have the same number of characters.')

  if (ignore_case) {

    baseline_seq = toupper(baseline_seq)
    compare_seqs = toupper(compare_seqs)

  }

  baseline_seq = unlist(split_n_characters(baseline_seq))
  compare_seqs = split_n_characters(compare_seqs)

  all_position_diffs = lapply(compare_seqs, function(x) which(x != baseline_seq))
  all_num_diffs = sapply(all_position_diffs, length)

  baseline_seq_aa = codon_to_aa(baseline_seq, seq_type = seq_type)
  compare_seqs_aa = lapply(compare_seqs, function(x) codon_to_aa(x, seq_type = seq_type))
  ns_position_diffs = sapply(compare_seqs_aa, function(x) which(x != baseline_seq_aa))
  ns_num_diffs = sapply(ns_position_diffs, length)

  if (diff_type == 'all') {

    position_diffs = all_position_diffs
    num_diffs = all_num_diffs

  } else if (diff_type == 'non-synonymous') {

    position_diffs = ns_position_diffs
    num_diffs = ns_num_diffs

  } else {

    position_diffs = list()
    num_diffs = c()
    for (i in seq_along(all_position_diffs)) {

      position_diffs = append(position_diffs, list(setdiff(all_position_diffs[[i]], ns_position_diffs[[i]])))
      num_diffs = append(num_diffs, all_num_diffs[[i]] - ns_num_diffs[[i]])

    }

  }

  if (output == 'num') {

    return(num_diffs)

  } else {

    return(position_diffs)

  }

}



