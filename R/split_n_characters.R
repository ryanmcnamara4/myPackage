#' Split string every n characters
#'
#' Function splits a string every n characters and returns a list.
#'
#'
#' @param x Character vector
#' @param n Integer - number of characters to split the string(s) on.
#'     Defaults to 3.
#'
#' @return List of character vectors broken into chunks of n characters
#' @export
#'
#' @examples
#' split_n_characters('ACTGACGGG')
#' split_n_characters('This is a sentence.', n = 2)
split_n_characters = function(x, n = 3) {

  if (n < 1) {
    n = 1
  }

  ans = gsub(paste0('(.{', as.character(n), '})'), '\\1s_h', x)
  strsplit(ans, 's_h')

}
