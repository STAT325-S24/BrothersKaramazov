#' Package 'BrothersKaramazov'
#'
#' This package contains a dataset of all the lines 
#' of The Brothers Karamazov by Fyodor Dostoyevsky.  
#' Included are the line, part, book, and chapter numbers, 
#' for use when analyzing the text, provided by Project Gutenberg.
#'
#' @format ## `BrothersKaramazov`
#' A data frame with 30801 rows and 6 columns:
#' \describe{
#'   \item{gutenberg_id}{The ID of this work. The same for all rows.}
#'   \item{text}{Each entry represents one line of the work, about 60 characters.}
#'   \item{linenumber}{The line number of the work this line is from.}
#'   \item{part}{The part of the work this line is from.}
#'   \item{book}{The book of the work this line is from.}
#'   \item{chapter}{The chapter of the work this line is from.  Note, the book repeats chapters, but this does not.}
#'   \item{book_chapter}{The chapter number from the book.  The chapters here cycle, starting over at 1 everytime there is a new part.}
#' }
#' @source <https://www.gutenberg.org/ebooks/28054>
"BrothersKaramazov"
#' Annotated Brothers K
#'
#' An annotated version of "The Brothers Karamazov."  See the cleanNLP package 
#' for more information on what this entails.
#'  
#' @format ## `AnnotatedBK`
#' A list with 3 elements:
#' \describe{
#'   \item{token}{Contains token information}
#'   \item{entity}{Contains entity information}
#'   \item{document}{Contains document information}
#' }
#' @source <https://www.gutenberg.org/ebooks/28054>
"AnnotatedBK"



