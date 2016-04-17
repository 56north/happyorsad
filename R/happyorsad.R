#' Sentiment scores a string
#'
#' @name happyorsad
#'
#' @param string: a text string
#' @param language: "en" for english, "da" for danish, "emoticon" for emoticons.
#'
#' @description
#' happyorsad sentiment-scores a string of text. It uses the AFINN sentiment scoring. It assigns a value to each recognized word or emoticon in the string and returns the sum.
#'
#' @return
#' The function returns the sum of sentiment.
#'
#' @export
#'
#' @examples
#' # Examples of sentiment scoring
#'
#' library(happyorsad)
#'
#' # Score danish words
#' string_da <- 'Hvis ikke det er det mest afskyelige elendige flueknepperi...'
#' happyorsad(string_da, "da")
#'
#' # Score english words
#' string_en <- 'This is utterly excellent!'
#' happyorsad(string_en, "en")
#'
#' # Score emoticons
#' string_emoticon <- 'I saw that yesterday :)'
#' happyorsad(string_emoticon, "emoticon")


happyorsad <- function(string, language = "da"){

  if(language == "da"){
    split_string <- stringr::str_replace_all(string, "[[:punct:]]", "")
    split_string <- unlist(stringr::str_split(split_string, " "))
    split_string <- stringr::str_to_lower(split_string)

    score <- lapply(split_string, function(x){AFINN_da_32$score[AFINN_da_32$word %in% x]})
    score <- sum(unlist(score))
    }

  if(language == "en"){
    split_string <- stringr::str_replace_all(string, "[[:punct:]]", "")
    split_string <- unlist(stringr::str_split(split_string, " "))
    split_string <- stringr::str_to_lower(split_string)

    score <- lapply(split_string, function(x){AFINN_en_165$score[AFINN_en_165$word %in% x]})
    score <- sum(unlist(score))
    }

  if(language == "emoticon"){
    split_string <- stringr::str_replace_all(string, "[^[:punct:]]", "")
    split_string <- unlist(stringr::str_split(split_string, " "))
    split_string <- stringr::str_to_lower(split_string)

    score <- lapply(split_string, function(x){AFINN_emoticon_8$score[AFINN_emoticon_8$word %in% x]})
    score <- sum(unlist(score))
    }

  return(score)
}
