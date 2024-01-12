# Copyright 2021 Robert Carnell

#' Get CFPB base URL
#'
#' @return CFPB base URL
get_cfpb_url <- function() {
  return("https://www.consumerfinance.gov")
}

#' Get CFPB URL path
#'
#' @param pathend the string or string vector of ending url paths
#'
#' @return CFPB URL path
get_cfpb_url_path <- function(pathend) {
  return(c("data-research", "consumer-complaints", "search", "api", "v1", pathend))
}

#' HTTP success code
#'
#' @return code
get_success_code <- function() {
  return(200)
}

#' HTTP invalid code
#'
#' @return code
get_invalid_status_value <- function() {
  return(400)
}

#' Get function arguments, including defaults
#'
#' @return arguments
match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for (i in setdiff(names(formals), names(call))) {
    call[i] <- list(formals[[i]])
  }


  match.call(sys.function(sys.parent()), call)
}
