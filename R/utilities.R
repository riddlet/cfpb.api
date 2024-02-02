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

#' convert timestamps
#'
#' @return formatted timestamps
convert_timestamps <- function(timestamps, trend_interval='year') {

  if (trend_interval=='year') {
    return(format(as.Date(timestamps, format="%Y-%m-%d"), '%Y'))
  } else if (trend_interval=='quarter') {
    return(format(as.Date(timestamps, format="%Y-%m-%d"), '%Y-%m'))
  } else if (trend_interval=='month') {
    return(format(as.Date(timestamps, format="%Y-%m-%d"), '%Y-%m'))
  } else if (trend_interval=='week') {
    return(format(as.Date(timestamps, format="%Y-%m-%d"), '%Y-%m-%d'))
  } else {
    return(format(as.Date(timestamps, format="%Y-%m-%d"), '%Y-%m-%d'))
  }
}
