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
get_success_code <- function(){
  return(200)
}

#' HTTP invalid code
#'
#' @return code
get_invalid_status_value <- function() {
  return(400)
}
