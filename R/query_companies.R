# Copyright 2021 Robert Carnell

#' Suggest Possible Companies based on partial match
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param text required (string) text to use for suggestions
#' @param size (integer) Limit the size of the results
#' @param company_public_response (string array) Filter the results to only return these types of public response by the company
#' @param company_received_max (string) Return results with date < company_received_max (i.e. 2017-03-04)
#' @param company_received_min (string) Return results with date >= company_received_min (i.e. 2017-03-04)
#' @param company_response (string array) Filter the results to only return these types of response by the company
#' @param consumer_consent_provided (string array) Filter the results to only return these types of consent consumer provided
#' @param consumer_disputed (string array) Filter the results to only return the specified state of consumer disputed, i.e. yes, no
#' @param date_received_max (string) Return results with date < date_received_max (i.e. 2017-03-04)
#' @param date_received_min (string) Return results with date >= date_received_min (i.e. 2017-03-04)
#' @param has_narrative (string array) Filter the results to only return the specified state of whether it has narrative in the complaint or not, i.e. yes, no
#' @param issue (string array) Filter the results to only return these types of issue and subissue, i.e. product-only: Getting a Loan, subproduct needs to include product, separated by '•', Getting a Loan•Cant qualify for a loan
#' @param product (string array) Filter the results to only return these types of product and subproduct, i.e. product-only: Mortgage, subproduct needs to include product, separated by '•', Mortgage•FHA mortgage
#' @param state (string array) Filter the results to only return these states (use abbreviation, i.e. CA, VA)
#' @param submitted_via (string array) Filter the results to only return these types of way consumers submitted their complaints
#' @param tags (string array) Filter the results to only return these types of tag
#' @param timely (string array) Filter the results to show whether a response was timely
#' @param zip_code (string array) Zip Code
#'
#' @return character vector matching text string
#' @export
#'
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'   query_companies(text = "chase")
#'}
query_companies <- function(text, size = NULL, company_public_response = NULL,
                            company_received_max = NULL, company_received_min = NULL,
                            company_response = NULL, consumer_consent_provided = NULL,
                            consumer_disputed = NULL, date_received_max = NULL,
                            date_received_min = NULL, has_narrative = NULL,
                            issue = NULL, product = NULL, state = NULL,
                            submitted_via = NULL, tags = NULL, timely = NULL,
                            zip_code = NULL)
{
  if (length(text) > 1 || any(is.na(text)) || !all(is.character(text)))
  {
    stop("text must be a character vector of length 1")
  }
  cfpb_query_list <- as.list(match.call(expand.dots = FALSE))[-1]
  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path("_suggest_company"),
    query = cfpb_query_list
  )
  res <- httr::GET(cfpb_query_path)

  if (res$status_code == get_success_code())
  {
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    return(text_res)
  } else if (res$status_code == get_invalid_status_value())
  {
    stop(paste("Invalid status value.  HTTP return code:", res$status_code))
  } else
  {
    stop(paste("HTTP return code:", res$status_code))
  }
}

