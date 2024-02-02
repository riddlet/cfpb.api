#' Suggest Possible Companies based on partial match
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param text required (string) text to use for suggestions
#' @param state (string array) Filter the results to only return results in these states (use abbreviation, i.e. CA, VA)
#' @param zip_code (string array) Filter the results to only return results in a given zip code
#'
#' @return character vector matching text string
#' @export
#'
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' find_company_name(text = "chase")
#' }
find_company_name <- function(text, state = NULL, zip_code = NULL) {
  if (length(text) > 1 || any(is.na(text)) || !all(is.character(text))) {
    stop("text must be a character vector of length 1")
  }
  cfpb_query_list <- as.list(match.call(expand.dots = FALSE))[-1]
  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path("_suggest_company"),
    query = cfpb_query_list
  )
  res <- httr::GET(cfpb_query_path)

  if (res$status_code == get_success_code()) {
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    return(text_res)
  } else if (res$status_code == get_invalid_status_value()) {
    stop(paste("Invalid status value.  HTTP return code:", res$status_code))
  } else {
    stop(paste("HTTP return code:", res$status_code))
  }
}
