#' Query for complaint trends
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param search_term (string) Return results containing specific term
#' @param lens (string) The data lens through which to view complaint trends over time. Available values are overview (default), issue, product, or tags.
#' @param trend_interval (string) The interval of time to use for aggregations. When using day intervals, we recommend querying for periods of less than one year. Available values: year (default), quarter, month, week, day
#' @param field (string) If the parameter "search_term" has a value, use "field" to specify which field is searched. Available values: complaint_what_happened (default), company_public_response, all.
#' @param company (string array) Filter the results to only return these companies
#' @param company_public_response (string array) Filter the results to only return these types of public response by the company
#' @param company_received_max (string) Return results with date < company_received_max (i.e. 2017-03-04)
#' @param company_received_min (string) Return results with date >= company_received_min (i.e. 2017-03-04)
#' @param company_response (string array) Filter the results to only return these types of response by the company. Available strings are 'Closed', 'Closed with explanation', 'Closed with monetary relief', 'Closed with non-monetary relief', 'Closed with relief', 'Closed without relief', 'In progress', or 'Untimely response'.
#' @param consumer_consent_provided (string array) Filter the results to only return these types of consent consumer provided. Available strings are 'Consent not provided', 'Consent provided', 'Consent withdrawn', 'N/A', or 'Other'.
#' @param consumer_disputed (string array) Filter the results to only return the specified state of consumer disputed. Available values are 'Yes', 'No', or 'N/A'.
#' @param date_received_max (string) Return results with date < date_received_max (i.e. 2017-03-04)
#' @param date_received_min (string) Return results with date >= date_received_min (i.e. 2017-03-04)
#' @param has_narrative (string array) Filter the results to only return the specified state of whether it has narrative in the complaint or not. Available values are 'true', or 'false'.
#' @param issue (string array) Filter the results to only return these types of issue and subissue, i.e. product-only: Getting a Loan, subproduct needs to include product, separated by '•', Getting a Loan•Cant qualify for a loan
#' @param product (string array) Filter the results to only return these types of product and subproduct, i.e. product-only: Mortgage, subproduct needs to include product, separated by '•', Mortgage•FHA mortgage
#' @param state (string array) Filter the results to only return these states (use abbreviation, i.e. CA, VA)
#' @param submitted_via (string array) Filter the results to only return these types of way consumers submitted their complaints. Available values are 'Fax', 'Phone', 'Postal mail', 'Referral', 'Web', or 'Web Referral'.
#' @param tags (string array) Filter the results to only return these types of tag. Available tags are 'Older American', 'Servicemember', and 'Older American, Servicemember'.
#' @param timely (string array) Filter the results to show whether a response was timely. Available values are 'Yes', or 'No'.
#' @param trend_depth (integer) The top X trend aggregations will be returned, where X is the supplied trend_depth. Defaults to 10
#' @param zip_code (string array) Zip Code
#'
#' @return A data frame containing the lens categories (e.g. product/issue/tag categories) and:
#' \describe{
#'   \item{\code{timestamp}}{string}
#'   \item{\code{doc_count}}{Number of complaints returned from the search}
#' }
#'
#' @export
#'
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' query_complaints(size = 1)
#' }
#'
query_trends <- function(search_term = NULL, lens = "overview", trend_interval = "year",
                         field = "complaint_what_happened", company = NULL,
                         company_public_response = NULL, company_received_max = NULL,
                         company_received_min = NULL, company_response = NULL,
                         consumer_consent_provided = NULL, consumer_disputed = NULL,
                         date_received_max = NULL, date_received_min = NULL,
                         has_narrative = NULL, issue = NULL, product = NULL,
                         state = NULL, submitted_via = NULL, tags = NULL,
                         timely = NULL, trend_depth = 10, zip_code = NULL) {
  cat(paste0("Searching for '", search_term, "' in ", field, "\n"))

  cfpb_query_list <- as.list(match.call.defaults(expand.dots = FALSE))[-1]
  cfpb_query_list <- lapply(cfpb_query_list, eval.parent, n = 2)
  cfpb_query_list$sub_lens <- "company"

  if (is.null(cfpb_query_list$lens)) {
    cfpb_query_list$lens <- "overview"
  }
  if (is.null(cfpb_query_list$trend_interval)) {
    cfpb_query_list$trend_interval <- "year"
  }
  # print(cfpb_query_list)

  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path("trends"),
    query = cfpb_query_list
  )
  res <- httr::GET(cfpb_query_path)

  if (check_response_status(res, cfpb_query_path)) {
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    if (lens == "overview") {
      outdat <- text_res$aggregations$dateRangeBrush$dateRangeBrush$buckets
      outdat$timestamp <- convert_timestamps(
        outdat$key_as_string, cfpb_query_list$trend_interval
        )
      outdat <- outdat[, c("timestamp", "doc_count")]
      return(outdat)
    } else if (lens == "product") {
      outdat_list <- text_res$aggregations$product$product$buckets$trend_period$buckets
      products <- text_res$aggregations$product$product$buckets$key
      outdat <- dplyr::bind_rows(outdat_list)
      outdat$product <- rep(products, sapply(outdat_list, nrow))
      outdat$timestamp <- convert_timestamps(
        outdat$key_as_string, cfpb_query_list$trend_interval
      )
      outdat <- outdat[, c("product", "timestamp", "doc_count")]
      return(outdat)
    } else if (lens == "issue") {
      outdat_list <- text_res$aggregations$issue$issue$buckets$trend_period$buckets
      issues <- text_res$aggregations$issue$issue$buckets$key
      outdat <- dplyr::bind_rows(outdat_list)
      outdat$issue <- rep(issues, sapply(outdat_list, nrow))
      outdat$timestamp <- convert_timestamps(
        outdat$key_as_string, cfpb_query_list$trend_interval
      )
      outdat <- outdat[, c("issue", "timestamp", "doc_count")]
      return(outdat)
    } else if (lens == "tags") {
      outdat_list <- text_res$aggregations$tags$tags$buckets$trend_period$buckets
      tags <- text_res$aggregations$tags$tags$buckets$key
      outdat <- dplyr::bind_rows(outdat_list)
      outdat$tag <- rep(tags, sapply(outdat_list, nrow))
      outdat$timestamp <- convert_timestamps(
        outdat$key_as_string, cfpb_query_list$trend_interval
      )
      outdat <- outdat[, c("tag", "timestamp", "doc_count")]
      return(outdat)
    }
  }
}
