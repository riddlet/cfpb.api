#' Query For Complaints
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param search_term (string) Return results containing specific term
#' @param field (string) If the parameter "search_term" has a value, use "field" to specify which field is searched. If not specified, "complaint_what_happened" will be searched.
#' @param frm (integer) Return results starting from a specific index, only if format parameter is not specified, ignore otherwise
#' @param size (integer) Limit the size of the results.  Max limit is 1000
#' @param sort (string) Return results sort in a particular order
#' @param format (string) Format to be returned, if this parameter is not specified, frm/size parameters can be used properly, but if a format is specified for exporting, frm/size will be ignored
#' @param no_aggs (boolean) Include aggregations in result or not, True means no aggregations will be included, False means aggregations will be included.
#' @param no_highlight (boolean) Include highlight of search term in result or not, True means no highlighting will be included, False means highlighting will be included.
#' @param company (string array) Filter the results to only return these companies
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
#' @return A data frame:
#' \describe{
#'   \item{\code{tags}}{Data that supports easier searching and sorting of complaints submitted by or on behalf of consumers.}
#'   \item{\code{zip_code}}{string}
#'   \item{\code{complaint_id}}{string}
#'   \item{\code{issue}}{factor The issue the consumer identified in the complaint}
#'   \item{\code{date_received}}{POSIXlt The date the CFPB received the complaint}
#'   \item{\code{state}}{factor}
#'   \item{\code{consumer_disputed}}{factor Whether the consumer disputed the company’s response}
#'   \item{\code{product}}{factor The type of product the consumer identified in the complaint}
#'   \item{\code{has_narrative}}{boolean}
#'   \item{\code{company_response}}{factor }
#'   \item{\code{company}}{string The complaint is about this company}
#'   \item{\code{submitted_via}}{factor How the complaint was submitted to the CFPB}
#'   \item{\code{date_sent_to_company}}{POSIXlt The date the CFPB sent the complaint to the company}
#'   \item{\code{company_public_response}}{string The company's optional, public-facing response to a consumer's complaint}
#'   \item{\code{sub_product}}{factor The type of sub-product the consumer identified in the complaint}
#'   \item{\code{timely}}{factor Whether the company gave a timely response}
#'   \item{\code{complaint_what_happened}}{string Consumer complaint narrative is the consumer-submitted description of "what happened" from the complaint.}
#'   \item{\code{sub_issue}}{factor The sub-issue the consumer identified in the complaint}
#'   \item{\code{consumer_consent_provided}}{factor Identifies whether the consumer opted in to publish their complaint narrative.}
#'}
#'
#' @export
#'
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'   query_complaints(size = 1)
#' }
#'
query_complaints <- function(search_term = NULL, field = NULL, frm = NULL,
                             size = NULL, sort = NULL, format = NULL,
                             no_aggs = NULL, no_highlight = NULL,
                             company = NULL, company_public_response = NULL,
                             company_received_max = NULL, company_received_min = NULL,
                             company_response = NULL, consumer_consent_provided = NULL,
                             consumer_disputed = NULL, date_received_max = NULL,
                             date_received_min = NULL, has_narrative = NULL,
                             issue = NULL, product = NULL, state = NULL,
                             submitted_via = NULL, tags = NULL, timely = NULL,
                             zip_code = NULL)
{
  if (!is.null(field) & !is.null(search_term))
  {
    cat(paste0("Searching for ", search_term, " in ", field, "\n"))
  } else if (!is.null(field))
  {
    cat(paste0("Searching for ", search_term, " in complaint_what_happened\n"))
  }

  if (!is.null(format))
    cat("format is specified, ingoring frm and size\n")

  if (size > 1000)
    warning("Only 1,000 results will be returned")

  cfpb_query_list <- as.list(match.call(expand.dots = FALSE))[-1]
  #print(cfpb_query_list)
  #print(lapply(cfpb_query_list, eval))
  #cfpb_query_list <- lapply(cfpb_query_list, eval)
  cfpb_query_list <- lapply(cfpb_query_list, eval.parent, n = 2)
  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path(""),
    query = cfpb_query_list
  )
  #print(cfpb_query_path)
  res <- httr::GET(cfpb_query_path)

  if (res$status_code == get_success_code())
  {
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    res_data <- text_res$hits$hits$`_source`
    res_data$consumer_consent_provided <- as.factor(res_data$consumer_consent_provided)
    res_data$state <- as.factor(res_data$state)
    res_data$product <- as.factor(res_data$product)
    res_data$sub_product <- as.factor(res_data$sub_product)
    res_data$issue <- as.factor(res_data$issue)
    res_data$sub_issue <- as.factor(res_data$sub_issue)
    res_data$timely <- as.factor(res_data$timely)
    res_data$date_received <- strptime(res_data$date_received, "%Y-%m-%dT%H:%M:%S")
    res_data$date_sent_to_company <- strptime(res_data$date_sent_to_company, "%Y-%m-%dT%H:%M:%S")
    res_data$submitted_via <- as.factor(res_data$submitted_via)
    res_data$company_response <- as.factor(res_data$company_response)
    return(res_data)
  } else if (res$status_code == get_invalid_status_value())
  {
    cat(cfpb_query_path, "\n")
    stop(paste("Invalid status value.  HTTP return code:", res$status_code))
  } else
  {
    cat(cfpb_query_path, "\n")
    stop(paste("HTTP return code:", res$status_code))
  }
}
