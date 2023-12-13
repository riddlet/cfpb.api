#' Query for complaint information at aggregate level
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param search_term (string) Return results containing specific term
#' @param agg_type (string) Type of aggregation to return. Options are state (default), product, or issue
#' @param field (string) If the parameter "search_term" has a value, use "field" to specify which field is searched. If not specified, "complaint_what_happened" will be searched.
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
query_aggregates <- function(search_term = NULL, agg_type = 'state', field = 'complaint_what_happened',
                             company = NULL, company_public_response = NULL,
                             company_received_max = NULL, company_received_min = NULL,
                             company_response = NULL, consumer_consent_provided = NULL,
                             consumer_disputed = NULL, date_received_max = NULL,
                             date_received_min = NULL, has_narrative = NULL,
                             issue = NULL, product = NULL, state = NULL,
                             submitted_via = NULL, tags = NULL, timely = NULL,
                             zip_code = NULL)
{
  if (missing(search_term))
  {
    stop('Search term required')
  }

  cat(paste0("Searching for '", search_term, "' in ", field, "\n"))

  cfpb_query_list <- as.list(match.call(expand.dots = FALSE))[-1]
  cfpb_query_list <- lapply(cfpb_query_list, eval.parent, n = 2)

  if (agg_type == 'product')
  {
    cfpb_query_path <- httr::modify_url(
      url = get_cfpb_url(),
      path = get_cfpb_url_path(""),
      query = cfpb_query_list
    )

    res <- httr::GET(cfpb_query_path)
    if (check_response_status(res, cfpb_query_path)) {
      text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      products <- text_res$aggregations$product$product$buckets$key
      sub_prod_list <- text_res$aggregations$product$product$buckets$sub_product.raw$buckets
      outdat <- dplyr::bind_rows(sub_prod_list)
      outdat$product <- rep(products, sapply(sub_prod_list, nrow))
      outdat$subproduct <- outdat$key
      outdat <- outdat[,c('product', 'subproduct', 'doc_count')]
      missing_prods <- text_res$aggregations$product$product$buckets
      missing_prods <- missing_prods[!missing_prods$key %in% outdat$product,]
      missing_prods$product <- missing_prods$key
      missing_prods$subproduct <- missing_prods$key
      missing_prods <- missing_prods[,c('product', 'subproduct', 'doc_count')]
      outdat <- rbind(outdat, missing_prods)
    }
  } else if (agg_type == 'issue')
  {
    cfpb_query_path <- httr::modify_url(
      url = get_cfpb_url(),
      path = get_cfpb_url_path(""),
      query = cfpb_query_list
    )

    res <- httr::GET(cfpb_query_path)
    if (check_response_status(res, cfpb_query_path)) {
      text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      issues <- text_res$aggregations$issue$issue$buckets$key
      subissues <- text_res$aggregations$issue$issue$buckets$sub_issue.raw$buckets
      outdat <- dplyr::bind_rows(subissues)
      outdat$issue <- rep(issues, sapply(subissues, nrow))
      outdat$subissue <- outdat$key
      outdat <- outdat[,c('issue', 'subissue', 'doc_count')]
      missing_issues <- text_res$aggregations$issue$issue$buckets
      missing_issues <- missing_issues[!missing_issues$key %in% outdat$issue,]
      missing_issues$issue <- missing_issues$key
      missing_issues$subissue <- missing_issues$key
      missing_issues <- missing_issues[,c('issue', 'subissue', 'doc_count')]
      outdat <- rbind(outdat, missing_issues)
    }
  } else if (agg_type == 'state')
  {
    cfpb_query_path <- httr::modify_url(
      url = get_cfpb_url(),
      path = get_cfpb_url_path("geo/states"),
      query = cfpb_query_list
    )
    res <- httr::GET(cfpb_query_path)
    if (check_response_status(res, cfpb_query_path)){
      text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      outdat <- data.frame(
        state = text_res$aggregations$state$state$buckets$key,
        doc_count = text_res$aggregations$state$state$buckets$doc_count
      )
    }
  } else
  {
    stop("Unknown aggregation type. Specify 'state', 'product', or 'issue'.")
  }
  return(outdat)
}
