#' Query For Complaints
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param search_term (string) Return results containing specific term
#' @param field (string) If the parameter "search_term" has a value, use "field" to specify which field is searched. If not specified, "complaint_what_happened" will be searched.
#' @param size (integer) Limit the size of the results.  Max limit is 10000
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
query_complaints <- function(search_term = NULL, field = 'complaint_what_happened',
                             size = 10000, company = NULL, company_public_response = NULL,
                             company_received_max = NULL, company_received_min = NULL,
                             company_response = NULL, consumer_consent_provided = NULL,
                             consumer_disputed = NULL, date_received_max = NULL,
                             date_received_min = NULL, has_narrative = NULL,
                             issue = NULL, product = NULL, state = NULL,
                             submitted_via = NULL, tags = NULL, timely = NULL,
                             zip_code = NULL, page=TRUE)
{
  if (missing(search_term))
  {
    stop('Search term required')
  }

  if (size > 10000)
  {
    warning("Only 10,000 results will be returned")
    size <- 10000
  }

  cat(paste0("Searching for '", search_term, "' in ", field, "\n"))

  cfpb_query_list <- as.list(match.call.defaults(expand.dots = FALSE))[-1]
  cfpb_query_list <- lapply(cfpb_query_list, eval.parent, n = 2)
  # don't want page as an actual argument to the API, as no such thing exists
  cfpb_query_list$page <- NULL
  if (page==TRUE){
    res_data <- query_page(cfpb_query_list)
  } else {
    res_data <- query_nopage(cfpb_query_list)
  }
  return(res_data)
}

#' Query without paging
#'
#' @param cfpb_query_list (list) A list of arguments to be passed in the query URL
#'
query_no_page <- function(cfpb_query_list) {

  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path(""),
    query = cfpb_query_list
  )

  res <- httr::GET(cfpb_query_path)
  if (check_response_status(res, cfpb_query_path)) {
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    res_data <- to_dataframe(text_res)
  }
  cat(
    paste(
      'Returning',
      dim(res_data)[1],
      'complaints of',
      text_res$hits$total$value,
      'hits'
    )
  )
  return(res_data)
}

#' Query with paging
#'
#' @param cfpb_query_list (list) A list of arguments to be passed in the query URL
#'
query_page <- function(cfpb_query_list){

  cfpb_query_list$sort <- 'created_date_desc'
  cfpb_query_list$size <- 10000
  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path(""),
    query = cfpb_query_list
  )

  #initial request ------------------
  res <- httr::GET(cfpb_query_path)
  if (check_response_status(res, cfpb_query_path)){
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    res_data <- to_dataframe(text_res)
  }

  total_hits <- text_res$hits$total$value
  #is paging necessary?
  if (total_hits > 10000) {
    remaining_dat <- TRUE
    n_iterations <- ceiling(total_hits/10000)
    pb <- utils::txtProgressBar(label = "Accessing Complaint API:", style = 3)
    i <- 1


    while(remaining_dat == TRUE) {

      #Using date as a paging mechanism -----------------------------
      cfpb_query_list$date_received_max <- as.Date(
        min(res_data$date_received), format="%Y-%m-%d"
      )

      #construct query -------------------
      cfpb_query_path <- httr::modify_url(
        url = get_cfpb_url(),
        path = get_cfpb_url_path(""),
        query = cfpb_query_list
      )

      #get response ---------------------
      res <- httr::GET(cfpb_query_path)
      if (check_response_status(res, cfpb_query_path)) {
        text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
        tmp_dat <- to_dataframe(text_res)

        #append to dataframe, removing duplicates --------------
        res_data <- rbind(
          res_data,
          tmp_dat[!tmp_dat$complaint_id %in% res_data$complaint_id,]
        )
        # stop?
        utils::setTxtProgressBar(pb, i/(n_iterations-1))
        i <- i+1
        remaining_dat <- ifelse(total_hits > dim(res_data)[1], TRUE, FALSE)
      }
    }
  } else {
    return(res_data)
  }

}

#' Construct dataframe from text-ified response
#'
#' @param text_res (string) The content of the API response, converted using jsonlite::FromJSON
#'
to_dataframe <- function(text_res){
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
}

#' Check the API response to be sure it's a-okay
#'
#' @param query_results (string) A list of arguments to be passed in the query URL
check_response_status <- function(query_results, cfpb_query_path) {
  if (query_results$status_code == get_success_code())
  {
    return(TRUE)
  } else if (query_results$status_code == get_invalid_status_value())
  {
    cat(cfpb_query_path, "\n")
    stop(paste("Invalid status value.  HTTP return code:", query_results$status_code))
  } else
  {
    cat(cfpb_query_path, "\n")
    stop(paste("HTTP return code:", query_results$status_code))
  }
}
