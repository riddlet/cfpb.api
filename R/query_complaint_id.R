#' Query For an individual complaint
#'
#' API docs:  https://cfpb.github.io/api/ccdb/
#'
#' @param complaintID (integer) ID of the complaint
#'
#' @return A data frame:
#' \describe{
#'   \item{\code{product}}{factor The type of product the consumer identified in the complaint}
#'   \item{\code{complaint_what_happened}}{string Consumer complaint narrative is the consumer-submitted description of "what happened" from the complaint.}
#'   \item{\code{date_sent_to_company}}{POSIXlt The date the CFPB sent the complaint to the company}
#'   \item{\code{issue}}{factor The issue the consumer identified in the complaint}
#'   \item{\code{sub_product}}{factor The type of sub-product the consumer identified in the complaint}
#'   \item{\code{zip_code}}{string}
#'   \item{\code{tags}}{Data that supports easier searching and sorting of complaints submitted by or on behalf of consumers.}
#'   \item{\code{has_narrative}}{boolean}
#'   \item{\code{complaint_id}}{integer }
#'   \item{\code{timely}}{factor Whether the company gave a timely response}
#'   \item{\code{consumer_consent_provided}}{factor Identifies whether the consumer opted in to publish their complaint narrative.}
#'   \item{\code{company_response}}{string }
#'   \item{\code{submitted_via}}{factor How the complaint was submitted to the CFPB}
#'   \item{\code{company}}{string The complaint is about this company}
#'   \item{\code{date_received}}{POSIXlt The date the CFPB received the complaint}
#'   \item{\code{state}}{factor}
#'   \item{\code{consumer_disputed}}{factor Whether the consumer disputed the company’s response}
#'   \item{\code{company_public_response}}{string The company's optional, public-facing response to a consumer's complaint}
#'   \item{\code{sub_issue}}{factor The sub-issue the consumer identified in the complaint}
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
query_complaint_id <- function(complaintID = NULL)
{
    cat(paste0("Searching for ", complaintID, "\n"))


  cfpb_query_path <- httr::modify_url(
    url = get_cfpb_url(),
    path = get_cfpb_url_path(complaintID),
  )
  res <- httr::GET(cfpb_query_path)

  if (res$status_code == get_success_code())
  {
    text_res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    if (text_res$hits$total$value>0)
    {
      res_data <- text_res$hits$hits$`_source`
      res_data <- res_data[,!names(res_data) %in% c(':updated_at',
                                                    'date_received_formatted',
                                                    'date_sent_to_company_formatted',
                                                    'date_indexed',
                                                    'date_indexed_formatted')]
      res_data$consumer_disputed <- as.factor(res_data$consumer_disputed)
      res_data$date_received <- strptime(res_data$date_received, "%Y-%m-%dT%H:%M:%S")
      res_data$date_sent_to_company <- strptime(res_data$date_sent_to_company, "%Y-%m-%dT%H:%M:%S")
      res_data$issue <- as.factor(res_data$issue)
      res_data$product <- as.factor(res_data$product)
      res_data$state <- as.factor(res_data$state)
      res_data$sub_issue <- as.factor(res_data$sub_issue)
      res_data$sub_product <- as.factor(res_data$sub_product)
      res_data$submitted_via <- as.factor(res_data$submitted_via)
      res_data$timely <- as.factor(res_data$timely)
      return(res_data)
    } else
    {
      stop("No complaints found with that ID")
    }
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
