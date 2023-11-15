#' Get all CFPB complaints
#'
#' @param quiet Should progress messages be printed?
#' @param file if NA, the file will be downloaded.  If a file exists, it will be read and transformed.  If the file does not exist, it will be created.
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
#' @export
#'
#' @importFrom utils download.file unzip read.csv
#'
#' @examples
#' \dontrun{
#'   test <- get_all_complaints()
#' }
get_all_complaints <- function(quiet = TRUE, file = NA)
{
  temp_dir <- tempdir()
  cfpb_download_url <- "http://files.consumerfinance.gov/ccdb/complaints.csv.zip"
  download_file <- file.path(temp_dir, "complaints.csv.zip")
  csv_file <- file.path(temp_dir, "complaints.csv")

  if (is.na(file) | !file.exists(file))
  {
    if (!quiet)
      cat("Downloading...\n")
    utils::download.file(url = cfpb_download_url, destfile = download_file,
                         quiet = quiet)
    if (!quiet)
      cat("Unzipping...\n")
    utils::unzip(zipfile = download_file, exdir = temp_dir)
    if (!file.exists(file))
    {
      file.copy(from = csv_file, to = file)
    }
    if (!quiet) cat("Reading CSV...\n")
    X <- utils::read.csv(file = csv_file, stringsAsFactors = FALSE)
    unlink(download_file)
    unlink(csv_file)
  } else if (file.exists(file))
  {
    X <- utils::read.csv(file = file, stringsAsFactors = FALSE)
  }

  if (!quiet) cat("Transforming CSV...\n")
  names(X) <- c("date_received", "product", "sub_product",
                "issue", "sub_issue", "complaint_what_happened",
                "company_public_response", "company", "state",
                "zip_code", "tags", "consumer_consent_provided",
                "submitted_via", "date_sent_to_company",
                "company_response", "timely", "consumer_disputed", "complaint_id")
  X$date_received <- strptime(X$date_received, "%Y-%m-%dT%H:%M:%S")
  X$product <- as.factor(X$product)
  X$sub_product <- as.factor(X$sub_product)
  X$issue <- as.factor(X$issue)
  X$sub_issue <- as.factor(X$sub_issue)
  X$company <- as.factor(X$company)
  X$state <- as.factor(X$state)
  X$consumer_consent_provided <- as.factor(X$consumer_consent_provided)
  X$submitted_via <- as.factor(X$submitted_via)
  X$date_sent_to_company <- strptime(X$date_sent_to_company, "%Y-%m-%dT%H:%M:%S")
  X$company_response <- as.factor(X$company_response)
  X$timely <- as.factor(X$timely)
  X$consumer_disputed <- as.factor(X$consumer_disputed)
  X$has_narrative <- ifelse(X$complaint_what_happened == "", FALSE, TRUE)
  return(X)
}
