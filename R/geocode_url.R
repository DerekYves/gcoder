#' Geocode an address vector using the (paid) Google for Work API.
#'
#' This function attempts to geocode a vector of physical addresses using Google's
#' (paid) \href{https://www.google.com/work/}{Google for Work API}. A version of this
#' function is discussed in # A version of this function is discussed
#' \href{http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps}{in this thread}.
#' This modified version of the function retains additional parameters in the JSON-formatted return:
#' modified version adds to parameters from the JSON return:
#' \itemize{
#'   \item \emph{location_type}
#'   \item \emph{status}
#'   }
#

#'
#' @param address A "url-safe" vector of fully formed URLs that are encoded with your private Google key (see: \code{\link{google_encode64}}).
#' @param verbose Display additional output in the returns from Google?
#' @param add_date Adds a column with today's date to the returned data.frame.
#' @param fuzzy Uses \code{\link{runif}} to add a random number of days between 1 and 30 to the current date when add_date is TRUE. This can be useful to avoid sending large batches if your scripts recertify/retry geocoding after a fixed number of days.
#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl getURL
#' @export

geocode_url <- function(address, verbose=FALSE, add_date=TRUE, fuzzy=FALSE) {
	if(verbose) cat(address,"\n")
	doc   <- lapply(address, RCurl::getURL)
	json  <- lapply(doc, RJSONIO::fromJSON, simplify = FALSE)
	coord <- t(sapply(json,function(x) {
		if(x$status=="OK") {
			lat <- x$results[[1]]$geometry$location$lat
			lng <- x$results[[1]]$geometry$location$lng
			location_type   <- x$results[[1]]$geometry$location_type
			status <- x$status
			return(c(lat, lng, location_type, status))
		} else {
			return(c(NA, NA, NA, "ZERO_RESULTS"))
		}
	}, simplify="T" ))

	if(length(address)>1) colnames(coord)=c("lat","lng", "location_type", "status")
	else names(coord)=c("lat","lng", "location_type", "status")
	out <- data.frame(url_for_google_geocode=address, coord)
	if(add_date & fuzzy==FALSE) out$google_geocode_dt <- Sys.Date()
	if(add_date & fuzzy) out$google_geocode_dt <-
		Sys.Date() + as.integer(runif(n=nrow(out), min=1, max=30))
	return(out)
}


