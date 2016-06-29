#' Geocode an address vector using the Google Maps API.
#'
#' geocode_url uses the Google Maps API to estimate latitude and longitude coordinates for a character vector of physical addresses.
#' Optionally, one may use their (paid) \href{https://www.google.com/work/}{Google for Work} API key to sign the request with the \code{\link{hmac}} sha1 algorithm.
#' For smaller batch requests, it is also possible to access Google's "standard API"
#' with this function (see \href{https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key}{this page} to obtain a free API key).
#' Geocode_url returns a data.frame with (numeric) lat/long coordinates and two additional parameters from the response object (see \href{https://developers.google.com/maps/documentation/geocoding/intro#GeocodingResponses}{this page} for additional information):
#' \itemize{
#'   \item \strong{location_type:} an estimate of the response object's coordinate accuracy. Currently, these are:
#'   \itemize{
#'   \item ROOFTOP: indicates that the returned result is accurate to the level of precise street address.
#'   \item RANGE_INTERPOLATED: indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#'   \item GEOMETRIC_CENTER: indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#'   \item APPROXIMATE: indicates that the returned result is approximate.
#'   }
#'   \item \strong{status:} the gecode status of a response object. Currently, these are:
#'   \itemize{
#'   \item \emph{OK:} indicates that no errors occurred; the address was successfully parsed and at least one geocode was returned.
#'   \item \emph{ZERO_RESULTS:} indicates that the geocode was successful but returned no results. This may occur if the geocoder was passed a non-existent address.
#'   \item \emph{OVER_QUERY_LIMIT:} indicates that you are over your quota.
#'   \item \emph{REQUEST_DENIED:} indicates that your request was denied.
#'   \item \emph{INVALID_REQUEST:} Indicates that some part of the query (address, URL components, etc.) is missing.
#'   \item \emph{UNKNOWN_ERROR:} indicates that the request could not be processed due to a server error. The request may succeed if you try again.
#'   }
#'   }
#' @param address A 1xN vector of address(es) with "url-safe" characters. Enabling the "clean" parameter (see below) will strip or replace common character patterns in this vector that are incompatible with the Maps API.
#'        Note: addresses should be in raw form, \emph{not} URL encoded (e.g., of the form: 123 Main Street, Somewhere, NY 12345, USA). Note: country is optional but recommended.
#' @param auth character string; one of: "standard_api" (the default), or "work".
#'        In this function, authentication via the standard API requires a (free) \href{https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key}{Google API key}.
#'        Authentication via the "work" method requires the client ID and private API key associated with your (paid) \href{https://www.google.com/work/}{Google for Work} account.
#' @param privkey character string; your Google API key (whether of the "work" or "standard_api" variety).
#' @param clientid character string; your Google for Work client ID (generally, these are of the form 'gme-[company]')
#'        This parameter should \emph{not} be set when authenticating through the standard API.
#' @param clean logical; when \emph{TRUE}, applies \code{\link{address_cleaner}} to the address vector prior to URL encoding.
#' @param verbose logical; when \emph{TRUE}, displays additional output in the returns from Google.
#' @param add_date character string; "none" (the default) or one of: "today", "fuzzy". When set to "today", a column named \emph{geocode_dt} with today's calendar date is added to the returned data frame.
#'        When set to "fuzzy", a random positive number of days between 1 and 30 is added to \emph{geocode_dt}. This can be useful to avoid sending large batches of geocode requests if your scripts recertify/retry geocoding after a fixed period of time.
#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl getURL
#' @importFrom stats runif
#' @export


geocode_url <- function(address, auth="standard_api", privkey=NULL,
						clientid=NULL, clean=FALSE, verbose=FALSE,
						add_date="none") {

	options(stringsAsFactors=FALSE)

	# Input validation
	if(!grepl("standard_api|work", auth))
		stop("Invalid auth paramater. Must be 'standard_api' or 'work'.")
	if(is.null(privkey))
		stop("You must specify an API key. See: https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key")
	if(auth=="work" & is.null(clientid))
		stop("You must specify a client ID with the work authentication method!")
	if(!grepl("today|fuzzy|none", add_date))
		stop("Invalid add_date paramater. Must be 'today', 'fuzzy', or 'none'")
	if(!is.vector(address, mode="character"))
		stop("Address and destination must be character vectors!")


	# Optionally apply the address clean function
	if(clean) address <- gcoder::address_cleaner(address, verbose=FALSE)

	# Recode NA's and empty strings to "INVALID_REQUEST" to falsely coding
	# invalid requests as, e.g., the country of Nambia.
	not_nambia <- function(x){
		x[x %in% c("", " ", NA)] <- "INVALID-REQUEST"
		return(x)
	}
	address <- not_nambia(address); dest <- not_nambia(dest)

	# Apply url encoding to the raw locations vector
	enc <- urltools::url_encode(address)


	# Generate the digital signature key if using Google for Work authentication
	if(auth=="work"){
		togoogle <- gcoder::google_encode64(enc, gmode="geocode",
											privkey=privkey, clientid=clientid,
											verbose = verbose, debug=FALSE)
	}

	if(auth=="standard_api"){
		togoogle <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
						   enc,"&key=", privkey)
	}

	if(verbose) cat("Sending address vector (n=", length(togoogle), ") to Google...\n", sep="")
	doc   <- lapply(togoogle, RCurl::getURL)
	json  <- lapply(doc, RJSONIO::fromJSON, simplify=FALSE)

	coord <- t(vapply(json,function(x) {
		if(x$status=="OK") {
			lat <- as.character(x$results[[1]]$geometry$location$lat)
			lng <- as.character(x$results[[1]]$geometry$location$lng)
			location_type <- as.character(x$results[[1]]$geometry$location_type)
			status <- as.character(x$status)
			return(c(lat, lng, location_type, status))
		} else {
			return(c(NA, NA, NA, x$status))
		}
	}, character(4)))

	if(is.matrix(coord)){
		out <- as.data.frame(coord)
	} else if(length(coord)==4) {
		out <- data.frame(t(unlist(coord)))
	}
	colnames(out) <- c("lat", "lng", "location_type", "status")
	out[, c("lat", "lng")] <- vapply(out[, c("lat", "lng")],
									  as.numeric, numeric(nrow(out)))

	out$location <- address
	out$input_url <- togoogle

	# Optionally add the date parameter
	if(!add_date=="none"){
		out$geocode_dt <- Sys.Date()
		if(add_date=="fuzzy") out$geocode_dt <- out$geocode_dt + runif(nrow(out), 1, 30)
	}

	nrow(out[out$status=="OK", ])

	if(verbose) cat("Finished.",nrow(out[out$status=="OK", ]), "of", nrow(out),
					"records successfully geocoded.\n")

	return(out)
}



