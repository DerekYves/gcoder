#' Get travel time and distance between two point using the Google API.
#'
#' This function uses the Google Maps API to estimate travel time and distance between two physical addresses.
#' Optionally, one may use a (paid) \href{https://www.google.com/work/}{Google for Work} API key to sign the request with the \code{\link{hmac}} sha1 algorithm.
#' For smaller batch requests, it is also possible to access Google's "standard API"
#' with this function (see \href{https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key}{this page} to obtain a free API key).

#' @param address A 1xN vector of locations. These may be either (1) latitude and longitude coordinates
#'        (formatted as "lat,long" with no spaces) or physical addresses that contain "url-safe" UTF-8 characters.
#'        Enabling the "clean" parameter (see below) attempts to strip or replace common character patterns
#'        from physical addresses that are incompatible with the Maps API key.
#'        This vector becomes the starting point of the distance calculation.
#'        If the length of this parameter is 1 and destination's length is greater than 1, address is replicated to match the length of destination
#'        (e.g., when you are calculating distances between one specific location and two or more destinations).
#'        Note: addresses should be in raw form, \emph{not} URL encoded (e.g., of the form: 123 Main Street, Somewhere, NY 12345 USA)(country is optional but recommended).
#' @param dest A 1xN vector of destinations. These may be either (1) latitude and longitude coordinates
#'        (formatted as "lat,long" with no spaces) or physical addresses that contain "url-safe" UTF-8 characters.
#' @param auth character string; one of: "work" (the default), or "standard_api".
#'        While you may specify and empty string for privkey when using the standard API, for some direction types (e.g., transit) a (free) API key from Google is required (see \href{https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key}{this page}.
#'        Authentication via the "work" method requires the private API key associated with your (paid) \href{https://www.google.com/work/}{Google for Work} account.
#' @param privkey character string; your Google API key (whether of the "work" or "standard_api" variety).
#' @param clientid character string; your Google for Work client id (generally of the form 'gme-[company]')
#'        This parameter should not be set when authenticating through the standard API.
#' @param clean logical; when \emph{TRUE}, applies \code{\link{address_cleaner}} to the address and destination vectors.
#' @param travel_mode character string; currently, valid values include (\href{https://developers.google.com/maps/documentation/javascript/distancematrix#distance_matrix_requests}{see this page for details}):
#'   \itemize{
#'   \item driving (the default): indicates standard driving directions using the road network.
#'   \item transit: requests directions via public transit routes (requires and API key).
#'   \item walking: requests walking directions via pedestrian paths & sidewalks (where available).
#'   \item bicycling: requests bicycling directions via bicycle paths & preferred streets (currently only available in the US and some Canadian cities).
#'         }
#' @param units character string; must be either "metric" (the default) or "imperial".
#' Specifying "metric" will return distance between origin and destination as kilometers,
#' whereas "imperial" returns distance in miles. For geocode requests this parameter
#' is ignorned if non-null.
#' @param verbose logical; when \emph{TRUE}, displays additional progress output.
#' @param add_date character string; one of: "none" (the default), "today", or "fuzzy". When set to "today", a column with today's calendar date is added to the returned data frame.
#'        When set to "fuzzy", a random positive number of days between 1 and 30 is added to this date column. "Fuzzy" date values can be useful to avoid sending large batches of geocode requests on the same day if your scripts recertify/retry distance calculations after a fixed period of time.
#' @param language character string; localization of the returned object. This set to "en-EN" by default, but refer to
#' \href{https://developers.google.com/maps/faq#using-google-maps-apis}{this page}
#' for an up-to-date list of all supported languages.

#' @return #' Drive_time returns a data frame with the following parameters stored from the response object:
#' \itemize{
#'   \item \strong{distance:} The distance between address[\emph{i}] and dest[\emph{i}] in kilometers.
#'   \item \strong{travel_time:} The estimated travel time given the specified mode of transportation.
#'   \item \strong{status:} The status field of the response object. Currently, these are:
#'   \itemize{
#'   \item \emph{OK:} Indicates that no errors occurred; the address was successfully parsed and at least one geocode was returned.
#'   \item \emph{ZERO_RESULTS:} Indicates that the geocode was successful but returned no results. This may occur if the geocoder was passed a non-existent address.
#'   \item \emph{OVER_QUERY_LIMIT:} Indicates that you are over your quota.
#'   \item \emph{REQUEST_DENIED:} Indicates that your request was denied.
#'   \item \emph{INVALID_REQUEST:} Indicates that some part of the query (address, URL components, etc.) is missing.
#'   \item \emph{UNKNOWN_ERROR:} indicates that the request could not be processed due to a server error. The request may succeed if you try again.
#'   }
#'   }

#' @examples
#' # Bike from the White House to Google!
#' address <- c("1600 Pennsylvania Ave NW, Washington, DC 20500, USA",
#' 			 "1600 Amphitheatre Pkwy, Mountain View, CA 94043, USA")
#'
#' coordset <- geocode_url(address, auth="standard_api", privkey="",
#'             clean=TRUE, add_date='today', verbose=TRUE)
#'
#' # Save coordinates. Google requires a format of: "lat,lng" (with no spaces)
#' from <- paste(coordset$lat[1],coordset$lng[1], sep=",")
#' dest <- paste(coordset$lat[2],coordset$lng[2], sep=",")
#'
#' # Get the travel time by bike (a mere 264 hours!) and distance in miles:
#' howfar_miles <- drive_time(address=from, dest=dest, auth="standard_api",
#' 						   privkey="", clean=FALSE, add_date='today',
#' 						   verbose=FALSE, travel_mode="bicycling",
#' 						   units="imperial")
#'
#' # Get the distance in kilometers using physical addresses instead of lat/lng:
#' howfar_kms <- drive_time(
#'      address="1600 Pennsylvania Ave NW, Washington, DC 20500, USA",
#' 		dest="1600 Amphitheatre Pkwy, Mountain View, CA 94043",
#' 		auth="standard_api", privkey="", clean=FALSE,
#' 		add_date='today', verbose=FALSE, travel_mode="bicycling",
#' 		units="metric"
#' 		)
#'
#' with(howfar_kms, cat("Travelling from the White House to ", destination,
#' 					 ":\n", dist_txt, "\n", time_txt, sep=""))



#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl getURL
#' @importFrom stats runif
#' @export

drive_time <- function(address, dest, auth="standard_api", privkey=NULL,
					   clientid=NULL, clean="TRUE", travel_mode="driving",
					   units="metric", verbose=FALSE, add_date="none",
					   language="en-EN") {

	options(stringsAsFactors=F)

	# Input validation
	if(!grepl("standard_api|work", auth))
		stop("Invalid auth paramater. Must be 'standard_api' or 'work'.")
	if(is.null(privkey))
		stop("You must specify an API key. See: https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key")
	if(auth=="work" & is.null(clientid))
		stop("You must specify a client ID with the work authentication method!")
	if(!grepl("driving|bicycling|transit|walking", travel_mode, ignore.case=TRUE))
		stop("You must specify a valid travel mode.")
	if(!grepl("metric|imperial", units))
		stop("Invalid units paramater. Must be 'metric' or 'imperial'")
	if(length(address)>1 & length(address)!=length(dest))
		stop("Address must be singular or the same length as destination!")
	if(!is.vector(c(address, dest), mode="character"))
		stop("Address and destination must be character vectors!")
	if(!grepl("today|fuzzy|none", add_date))
		stop("Invalid add_date paramater. Must be 'today', 'fuzzy', or 'none'")

	if(clean){
		if(verbose) cat("Cleaning origin addresses...\n")
		address <- gcoder::address_cleaner(address, verbose=verbose)
		if(verbose) cat("Cleaning destination addresses...\n")
		dest    <- gcoder::address_cleaner(dest, verbose=verbose)
	}

	#Recode NA's and empty strings to "INVALID_REQUEST" to avoid coding Nambia.
	not_nambia <- function(x){
		x[x %in% c("", " ", NA)] <- "INVALID-REQUEST"
		return(x)
	}
	address <- not_nambia(address); dest <- not_nambia(dest)

	# Encode the URLs
	enc  <- urltools::url_encode(address)
	dest <- urltools::url_encode(dest)

	if(auth=="standard_api") {
		inbound <- data.frame(address=enc, dest=dest)
		baserl <- "https://maps.googleapis.com/maps/api/distancematrix/json?origins="
		inbound$full_url <- paste0(baserl, inbound$address,
								  "&destinations=",
								  inbound$dest,
								  "&units=",
								  tolower(units),
								  "&mode=",
								  tolower(travel_mode),
								  "&language=",
								  language,
								  "&key=", privkey)
		togoogle <- inbound$full_url
		}

	if(auth=="work"){
		togoogle <- gcoder::google_encode64(enc, dest=dest, gmode="dtime",
											privkey=privkey, clientid=clientid,
											verbose = verbose, units=units)
	}

	if(verbose) cat("Sending locations (n=", length(togoogle),
					") to Google for distance calculation...\n", sep="")

	doc   <- lapply(togoogle, RCurl::getURL)
	json  <- lapply(doc, RJSONIO::fromJSON, simplify=FALSE)

	coord <- t(vapply(json, function(x) {
		if(x$status=="OK"){
			if(x$rows[[1]]$elements[[1]]$status=="OK"){
			origin      <- as.character(x$origin_addresses)
			destination <- as.character(x$destination_addresses)
			# Distance is returned as meters, regardless of the "unit" API call.
			dist_num    <- as.character(x$rows[[1]]$elements[[1]]$distance$value / 1000)
			if(units=="imperial") dist_num <- as.character(as.numeric(dist_num) * 0.621371)
			dist_txt    <- as.character(x$rows[[1]]$elements[[1]]$distance$text)
			time_secs   <- as.character(x$rows[[1]]$elements[[1]]$duration$value)
			time_mins   <- as.character(as.numeric(time_secs) * 0.0166667)
			time_hours  <- as.character(as.numeric(time_secs) * 0.000277778)
			time_txt    <- as.character(x$rows[[1]]$elements[[1]]$duration$text)
			status      <- as.character(x$rows[[1]]$elements[[1]]$status)
			return(c(origin, destination, dist_num, dist_txt, time_secs, time_mins, time_hours, time_txt, status))
			}
		} else if(x$status=="REQUEST_DENIED"){
			return(c(NA, NA, NA, NA, NA, NA, NA, NA, "REQUEST_DENIED"))
		} else {
			return(c(as.character(x$origin_addresses), as.character(x$destination_addresses), NA, NA, NA, NA, NA, NA, as.character(x$rows[[1]]$elements[[1]]$status)))
		}
	}, character(9)))

	if(is.matrix(coord)){
		out <- as.data.frame(coord)
	} else if(length(coord)==7) {
		out <- data.frame(t(unlist(coord)))
	}

	colnames(out) <- c("origin", "destination", "dist_num", "dist_txt",
					   "time_secs", "time_mins", "time_hours", "time_txt",
					   "status")

	numbas <- c("dist_num", "time_secs", "time_mins", "time_hours")
	out[, numbas] <- vapply(out[, numbas], function(x) {
		x <- round(as.numeric(x), digits=2)
		return(x)
		}, numeric(nrow(out)))

	out$input_url <- togoogle

	# Optionally add the date parameter
	if(!add_date=="none"){
		out$geocode_dt <- Sys.Date()
		if(add_date=="fuzzy") out$geocode_dt <- out$geocode_dt + stats::runif(nrow(out), 1, 30)
	}

	if(verbose){
		cat("Finished.",nrow(out[out$status=="OK", ]), "of", nrow(out),
					"distance calculations were successful.\n")
		if(units=="imperial"){
			len <- "miles"
		}else{
			len <- "kilometers"
		}
		message("Note: numeric distances in the 'dist_num' column are expressed in ", len, ".\n")
	}

	return(out)
}





