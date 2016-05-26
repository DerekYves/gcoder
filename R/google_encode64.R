#' Build fully formed URLs for geocoding using one's Google for Work private API key.
#'
#' This function creates a fully formed vector of URLs encoded with your
#' \href{https://www.google.com/work/}{Google for Work} private API key.
#' The general pattern of this function follows the \href{https://developers.google.com/maps/documentation/business/image/auth?hl=en}{Google Developer documention}
#' for generating a digital signature. Prior to applying this function you may need to strip your address vector of non-URL-safe string values (see: ).
#

#' @param address A 1xN locations vector with UTF-8 encoded ASCII-characters (see: ).
#' @param privkey Your Google for Work API key
#' @param clientid This parameter is generall of the form 'gme-[company]'
#' @param sigfile Save out a complete file of locations and URL signatures (this can be useful for debugging malformed URLs)?
#' @param sigfile_loc A fully formed folder/file location to save the function's output. If NULL and sigfile=TRUE, file is saved as "signature_file_[date-time].Rds" in the current working directory.
#' @param verbose Displays additional progress output
#'
#' @importFrom digest hmac
#' @importFrom urltools url_encode
#' @importFrom RCurl base64Decode
#' @importFrom base64enc base64encode
#' @export


google_encode64 <-
	function(address, privkey, clientid, sigfile = FALSE, sigfile_loc = NULL,
			 verbose = FALSE) {
		if (sigfile == TRUE &
			is.null(sigfile_loc))
			sigfile_loc <- paste0("signature_file_", Sys.time(), ".Rds")

		# Load the vector of locations to geocode
		x <- data.frame(locations=address)

		if (verbose)
			cat("\nNumber of records to geocode: ", nrow(x), "\n")

		# The steps below follow:
		# https://developers.google.com/maps/documentation/business/image/auth?hl=en

		#####################################################################
		# Step (1): Set URL parameters:
		#     Note 1: Any non-standard characters need to be URL-encoded.
		#     Note 2: All Google services require UTF-8 character encoding.

		x$domain  <- "https://maps.googleapis.com"
		x$url     <- "/maps/api/geocode/json?address="
		x$url_location <- urltools::url_encode(x$locations)
		x$client  <- paste0("&client=", clientid)

		#####################################################################
		# (2) Strip off the domain portion of request and create the url to sign:
		x$tosign <- with(x, paste0(url, url_location, client))

		#####################################################################
		# (3) Encode the private key in "url-safe" Base64.
		#     Note 1: "url-safe" keys replace '+' and '/' with '-' and '_' respectively.
		#     Note 2: base::base64decode does not work; base64Decode from RCurl needed.
		#     Note 3: RCurl's base64Decoder is not URL safe; therefore, gsub is needed.
		#See: http://stackoverflow.com/questions/28376071/url-safe-base64-decoding-for-r
		b64dkey   <- RCurl::base64Decode(gsub("-", "+", gsub("_", "/", privkey)))
		x$b64dkey <- b64dkey

		#####################################################################
		# (4) Sign the URL using the HMAC-SHA1 algorithm and decode to binary.
		#     Note: hmac requires the raw=T argument for the Google key

		# Define hash/encoding function
		hmac_sha1 <- function(key, string) {
			hash  <- digest::hmac(key, string, "sha1", raw = TRUE)
			base64enc::base64encode(hash)
		}
		if (verbose)
			cat("\nEncoding the private key")

		x$enc_sig <-
			sapply(x[, 'tosign'],  function(x)
				hmac_sha1(b64dkey, x))
		#####################################################################

		#####################################################################
		# (5) Encode the binary into url-safe Base64 for the signature.

		x$enc_sig_url <- sapply(x[, 'enc_sig'], function(x)
			gsub("+", "-", fixed = T, gsub("/", "_", x, fixed = T)))
		#####################################################################

		#####################################################################
		# (6) Build the fully formed, signed URLs

		x$full_url <- with(x, paste0(domain, tosign, "&signature=", enc_sig_url))
		#####################################################################

		#####################################################################
		# (7) Save out signatures to debug any malformed/failed addresses.

		if (verbose & sigfile)
			cat("\nSaving URL signature file for debugging")
		if (sigfile) saveRDS(x,  file = sigfile_loc)
		#####################################################################

		#####################################################################
		# (8) Return the vector

		fullurl <- x$full_url
		return(fullurl)
	}



