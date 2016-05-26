#' Performs common character transformations to a data frame of
#' addresses in order to build "web-safe" URLs for the Google API.
#'
#' This function scrubs a data frame of addresses (e.g., one of the form: address, city, state, postal code, country)
#' of character values that may inhibit sucessful geocoding with the \href{https://developers.google.com/maps/documentation/geocoding/start}{Google maps API}.
#' Specifically, this function:
#' \itemize{
#'   \item {Replaces non-breaking spaces with " "}
#'   \item {Removes ASCII control characters (001-031 and 177)}
#'   \item {Trims runs of spaces and spaces which begin/end a string}
#'   \item {Converts special addressing charecters, such as ordinals}
#'   \item {Strips latin1 characters}
#'   \item {Removes leading, trailing, and repeated commas}
#'   \item {Remove various permutations of the "c/o" flag}
#'   }
#'
#'   Note: We recommend closely reviewing the output of this function against the original to ensure that these
#'   transformations comport with the issues (if any) found in your address list.

#' @param address A data frame of addresses fields, generally of the form: address, city, state, postal code, country.
#' @param verbose Displays additional progress output

#' @importFrom stringi stri_trans_general
#' @export

address_cleaner <- function(address, verbose = TRUE) {

	# Replace non-breaking spaces with " "
	if (verbose)
		cat("\nReplacing non-breaking spaces")
	address <- as.data.frame(sapply(address, function(x)
		gsub(intToUtf8(160), " ", x)))

	# Remove ASCII control characters (\001-\031 and \177)
	if (verbose)
		cat("\nRemoving control characters")
	address <- as.data.frame(sapply(address, function(x)
		gsub("[\001-\031\177]", " ", x)))

	# Trim runs of spaces and spaces which begin/end a string
	if (verbose)
		cat("\nRemoving leading/trailing spaces, and runs of spaces")
	trim <- function(x)
		return(gsub("^ +|(?<= ) +| +$", "", x, perl = T))
	address <- as.data.frame(sapply(address, function(x)
		trim(x)))

	# Strip latin1 characters from the matrix
	if (verbose)
		cat("\nTransliterating latin1 characters")
	address <- as.data.frame(sapply(address,
									function(x) stri_trans_general(x, "Latin-ASCII")))

	# Convert special addressing charecters
	if (verbose)
		cat("\nConverting special address markers")
	address <- as.data.frame(sapply(address, function(x)
		gsub("½", "1/2", fixed = T, gsub("ª", "a", fixed = T,
										gsub("º", "o", x, fixed = T))
		)))

	# Remove remaining non-ASCII characters and replace with " "
	if (verbose)
		cat("\nRemoving all remaining non-ASCII characters")
	address <- as.data.frame(sapply(address,
									function(x)
										iconv(x, 'ASCII', sub = " ")))

	# Remove leading, trailing, and repeated commas
	if (verbose)
		cat("\nRemoving leading, trailing, and repeated commas")
	address <- as.data.frame(sapply(address, function(x)
		gsub("^,*|(?<=,),|,*$", "", x, perl = T)))

	# Remove "c/o" flag
	if (verbose)
		cat("\nRemoving various c/o string patterns")
	address <- as.data.frame(sapply(address, function(x)
		gsub("c/o|c/0|c/", "", x, perl = T, ignore.case = T)))

	#Recode empty strings to NA to avoid excess commas in the paste operation
	address[address == ""]  <- NA
	return(address)
}
