###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Changing testthat (>2.0) functions to work in RTest                                             #
#                                                                                                 #
# Date:           22 - Dec - 2018                                                                 #
# Author:         Sebastian Wolf sebastian@mail-wolf.de                                           #
#                                                                                                 #
###################################################################################################

#' testthat function: Function to return expectation after being executed
#' 
#' In comparison to \code{testthat} this function always exports the message, even
#' in case of success
#' 
#' @param x object to test for class membership
#' @param message \code{character} character string to be the execution message
#' @param info Additional information. Included for backward compatibility
#'   only and new expectations should not use it.
#' @param srcref Only needed in very rare circumstances where you need to
#'   forward a srcref captured elsewhere.
#' @param ... Unused, just defined inside testthat
#' 
#' @author Sebastian Wolf
#' @export
as.expectation.logical <- function(x, message, ..., srcref = NULL, info = NULL) {
	type <- if (x) "success" else "failure"
	`%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
				inherits = FALSE)
	
	add_info <-  "testthat" %:::% "add_info"
	
	message <- add_info(message, info)
	expectation(type, message, srcref = srcref)
}

# unlockBinding("as.expectation.logical", getNamespace("testthat"))
# assign("as.expectation.logical", as.expectation.logical, getNamespace("testthat"))


#' Expect a function call to run silent
#' 
#' In case the function call is not silent, a message including all outputs, messages, warnings
#' is given.
#' 
#' @export
#' @importFrom glue glue glue_collapse
#' @importFrom rlang enquo
#' 
#' @param object executable function call
#' 
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
expect_silent_RTest <- function(object) {
	
	`%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
				inherits = FALSE)
	
	quasi_capture <-  "testthat" %:::% "quasi_capture"
	
	act <- quasi_capture(rlang::enquo(object),
			evaluate_promise)
	outputs <- c(
			outputs = if (!identical(act$cap$output, "")) act$cap$output,
			warnings = if (length(act$cap$warnings) > 0) act$cap$warnings %>% 
						strsplit(split="\n") %>% unlist(),
			messages = if (length(act$cap$messages) > 0) act$cap$messages %>% 
						strsplit(split="\n") %>% unlist()
	)
	
	# Create a phrase that contains which outputs and warnings were created
	outputs_string <- glue::glue("{names(outputs)} = '{outputs}'") %>%
			glue::glue_collapse(sep="\n")
	
	# Create the output for the test
	expect(
			length(outputs) == 0,
			#sprintf("%s produced %s.", act$lab, paste(outputs, collapse = ", ")),
			glue::glue("{act$lab} produced:\n {outputs_string}")
	)
	
	invisible(act$cap$result)
}