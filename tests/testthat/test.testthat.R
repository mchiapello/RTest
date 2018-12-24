context("testthat functionalities replacements")

my_reporter <- function(){
	
	global_rep <- get_reporter()
	
	# Try to get the output of expect_silent_RTest
	reporter     <- ListReporter$new()
	
	set_reporter(reporter)
	reporter$start_reporter()
	
	# Execute the wrapper function
	tmpExec <- NULL
	
	with_reporter(reporter, 
			test_that("length computed correctly", {
						expect_silent_RTest(message("xx"))
					})
	)
	reporter$end_context()
	reporter$end_reporter()
	
	set_reporter(global_rep)
	
	return(reporter)
}

test_that("expect_silent_RTest function works", {
			
			b <- 1
			
			expect_error(as.logical(expect_silent_RTest(a <- c_value)))
			
			expect_equal(as.logical(expect_silent_RTest(a <- b)),TRUE)
			
			reporter <- my_reporter()
			
			expect_equal(
					reporter$results$as_list()[[1]]$results[[1]]$message,
					"`message(\"xx\")` produced:\nmessages = 'xx'")
		})


my_reporter <- function(){
	global_rep <- get_reporter()
	# Try to get the output of expect_silent_RTest
	reporter     <- ListReporter$new()
	set_reporter(reporter)
	reporter$start_reporter()
	
	# Execute the wrapper function
	tmpExec <- NULL
	myfun <- function(){
		warning("one")
		warning("two")
	}
	with_reporter(reporter, 
			test_that("length computed correctly", {
						RTest::expect_silent_RTest(myfun())
					})
	)
	reporter$end_context()
	reporter$end_reporter()
	set_reporter(global_rep)
	return(reporter)
}

test_that("expect_silent_RTest function warnings works", {
			
			reporter <- my_reporter()
			
			expect_equal(
					reporter$results$as_list()[[1]]$results[[1]]$message,
					"`myfun()` produced:\nwarnings1 = 'one'\nwarnings2 = 'two'")
			
		})

test_that("expect_silent_RTest function outptus works", {
			
			global_rep <- get_reporter()
			# Try to get the output of expect_silent_RTest
			reporter     <- ListReporter$new()
			
			set_reporter(reporter)
			reporter$start_reporter()
			
			# Execute the wrapper function
			tmpExec <- NULL
			myfun <- function(){
				print("one")
				print("two")
			}
			with_reporter(reporter, 
					test_that("length computed correctly", {
								expect_silent_RTest(myfun())
							})
			)
			reporter$end_context()
			reporter$end_reporter()
			set_reporter(global_rep)
			expect_equal(
					reporter$results$as_list()[[1]]$results[[1]]$message,
					"`myfun()` produced:\noutputs = '[1] \"one\"\n[1] \"two\"'")
		})