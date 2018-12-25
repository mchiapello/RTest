library(testthat)

context("utils.test")

test_that("test_execution silent",{
	
	global_reporter <- get_reporter()
	
	my_reporter <- ListReporter$new()
	
	set_reporter(my_reporter)
	
	options("force_implementation"=TRUE)
	
	my_fun <<- function(){
		1+2
	}
	
	test_execution("my_fun",args=list(),
			xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="silent")))
	
	options("force_implementation"=FALSE)

	set_reporter(global_reporter)

	expect_equal(
			class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
			"expectation_success"
			)
})

test_that("test_execution output",{
	
	
	global_reporter <- get_reporter()
	
	my_reporter <- ListReporter$new()
	
	set_reporter(my_reporter)
	
	options("force_implementation"=TRUE)
	
	my_fun <<- function(){
		1+2
	}
	
	test_execution("my_fun",args=list(),
			xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="output")))
	
	options("force_implementation"=FALSE)
	my_reporter$end_reporter()
	set_reporter(global_reporter)
	
	expect_equal(
			class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
			"expectation_failure"
	)
	
	# Start a new internal reporter
	my_reporter <- ListReporter$new()
	
	set_reporter(my_reporter)
	my_fun <<- function(){
		print(2)
	}
			
	test_execution("my_fun",args=list(),
			xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="output")))
	
	set_reporter(global_reporter)
	
	expect_equal(
			class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
			"expectation_success"
	)
})

test_that("test_execution message",{
	
	global_reporter <- get_reporter()
	
	my_reporter <- ListReporter$new()
	
	set_reporter(my_reporter)
	
	options("force_implementation"=TRUE)
	
	my_fun <<- function(){
		1+2
	}
	
	test_execution("my_fun",args=list(),
			xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="message")))
	
	options("force_implementation"=FALSE)
	my_reporter$end_reporter()
	set_reporter(global_reporter)
	
	expect_equal(
			class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
			"expectation_failure"
	)
	
	# Start a new internal reporter
	my_reporter <- ListReporter$new()
	
	set_reporter(my_reporter)
	my_fun <<- function(){
		message("2")
	}
			
	test_execution("my_fun",args=list(),
			xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="message")))
	
	set_reporter(global_reporter)
	
	expect_equal(
			class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
			"expectation_success"
	)
})


test_that("test_execution warning",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			options("force_implementation"=TRUE)
			
			my_fun <<- function(){
				1+2
			}
			
			test_execution("my_fun",args=list(),
					xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="warning")))
			
			options("force_implementation"=FALSE)
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			# Start a new internal reporter
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			my_fun <<- function(){
				warning("2")
			}
			
			test_execution("my_fun",args=list(),
					xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="warning")))
			
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_success"
			)
		})

test_that("test_execution error",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			options("force_implementation"=TRUE)
			
			my_fun <<- function(){
				1+2
			}
			
			test_execution("my_fun",args=list(),
					xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="error")))
			
			options("force_implementation"=FALSE)
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			# Start a new internal reporter
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			my_fun <<- function(){
				stop("2")
			}
			
			test_execution("my_fun",args=list(),
					xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="error")))
			
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_success"
			)
})


test_that("test_execution wrongtype",{
			
			
			my_fun <<- function(){
				1+2
			}
			
			expect_error(
					test_execution("my_fun",args=list(),
					xmlTestSpec = XML::xmlNode("execution",attrs=c("execution-type"="xxxx")))
			)
			
})
test_that("test_execution NULL",{
			
			
			my_fun <<- function(){
				1+2
			}
			
			expect_silent(
					test_execution("my_fun",args=list())
			)
			
})

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#test_returnValue_variable 


test_that("test_returnValue_variable factor success",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			
			RTest:::test_returnValue_variable(result = as.factor("a"),
					reference=as.factor("a"),
					xmlTestSpec=NULL
					)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_success"
			)
			
			
		})
test_that("test_returnValue_variable factor failure",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			
			test_returnValue_variable(result = as.factor("a"),
					reference=as.factor("b"),
					xmlTestSpec=NULL
					)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			
})

test_that("test_returnValue_variable tolerance success",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(result = 5.0001,
					reference=5.0,
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("tolerance"=1E-3)
							)
					)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_success"
			)
			
			
})

test_that("test_returnValue_variable tolerance failure",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(result = 5.0001,
					reference=5.0,
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("tolerance"=1E-15)
							)
					)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			
})

test_that("test_returnValue_variable tolerance failure zero",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(result = 5.0001,
					reference=5.0,
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("tolerance"=0)
					)
			)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			
		})

test_that("test_returnValue_variable compare-type less_than",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(
					result = 5.0001,
					reference=5.0,
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("compare-type"="less_than")
					)
			)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			
		})

test_that("test_returnValue_variable compare-type more_than",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(
					result = 5.0001,
					reference=5.0,
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("compare-type"="more_than")
					)
			)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_success"
			)
			
			
		})

test_that("test_returnValue_variable compare-type regex success",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(
					result = "My friends are great.",
					reference="fr[i|e]{2}",
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("compare-type"="regex")
					)
			)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_success"
			)
			
			
		})

test_that("test_returnValue_variable compare-type regex failure",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			test_returnValue_variable(
					result = "My friends are great.",
					reference="fr[i|e]{3}",
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("compare-type"="regex")
					)
			)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			expect_equal(
					class(my_reporter$results$as_list()[[1]]$results[[1]])[1],
					"expectation_failure"
			)
			
			
		})
test_that("test_returnValue_variable compare-type notype failure",{
			
			expect_error(test_returnValue_variable(
					result = "My friends are great.",
					reference="fr[i|e]{3}",
					xmlTestSpec=XML::xmlNode(
							"return-value",attrs=c("compare-type"="notype")
					)
			))
			
			
		})