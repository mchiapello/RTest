###################################################################################################
#                                                                                                 #
# Author:         Sebastian Wolf								                                  #
#                                                                                                 #
###################################################################################################

library(testthat)
library(RTest)

# - Create a test Adapter function that uses execCache
setTestMethod(
		"test.RTest.funct_02", 
		signature  = "RTestCase",
		definition = function(object, inputData, execCache, xmlDef, ...) {
			
			result <- execCache$funct_01[[1]]
			
			# Read reference
			reference <- xmlReadData_data.frame(xmlDef[["reference"]])
			
			# Execute test
			if(!is.null(xmlDef[["testspec"]][["return-value"]]))
				test_returnValue_data.frame_cellbycell(
						result, 
						reference, 
						xmlDef[["testspec"]][["return-value"]]
				)    
			
			
			# Return result (will be cached)
			return(result)
			
		})

# Create a Test Function without any tests to test
# the "NO-TESTs" functionalities
setTestMethod(
		"test.RTest.funct_03", 
		signature  = "RTestCase",
		definition = function(object, inputData, execCache, xmlDef, ...) {
			
			#result <- execCache$funct_01[[1]]
			
			# Return result (will be cached)
			return(NULL)
			
		})

test_that("execCache",{
			
			global_rep <- get_reporter()
			# Create test collection
			testCollection <- new("RTestCollection", 
					project.name    = "RTest Vignette", 
					project.details = "Example test exectuion",
					tester          = "Example tester",
					test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
			
			# Import test cases from text.execCache.xml
			testCollection <- importTCsFromDir(testCollection,
					xml.dPath = paste0(find.package("RTest"),"/tests/testthat"),f.pattern="test.execCache.xml")
			
			outf <- tempfile(fileext=".html")
			
			testCollection <- exec(testCollection, out.fPath = outf, open=FALSE)
			
			set_reporter(global_rep)
			expect_error(
					execAdapter(
					testCollection@collection[[1]],"RTest", 1, "funct_012", tf.func.i = 1))
	
			expect_equal(
					testCollection@collection[[1]]@tests[["RTest"]][[1]]$funct_02[[1]]$result,
					"success"
			
			)
		})

test_that("execSummary",{
			
			global_rep <- get_reporter()
			# Create test collection
			testCollection <- new("RTestCollection", 
					project.name    = "RTest Vignette", 
					project.details = "Example test exectuion",
					tester          = "Example tester",
					test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
			
			testCollection <- importTCsFromDir(testCollection,
					xml.dPath = paste0(find.package("RTest"),"/tests/testthat"),
					f.pattern="test.execCache.xml")
			
			outf <- tempfile(fileext=".html")
			
			intern_reporter <- get_reporter()
			
			set_reporter(global_rep)
			
			expect_error(
					RTest:::getExecSummary(
							testCollection@collection[[1]])
			)
			expect_equal(
					RTest:::getExecSummary.html(
							testCollection@collection[[1]]),c()
			)
			
			set_reporter(intern_reporter)
			# Execute all tests
			testCollection <- exec(testCollection, out.fPath = outf, open=FALSE)

			set_reporter(global_rep)
			
			expect_output(
					RTest:::getExecSummary(
							testCollection@collection[[1]]),
					regexp = "'data.frame'\\:\\s2\\sobs.\\sof\\s{1,3}3\\svariables"
			)
			
		})



