library(testthat)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#test_returnValue_variable 
context("utils.test variable")

test_that("test_returnValue_list",{
			
			global_reporter <- get_reporter()
			
			my_reporter <- ListReporter$new()
			
			set_reporter(my_reporter)
			
			RTest:::test_returnValue_list_nodebynode(result =list(
							a = c("a","b","c"),
							d = data.frame(x=c(1,2)),
							f = list(
									e = c("a","b","c")
							)
					),
					reference=list(
							a = c("a","b","c"),
							d = data.frame(x=c(1,2)),
							f = list(
									e = c("a","b","c")
									)
							),
					xmlTestSpec=NULL
					)
			
			my_reporter$end_reporter()
			set_reporter(global_reporter)
			
			for(i in 1:4){
				
				expect_equal(
						class(my_reporter$results$as_list()[[1]]$results[[i]])[1],
						"expectation_success"
				)
				
			}
			
		})

test_that("test_returnValue_list falure",{
			
	expect_error(test_returnValue_list_nodebynode(
			result = list("My friends are great."),
			reference= list("fr[i|e]{3}"),
			xmlTestSpec=XML::xmlNode(
					"return-value",attrs=c("compare-type"="notype")
			)
	))
	
	
})