library(testthat)
library(RTest)

context("xmlReadData")


test_that("xmlReadData NULL",
		{
			data <- '<variable name="myvar" value="4" type="numeric"/>'
			item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
 expect_equal(xmlReadData_variable(item),4)
 expect_equal(xmlReadData_variable(NULL),NULL)

})

