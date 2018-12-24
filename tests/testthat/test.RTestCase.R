context("RTestCase")

# Check that an empty RTestCase can be initialized
test_that("RTestCase initialize",{
			
			expect_equal(		
					RTestCase(
							ID                = "TC01",
							tc.type           = "mytest" ,
							synopsis          = list(),
							input.data        = list(one=data.frame(x=c(1,2))),
							tests             = new.env(),
							test.for          = "mypackage",
							test.result       = NULL,
							xml.fPath = "",
							xml.root = XML::xmlNode("empty")
					) %>% class() %>% as.character(),"RTestCase")
			expect_equal(		
					RTestCase(
							ID                = "TC01",
							tc.type           = "mytest" ,
							synopsis          = list(),
							input.data        = list(one=data.frame(x=c(1,2))),
							tests             = new.env(),
							test.for          = "mypackage",
							test.result       = NULL,
							xml.fPath = "",
							xml.root = XML::xmlNode("empty")
					) %>% getXMLRoot(),XML::xmlNode("empty"))
			expect_equal(		
					RTestCase(
							ID                = "TC01",
							tc.type           = "mytest" ,
							synopsis          = list(),
							input.data        = list(one=data.frame(x=c(1,2))),
							tests             = new.env(),
							test.for          = "mypackage",
							test.result       = NULL,
							xml.fPath = "",
							xml.root = XML::xmlNode("empty")
					) %>% getTestFor(),"mypackage")
			
			
		
		})