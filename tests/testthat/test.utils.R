library(base64)
library(magrittr)
context("utils")

# Test the right implementation of the image converter
# which first has to work without any tag specification
test_that("plot2png src", {
			
			img.fPath <- list.files(find.package('RTest'),recursive = T,pattern='Roche_Logo',full.names=T)[1]
			
			tf <- tempfile()
			base64::encode(img.fPath, tf)
			src <- sprintf("data:image/png;base64,%s", paste(readLines(tf),
							collapse = ""))
			
			expect_equal(
					RTest:::png2base64(img.fPath,
							img.returnAsTag = FALSE,
							img.title = "Roche Logo",
							img.width = "100px"),
					src)
		})

# Second without any defined image width
test_that("plot2png width", {
			
			img.fPath <- list.files(find.package('RTest'),recursive = T,pattern='Roche_Logo',full.names=T)[1]
			
			tf <- tempfile()
			base64::encode(img.fPath, tf)
			src <- sprintf("data:image/png;base64,%s", paste(readLines(tf),
							collapse = ""))
			value <- sprintf("<img src='%s' alt='%s' />", src, "Roche Logo")
			
			expect_equal(
					RTest:::png2base64(img.fPath,
							img.returnAsTag = TRUE,
							img.title = "Roche Logo",
							img.width = NULL),
					value)
		})
# NOTE: With the width it is tested in examples / vignettes

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# systemInfo.packages

test_that("systemInfo.packages error",{
			expect_error(
					RTest:::systemInfo.packages(which="error")
					)
		})

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# normalizeDate
test_that("normalizeDate",{
			expect_equal(normalizeDate("15.September.1988",FALSE),"15.09.1988")
			
			expect_equal(
					normalizeDate("15.MyName.1988",FALSE,months = c("jan"="january","sep"="myname")),
					"15.02.1988")
			
			expect_equal(
					normalizeDate("15.sep.1988",FALSE,months = c("jan"="january","sep"="myname")),
					"15.02.1988")
			
			expect_equal(normalizeDate("15.MyName.1988",TRUE,months = c("jan"="january","sep"="myname"))
				%>% class(),
					"Date")
			
		})