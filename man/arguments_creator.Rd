% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RTestCase.test-adapter.R
\name{arguments_creator}
\alias{arguments_creator}
\title{Read arguments from RTest 'param'-XML Node}
\usage{
arguments_creator(parameters_xml_definition, input_data = NULL)
}
\arguments{
\item{parameters_xml_definition}{(\code{XMLNode}) An XML Node that has elements of
 kind RTestData_variable, RTestData_image, RTestData_vector, RTestData_list,
 RTestData_data.frame in it. Please see the \code{RTest.xsd} to be found in 
\code{file.path(find.package("RTest")/"xsd/RTest.xsd")}}

\item{input_data}{(\code{list}) A named list of values of kind data.frame, character, 
numeric or list of those}
}
\value{
A named list. For the name of the RTestData_input_data element, please see
	details.
}
\description{
Read arguments from RTest 'param'-XML Node
}
\details{
This function will read in all parameters exept the one named "RTestData_input_data"
	into a list by using \link{xmlReadData_to_list}. The parameter "RTestData_input_data" is
 written into an additional item of the list. The name of this item is given
 by the "param" attribute of the XMLNode "RTestData_input_data". The value is given
 by the list item of the list "input_data" that can be found by the "name" attribute
 of the XMLNode "RTestData_input_data".
}
\author{
Sebastian Wolf \email{sebastian@mail-wolf.de}
}
