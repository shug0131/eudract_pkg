% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clintrials_gov_upload.R
\name{clintrials_gov_upload}
\alias{clintrials_gov_upload}
\title{applies a conversion using xslt from a simple xml file to a ClinicalTrials.gov compatible file, merges into a study record from the portal, and uploads the result.}
\usage{
clintrials_gov_upload(
  input,
  orgname,
  username,
  password,
  studyid,
  url = "https://register.clinicaltrials.gov/",
  check = interactive(),
  output = "study_file.xml",
  backup = "bak_study_file.xml",
  xslt = system.file("extdata", "simpleToCtGov.xslt", package = "eudract"),
  schema_input = system.file("extdata", "simple.xsd", package = "eudract"),
  schema_results = system.file("extdata", "RRSUploadSchema.xsd", package = "eudract"),
  schema_output = system.file("extdata", "ProtocolRecordSchema.xsd", package = "eudract"),
  soc = system.file("extdata", "soc.xml", package = "eudract")
)
}
\arguments{
\item{input}{a character string giving the file path to the simple xml file}

\item{orgname}{a character string giving the organisation name used to log in}

\item{username}{a character string giving the user-name used to log in}

\item{password}{a character string giving the password used to log in}

\item{studyid}{a character string given the unique study id within the portal}

\item{url}{a character string giving the URL of the website to log in. Defaults to the live site, but the testing site is the alternative.}

\item{check}{a logical that will check on the command line if you want to proceed as results will be overwritten. Defaults to TRUE.}

\item{output}{a character string naming the output xml file}

\item{backup}{a character string naming the copy of the original data that is created.}

\item{xslt}{a character string giving the file path to the xslt script. Defaults to the script provided in this package}

\item{schema_input}{a character string giving the file path to the schema for the simple xml file. Defaults to the schema provided in this package}

\item{schema_results}{a character string giving the file path to the schema for the results section of the output. A copy was downloaded and is provided in this package as the default.}

\item{schema_output}{a character string giving the file path to the schema for the overall output. A copy was downloaded and is provided in this package as the default.}

\item{soc}{a character string giving an xml file that contains the System Organ Class look-up table going from EudraCT numbers to ClinicalTrials words.}
}
\value{
Invisibly returns the results from the two API with the portal \code{\link[httr]{response}}, \code{\link[httr]{POST}}  .  A new file is created as a side-effect, which is uploaded into ClinicalTrials.gov. This over-writes the original safety data online with the additional safety events. A backup copy of the original data is also saved.
}
\description{
applies a conversion using xslt from a simple xml file to a ClinicalTrials.gov compatible file, merges into a study record from the portal, and uploads the result.
}
\examples{
safety_statistics <- safety_summary(safety,
                                    exposed=c("Experimental"=60,"Control"=67))
simple <- tempfile(fileext = ".xml")
eudract <- tempfile(fileext = ".xml")
ct <- tempfile(fileext = ".xml")
simple_safety_xml(safety_statistics, simple)
eudract_convert(input=simple,
                output=eudract)
clintrials_gov_convert(input=simple,
                       original=system.file("extdata", "1234.xml", package ="eudract"),
                output=ct)
\dontrun{
  # This needs a real user account to work
  clintrials_gov_upload(
    input=simple,
    orgname="CTU",
    username="Student",
    password="Guinness",
    studyid="1234"
    )

}
}
\seealso{
\code{\link{safety_summary}} \code{\link{simple_safety_xml}} [ClinicalTrials.gov manual](https://prsinfo.clinicaltrials.gov/prs-users-guide.html#section10)
}
