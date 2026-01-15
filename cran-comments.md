## Change Jan 26

Conditionally escaping from xml2 bug on Mac to fix Errors in cran checks.



## change Oct 25

bug fix for order_filter when more than 2 arms


## Change Mar 25

Simplified schema file to pass CRAN tests

## Change Nov 24

Update to print.summary_statistics() on how options() reset is delivered. To fix errors on CRAN checks


## Change July 

Minor bug fixes for relative_risk and dot_plot when the reference group is changed.

The website is still down, so the url is modified. 


## Change Mar 2024

Added extra functionality for reporting

The link to https://eudract-tool.medschl.cam.ac.uk/  is temporarily down. It is mirroring
the github site https://shug0131.github.io/eudraCT/ 

## Test environments
* Rocker Ubuntu
* devtools::check_win_release()
* devtools::check_rhub()
* github

## Change July 2023

Updated contact details

## Change January 2022

Further testing of clinicaltrials.gov functions. Update to provide future compatibility with tidyr::complete().  

Note that the "Clinica lTrials.gov" is not a spelling error in the DESCRIPTION file as it refers to an website
of that name. 


## Change April2021

Adding new function and underlying xslt to facilitate uploads to ClinicalTrials.gov as well as EudraCT .


## Change 24FEB2021

an error is created if input fails the check of group names being at least 4 characters in length.


## Change 03APR2020

Minor corrections to test code, setting a resetting options(StringsAsFactors).

## Change 12FEB2020

Modified the  simpleToEudraCT.xslt file that conditionally sets the version to be 3 for this SOC, and 22 for all other SOC values. A bug was detected with the version number associated with SOC "Product issues", where the default value of 19 is not accepted by the EudraCT portal.

## Changes 06NOV2019

Typos, punctuation corrected in the DESCRIPTIOn file

* u  nits   -->  units
* event;for  -->  event; for
* package: prepares --> package prepares
* EudraCT; formats -->  EudraCT and formats


## Changes 04NOV2019 v2, DESCRIPTION file

Angled brackets removed in the URL field, but a reference with <> is added within the Description field

## Changes 04NOV2019

* In DESCRIPTION the URL is now contained in <> brackets
* capitals have been removed from the description line in DESCRIPTION
* In the vignette the second URL https://eudract.ema.europa.eu/docs/technical/schemas/clinicaltrial/results/adverseEvents.xsd , is retained. Further comment is added to explain it is semi-readible data/code file rather than a webpage.
* message() has replace cat or print in eudract_convert(), simple_safety_xml()
* the on.exit(options()) line has been moved within print.safety_summary() to be immediately after the options() modification.
* The examples, vignette, and testing file have been checked for writing files, and all cases have been modified to use tempdir() or tempfile().



## Previous notes are now fixed:

Found the following (possibly) invalid file URIs:
     URI: medra
       From: man/soc_code.Rd
     URI: eutctid
       From: man/soc_code.Rd
Check: for non-standard things in the check directory, Result: NOTE
   Found the following files/directories:
     'simple.xml' 'table_eudract.xml'



## R CMD check results
There were no ERRORs or WARNINGs.

There is 1 NOTE:

---

Found the following (possibly) invalid URLs:
    URL: https://eudract.ema.europa.eu/
      From: inst/doc/eudract.html
      Status: Error
      Message: libcurl error code 60:
        	server certificate verification failed. CAfile: none CRLfile: none
        	(Status without verification: OK)
    URL: https://eudract.ema.europa.eu/docs/technical/schemas/clinicaltrial/results/adverseEvents.xsd
      From: inst/doc/eudract.html
      Status: Error
      Message: libcurl error code 60:
        	server certificate verification failed. CAfile: none CRLfile: none
        	(Status without verification: OK)
---  

Both websites are outside of my control but give crucial documentation relevant to this package. The URLs are both in full working order. Hence this is a false positive.

## Downstream dependencies

New package so no downstream dependencies.
