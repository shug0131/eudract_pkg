# eudract 1.0.4

Correction to an overly-complex xml schema. 

# eudract 1.0.2

Evading a windows bug whereby a call to xml2::xml_validate() crashes the system. 
The checks are not run if the OS is windows. 


# eudract 1.0.1

Fixing a bug when a relative_risk object was passed into dot_plot and the reference
group is changed from the default. 

# eudract 1.0.0

Adding extra functionality to provide standard reports and dot plot figures


# eudract v0.10.2

modifying contact details


# eudract v0.10.1

Added in testing of ClinicalTrials.gov functions, some of which works via test_that, and some needs user/passwords, and so should be sensitively skipped.  Resolving future-compatibility issues with `tidyr::complete` .



# eudract v0.10.0

Major version change to include uploads of the equivalent information to ClinicalTrials.gov.


# eudract v0.9.3

Error is caused if any of the group names are less than 4 characters in length, as EudraCT will reject this.


# eudract v 0.9.2

Minor bug fix in the test scripts


# eudract v0.9.1

A bug was detected with the version number associated with SOC "Product issues", where the default value of 19 is not accepted by the EudraCT portal. A fix is implemented in the simpleToEudraCT.xslt file that conditionally sets the version to be 3 for this SOC, and 22 for all other SOC values. 

# eudract v0.9.0

This is the first release on CRAN
