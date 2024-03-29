# Test when the varnames are missing some

# Test when there are extra superfluous variables


# want there to be no group at all for nonserious, and a warning
context("testing error catching")

if( is_testing()){path <- "."} else{ path <- "tests/testthat"}

test_that("no aes",
          {

            no_aes <- read.csv(file.path(path,"data/no_aes.csv"), stringsAsFactors = FALSE)
            expect_warning(output <- safety_summary(no_aes, exposed=10, soc="soc_term"))
            expect_equal( names(output), c("GROUP", "SERIOUS"))
            simple_safety_xml(output, file.path(tempdir(),"test_simple.xml"))
            expect_true(eudract_convert( file.path(tempdir(),"test_simple.xml"),
                                         file.path(tempdir(),"test_output.xml")))


          }
            )

test_that("no saes",
          {
            no_saes <- read.csv(file.path(path,"data/no_saes.csv"), stringsAsFactors = FALSE)
            expect_warning(output <- safety_summary(no_saes, exposed=10, soc="soc_term"))
            expect_equal( names(output), c("GROUP", "NON_SERIOUS"))
            simple_safety_xml(output, file.path(tempdir(),"test_simple.xml"))
            expect_true(eudract_convert( file.path(tempdir(),"test_simple.xml"),
                                         file.path(tempdir(),"test_output.xml") ))

          }
)

# need to write a test when there are no events at all for a group
test_that("empty group",
          {
            no_saes <- read.csv(file.path(path,"data/no_saes.csv"), stringsAsFactors = FALSE)
            expect_error(output <- safety_summary(no_saes, exposed=c(10,10), soc="soc_term"))
            expect_error(
              output <- safety_summary(no_saes,
                                       exposed=c("Experimental"=10,"Control"=10),
                                       soc="soc_term")
            )
            expect_warning(
              output <- safety_summary(no_saes,
                                       exposed=c("Experiment"=10,"Control"=10),
                                       soc="soc_term")
            )
            expect_equal(nrow(output$GROUP),2)
            simple_safety_xml(output, file.path(tempdir(),"/test_simple.xml"))
            expect_true(eudract_convert( file.path(tempdir(),"/test_simple.xml"),
                                         file.path(tempdir(),"/test_output.xml") ))
            }
)

test_that("missing variable",{
  aes <- read.csv(file.path(path,"data/events.csv"), stringsAsFactors = FALSE)
  expect_is(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term"),"safety_summary")
  expect_error(safety_summary(aes[,-2], exposed=c(700,750,730), soc_index = "soc_term"),"your input data are missing the following variables:")
})


test_that("Too short group names",{

  aes <- read.csv(file.path(path,"data/events.csv"), stringsAsFactors = FALSE)
  aes$group <- ifelse(aes$group=="Group A", "A", aes$group)
  expect_error(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term"),
               "Group names must be at least 4 characters in length.")

})


test_that("exposed has too few elements",{
  aes <- read.csv(file.path(path,"data/events.csv"), stringsAsFactors = FALSE)
  expect_is(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term"),"safety_summary")
  expect_error(safety_summary(aes, exposed=c(700,750), soc_index = "soc_term"),"the argument 'exposed' has fewer elements than the number of groups")

})

test_that(" numbers exposed to low",
          {
            expect_is( safety_summary(safety, exposed=c(15,33)), "safety_summary")

            expect_error(safety_summary(safety, exposed=c(15,32)),
                         "'exposed' argument has elements that are too small"
            )
          }
)



test_that("wrong lenght for excess deaths",{
  aes <- read.csv(file.path(path,"data/events.csv"), stringsAsFactors = FALSE)
  expect_is(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term"),"safety_summary")
  expect_error(safety_summary(aes, exposed=c(700,750,730),
                              excess_deaths = c(1,2),
                              soc_index = "soc_term"),
               "the argument 'excess_deaths' needs to be the same length as the number of groups")

})

test_that("wrong class input to simple_safety_xml",
          { expect_error(simple_safety_xml(1:10,file.path(tempdir(),"simple.xml")), "is not a safety_summary object")}
          )


test_that("induced errors in the safety_summary output",
          {
            aes <- read.csv(file.path(path,"data/events.csv"), stringsAsFactors = FALSE)
            safety_stats <- safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term")
            x <- safety_stats$SERIOUS$groupTitle
            x <- ifelse(x=="Group A", "Placebo",x)
            safety_stats$SERIOUS$groupTitle <- x
            expect_warning( simple_safety_xml(safety_stats,file.path(tempdir(),"simple.xml")) )
            expect_error(eudract_convert(file.path(tempdir(),"simple.xml"),
                                         file.path(tempdir(),"eudract.xml")))
            expect_error(clintrials_gov_convert(file.path(tempdir(),"simple.xml"),
                                         file.path(tempdir(),"ct.xml")))

          }
          )


test_that("wrong names for excess deaths",{
  expect_error(safety_summary(safety, exposed=c(67,60),excess_deaths =c("Rx"=60,"Cx"=67)),
               "the names of 'excess_deaths' do not match up to the values in 'data\\$group'"
               )
  })


test_that("create.safety_summary name checks",
          {
            safety_stats <- safety_summary(safety, c(67,60))
            expect_is( create.safety_summary(safety_stats$GROUP,
                                             safety_stats$NON_SERIOUS,
                                             safety_stats$SERIOUS), "safety_summary")
            expect_error(
              create.safety_summary(safety_stats$GROUP %>% select(-title),
                                    safety_stats$NON_SERIOUS,
                                    safety_stats$SERIOUS),
            "group data.frame does not have the correct variable names"
              )
            expect_error(
              create.safety_summary(safety_stats$GROUP,
                                    safety_stats$NON_SERIOUS %>% select(-subjectsAffected),
                                    safety_stats$SERIOUS),
              "non_serious data.frame does not have the correct variable names"
            )
            expect_error(
              create.safety_summary(safety_stats$GROUP ,
                                    safety_stats$NON_SERIOUS,
                                    safety_stats$SERIOUS%>% select(-deaths)),
              "serious data.frame does not have the correct variable names"
            )



          }

          )

test_that("empty user details",
          {
            expect_error( clintrials_gov_upload(
              input=file.path(path, "simple.xml"),
              backup=file.path(path,"bak_study_file.xml"),
              output=file.path(path,"study_file.xml"),
              orgname="",
              username=Sys.getenv("ct_user"),
              password=Sys.getenv("ct_pass"),
              studyid="1234",
              url="https://prstest.clinicaltrials.gov/"),
              "invalid orgname argument"
            )
            
            expect_error( clintrials_gov_upload(
              input=file.path(path, "simple.xml"),
              backup=file.path(path,"bak_study_file.xml"),
              output=file.path(path,"study_file.xml"),
              orgname="AddenbrookesH",
              username="",
              password=Sys.getenv("ct_pass"),
              studyid="1234",
              url="https://prstest.clinicaltrials.gov/"),
              "invalid username argument"
            )
            
            if(Sys.getenv("ct_user")=="" | Sys.getenv("ct_pass")==""){skip("Need to have the userid/password as environment variables")}
            expect_error( clintrials_gov_upload(
              input=file.path(path, "simple.xml"),
              backup=file.path(path,"bak_study_file.xml"),
              output=file.path(path,"study_file.xml"),
              orgname="AddenbrookesH",
              username=Sys.getenv("ct_user"),
              password="",
              studyid="1234",
              url="https://prstest.clinicaltrials.gov/"),
              "invalid password argument"
            )
            
            expect_error( clintrials_gov_upload(
              input=file.path(path, "simple.xml"),
              backup=file.path(path,"bak_study_file.xml"),
              output=file.path(path,"study_file.xml"),
              orgname="AddenbrookesH",
              username=Sys.getenv("ct_user"),
              password=Sys.getenv("ct_pass"),
              studyid="",
              url="https://prstest.clinicaltrials.gov/"),
              "invalid studyid argument"
            )
            
            
            
            }
          
          )

test_that("missing values",
          {
          aes <- read.csv(file.path(path,"data/events.csv"), stringsAsFactors = FALSE)
          aes$soc[1] <- NA
          expect_warning(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term", na.action=na.omit),"Your input data contain missing")   
          aes$soc[1] <- ""
          expect_warning(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term", na.action=na.omit),"Your input data contain missing")   
          expect_error(safety_summary(aes, exposed=c(700,750,730), soc_index = "soc_term"))   
          
          }
)


test_that("invalid ct inputs",{
  
  if( is_testing()){path <- tempdir()} else{ path <- "tests/testthat"}
  if(is_testing()){original_path <- "."}else{original_path <- "tests/testthat"}
  safety_statistics <- safety_summary(safety, exposed=c("Experimental"=60,"Control"=67))
  simple_safety_xml(safety_statistics, file.path(path,"simple.xml"))
  
  expect_error(
    clintrials_gov_convert(input=file.path(path,"simple.xml"),
                           original=file.path(path,"simple.xml"),
                           output=file.path(path,"table_ct.xml")
                           # schema_results = system.file("extdata","ProtocolRecordSchema.xsd", package="eudract")
                           
    ),"original study file is invalid")
  
  expect_error(
    clintrials_gov_convert(input=file.path(original_path, "1234.xml"),
                           original=file.path(original_path, "1234.xml"),
                           output=file.path(path,"table_ct.xml")
                           # schema_results = system.file("extdata","ProtocolRecordSchema.xsd", package="eudract")
                           
    )
    ,"safety data is invalid")
})



## SOC=Product Issue 
#  For this SoC,  if we use the default MedDRA version =22, then uploading to EUdraACT comes with 
# and error " EUTCT ID [100000167503] with version [22] is non current or has never been previously used."

#  So the xlst conversion step detects this SoC and changes the version to 3, which EUdract likes

if(FALSE){
df <- data.frame(
  subjid=1:2, term="bad drug",
  soc=10077536,
  serious=FALSE, related=FALSE,fatal=FALSE,
  group=c("Experimental","Control")
)
safety_stats <- safety_summary(df, exposed=c("Experimental"=60,"Control"=67))
simple_safety_xml(safety_stats, file = "simple.xml")
eudract_convert(input="simple.xml",
                output="eudract.xml")
}
# upload this version (to the test site) to check it works Ok
# manually edit line 42 of eudract.xml
# to <version>22</version> to reproduce the error on upload.

