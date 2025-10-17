


## Load packages
library(eudract)
library(foreign)
library(haven)
library(utf8)
library(stringi)
library(data.table)


## Load safety dataset
# Before loading the dataset, make sure that no other data is in the R workspace.
rm(list = ls(all.names = TRUE))

# Set working directory, where needed datasets are saved
setwd("C:/EMA Upload_Windows_Full")
setwd("tests/testthat/data/")
setwd("EMA Upload_Windows_Full/")

# Load safety dataset in R 
safety <- read.delim("SyntheticData_safety_dataset_External.txt", header = TRUE, sep = "\t")
dim(safety)
head(safety)

# Load basic dataset in R 
basic <-  read.delim("SyntheticData_basic_dataset.txt", header = TRUE, sep = "\t")
dim(basic)
head(basic)


## Definition of safety population:

safety <- safety[safety$A_SP==1,]

basic$SAP <- ifelse(basic$A_SP==1, 1, 0)
table(basic$SAP)
# N=1600 patients belong to the safety population. 
basic <- basic[basic$SAP==1,]

# Merge safety dataset with basic dataset
data <- merge(basic, safety, by="PatID")
dim(data)
head(data)
table(data$cohort.x,data$cohort.y)
table(data$A_SP.x,data$A_SP.y)
# Check if all cases belong to the safety population
table(data$SAP)
# n = 20000 cases belong to the safety population

data <- data[data$SAP==1,] 


## Needed variables for EMA-upload

# term - a text description of the event
# Delete whitespace before and after each term
data$term <- trimws(data$PT_MedDRA)

# Check: All string variables need to be UTF-8 coded!

table(Encoding(data$term))
# All strings are coded "unknown" (which works fine)

table(data$term)


# soc - the meddra code for the System Organ Class
# Trim whitespace before and after each term
table(data$SOC_MedDRA)
sum(is.na(data$SOC_MedDRA))
names(data)[names(data)=="SOC_MedDRA"] <- "soc"
data$soc <- trimws(data$soc)
table(data$soc)


# subjid - a unique subject identifier
# Rename the variable PatID to subjid
names(data)[names(data)=="PatID"] <- "subjid"


# fatal - a numerical 0/1 to indicate if the event was fatal (1) or not (0)
table(data$CTC_grade) 
sum(is.na(data$CTC_grade)) # no missings

data$fatal <- as.integer(ifelse(data$CTC_grade==5, 1, 0))
table(data$fatal)
table(data$CTC_grade, data$fatal) # correct


# serious - a numerical 0/1 to indicate if the event was serious (1) or not (0)
table(data$SAE) 
sum(is.na(data$SAE)) # no missings

data$serious <- as.integer(ifelse(data$SAE == "missing", 9,
				  ifelse(data$SAE == 0, 0, 1)))

table(data$serious)
table(data$SAE, data$serious) # correct


# group - the treatment group for the subject
# The level of the group-variable must consist of a name with at least 4 letters. 
# Therefore the levels of the grouping variable will be renamed.
table(data$cohort.x) 

sum(is.na(data$cohort.x)) # no missings

table(data$cohort.x)
data$group <- ifelse(data$cohort.x == 1, 0, ifelse(data$cohort.x == 2, 1, ifelse(data$cohort.x == 3, 2, 3)))
data$group <- factor(data$group, 
                     levels=c(0,1,2,3),
                     labels=c("A arm","B arm", "C arm","D arm"))

table(data$group)
table(data$cohort.x, data$group) # correct


# related - a logical indicating if the event is related to the treatment (1) or not (0)
table(data$relationship_study_drugs) 
sum(is.na(data$relationship_study_drugs)) # no missings

data$related <- as.integer(data$relationship_study_drugs)
table(data$related) 
table(data$relationship_study_drugs, data$related) # correct


## Keep only necessary variables

data_ema <- data[, c("subjid", "soc", "fatal", "serious", "group", "term", "related")]
dim(data_ema) 
head(data_ema)

# Add meddra-code
data_ema <- merge(data_ema, soc_code, by.x="soc", by.y="soc_term")

# The variables Meddra is the information which is needed for soc. Therefore the initial variable
# soc will be renamed into soc_term. 
names(data_ema)
names(data_ema)[names(data_ema) == "soc"] <- "soc_term" # rename new variable
names(data_ema)[names(data_ema) == "meddra"] <- "soc" # rename new variable
names(data_ema)

# Change format of eutctId 

data_ema$eutctId <- format(data_ema$eutctId, digits=12, nsmall=0)

# Check structure of processed dataset
head(data_ema, n = 6)
str(data_ema)
# The structure of the dataset corresponds to the EMA format.


## Add missing information
# The EMA-package doesn't allow missing values within the dataset, because they doesn't
# allow to compute summary statistics. Therefore all data rows with at least one missing
# information need to be imputed.

sum(is.na(data_ema)) 
dim(data_ema)
# There are no rows with missing data.

data_final <- subset(data_ema, !is.na(subjid) & !is.na(group) & !is.na(serious) & !is.na(fatal) &
				      !is.na(related) & !is.na(soc))

sum(is.na(data_final))
# There are no rows with missing data.
dim(data_final)

## Calculate summery statistics 
# EMA provide a function that derives the patient and event counts as required in a format internal to R.

# Number of patients in each group.
# ATTENTION: Names of groups need to match to the names defined in variable "group"


basic$cohort_SP2 <- ifelse(basic$cohort==1, 0, ifelse(basic$cohort==2, 1, ifelse(basic$cohort==3, 2, 3)))
table(basic$cohort_SP2)
subjectsExposed <- as.vector(table(basic$cohort_SP2))
names(subjectsExposed) <- names(table(data$group))


# count of deaths !not! in the Safety data. Could be c(0,0,0,0).
data_final$group2 <- ifelse(data_final$group=="A arm", 0,ifelse(data_final$group=="B arm", 1,ifelse(data_final$group=="C arm", 2,3)))
table(data_final$group2,data_final$group)
fatal_data <- data_final[, c("subjid", "fatal", "group2")]
fatal_data <- aggregate(fatal_data, by = list(data_final$subjid), FUN = max)
dim(fatal_data)
dim(unique(fatal_data))
deaths_internal <- table(fatal_data$fatal, fatal_data$group2)
data_final$group2 <- NULL



table(basic$os_status)

table(basic$os_status,basic$cohort)

# Internal deaths (deaths without CTC 5 AE) need to be calculated out.
basic$death_status<-ifelse(basic$os_status==1, 1, NA)
deaths_total <- table(!is.na(basic$death_status), basic$cohort)
deathsExternal <- deaths_total[c(2,4,6,8)] - deaths_internal[c(2,4,6,8)]  
names(deathsExternal) <- names(table(data_final$group))

safety_statistics <- safety_summary(data_final, 
                                    exposed=subjectsExposed,
                                    excess_deaths = deathsExternal, 
                                    freq_threshold = 0
                                    )

safety_statistics


#saveRDS(safety_statistics, file="safety_statistics.RData")
#?saveRDS

#readRDS("safety_statistics.RData")


## Compute XML-file
# First, safety_statistics will be exported into a XML-file called "simple.xml".
# Then, the XML-file will be converted into the EudraCT-format.
simple <- tempfile(fileext = ".xml")
eudract_upload_file <- tempfile(fileext = ".xml")
simple_safety_xml(safety_statistics, simple)

#Warning message:
#  In simple_safety_xml(safety_statistics, simple) :
#  Element 'TABLE': Missing child element(s). Expected is one of ( NON_SERIOUS, SERIOUS ).



## Converting XML-Datei into EudraCT-format
eudract_convert(input=simple, output=eudract_upload_file)

#Error in eudract_convert(input = simple, output = eudract_upload_file) : 
#  Element 'TABLE': Missing child element(s). Expected is one of ( NON_SERIOUS, SERIOUS ).
