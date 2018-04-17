#CS513 - Knowledge Distribution and Data Mining
#US Visa dataset

library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

dataset <- read.csv("us_perm_visas.csv")
summary(dataset)
clean_dataset <- dataset
#start cleaning dataset
#pass values from country of citizenship 2 to the blank rows of country of citizenship 1
clean_dataset[is.na(clean_dataset[,11]),11] <- clean_dataset[!is.na(clean_dataset[,12]),12]
#eliminate extra row for country of citizenship
clean_dataset <- clean_dataset[,-12]
#eliminate non-meaningful fields
clean_dataset$employer_address_1 <- NULL
clean_dataset$employer_address_2 <- NULL
clean_dataset$employer_phone <- NULL
clean_dataset$employer_phone_ext <- NULL
clean_dataset$case_no <- NULL
clean_dataset$case_number <- NULL
