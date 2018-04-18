#CS513 - Knowledge Distribution and Data Mining
#US Visa dataset

#Uncomment these for the first run and then comment again
#install.packages('rstudioapi')
#install.packages('readr')


library(rstudioapi) 
library(readr)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

dataset <- read_csv("us_perm_visas.csv")
summary(dataset)
clean_dataset <- dataset
#start cleaning dataset
#pass values from country of citizenship 2 to the blank rows of country of citizenship 1
clean_dataset$country_of_citizenship = ifelse(is.na(clean_dataset$country_of_citizenship), clean_dataset$country_of_citzenship, clean_dataset$country_of_citizenship)
#eliminate extra row for country of citizenship
clean_dataset <- clean_dataset[,-12]
#eliminate non-meaningful fields
clean_dataset$employer_address_1 <- NULL
clean_dataset$employer_address_2 <- NULL
clean_dataset$employer_phone <- NULL
clean_dataset$employer_phone_ext <- NULL
clean_dataset$case_no <- NULL
clean_dataset$case_number <- NULL
clean_dataset$employer_decl_info_title <- NULL
clean_dataset$employer_postal_code <- NULL
clean_dataset$foreign_worker_info_city <- NULL
clean_dataset$orig_case_no <- NULL
clean_dataset$pw_soc_title <- NULL
