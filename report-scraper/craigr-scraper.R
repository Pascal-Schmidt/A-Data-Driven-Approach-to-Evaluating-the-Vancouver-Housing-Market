library(craigr)
library(tidyverse)

results_vancouver <- craigr::rentals(location = "vancouver", area = "van", 
                                     max_results = 3000)

results_burnaby_newwest <- craigr::rentals(location = "vancouver", area = "bnc", 
                                           max_results = 3000)

results_richmond <- craigr::rentals(location = "vancouver", area = "rch", 
                                    max_results = 3000)

results_coquitlam <- craigr::rentals(location = "vancouver", area = "pml", 
                                     max_results = 3000)

results_surrey <- craigr::rentals(location = "vancouver", area = "rds", 
                                     max_results = 3000)

df_craigslist <- rbind(results_vancouver, results_burnaby_newwest,
                       results_richmond, results_coquitlam, results_surrey)

df_craigslist <- read.csv("craigi.csv")

df_craigslist %>%
  dplyr::mutate(., Location = stringr::str_replace_all(df_craigslist$Location, " [0-9]+ ", "") %>%
                  stringi::stri_trim_both(.) %>%
                  stringr::str_to_title(.) %>%
                  stringr::str_replace_all(., "\\bSt\\.\\b|\\bSt\\b|\\bst\\.\\b|\\bst\\b", "Street") %>%
                  stringr::str_replace_all(., "Ave\\.|ave\\.|Ave|ave", "Avenue") %>%
                  stringr::str_replace_all(., "Th |th ", "th ") %>%
                  stringr::str_replace_all(., "[[:punct:]]", "") %>%
                  stringi::stri_trim_both(.)) -> dff

test <- dff$Location

streets <- read.csv("van.csv", header = FALSE)
streets <- as.character(streets$V1)
streets <- stringr::str_replace_all(streets, "East", "E")
streets <- stringr::str_replace_all(streets, "West", "W")

try <- read.csv("craigiiiii.csv")
try %>% 
  tidyr::drop_na(Location, Price) -> try

try$Location <- stringi::stri_trim_both(try$Location)
try <- try[1:2000, ]
try %>%
  group_by(., Location) %>%
  summarise(., count = n()) %>%
  dplyr::arrange(desc(count))


dff$Location[grepl("Yaletown", dff$Location, ignore.case = TRUE)] <- "Homer Street, Yaletown"
dff$Location[grepl("Kitsilano", dff$Location, ignore.case = TRUE)] <- "W Broadway, Kitselano"
dff$Location[grepl("Olympic Village", dff$Location, ignore.case = TRUE)] <- "W 2nd Avenue, Olympic Village"
dff$Location[grepl("Oakridge", dff$Location, ignore.case = TRUE)] <- "Oak Street"
dff$Location[grepl("Coal Harbour", dff$Location, ignore.case = TRUE)] <- "Cordova Street, Coal Harbour"
dff$Location[grepl("False Creek", dff$Location, ignore.case = TRUE)] <- "W 1st Avenue, False Creek"
dff$Location[grepl("Gastown", dff$Location, ignore.case = TRUE)] <- "Powell Street, Gastown"
dff$Location[grepl("Point Grey", dff$Location, ignore.case = TRUE)] <- "W 10th Avenue"
dff$Location[grepl("Mount Pleasant", dff$Location, ignore.case = TRUE)] <- "E 7th Avenue"
dff$Location[grepl("West End", dff$Location, ignore.case = TRUE)] <- "Barclay Street"
dff$Location[grepl("Westend", dff$Location, ignore.case = TRUE)] <- "Barclay Street"
dff$Location[grepl("Chinatown", dff$Location, ignore.case = TRUE)] <- "Keefer Street, Chinatown"
dff$Location[grepl("Science World", dff$Location, ignore.case = TRUE)] <- "Quebec Street"
dff$Location[grepl("Knight", dff$Location, ignore.case = TRUE)] <- "Knight Street"
dff$Location[grepl("Kerrisdale", dff$Location, ignore.case = TRUE)] <- "Larch Street"
dff$Location[grepl("Strathcona", dff$Location, ignore.case = TRUE)] <- "Prior Street"
dff$Location[grepl("Joyce", dff$Location, ignore.case = TRUE)] <- "Vanness Avenue"
dff$Location[grepl("Ontario", dff$Location, ignore.case = TRUE)] <- "Ontario Street"
dff$Location[grepl("Mackenzie", dff$Location, ignore.case = TRUE)] <- "Mackenzie Street"
dff$Location[grepl("Killarney", dff$Location, ignore.case = TRUE)] <- "Kerr Street"
try$Location[grepl("Saughnessey", try$Location, ignore.case = TRUE)] <- "W King Edward Avenue"
try$Location[grepl("South Granville", try$Location, ignore.case = TRUE)] <- "Granville Street"
try$Location[grepl("Commercial Drive", try$Location, ignore.case = TRUE)] <- "E 1st Avenue"
try$Location[grepl("UBC", try$Location, ignore.case = TRUE)] <- "University Boulevard"
try$Location[grepl("Point Grey", try$Location, ignore.case = TRUE)] <- "W 16th Avenue"
try$Location[grepl("Crosstown", try$Location, ignore.case = TRUE)] <- "Union Street"
try$Location[grepl("Marine Drive", try$Location, ignore.case = TRUE)] <- "Collingwood Street"
try$Location[grepl("Fairview", try$Location, ignore.case = TRUE)] <- "W 6th Avenue"
try$Location[grepl("Marpole", try$Location, ignore.case = TRUE)] <- "W 70th Avenue"
try$Location[grepl("Collingwood", try$Location, ignore.case = TRUE)] <- "E 22nd Avenue"



dff$Location[grepl("Yaletown", dff$Location, ignore.case = TRUE)] <- "Homer Street, Yaletown"
dff$Location[grepl("Kitsilano", dff$Location, ignore.case = TRUE)] <- "W Broadway, Kitselano"
dff$Location[grepl("Olympic Village", dff$Location, ignore.case = TRUE)] <- "W 2nd Avenue, Olympic Village"
dff$Location[grepl("Oakridge", dff$Location, ignore.case = TRUE)] <- "Oak Street"
dff$Location[grepl("Coal Harbour", dff$Location, ignore.case = TRUE)] <- "Cordova Street, Coal Harbour"
dff$Location[grepl("False Creek", dff$Location, ignore.case = TRUE)] <- "W 1st Avenue, False Creek"
dff$Location[grepl("Gastown", dff$Location, ignore.case = TRUE)] <- "Powell Street, Gastown"
dff$Location[grepl("Point Grey", dff$Location, ignore.case = TRUE)] <- "W 10th Avenue"
dff$Location[grepl("Mount Pleasant", dff$Location, ignore.case = TRUE)] <- "E 7th Avenue"
dff$Location[grepl("West End", dff$Location, ignore.case = TRUE)] <- "Barclay Street"
dff$Location[grepl("Westend", dff$Location, ignore.case = TRUE)] <- "Barclay Street"
dff$Location[grepl("Chinatown", dff$Location, ignore.case = TRUE)] <- "Keefer Street, Chinatown"
dff$Location[grepl("Science World", dff$Location, ignore.case = TRUE)] <- "Quebec Street"
dff$Location[grepl("Knight", dff$Location, ignore.case = TRUE)] <- "Knight Street"
dff$Location[grepl("Kerrisdale", dff$Location, ignore.case = TRUE)] <- "Larch Street"
dff$Location[grepl("Strathcona", dff$Location, ignore.case = TRUE)] <- "Prior Street"
dff$Location[grepl("Joyce", dff$Location, ignore.case = TRUE)] <- "Vanness Avenue"
dff$Location[grepl("Ontario", dff$Location, ignore.case = TRUE)] <- "Ontario Street"
dff$Location[grepl("Mackenzie", dff$Location, ignore.case = TRUE)] <- "Mackenzie Street"
dff$Location[grepl("Killarney", dff$Location, ignore.case = TRUE)] <- "Kerr Street"


dplyr::setdiff(test, dff$Location)
test <- dff$Location


for (j in 1:(nrow(dff))) {
  for (i in seq_along(streets)) {
    dff$location[[j]] <- stringr::str_replace_all(dff$Location[[j]], paste0(".*\\b", streets[[i]], "\\b.*"), streets[[i]])
  }
}
