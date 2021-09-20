## JMMI validation
rm(list = ls())

## libs
library(tidyverse)
library(openxlsx)
library(cleaninginspectoR)
library(cluster)

## sources
source("./R/data_falsification.R")
source("./R/check_log.R")
source("./R/minimum_standards.R")
source("./R/check_time.R")
source("./R/item_boxplots.R")

## load tool
tool <- read.xlsx("./inputs/kobo_tool.xlsx")

## load data
#data <-"./inputs/jmmi_dataset_may_2021.xlsx"

#sheets <- openxlsx::getSheetNames(data)
#SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data)
#names(SheetList) <- sheets

#raw <- SheetList[[1]]
#log <- SheetList[[2]]
#clean <- SheetList[[3]]

clean <- read.csv("./inputs/cleaned_june_jmmi_raw_data.csv", stringsAsFactors = FALSE)
clean$X <- NULL
clean$X_index <- NULL
names(clean)[names(clean) == "_uuid"] <- "uuid"
clean <- clean %>% mutate(., index = 1:nrow(.))

log <- read.csv("./inputs/UPDATED_CLEANING_LOG_jmmi.csv", stringsAsFactors = FALSE)


## check data
clean.i <- inspect_all(clean) %>% mutate(uuid= clean[.$index,"uuid",drop=T],
                                         area = clean[.$index,"location_city",drop=T]) %>% filter(!is.na(uuid))

clean.c <- semi_join(clean.i, log, c("uuid" = "uuid"))

write.xlsx(clean.i, paste0("./outputs/cleaning issues_",lubridate::today(),".xlsx"))

## check log
log <- log %>% filter(Question %in% names(clean))

log.c <- check_log(data = clean, log = log, variable = "Question", 
                   old_log_var = "Old_Value", new_log_var = "New_Value", 
                   uuid_data = "uuid", uuid_log = "X_uuid") %>% 
         mutate(check = ifelse(New_Value == value_extracted, "Log Applied", "Please check")) %>% filter(., check == "Please check")

write.xlsx(log.c, paste0("./outputs/nga2009_jmmi_log application_",lubridate::today(),".xlsx"))

## minimum standards
min.standards <- minium_standards(clean, "town", ends_with("_price")) %>% filter(., minimum_standards == "requirement not met")
write.xlsx(min.standards, paste0("./outputs/standards not met_",lubridate::today(),".xlsx"))

## similar surveys
similar.surveys <- calculateDifferences(clean, tool) %>% filter(., number.different.columns < 6)
write.xlsx(similar.surveys, paste0("./outputs/similar surveys_",lubridate::today(),".xlsx"))

## time checks
time.c <- check_time(clean, "5", "60") %>% filter(., value < 4)
clean.red <- clean %>% select("uuid", "items_sell")

time.c <- left_join(time.c, clean.red, "uuid")
write.xlsx(time.c, paste0("./outputs/time check_",lubridate::today(),".xlsx"))

## price analysis
item_boxplots(clean, "town", ends_with("_price"))

price.analysis <- clean %>% select("lga", "town", ends_with("_price")) %>% melt()

price.comparison <- price.analysis %>% group_by(town, variable) %>% filter(!is.na(value)) %>% dplyr::summarise(., mean = round(mean(value),0), 
                                                                                                                  min = min(value), 
                                                                                                                  max = max(value),
                                                                                                                  sd = sd(value)) %>%
                                                                      mutate(coeff_var = sd/mean,
                                                                             check = ifelse(coeff_var > 1, "check", "ok"))

write.xlsx(price.comparison, paste0("./outputs/price comparison by town_",lubridate::today(),".xlsx"))

## By unit

# rice <- clean %>% select("rice_unit", "rice_price") %>% group_by(rice_unit) %>% filter(!is.na(rice_price)) %>% 
#                  dplyr::summarise(., mean = round(mean(rice_price),0), 
#                                      min = min(rice_price), 
#                                      max = max(rice_price),
#                                      sd = sd(rice_price))

# Create list of dataframe for each item
items <- select(clean, ends_with("_price")) %>% colnames(.)
items <- gsub("_price", "", items)

# Split dataframe in a list of dataframe for each items
splitted_df <- sapply(items,
                      function(x) clean[startsWith(names(clean),x)],
                      simplify = FALSE)

# Select only the items you want and rename
splitted_df <- lapply(splitted_df, function (x) {select(x, ends_with("_unit"), ends_with("_price"))})

names <- c("unit", "price")

splitted_df <- lapply(splitted_df, setNames, names)

# Apply summarise function
unit.comparison <- map(splitted_df, ~ .x %>% group_by(unit) %>% filter(!is.na(price))%>% summarise(mean = round(mean(price), 0),
                                                                                                        min = min(price),
                                                                                                        max = max(price),
                                                                                                        sd = sd(price)))
unit.comparison <- lapply(unit.comparison, function(x) { x[is.na(x)] <- "standard_unit"})

write.xlsx(unit.comparison, paste0("./outputs/unit and price comparison by item_",lubridate::today(),".xlsx"))



