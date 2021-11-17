library(tidyverse)

path_to_data <- '~/Repos/drug_safety_scopingreview/data/review_data_cleaned.csv'

cleaned_data <- read_csv(path_to_data)

table(cleaned_data$Cleaned_Dataset)

cleaned_data$`Sample Size` <- as.numeric(cleaned_data$`Sample Size`)

cleaned_data %>% group_by(Cleaned_Dataset) %>% summarise(n=n(), 
                                                         mean=mean(`Sample Size`, na.rm = T), 
                                                         median=median(`Sample Size`, na.rm = T), 
                                                         std=sd(`Sample Size`, na.rm = T), 
                                                         iqr=IQR(`Sample Size`, na.rm = T)) %>% write_csv(., '~/Repos/drug_safety_scopingreview/data/dataset_stats.csv')
