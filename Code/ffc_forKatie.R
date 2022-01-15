# Script to get flow metrics results for timeseries data using ffcAPIClient package
# Annie Holt, 01/14/2022

# ffcAPIClient::evaluate_alteration requires one "date" column with format MM/DD/YYYY and a corresponding "flow" column
# evaluate_alteration also requires your ffc token and each sites corresponding gage ID, COMID, or latitude/longitude

# UPDATE: new ffc set up, as of November 13 2020, still same data requirements. use FFCProcessor$new(), extract only ffc_results for now
# this update also has new arguments you can add if desired, including ones related to fail years, data format, output, max missing days, etc. 

# # installation for the first time: 
# install.packages("processx", "devtools","glue")
# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
# # retrieve token: log into https://eflows.ucdavis.edu. 
# # once logged in, press F12 and switch to the Consol tab on the Inspector.
# # in consol, type localStorage.getItem('ff_jwt'). text placed on the line after is your "token"
# # make sure to set_token() after loading ffcAPIClient 

# load libraries
library(ffcAPIClient)
library(devtools)
library(processx)
library(scales)
library(tidyverse)
library(lubridate)


#my token from eflows website: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBbm5lIiwibGFzdE5hbWUiOiJIb2x0IiwiZW1haWwiOiJhZ2hvbHRAdWNkYXZpcy5lZHUiLCJyb2xlIjoiVVNFUiIsImlhdCI6MTYwNTI5NTQzMn0.wzbBa5rLs6DAvEpUQVVLmQx23g2EBgEQhM2wcqyweUc"
ffcAPIClient::set_token("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBbm5lIiwibGFzdE5hbWUiOiJIb2x0IiwiZW1haWwiOiJhZ2hvbHRAdWNkYXZpcy5lZHUiLCJyb2xlIjoiVVNFUiIsImlhdCI6MTYwNTI5NTQzMn0.wzbBa5rLs6DAvEpUQVVLmQx23g2EBgEQhM2wcqyweUc")


### FLOW DATA IMPORT AND PREPPPING ####

# input data, from Katie
# stored in "data" folder
load("data/01_high_MP_gages_streamflow.RData")
load("data/01_low_MP_gages_streamflow.RData")

# reformatting data, so proper date formatting and column names
# site number is the gage id

highMP_gages_final <- highMP_gages %>% 
  select(site_no,Date,Flow) %>% 
  mutate(date = format(Date, "%m/%d/%Y")) %>% 
  rename(flow = Flow) %>% 
  mutate(flow = as.character(flow)) %>% 
  select(site_no, date,flow)

lowMP_gages_final <- lowMP_gages %>% 
  select(site_no,Date,Flow) %>% 
  mutate(date = format(Date, "%m/%d/%Y")) %>% 
  rename(flow = Flow) %>% 
  mutate(flow = as.character(flow)) %>% 
  select(site_no,date,flow)


#### TEST RUN ####

# # just pull one site to test the process
# test <- highMP_gages_final %>% 
#   filter(site_no == "10290500") %>% 
#   select(date, flow)
# 
# ffc <- FFCProcessor$new()
# ffc$fail_years_data <- 0
# ffc$set_up(timeseries = test,
#            token = get_token(),
#            gage_id = "10290500")
# ffc$run()
# my_results <- ffc$ffc_results


#### FIANL RUN ####

# looping through each site and calculating metrics like above
# append together in long format 


# create empty data frame to write results to
ffm_final <- data.frame(site_no = NULL, year = NULL, flow_metric = NULL, result = NULL)

# list of sites to iterate through (one list for high gages, one for low in this case)
# site_list <- unique(highMP_gages_final$site_no)
site_list <- unique(lowMP_gages_final$site_no)

for (i in 1:length(site_list)) {
  
  # retain one site at a time, select just date and flow data
  # either high or low gages dataset
  input_flow_df <- lowMP_gages_final %>% 
    filter(site_no == site_list[[i]]) %>% 
    select(date,flow)
  
  # call FFC processor and feed in data, including site number/gage id
  ffc <- FFCProcessor$new()
  ffc$fail_years_data <- 0
  ffc$set_up(timeseries = input_flow_df,
             token = get_token(),
             gage_id = site_list[[i]])
  ffc$run()
  # extract results
  ffm_results <- ffc$ffc_results
  
  # reformat results
  ffm_results_long <- ffm_results %>% 
    rename(year = Year) %>% 
    # pivot all columns but year column
    pivot_longer(-year, names_to = "flow_metric", values_to = "result") %>% 
    # add site info column
    mutate(site = site_list[[i]]) %>% 
    select(site, year, flow_metric, result)
  
  # append to final dataset
  ffm_final <- rbind(ffm_results_long, ffm_final)
  
}

# assign to separate final datasets
# ffm_highMP_gages <- ffm_final
ffm_lowMP_gages <- ffm_final


#### EXPORT ####

write_csv(ffm_highMP_gages, "data/highMP_gages_ffms.csv")
write_csv(ffm_lowMP_gages, "data/lowMP_gages_ffms.csv")

#### END ####

