# function to get flow metrics results for timeseries data using ffcAPIClient package

# ffcAPIClient::evaluate_alteration requires one "date" column with format MM/DD/YYYY and a corresponding "flow" column
# evaluate_alteration also requires your ffc token and each sites corresponding gage ID, COMID, or latitude/longitude
# imported dataset (in this case .RData type) can have any number of columns corresponding to timeseries flow data for various sites
# column names of the imported dataset correspond to unique sites. if data is already formatted correctly, can comment out some of the data tidying steps
# function uses unique site name to link timeseries data to summary site latitude/longitude data file

# UPDATE: new set up for ffc, still same data requirements. use FFCProcessor$new(), extract only ffc_results for now
# this update also has new arguments you can add if desired, including ones related to fail years, data format, output, max missing days, etc. 


my_ffm_function <- function(my_data, my_output_dir){
  # importing dataset 
  # loads as dataframe called import_df as that was how I generated the .RData file from the .csv files. change "import_df" depending on file type/name
  # see "csv_to_rdata.R" for converting list of csv files to .RData format
  # load(my_data) 
  
  # if data in form of .csv file for example
  # import_df <- read.csv(my_data, header = TRUE, stringsAsFactors = FALSE)
  
  # to get list of site names for function input
  col_names <- names(import_df) #note that first column is date
  col_names
  # load lat/long reference file if don't have gage ID or COMID
  # latlong_data <- read.csv("latlong_site_subset.csv", header = TRUE, stringsAsFactors = FALSE)
  # comid_data <- read.csv("C:/Users/anneh/Documents/Repositories/soc_flowecology_ffc/ffc_input_data/site_comid_class.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # run evaluate_alteration for each site/flow column in input dataset
  get_gage_results <- lapply(2:length(col_names),function(i) {
    i=2
    # tidying/prepping data to meet formatting requirements (ie columns called "date" and "flow", date format MM/DD/YYYY)
    my_input_data <- import_df %>% 
      select(Date,col_names[[i]]) %>% 
      mutate(date2 =  as.POSIXct(strptime(Date,format = "%Y-%m-%d"))) %>% 
      mutate(date = format(date2,"%Y-%m-%d")) %>%
      rename(flow = col_names[[i]]) %>% 
      mutate(flow = as.character(flow)) %>% 
      # mutate(flow = ifelse(flow == 0, 0.0001, flow)) %>% #for nonzero approach, replace all 0 flow with 0.0001 value
      select(date,flow)
    head(my_input_data)
    # define site name, gage ID, or COMID
    # remove leading character "X" generated with import if column names are siteIDs that don't meet R formatting requirements
    site <- sub('X','', col_names[[i]])
    site
    # if don't have gage ID or COMID, retrieve latitudes and longitudes from reference file
    # latlong_site <- latlong_data %>% filter(siteID == site)
    # lat <- latlong_site$latitude
    # long <- latlong_site$longitude
    
    # comid_site <- comid_data %>% filter(siteID == site)
    # comid <- comid_site$COMID
    
    # updated ffc set up, November 13 2020
    ffc
    ffc <- FFCProcessor$new()
    ffc$fail_years_data <- 0
    
    tryCatch({
      ffc$set_up(timeseries = my_input_data,
                 token = get_token(),
                 comid = comid)
      
      ffc$run()
    },
    # warning = function(w){
    #   ffc$run()
    # },
    error = function(err){
      ffc$ffc_results <- NA
      
    }
    )
    # can pull various metrics out as dataframes
    # ffc$alteration
    # ffc$doh_data
    # ffc$ffc_percentiles
    # ffc$ffc_results
    # ffc$predicted_percentiles 
    # ffc$predicted_wyt_percentiles
    
    # just pull out ffc_results for now (annual flow metrics results)
    
    my_results <- ffc$ffc_results
    
    if(is.na(my_results)){
      
      print("there was an error")
      
    } else{
      
      # generating unique file name for each site based on input files. change this depending on preferences
      my_file_name <- paste0(my_output_dir, "/", site, "_", ifelse(grepl("cur", my_data) == TRUE, "cur","ref"),"_", "ffm_results.csv")
      
      # generate .csv in desired output location
      write.csv(my_results, file = my_file_name, row.names = FALSE)
    }
    
  })
  
  
  
  # previous ffc set up; left for now to reference
  
  #   call evaluate_alteration()
  #   use tryCatch() to handle errors. if there are any errors per site/timeseries data, no output file will be generated
  #   if no error, will take ffc_results from functions output environment and write as .csv file
  #   can also use (comid = "" or gage_id = "" instead of latitude/longitude. if want to save plots, use plot_output_folder = "")
  #   my_results <- tryCatch({
  #     ffcAPIClient::evaluate_alteration(
  #       timeseries_df = my_input_data,
  #       token = get_token(),
  #       comid = comid
  #     )
  #   },
  #   warning = function(w){
  #     ffcAPIClient::evaluate_alteration(
  #       timeseries_df = my_input_data,
  #       token = get_token(),
  #       comid = comid
  #     )
  #   },
  #   error = function(err){
  #     data.frame(ffc_results = "error",stringsAsFactors = FALSE)
  # 
  #   }
  #   )
  # 
  #   if(my_results$ffc_results == "error"){
  #     print("there was an error")
  # 
  #   } else {
  #     # retrieving flow metrics dataframe
  #     my_ffc_results <- my_results$ffc_results
  # 
  #     # generating unique file name for each site based on input files. change this depending on preferences
  #     my_file_name <- paste0(my_output_dir, "/", site, "_", ifelse(grepl("cur", my_data) == TRUE, "cur","ref"),"_", "ffm_results.csv")
  # 
  #     # generate .csv in desired output location
  #     write.csv(my_ffc_results, file = my_file_name, row.names = FALSE)
  # 
  #   }
  # 
  # })
}




