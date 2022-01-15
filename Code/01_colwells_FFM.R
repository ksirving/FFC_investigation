### find colwells scores, check hydrograph/ffm values for low scoring 


# Colwell calculation - from Ryan Peek - flow seasonality repo ------------

# rerun Colwell with revised flows from REF

library(hydrostats)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)
library(tictoc) # timing stuff
library(fs)
library(purrr)


# 01: Import Flow/GAGE Data -----------------------------------------------

# flow data
load("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/data/usgs_Q_daily_alt_gages_trim.rda") # nrow=8977068
load("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/data/usgs_Q_daily_ref_gages_trim.rda") # nrow=2442511

sort(unique(usgs_flows_alt_trim$site_no))

# get gage metadata:
gage_alt_meta <- usgs_flows_alt_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm:flowcnt) # n=517
gage_ref_meta <- usgs_flows_ref_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm, usgs_lat, usgs_lon, huc8, parm_cd, stat_cd, 
         ref_yr_start, ref_yr_end, ref_por_range, yr_begin, yr_end, yr_total, 
         gagetype, parm_cd:flowcnt) # n=221

# bind rows
gage_metadata <- bind_rows(gage_alt_meta, gage_ref_meta)

# 02: CALCULATE COLWELL -------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)
# view(usgs_flows_ref_trim)
head(usgs_flows_ref_trim)
# standardize
df_ref <- usgs_flows_ref_trim %>% 
  ungroup() %>% 
  rename("Q"=Flow, Date=date) %>% 
  select(Date, Q, site_no, flowcnt) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract 30 to test
  map(., ~as.data.frame(.x))

# calc colwells and add ID
df_colwell_ref <- df_ref %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_ref_meta$site_no,
         gagetype = "REF")

# standardize
df_alt <- usgs_flows_alt_trim %>%   
  rename("Q"=Flow, Date=date) %>% 
  select(Date, Q, site_no) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract 30 to test
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df_colwell_alt <- df_alt %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_alt_meta$site_no,
         gagetype = "ALT")

summary(df_colwell_alt)
summary(df_colwell_ref)

# BIND: 
df_colwell_all <- bind_rows(df_colwell_alt, df_colwell_ref)
table(df_colwell_all$gagetype)
view(df_colwell_all)
dim(df_colwell_all)

# ALT REF 
# 517 221

# 03: JOIN WITH META ----------------------------------------------------------

df_colwell_all_meta <- df_colwell_all %>% 
  left_join(., gage_metadata %>% select(-gagetype), by=c("site_no"))

table(df_colwell_all$gagetype, useNA = "ifany")

# 04: SAVE ------------------------------------------------------------------

# save
write_rds(df_colwell_all_meta, file = "output/04_usgs_gages_colwells_metric.rds")

rm(df_alt, df_ref, df_colwell_all, df_colwell_alt, df_colwell_ref, 
   gage_alt_meta, gage_ref_meta, usgs_flows_alt_trim, usgs_flows_ref_trim)

# 05: READ IN DATA ---------------------------------------------------------

df_colwell_all_meta <- read_rds("output/04_usgs_gages_colwells_metric.rds")
view(df_colwell_all_meta)
# 06: PLOTS -----------------------------------------------------------------

# Histogram
df_colwell_all_meta %>% ggplot() + geom_histogram(aes(y=MP_metric, fill=gagetype))

# Boxplot of ref/alt
df_colwell_all_meta %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black") +
  # theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/boxplot_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")

# Notched Boxplot of ref/alt
df_colwell_all_meta %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black", notch = TRUE) +
  # theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Intra-annual Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/boxplot_notched_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")




# Low scoring gages -------------------------------------------------------

head(df_colwell_all_meta)
dim(df_colwell_all_meta) ## 738
sort(unique(df_colwell_all_meta$site_no))

## get all gages below 0.2 MP_Metric

low_scores <- df_colwell_all_meta %>%
  filter(MP_metric < 0.2) ## 84 gages

high_scores <- df_colwell_all_meta %>%
  filter(MP_metric > 0.8) ## 129 gages



# FFM  --------------------------------------------------------------------
# this uses the purrr package to loop through and pull ffc data for each gage
source("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/code/f_iterate_ffc.R")
source("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/code/f_ffc_collapse.R")
source("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/code/f_ffc_collapse.R")

## gage data

load("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/data/usgs_Q_daily_alt_gages.rda")
load("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_seasonality/data/usgs_Q_daily_ref_gages.rda")

# how many have data?
usgs_flows_alt %>% distinct(site_no) %>% tally() # n=748
usgs_flows_ref %>% distinct(site_no) %>% tally() # n=221

low_scores$site_no %in% usgs_flows_alt$site_no

lowMP_gages <- usgs_flows_alt %>%
  filter(site_no %in% low_scores$site_no)

head(lowMP_gages)

save(lowMP_gages, file = "output/01_low_MP_gages_streamflow.RData")

highMP_gages <- usgs_flows_alt %>%
  filter(site_no %in% high_scores$site_no)

save(highMP_gages, file = "output/01_high_MP_gages_streamflow.RData")
## get ffctoken from 00 code

# clean up
ffcAPIClient::clean_account(ffctoken)

## format for function


lowMP_gages_ffc <- lowMP_gages %>%
  pivot_wider(names_from = site_no, values_from = Flow) %>%
  select(-agency_cd, -waterYear, - Flow_cd)

head(lowMP_gages_ffc)

import_df <- lowMP_gages_ffc


my_ffm_function(lowMP_gages_ffc, "output/ffc_ref_bio") 

## high

highMP_gages_ffc <- highMP_gages %>%
  pivot_wider(names_from = site_no, values_from = Flow) %>%
  select(-agency_cd, -waterYear, - Flow_cd)

head(highMP_gages_ffc)

import_df <- highMP_gages_ffc


my_ffm_function(highMP_gages_ffc, "output/ffc_ref_bio") 


# # chunk a set number at a time
# gagedata_low <- lowMP_gages %>% 
#   split(.$site_no)
# 
# head(gagedata_low)
# 
# ### get ffm
# tic() # start time
# ffcs_ref <- map(gagedata_low, ~ffc_possible(flowseries_df = .x,
#                                             ffctoken=ffctoken,
#                                             dirToSave="output/ffc_ref_bio", 
#                                             save=TRUE))
# toc() # end time
# # 383.114 sec elapsed
# ffcs_ref
# # see names
# names(ffcs_ref)
# 
# # a timestamp: format(Sys.time(), "%Y-%m-%d_%H%M")
# (file_ts <- format(Sys.time(), "%Y%m%d_%H%M"))
# 
# # identify missing:
# ffcs_ref %>% keep(is.na(.)) %>% length()
# 
# # make a list of missing gages and save out:
# # miss_gages_ref <- ffcs_ref %>% keep(is.na(.)) %>% names()
# # write_lines(miss_gages_ref, file = glue("output/usgs_ffm_alt_missing_gages_{file_ts}.txt"))
# 
# # SAVE: FFC R6 object (only if save=FALSE)
# save(ffcs_ref, file = glue("output/usgs_ffm_low_scores_{file_ts}.rda"))
# 
# load(file = "output/usgs_ffm_low_scores_20220113_1533.rda")
