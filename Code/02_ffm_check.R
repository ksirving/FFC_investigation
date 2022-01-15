## checking ffm for low and high MP

library(tidylog)
library(tidyverse)
library(tidyr)

### data from Annie
lowMP <- read.csv("data/lowMP_gages_ffms.csv")
highMP <- read.csv("data/highMP_gages_ffms.csv")


# Low MP scores -----------------------------------------------------------

head(lowMP)
unique(lowMP$year)

## make wide to see years with no data
lowMP_wide <- lowMP %>%
  pivot_wider(names_from = flow_metric, values_from =  result)

head(lowMP_wide)

dim(lowMP_wide)

colnames(lowMP_wide)[3:29] ## 27 metrics

## find and remove years with no data
lowMP_wide <- lowMP_wide %>%
  mutate(NumberNAs = rowSums(is.na(lowMP_wide))) %>%
  mutate(PercNAs = (NumberNAs/27)*100) %>%
  filter(!PercNAs == 100)

dim(lowMP_wide)

## find number of years for each gage
gage_years <- lowMP_wide %>%
  group_by(site) %>%
  summarise(numberYears = sum(length(year)))
  
## remove gages with ess than 10 years
gage_years_under_10 <- gage_years %>%
  filter(!numberYears < 10)

gagesKeep <- gage_years_under_10$site

## remove from main df
lowMP_wideRed <- lowMP_wide %>%
  filter(site %in% gagesKeep)

## find  gages with each 0% missing metrics/years

gagesComplete<- lowMP_wideRed %>%
  group_by(site) %>%
  summarise(CompleteGages = length(PercNAs == 0))

gagesComplete
