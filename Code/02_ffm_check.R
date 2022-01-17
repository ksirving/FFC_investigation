## checking ffm for low and high MP

library(tidylog)
library(tidyverse)
library(tidyr)
getwd()
## directory for figures
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/FFC_investigation/figures/"

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
  filter(!numberYears < 5)

gagesKeep <- gage_years_under_10$site

## remove from main df
lowMP_wideRed <- lowMP_wide %>%
  filter(site %in% gagesKeep)
lowMP_wideRed
length(unique(lowMP_wideRed$site)) ## 96
## find  gages with each 0% missing metrics/years

gagesComplete<- lowMP_wideRed %>%
  group_by(site) %>%
  summarise(CompleteGages = length(PercNAs == 0))

dim(gagesComplete)

## check missingness

library(Amelia)
library(reshape2)
library(ggplot2)

lowMP_mis <- lowMP_wideRed %>%
  select(-site, -PercNAs, - year, -NumberNAs)

missmap(lowMP_mis)  ## missmap default, take %

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present (67%)","Missing (33%)")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1)) + #vjust=0.5
    labs(title = "Low Colwells (< 0.2) missingness",x = "Functional Flow Metrics",
         y = "Observations")
}

mis <- ggplot_missing(lowMP_mis)

mis

file.name1 <- paste0(out.dir, "LowMP_missingness_map.jpg")
ggsave(mis, filename=file.name1, dpi=300, height=5, width=6)



# High MP -----------------------------------------------------------------

head(highMP)
unique(highMP$year)

## make wide to see years with no data
highMP_wide <- highMP %>%
  pivot_wider(names_from = flow_metric, values_from =  result)

head(highMP_wide)

dim(highMP_wide)

colnames(highMP_wide)[3:29] ## 27 metrics

## find and remove years with no data
highMP_wide <- highMP_wide %>%
  mutate(NumberNAs = rowSums(is.na(highMP_wide))) %>%
  mutate(PercNAs = (NumberNAs/27)*100) %>%
  filter(!PercNAs == 100)

dim(highMP_wide)

## find number of years for each gage
gage_years <- highMP_wide %>%
  group_by(site) %>%
  summarise(numberYears = sum(length(year)))

## remove gages with ess than 10 years
gage_years_under_10 <- gage_years %>%
  filter(!numberYears < 5)

gagesKeep <- gage_years_under_10$site

## remove from main df
highMP_wideRed <- highMP_wide %>%
  filter(site %in% gagesKeep)
highMP_wideRed

length(unique(highMP_wideRed$site)) ## 39
## find  gages with each 0% missing metrics/years

gagesComplete<- highMP_wideRed %>%
  group_by(site) %>%
  summarise(CompleteGages = length(PercNAs == 0))

gagesComplete

## check missingness

library(Amelia)

highMP_mis <- highMP_wideRed %>%
  select(-site, -PercNAs, - year, -NumberNAs)

missmap(highMP_mis)  ## missmap default, take %

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present (72%)","Missing (28%)")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1)) + #vjust=0.5
    labs(title = "High Colwells (> 0.8) missingness", x = "Functional Flow Metrics",
         y = "Observations")
}
?labs
mis <- ggplot_missing(highMP_mis)
mis

file.name1 <- paste0(out.dir, "HighMP_missingness_map.jpg")
ggsave(mis, filename=file.name1, dpi=300, height=5, width=6)

