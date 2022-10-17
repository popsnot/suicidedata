library(tidyverse) #The mother of all analysis packages
library(rvest) #Scraping web pages
library(httr) #Lets us use requests for data from APIs
library(tidyr) #Functions to help keep our data tidy
library(magrittr) #Piping
library(purrr) #Useful functions for general df wrangling, especially for functions
library(glue) #Gives us useful functions for strings
library(stringr) #More useful functions for strings
library(polite) #For web scraping
library(xml2)
library(tm)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggthemes)
library(geojsonio)


suicides <- read.csv("https://raw.githubusercontent.com/popsnot/DATA201-Group-Project-2022/main/suicide-death-rates.csv") # reading csv directly from our github
names(suicides) <- c('Countries', 'Code', 'Years', 'Suicides_Per100k') #renaming
world <- ne_countries(scale = "medium", returnclass = "sf")
world = select(world, c(gu_a3, geometry)) # selecting chosen column
names(world) <- c('Code', 'Geometry') # merging by shared column
merged_sui_df = merge(suicides, world, by = "Code") %>% arrange(Years)
suicidedata = merged_sui_df

melted_suidf = merged_sui_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')
melted_suidf$`Years` = as.numeric(melted_suidf$`Years`)
coi_suicidedata = melted_suidf



depression <- read.csv("https://raw.githubusercontent.com/lostconnectionhere/mental_health/main/data/prevalence-of-depression-males-vs-females.csv")
names(depression) <- c('Countries', 'Code', 'Years', 'Prevalence_depr_male', 'Prevalence_depr_female', 'Population_Estimate', 'Continent')
depression = select(depression, -c('Continent'))
depression = na.omit(depression)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world = select(world, c(gu_a3, geometry))

names(world) <- c('Code', 'Geometry')
merged_dep_df = merge(depression, world, by = "Code") %>% arrange(Years)
depressiondata = merged_dep_df
melted_depdf = merged_dep_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')
melted_depdf$`Years` = as.numeric(melted_depdf$`Years`)
coi_depressiondata = melted_depdf


usethis::use_data(depressiondata, coi_depressiondata, suicidedata, coi_suicidedata, overwrite = TRUE)
