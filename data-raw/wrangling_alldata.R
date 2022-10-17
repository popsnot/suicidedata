#Modelling
library(tidyverse) #The mother of all analysis packages
library(rvest) #Scraping web pages
library(httr) #Lets us use requests for data from APIs
library(tidyr) #Functions to help keep our data tidy
library(magrittr) #Piping
library(purrr) #Useful functions for general df wrangling, especially for functions
library(glue) #Gives us useful functions for strings
library(stringr) #More useful functions for strings
library(polite) #For web scraping
library(xml2) #Allows us to work with xml files obtained through scraping
library(GGally) #Produces handy scatterplot matrix with correlations between columns in a dataframe
library(leaps) #Contains regsubsets() to help us select predictors for our model
library(car) #Contains AIC, which we're using for model analysis
library("jsonlite") #Gives functions to help us work with json from API request content
library(plotly) #Allows us to develop interactive graphs - compatible with ggplot2
library(ggthemr) #Extra nice-looking themes for ggplot2 graphs
library(tm) #Contains the function which allows us to remove numbers from a string
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggthemes)
library(geojsonio)

minimum_wage_url = "https://stats.oecd.org/index.aspx?DataSetCode=RMW" #Defines our the main url we will use for scraping
mw_page = read_html(minimum_wage_url) #Reads in the html body from the webpage
countries1 = mw_page %>% html_nodes(xpath = '//*[@class="RowDimLabel"]') %>% html_text()
countries1 = countries1[seq(1, length(countries1), 2)]

countries2 = mw_page %>% html_nodes(xpath = '//*[@class="RowDimLabel2"]') %>% html_text()
countries2 = countries2[seq(1, length(countries2), 2)]

data1 = mw_page %>% html_nodes(xpath = '//*[@class="Data"]') %>% html_text()
data2 = mw_page %>% html_nodes(xpath = '//*[@class="Data2"]') %>% html_text()

mergeCountries = function(array1, array2) {
    output = array()
    for (num in seq.int(1, length(array1))){
        output = output %>% append(c(array1[num], array2[num]))
    }
    return(na.omit(output))
}

countries = mergeCountries(countries1, countries2)

for (i in seq.int(1, length(countries))){
    if (countries[i] == 'Korea'){
        countries[i] = 'South Korea'
    }
}


step = 21
mw_data = as.vector(sapply(0 : (length(data1) / step - 1),
                           function(idx){
                               c(data1[1 : step + (idx * step)], data2[1 : step + (idx * step)])}))

remove_every_21st = function(array) {
    output = array[seq(1, length(array), 21)]
    array = array[2:length(array)]
    outputs = list(output, array)
    return(outputs)
}

outputs = remove_every_21st(mw_data)
mw_list_2001 = unlist(outputs[1])
next_set = unlist(outputs[2])

output = mw_list_2001

reorder = function(array, output) {
    if (length(output) < length(array)) {
        lists = remove_every_21st(array)
        leftover = unlist(lists[2])
        useful = unlist(lists[1])
        output = c(output, useful)
        return(reorder(leftover, output))
    }
    return(output)
}

mw_data = reorder(next_set, mw_list_2001)

mw_list_2001 = array()
mw_list_2002 = array()
mw_list_2003 = array()
mw_list_2004 = array()
mw_list_2005 = array()
mw_list_2006 = array()
mw_list_2007 = array()
mw_list_2008 = array()
mw_list_2009 = array()
mw_list_2010 = array()
mw_list_2011 = array()
mw_list_2012 = array()
mw_list_2013 = array()
mw_list_2014 = array()
mw_list_2015 = array()
mw_list_2016 = array()
mw_list_2017 = array()
mw_list_2018 = array()
mw_list_2019 = array()
mw_list_2020 = array()
mw_list_2021 = array()


future_cols= list(mw_list_2001, mw_list_2002, mw_list_2003, mw_list_2004, mw_list_2005, mw_list_2006, mw_list_2007, mw_list_2008, mw_list_2009, mw_list_2010, mw_list_2011,
                  mw_list_2012,mw_list_2013,mw_list_2014, mw_list_2015, mw_list_2016, mw_list_2017, mw_list_2018, mw_list_2019, mw_list_2020, mw_list_2021)

index=1
x1=1
x2=32
for (col in future_cols){
    future_cols[index] = list(mw_data[x1:x2])
    index=index+1
    x1=x1+32
    x2=x2+32
}

min_wage_df = tibble(Countries = countries, '2001' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[1]), perl=TRUE)),
                     '2002'=as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[2]), perl=TRUE)),
                     '2003' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[3]), perl=TRUE)),
                     '2004' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[4]), perl=TRUE)),
                     '2005' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[5]), perl=TRUE)),
                     '2006' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[6]), perl=TRUE)),
                     '2007' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[7]), perl=TRUE)),
                     '2008' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[8]), perl=TRUE)),
                     '2009' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[9]), perl=TRUE)),
                     '2010' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[10]), perl=TRUE)),
                     '2011' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[11]), perl=TRUE)),
                     '2012' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[12]), perl=TRUE)),
                     '2013' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[13]), perl=TRUE)),
                     '2014' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[14]), perl=TRUE)),
                     '2015' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[15]), perl=TRUE)),
                     '2016' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[16]), perl=TRUE)),
                     '2017' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[17]), perl=TRUE)),
                     '2018' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[18]), perl=TRUE)),
                     '2019' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[19]), perl=TRUE)),
                     '2020' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[20]), perl=TRUE)))
#Code to remove the non-ASCII character was found on Stack Overflow

melted_mwdf = min_wage_df %>% pivot_longer(`2001`:`2020`, names_to = "Years", values_to = "Wages")

melted_mwdf$`Years` = as.numeric(melted_mwdf$`Years`)

melted_mwdf = melted_mwdf %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')


union_url = "https://stats.oecd.org/index.aspx?DataSetCode=TUD" #Defines our the main url we will use for scraping
union_page = read_html(union_url) #Reads in the html body from the webpage

countries3 = union_page %>% html_nodes(xpath = '//*[@class="RowDimLabel"]') %>% html_text()
countries3 = countries3[seq(1, length(countries3), 2)]

countries4 = union_page %>% html_nodes(xpath = '//*[@class="RowDimLabel2"]') %>% html_text()
countries4 = countries4[seq(1, length(countries4), 2)]

data3 = union_page %>% html_nodes(xpath = '//*[@class="Data"]') %>% html_text()
data4 = union_page %>% html_nodes(xpath = '//*[@class="Data2"]') %>% html_text()

mergeCountries = function(array1, array2) {
    output = array()
    for (num in seq.int(1, length(array1))){
        output = output %>% append(c(array1[num], array2[num]))
    }
    return(na.omit(output))
}

countries_ = mergeCountries(countries3, countries4)

for (i in seq.int(1, length(countries_))){
    if (countries_[i] == 'Korea'){
        countries_[i] = 'South Korea'
    }
}


step = 21
union_data = as.vector(sapply(0 : (length(data3) / step - 1),
                              function(idx){
                                  c(data3[1 : step + (idx * step)], data4[1 : step + (idx * step)])}))

remove_every_21st = function(array) {
    output = array[seq(1, length(array), 21)]
    array = array[2:length(array)]
    outputs = list(output, array)
    return(outputs)
}

union_r21_outputs = remove_every_21st(union_data)
list_2000 = unlist(union_r21_outputs[1])
next_set2 = unlist(union_r21_outputs[2])

output = list_2000

reorder = function(array, output) {
    if (length(output) < length(array)) {
        lists = remove_every_21st(array)
        leftover = unlist(lists[2])
        useful = unlist(lists[1])
        output = c(output, useful)
        return(reorder(leftover, output))
    }
    return(output)
}

union_data = reorder(next_set2, list_2000)
union_data = union_data %>% na.omit()

list_2000 = array()
list_2001 = array()
list_2002 = array()
list_2003 = array()
list_2004 = array()
list_2005 = array()
list_2006 = array()
list_2007 = array()
list_2008 = array()
list_2009 = array()
list_2010 = array()
list_2011 = array()
list_2012 = array()
list_2013 = array()
list_2014 = array()
list_2015 = array()
list_2016 = array()
list_2017 = array()
list_2018 = array()
list_2019 = array()
list_2020 = array()


union_future_cols= list(list_2000, list_2001, list_2002, list_2003, list_2004, list_2005, list_2006, list_2007, list_2008, list_2009, list_2010, list_2011,list_2012,
                        list_2013, list_2014, list_2015,list_2016,list_2017,list_2018, list_2019, list_2020)
index=1
x1=1
x2=39
for (col in union_future_cols){
    union_future_cols[index] = list(union_data[x1:x2])
    index=index+1
    x1=x1+39
    x2=x2+39
}

union_df = tibble(Countries = countries_, '2000' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[1]), perl=TRUE)),
                  '2001'= as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[2]), perl=TRUE)),
                  '2002' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[3]), perl=TRUE)),
                  '2003' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[4]), perl=TRUE)),
                  '2004' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[5]), perl=TRUE)),
                  '2005' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[6]), perl=TRUE)),
                  '2006' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[7]), perl=TRUE)),
                  '2007' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[8]), perl=TRUE)),
                  '2008' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[9]), perl=TRUE)),
                  '2009' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[10]), perl=TRUE)),
                  '2010' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[11]), perl=TRUE)),
                  '2011' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[12]), perl=TRUE)),
                  '2012' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[13]), perl=TRUE)),
                  '2013' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[14]), perl=TRUE)),
                  '2014' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[15]), perl=TRUE)),
                  '2015' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[16]), perl=TRUE)),
                  '2016' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[17]), perl=TRUE)),
                  '2017' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[18]), perl=TRUE)),
                  '2018' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[19]), perl=TRUE)),
                  '2019' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[20]), perl=TRUE)),
                  '2020' = as.numeric(gsub("(*UCP)\\s*", "", unlist(union_future_cols[21]), perl=TRUE)))
union_df = union_df %>% filter(`Countries` != 'OECD - Total')

melted_uniondf = union_df %>% pivot_longer(`2000`:`2020`, names_to = "Years", values_to = "UnionDensity")

melted_uniondf$`Years` = as.numeric(melted_uniondf$`Years`)

melted_uniondf = melted_uniondf %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')

melted_uniondf = melted_uniondf %>% na.omit()



depression <- read.csv("https://raw.githubusercontent.com/lostconnectionhere/mental_health/main/data/prevalence-of-depression-males-vs-females.csv")
names(depression) <- c('Countries', 'Code', 'Years', 'Prevalence_depr_male', 'Prevalence_depr_female', 'Population_Estimate', 'Continent')
depression = select(depression, -c('Continent'))
depression = na.omit(depression)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world = select(world, c(gu_a3, geometry))

names(world) <- c('Code', 'Geometry')
merged_dep_df = merge(depression, world, by = "Code") %>% arrange(Years)
melted_depdf = merged_dep_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')
melted_depdf$`Years` = as.numeric(melted_depdf$`Years`)



suicides <- read.csv("https://raw.githubusercontent.com/popsnot/DATA201-Group-Project-2022/main/suicide-death-rates.csv") # reading csv directly from our github
names(suicides) <- c('Countries', 'Code', 'Years', 'Suicides_Per100k') #renaming
world <- ne_countries(scale = "medium", returnclass = "sf")
world = select(world, c(gu_a3, geometry)) # selecting chosen column
names(world) <- c('Code', 'Geometry') # merging by shared column
merged_sui_df = merge(suicides, world, by = "Code") %>% arrange(Years)

melted_suidf = merged_sui_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')
melted_suidf$`Years` = as.numeric(melted_suidf$`Years`)


#merging dataframes
main_df = merge(melted_uniondf, melted_mwdf, by = c("Countries", "Years"))
main_df = merge(main_df, melted_depdf, by=c("Countries", "Years"))
main_df = main_df %>% mutate(Countries = as.factor(Countries))
main_df = main_df %>% mutate(Dep_Prevalence = (Prevalence_depr_male + Prevalence_depr_female)/2)
maindata = merge(main_df, melted_suidf, by=c("Countries", "Years"))
maindata = maindata %>% select(-c(`Code.x`, `Code.y`, `Geometry.x`, `Geometry.y`))

usethis::use_data(maindata, overwrite = TRUE)
