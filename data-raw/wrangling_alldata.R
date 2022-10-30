#Importing libraries
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

#OECD minimum wages
minimum_wage_url = "https://stats.oecd.org/index.aspx?DataSetCode=RMW" #Defines our the main url we will use for scraping
mw_page = read_html(minimum_wage_url) #Reads in the html body from the webpage
countries1 = mw_page %>% html_nodes(xpath = '//*[@class="RowDimLabel"]') %>% html_text()
countries1 = countries1[seq(1, length(countries1), 2)]
#Reads in the first selector of countriesfrom html - removes unwanted values

countries2 = mw_page %>% html_nodes(xpath = '//*[@class="RowDimLabel2"]') %>% html_text()
countries2 = countries2[seq(1, length(countries2), 2)]
#Reads in the second selector of countries from html - removes unwanted values

data1 = mw_page %>% html_nodes(xpath = '//*[@class="Data"]') %>% html_text()
data2 = mw_page %>% html_nodes(xpath = '//*[@class="Data2"]') %>% html_text()
#Same as above but for the minimum wage data

mergeCountries = function(array1, array2) {
    output = array()
    for (num in seq.int(1, length(array1))){ #For each item in array
        output = output %>% append(c(array1[num], array2[num])) #Adds the values in order together as they appear
    }
    return(na.omit(output))
}
#Creates a function to merge the two vectors of country names together
#It's crucial that the countries are in the right order, as otherwise our data won't be matched to the right country

countries = mergeCountries(countries1, countries2)

for (i in seq.int(1, length(countries))){
    if (countries[i] == 'Korea'){
        countries[i] = 'South Korea'
    }
}
#In the OECD Stats datasets in particular - South Korea was called Korea, which preventing joins, as they had different values.
#This for loop just checks for 'Korea' in the array of countries and changes it to South Korea, allowing us to join
#this dataframe with the others.


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
#Takes every 21st value of our scraped data (which is every country's value for one year) - the plan is to repeat this
#for every year (which is a column of the original data), then

outputs = remove_every_21st(mw_data) #First application of our function to the data, returns the 2001 data and the remaining data with 2001
#removed
mw_list_2001 = unlist(outputs[1]) #2001 data
next_set = unlist(outputs[2]) #remaining

output = mw_list_2001

#The following is a recursive function to reorder the data into years - so it can be easily extracted into our dataframe
#Currently, it's in the form: aus2001 aus2002 aus2003 aus2004..... aus2020 bel2001..
#We want it in the form aus2001 bel2001 ..... so it can be more easily assigned to yearly variables, then melted from wide to long format
reorder = function(array, output) {
    if (length(output) < length(array)) {
        lists = remove_every_21st(array) #Applies previous function
        leftover = unlist(lists[2]) #Remaining data to be reordered
        useful = unlist(lists[1]) #The useful data we've extracted (one year's observations)
        output = c(output, useful) #Adds useful data to our output
        return(reorder(leftover, output)) #Recursively calls the function again - each call = 1 more year of data added to the output
    }
    return(output) #Returns the reordered data
}

mw_data = reorder(next_set, mw_list_2001)

index=1
x1=1
x2=32

#Function to create the dataframe - uses dynamic variable naming for years and gsub function to remove non-ASCII characters present after
#webscraping. Output is our wide-format union density dataframe, with each year of observations as a variable (as seen on website)
create_ud_dataframe = function(data, x1, x2, index){
    columns = array() #Initialises the array of year variables
    output = data.frame(Countries = countries) #Creates the dataframe using our Countries array
    while (x2 <= length(data)){ #While loop terminating when we have wrangled all data
        columns = columns %>% append(list(data[x1:x2])) #Adds a list of the data for the 32 countries for 1 year to the column output
        index=index+1 #Moves index forward
        x1=x1+32 #Increments first index variable to the first value for the next year of observations
        x2=x2+32 #Increments first index variable to the last value for the next year of observations
    }
    columns = columns[2:length(columns)] #Removes the NA created when initialising the array
    for(i in 1:length(columns)) { #For loop to add each column to the dataframe
        if (i < 10){
            sub10colnames = paste0("200", i-1) #Dynamic name of variable for dataframe
            output[[sub10colnames]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
            #Creates the variable in dataframe using the string defined before as the name - gsub function removes the non-ASCII character
            #which was present in numbers (e.g. 23 000 <-- instead of whitespace in between numbers, was some unidentifiable charcter)
            #Call to the gsub() function to remove this character was found on Stack Overflow:
            #https://stackoverflow.com/questions/43734293/remove-non-breaking-space-character-in-string
        }
        else{
            bov10colnames = paste0("20", i-1)
            output[[bov10colnames]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
            #Does the same as above, but we our variable name only has "20" instead of "200" as the prefix to produce correct year
        }
    }
    return(output) #Returns our complete dataframe
}

min_wage_df = create_mw_dataframe(mw_data, x1, x2, index)

#Code to remove the non-ASCII character was found on Stack Overflow

melted_mwdf = min_wage_df %>% pivot_longer(`2001`:`2020`, names_to = "Years", values_to = "Wages")

melted_mwdf$`Years` = as.numeric(melted_mwdf$`Years`)

melted_mwdf = melted_mwdf %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                         `Countries` == 'United States' | `Countries` == 'South Korea')





#OECD union wrangling
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
#In the OECD Stats datasets in particular - South Korea was called Korea, which prevented joins, as they had different values.
#This for loop just checks for 'Korea' in the array of countries and changes it to South Korea, allowing us to join
#this dataframe with the others.

step = 21
union_data = as.vector(sapply(0 : (length(data3) / step - 1),
                              function(idx){
                                  c(data3[1 : step + (idx * step)], data4[1 : step + (idx * step)])}))


union_r21_outputs = remove_every_21st(union_data) #Same function as used for minwage data, begins process of wrangling scraped data into
#desired format
list_2000 = unlist(union_r21_outputs[1])
next_set2 = unlist(union_r21_outputs[2])


output = list_2000
#Same recursive function to return data ordered by year as used for minwage dataset:

reorder = function(array, output) {
    if (length(output) < length(array)) {
        lists = remove_every_21st(array) #Applies previous function
        leftover = unlist(lists[2]) #Remaining data to be reordered
        useful = unlist(lists[1]) #The useful data we've extracted (one year's observations)
        output = c(output, useful) #Adds useful data to our output
        return(reorder(leftover, output)) #Recursively calls the function again - each call = 1 more year of data added to the output
    }
    return(output) #Returns the reordered data
}

union_data = reorder(next_set2, list_2000)
union_data = union_data %>% na.omit()


index = 1
x1 = 1
x2 = 39

#Same function as used for minwage dataframe, however adjusted for the additional countries present in this dataframe (39 vs 32)
create_ud_dataframe = function(data, x1, x2, index){
    columns = array() #Initialises the array of year variables
    output = data.frame(Countries = countries_) #Creates the dataframe using our Countries array
    while (x2 <= length(data)){ #While loop terminating when we have wrangled all data
        columns = columns %>% append(list(data[x1:x2])) #Adds a list of the data for the 39 countries for 1 year to the column output
        index=index+1 #Moves index forward
        x1=x1+39 #Increments first index variable to the first value for the next year of observations
        x2=x2+39 #Increments first index variable to the last value for the next year of observations
    }
    columns = columns[2:length(columns)] #Removes the NA created when initialising the array
    for(i in 1:length(columns)) { #For loop to add each column to the dataframe
        if (i < 10){
            sub10colnames = paste0("200", i-1) #Dynamic name of variable for dataframe
            output[[sub10colnames]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
            #Creates the variable in dataframe using the string defined before as the name - gsub function removes the non-ASCII character
            #which was present in numbers (e.g. 23 000 <-- instead of whitespace in between numbers, was some unidentifiable charcter)
            #Call to the gsub() function to remove this character was found on Stack Overflow:
            #https://stackoverflow.com/questions/43734293/remove-non-breaking-space-character-in-string
        }
        else{
            bov10colnames = paste0("20", i-1)
            output[[bov10colnames]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
            #Does the same as above, but we our variable name only has "20" instead of "200" as the prefix to produce correct year
        }
    }
    return(output) #Returns our complete dataframe
}

union_df = create_ud_dataframe(union_data, x1, x2, index) #Call to the function to make our union dataframe

union_df = union_df %>% filter(`Countries` != 'OECD - Total') #Removes the

melted_uniondf = union_df %>% pivot_longer(`2000`:`2020`, names_to = "Years", values_to = "UnionDensity")
#Converts the dataframe from wide to long format - with the 2000, 2001....2020 year variables being condensed into one column.
#Now, our data is in the correct format - where the UnionDensity is determined by the primary key of country name and year.

melted_uniondf$`Years` = as.numeric(melted_uniondf$`Years`)
#As they were variable names, the years weren't numbers - this function converts them to numbers for plotting/modelling purposes

melted_uniondf = melted_uniondf %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                               `Countries` == 'United States' | `Countries` == 'South Korea')
#Creates the dataframe of only our countries of interest

melted_uniondf = melted_uniondf %>% na.omit()
#Cleans data by removing rows with NA values (there were some present for older years)




#wrangling global depression
depression <- read.csv(
    "https://raw.githubusercontent.com/lostconnectionhere/mental_health/main/data/prevalence-of-depression-males-vs-females.csv")
names(depression) <- c('Countries', 'Code', 'Years', 'Prevalence_depr_male', 'Prevalence_depr_female', 'Population_Estimate', 'Continent')
depression = select(depression, -c('Continent'))
depression = na.omit(depression)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world = select(world, c(gu_a3, geometry))

names(world) <- c('Code', 'Geometry')
merged_dep_df = merge(depression, world, by = "Code") %>% arrange(Years)
melted_depdf = merged_dep_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                            `Countries` == 'United States' | `Countries` == 'South Korea')
melted_depdf$`Years` = as.numeric(melted_depdf$`Years`)






#wrangling global suicides
suicides <- read.csv("https://raw.githubusercontent.com/popsnot/DATA201-Group-Project-2022/main/suicide-death-rates.csv")
# reading csv directly from our github
names(suicides) <- c('Countries', 'Code', 'Years', 'Suicides_Per100k') #renaming
world <- ne_countries(scale = "medium", returnclass = "sf")
world = select(world, c(gu_a3, geometry)) # selecting chosen column
names(world) <- c('Code', 'Geometry') #
merged_sui_df = merge(suicides, world, by = "Code") %>% arrange(Years)

melted_suidf = merged_sui_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                            `Countries` == 'United States' | `Countries` == 'South Korea')
melted_suidf$`Years` = as.numeric(melted_suidf$`Years`)




#oecd labour wrangling
oecdlabour_df <- read.csv(
    'https://stats.oecd.org/sdmx-json/data/DP_LIVE/.HRWKD.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en')
oecdlabour_df <- (oecdlabour_df %>% select(LOCATION, TIME, Value))
oecdlabour_df = tibble(Countries = oecdlabour_df$`LOCATION`, Years = oecdlabour_df$`TIME`, Hours = oecdlabour_df$`Value`)

codes_url = 'https://www.worlddata.info/countrycodes.php'
code_page = read_html(codes_url) #Reads in the html body from the webpage
code_page %>% glimpse() #Double check that the body has read in correctly (we look at the classes of the objects)

countries_url = 'http://www.energybc.ca/cache/nuclear/nuclear2/www.oecd.org/document/1/0,2340,en_2649_201185_1889402_1_1_1_1,00.html'
countries_page = read_html(countries_url) #Reads in the html body from the webpage
countries_page %>% glimpse() #Double check that the body has read in correctly (we look at the classes of the objects)
valid_countries = countries_page %>% html_nodes(xpath = '//*[@class="more"]') %>% html_text()
valid_countries = valid_countries[1:length(valid_countries)-1]

for (i in seq.int(1, length(valid_countries))){
    if (valid_countries[i] == 'Korea'){
        valid_countries[i] = 'South Korea'
    }
}

all_codes = code_page %>% html_nodes(xpath = '//*[@class="std100 hover"]') %>% html_text()
country_codes = unlist(strsplit(all_codes, "[.]"))
output = array()
countries = array()
codes = array()
for (country_data in country_codes){
    country_data = substring(country_data, 1, nchar(country_data)-8)
    country_data = substring(country_data, 3, nchar(country_data))
    country_data = removeNumbers(country_data)
    code = substring(country_data, nchar(country_data)-2, nchar(country_data))
    country = substring(country_data, 1, nchar(country_data)-3)
    countries = countries %>% append(country)
    codes = codes %>% append(code)
}

output = output[4:length(output)]
countries = countries[5:length(countries)-1]
codes = codes[5:length(codes)-1]

countries2 = array()

for (country in countries){
    country = substring(country, 1, nchar(country)-2)
    countries2 = countries2 %>% append(country)
}

countries2 = countries2[2:length(countries2)] #Removes the NA value creating when initialising the array

#Function which returns a list which maps a country to a code, thus when converting countries into codes can be used like:
#country = list_for_conversion[code], thus we can convert the codes into countries for our primary key
countriesAndCodes = function(countries2, codes) {
    output_countries = array() #initializes countries array
    output_codes = array() #initializes codes array
    for (i in seq.int(1, length(countries2))){ #Both arrays are the same length
        if (countries2[i] %in% valid_countries){ #Checks if country is in the OECD
            output_countries = output_countries %>% append(countries2[i]) #Appends the country to the countries array
            output_codes = output_codes %>% append(codes[i]) #Appends the code to the codes array
        }
    }
    output_list = as.list(output_countries[2:length(output_countries)]) #Creates the list, with the countries as values
    names(output_list) = as.list(output_codes[2:length(output_codes)]) #Sets codes to be the keys to the countries
    return(output_list) #Returns the list
}

list_for_conversions = countriesAndCodes(countries2, codes) #Applies the function
table_codes = oecdlabour_df$`Countries` #Gets the codes to be converted into countries

#Looping through codes to convert them into countries one by one
converted_countries = array() #Intialises array
for (i in seq.int(1, length(table_codes))){ #Starts loop to terminate at the end of the codes list
    converted_countries = converted_countries %>% append(as.character(list_for_conversions[table_codes[i]])) #Uses the list we created with
    #the previous function --> country = list_for_conversions[code] and then adds the country to our array
}

oecdlabour_df$`Countries` = converted_countries[2:length(converted_countries)] #Changes the Countries column from the codes to the countries
filtered_oecd <- oecdlabour_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                              `Countries` == 'United States' | `Countries` == 'South Korea')
#Filters out all countries that are not countries of interest



#Merging dataframes
#Primary key = Countries, Years
main_df = merge(melted_uniondf, melted_mwdf, by = c("Countries", "Years"))
#creates the main dataframe by merging the union and minimum wage data on the common attribute pair of countries and years
main_df = merge(main_df, melted_depdf, by=c("Countries", "Years"))
#Merges depression dataframe with the main dataframe
main_df = main_df %>% mutate(Countries = as.factor(Countries))
#Changes countries to a factor so it can be used as a predictor in the regression model
main_df = main_df %>% mutate(Dep_Prevalence = (Prevalence_depr_male + Prevalence_depr_female)/2)
#Creates an average depression prevalence variable based on the mean of male and female
main_df = merge(main_df, melted_suidf, by=c("Countries", "Years"))
main_df = merge(main_df, filtered_oecd, by=c("Countries", "Years"))
main_df = subset(main_df, select = -c(Prevalence_depr_male, Prevalence_depr_female, Geometry.x, Geometry.y, Code.x, Code.y))
#Merged the generated dataframes and only selected relevant columns
maindata = main_df



usethis::use_data(maindata, overwrite = TRUE)
#Adds the maindata dataframe to our package, thus is can be called with "maindata" after importing the package to R
