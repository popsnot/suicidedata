library(tidyverse) #The mother of all analysis packages
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
ggthemr('dust') #Sets the themes for our plots


oecdlabour_df <- read.csv(
    'https://stats.oecd.org/sdmx-json/data/DP_LIVE/.HRWKD.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en')
#Reads in csv directly from OECD website
glimpse(oecdlabour_df) #the glimpse function allows us to have a peek at what the dataset will look like

oecdlabour_df <- (oecdlabour_df %>% select(LOCATION, TIME, Value))
oecdlabour_df = tibble(Countries = oecdlabour_df$`LOCATION`, Years = oecdlabour_df$`TIME`, Hours = oecdlabour_df$`Value`)

codes_url = 'https://www.worlddata.info/countrycodes.php'
code_page = read_html(codes_url) #Reads in the html body from the webpage
code_page %>% glimpse() #Double check that the body has read in correctly (we look at the classes of the objects)

countries_url = 'http://www.energybc.ca/cache/nuclear/nuclear2/www.oecd.org/document/1/0,2340,en_2649_201185_1889402_1_1_1_1,00.html'
countries_page = read_html(countries_url) #Reads in the html body from the webpage
countries_page %>% glimpse() #Double check that the body has read in correctly (we look at the classes of the objects)
oecd_countries = countries_page %>% html_nodes(xpath = '//*[@class="more"]') %>% html_text() #Uses the xpath selector corresponding to the codes in order to get the correct items
oecd_countries = oecd_countries[1:length(oecd_countries)-1] #The last row was irrelevant - this line removes it

#In the data that we read in, South Korea was called Korea - which is different to what it's called in the OECD dataset,
#thus we need to change them in order to appropriately merge this with the other scraped OECD data
for (i in seq.int(1, length(oecd_countries))){
    if (oecd_countries[i] == 'Korea'){ #For each country in the list of OECD countries, if the country is 'Korea' change its
        #value to 'South Korea'
        oecd_countries[i] = 'South Korea'
    }
}


all_codes = code_page %>% html_nodes(xpath = '//*[@class="std100 hover"]') %>% html_text()
#Using xpath selector to extract the desired text from page of country codes
country_codes = unlist(strsplit(all_codes, "[.]"))
#After extracting using the xpath selector, the codes had "." in between them, this function removes these and returns a list
#We then use "unlist()" to turn it into an array


output = array()
countries = array()
codes = array()
#Initialising variables
for (country_data in country_codes){
    country_data = substring(country_data, 1, nchar(country_data)-8)
    country_data = substring(country_data, 3, nchar(country_data))
    country_data = removeNumbers(country_data)
    code = substring(country_data, nchar(country_data)-2, nchar(country_data))
    country = substring(country_data, 1, nchar(country_data)-3)
    country = substring(country, 1, nchar(country)-2)
    countries = countries %>% append(country)
    codes = codes %>% append(code)
}
#Essentially, this data was really messy to read in (there were multiple different values for each country, such as website suffixes and mobile codes - all of different lengths)
#so I applied a few calls to the substring function to isolate the country name and the code we were interested in (the 3 letter one) from the other unwanted data.
#The for loop adds the countries to the countries array and codes to the codes array

#Note we are including non-OECD countries here - which will get weeded out when comparing this array to the array of OECD countries


#The following function which returns a list which maps a country to a code, thus when converting countries into codes can be used like:
#country = list_for_conversion[code], thus we can convert the codes into countries to be used as the primary key (along with year of observation) when joining the table
#containing labour hours to the main table containing all other data.
countriesAndCodes = function(countries, codes) {
    output_countries = array() #initializes countries array
    output_codes = array() #initializes codes array
    for (i in seq.int(1, length(countries))){ #Both arrays are the same length
        if (countries[i] %in% oecd_countries){ #Checks if country is in the OECD
            output_countries = output_countries %>% append(countries[i]) #Appends the country to the countries array
            output_codes = output_codes %>% append(codes[i]) #Appends the code to the codes array
        }
    }
    output_list = as.list(output_countries[2:length(output_countries)]) #Creates the list, with the countries as values
    names(output_list) = as.list(output_codes[2:length(output_codes)]) #Sets codes to be the keys to the countries
    return(output_list) #Returns the list
}

list_for_conversions = countriesAndCodes(countries, codes) #Applies the function

table_codes = oecdlabour_df$`Countries` #Gets the codes to be converted into countries

#Looping through codes to convert them into countries - which will then occupy the countries column in place of the codes
converted_countries = array() #Intialises array
for (i in seq.int(1, length(table_codes))){ #Starts loop to terminate at the end of the codes list
    converted_countries = converted_countries %>% append(as.character(list_for_conversions[table_codes[i]])) #Uses the list we created with
    #the previous function --> country = list_for_conversions[code] and then adds the country to our array
}

oecdlabour_df$`Countries` = converted_countries[2:length(converted_countries)] #Changes the Countries column from the codes to the countries

labourdata = oecdlabour_df
filtered_labour <- oecdlabour_df %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                                `Countries` == 'United States' | `Countries` == 'South Korea')
#Filters out all countries that are not countries of interest

coi_labourdata = filtered_labour
#Double checking our dataframe

usethis::use_data(labourdata, coi_labourdata, overwrite = TRUE)
#Adds the two labour dataframes to our package, thus is can be called with "labourdata" and "coi_labourdata" after importing the package to R

