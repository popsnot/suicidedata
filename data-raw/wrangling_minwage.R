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


index=1
x1=1
x2=32

create_mw_dataframe = function(mw_data, x1, x2, index){
    columns = array()
    output = data.frame(Countries = countries)
    while (x2 <= length(mw_data)){
        columns = columns %>% append(list(mw_data[x1:x2]))
        index=index+1
        x1=x1+32
        x2=x2+32
    }
    columns = columns[2:length(columns)]
    for(i in 1:length(columns)) {
        if (i < 10){
            sub10colname = paste0("200", i)
            output[[sub10colname]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
        }
        else{
            bov10colname = paste0("20", i)
            output[[bov10colname]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
        }
    }
    return(output)
}

min_wage_df = create_mw_dataframe(mw_data, x1, x2, index)

melted_mwdf = min_wage_df %>% pivot_longer(`2001`:`2020`, names_to = "Years", values_to = "Wages")
minwagedata = melted_mwdf


melted_mwdf$`Years` = as.numeric(melted_mwdf$`Years`)

coi_minwagedata = melted_mwdf %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                              `Countries` == 'United States' | `Countries` == 'South Korea')



usethis::use_data(minwagedata, coi_minwagedata, overwrite = TRUE)
