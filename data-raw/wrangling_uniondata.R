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

index = 1
x1 = 1
x2 = 39
create_ud_dataframe = function(data, x1, x2, index){
    columns = array()
    output = data.frame(Countries = countries_)
    while (x2 <= length(data)){
        columns = columns %>% append(list(data[x1:x2]))
        index=index+1
        x1=x1+39
        x2=x2+39
    }
    columns = columns[2:length(columns)]
    for(i in 1:length(columns)) {
        if (i < 10){
            sub10colnames = paste0("200", i-1)
            output[[sub10colnames]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
        }
        else{
            bov10colnames = paste0("20", i-1)
            output[[bov10colnames]] = with(output, as.numeric(gsub("(*UCP)\\s*", "", unlist(columns[i]), perl=TRUE)))
        }
    }
    return(output)
}

union_df = create_ud_dataframe(union_data, x1, x2, index)
union_df = union_df %>% filter(`Countries` != 'OECD - Total')
uniondata = union_df %>% pivot_longer(`2000`:`2020`, names_to = "Years", values_to = "UnionDensity")
uniondata$`Years` = as.numeric(uniondata$`Years`)

#Filtering dataframes for countries of interest
coi_uniondata = uniondata %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')
coi_uniondata = coi_uniondata %>% na.omit()


usethis::use_data(uniondata, coi_uniondata, overwrite = TRUE)
