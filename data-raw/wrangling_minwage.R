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


future_cols= list(mw_list_2001, mw_list_2002, mw_list_2003, mw_list_2004, mw_list_2005, mw_list_2006, mw_list_2007,
                  mw_list_2008, mw_list_2009, mw_list_2010, mw_list_2011,
                  mw_list_2012,mw_list_2013,mw_list_2014, mw_list_2015, mw_list_2016, mw_list_2017,
                  mw_list_2018, mw_list_2019, mw_list_2020, mw_list_2021)

index=1
x1=1
x2=32
for (col in future_cols){
    future_cols[index] = list(mw_data[x1:x2])
    index=index+1
    x1=x1+32
    x2=x2+32
}

min_wage_df = tibble(Countries = countries, '2001' = as.numeric(gsub("(*UCP)\\s*", "", unlist(future_cols[1]),
                                                                     perl=TRUE)),
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
minwagedata = melted_mwdf


melted_mwdf$`Years` = as.numeric(melted_mwdf$`Years`)

coi_minwagedata = melted_mwdf %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' |
                                              `Countries` == 'United States' | `Countries` == 'South Korea')



usethis::use_data(minwagedata, coi_minwagedata, overwrite = TRUE)
