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

uniondata = union_df %>% pivot_longer(`2000`:`2020`, names_to = "Years", values_to = "UnionDensity")

uniondata$`Years` = as.numeric(uniondata$`Years`)

coi_uniondata = uniondata %>% filter(`Countries` == 'New Zealand' | `Countries` == 'Netherlands' | `Countries` == 'United States' | `Countries` == 'South Korea')

coi_uniondata = coi_uniondata %>% na.omit()

usethis::use_data(uniondata, coi_uniondata, overwrite = TRUE)
