# suicidedata
Kia Ora! Sucidedata is an R Package containing a dataset used to predict number of suicides based on various socioeconomic factors for our OECD countries of
interest (COI) = {New Zealand, United States, South Korea and Netherlands}. This is a public project, feel free to use our data for plotting/regression
modelling. Please also consider checking out our Shiny app:
https://4q2fgc-luka0foy.shinyapps.io/interactivesuicideplots/?_ga=2.235376518.66622219.1666048774-1683258422.1666048774
which allows interactive visualisation of the variables in this dataset, and contact us if you wish to contribute to the package!

Importing:
#install.packages('devtools') #If you don't have it already!
library(devtools)
install_github('popsnot/suicidedata')

Usage:
library(suicidedata)

Datasets included:
minwagedata
- Dataset for (most) OECD Countries containing country name, observation year and their minimum wage in USD back to 2000
coi_minwagedata
- Dataset containing country name, year of observation and minimum wage in USD back to 2000 for our COI

uniondata
- Dataset for (most) OECD Countries containing country name, observation year and the percentage of employees in unions back to 2001
coi_uniondata
- Dataset containing year of observation and the percentage of employees in unions back to 2001 for our COI

suidata
- Global dataset of country names, observation year and the amount of deaths due to suicide per 100000 people back to 1990
coi_suidata
- Dataset containing country names, observation year and the amount of deaths due to suicide per 100000 people back to 1990 for our COI

depressiondata
- Global dataset of country names, observation year and male/female prevalence of depression back to 1990
coi_depressiondata
- Dataset containing country names, observation year and male/female prevalence of depression back to 1990 for our COI

labourdata
- Dataset for (most) OECD Countries containing country name, observation year and average hours of work back to 2001
coi_labourdata
- Dataset containing country name, observation year and average hours of work back to 2001 for our COI

maindata
- Dataset containing all variables going back to 2000 for our COI - created with a series of inner joins on the (Country name, Year) primary key
