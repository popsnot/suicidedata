#' @title Global Depression Data#' @description Dataset of Global Depression#' @format A data frame with 6000 rows and 7 variables:#' \describe{#'   \item{\code{Code}}{character Country Code}#'   \item{\code{Countries}}{character Country Name}#'   \item{\code{Years}}{integer Year of Observation}#'   \item{\code{Prevalence_depr_male}}{double Percentage of Males with Depression}#'   \item{\code{Prevalence_depr_female}}{double Percentage of Females with Depression}#'   \item{\code{Population_Estimate}}{double Estimate of Population}#'}#' @source \url{http://raw.githubusercontent.com/lostconnectionhere/mental_health/main/data/prevalence-of-depression-males-vs-females.csv /}"depressiondata"


#' @title Global Suicide Data#' @description Dataset of Global Suicide Rates#' @format A data frame with 6000 rows and 5 variables:#' \describe{#'   \item{\code{Code}}{character Country Code}#'   \item{\code{Countries}}{character Country Name}#'   \item{\code{Years}}{integer Year of Observation}#'   \item{\code{Suicides_Per100k}}{double Number of Suicides per 100,000 people}#'}#' @source \url{http://raw.githubusercontent.com/popsnot/DATA201-Group-Project-2022/main/suicide-death-rates.csv/}"suidata"


#' @title OECD Union Density Data#' @description Dataset of OECD Countries and the percentage of employees in unions#' @format A data frame with 798 rows and 3 variables:#' \describe{#'   \item{\code{Countries}}{character Country Name}#'   \item{\code{Years}}{double Year of Observation}#'   \item{\code{UnionDensity}}{double Percentage of Employees in Unions} #'}#' @source \url{http://stats.oecd.org/index.aspx?DataSetCode=TUD/}"uniondata"


#' @title OECD Minimum Wage Data#' @description Dataset of OECD Countries and their minimum wage in USD#' @format A data frame with 640 rows and 3 variables:#' \describe{#'   \item{\code{Countries}}{character Country Name}#'   \item{\code{Years}}{character Year of Observation}#'   \item{\code{Wages}}{double Minimum Wage in USD} #'}#' @source \url{https://stats.oecd.org/index.aspx?DataSetCode=RMW}"minwagedata"

#' @title OECD Labour Data
#' @description Dataset of OECD Countries and their average hours of work in one year
#' @format A data frame with 640 rows and 3 variables:
#' \describe{
#'   \item{\code{Countries}}{character Country Name}
#'   \item{\code{Years}}{character Year of Observation}
#'   \item{\code{Hours}}{double Average Hours Worked per Year} 
#'}
#' @source \url{https://stats.oecd.org/sdmx-json/data/DP_LIVE/.HRWKD.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en}
"labourdata"

#' @title Main Data#' @description Main dataframe containing all variables#' @format A data frame with 63 rows and 9 variables:#' \describe{#'   \item{\code{Countries}}{integer Country Name}#'   \item{\code{Years}}{double Year of Observation}#'   \item{\code{UnionDensity}}{double Percentage of Employees in Unions}#'   \item{\code{Wages}}{double Minimum Wage in USD}#'   \item{\code{Population_Estimate}}{double Estimate of Population}#'   \item{\code{Dep_Prevalence}}{double Percentage of Population with Depression}#'   \item{\code{Suicides_Per100k}}{double Number of Suicides per 100,000}#'   \item{\code{Hours}}{double Average Hours Worked per Year} #'}#' @source \url{wrangled}
"maindata"

