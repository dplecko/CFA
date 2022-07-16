#' Berkeley Admissions dataset (Bickel et. al., 1975).
#'
#' A dataset on admission statistics at UC Berkeley. Students apply to one of six
#' departments and the admission decision is based on gender and the department of choice.
#'
#' @format A data frame with 4,526 rows and 3 variables:
#' \describe{
#'   \item{gender}{gender of the individual}
#'   \item{dept}{department of application, taking 6 values A to F}
#'   \item{admit}{admission decision, taking two values Admitted/Rejected}
#' }
"berkeley"

#' COMPAS dataset.
#'
#' A real dataset from Broward County, Florida. Contains information on individuals
#' released on parole, and whether they reoffended within two years.
#'
#' @format A data frame with 1,000 rows and 9 variables:
#' \describe{
#'   \item{sex}{sex of the individual}
#'   \item{age}{age, measured in years}
#'   \item{race}{race, binary with values Non-White and White}
#'   \item{juv_fel_count}{count of juvenile felonies}
#'   \item{juv_misd_count}{count of juvenile misdemeanors}
#'   \item{juv_other_count}{count of other juvenile offenses}
#'   \item{priors_count}{count of prior offenses}
#'   \item{c_charge_degree}{degree of charge, with two values, F (felony) and M (misdemeanor)}
#'   \item{two_year_recid}{a logical TRUE/FALSE indicator of recidivism within two years after parole start}
#' }
"compas"

#' Census information of US government employees.
#'
#' The dataset contains various demographic, education and work information
#' of the employees of the US government. The data is taken from the 2018
#' US Census data.
#'
#' @format A data frame with 204,309 rows and 17 variables:
#' \describe{
#'     \item{sex}{gender of the employee}
#'     \item{age}{employee age in years}
#'     \item{race}{race of the employee}
#'     \item{hispanic_origin}{indicator of hispanic origin}
#'     \item{citizenship}{citizenship of the employee}
#'     \item{nativity}{indicator of nativity to the US}
#'     \item{marital}{marital status}
#'     \item{family_size}{size of the employee's family}
#'     \item{children}{number of children of the employee}
#'     \item{education_level}{education level measured in years}
#'     \item{english_level}{}
#'     \item{salary}{yearly salary in US dollars}
#'     \item{hours_worked}{hours worked every week}
#'     \item{weeks_worked}{weeks worked in the given year}
#'     \item{occupation}{occupation classification}
#'     \item{industry}{industry classification}
#'     \item{economic_region}{economic region where the person is employed in the US}
#' }
#' @source \url{https://www.census.gov/programs-surveys/acs/microdata/documentation.html}
"gov_census"
