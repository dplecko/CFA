

#' SFM_proj
#' 
#' Returns the Standard Fairness Model projection for the datasets that are
#' included in the faircause R-package.
#' 
#' @param dataset One of the following dataset names: `"berkeley"`, `"compas"`, 
#' or `"census"`
#' 
#' @export
SFM_proj <- function(dataset = c("berkeley", "compas", "census")) {
  
  dataset <- match.arg(dataset, c("berkeley", "compas", "census"))
  
  switch(dataset, compas = SFM_compas(),
         census = SFM_census(),
         berkeley = SFM_berkeley())
}

SFM_compas <- function() {
  
  list(
    X = "race",
    W = c("juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count",
          "c_charge_degree"),
    Z = c("age", "sex"),
    Y = "two_year_recid", x0 = "White", x1 = "Non-White", ylvl = 1
  )
}

SFM_berkeley <- function() {
  
  list(
    X = "gender",
    W = c("dept"),
    Z = character(0L),
    Y = "admit", x0 = "Male", x1 = "Female", ylvl = "Admitted"
  )
}

SFM_census <- function() {
  
  list(
    X = "sex",
    W = c("marital", "family_size", "children", "education_level",
          "english_level", "hours_worked", "weeks_worked", "occupation",
          "industry"),
    Z = c("age", "race", "hispanic_origin", "citizenship", "nativity",
          "economic_region"),
    Y = "salary", x0 = "male", x1 = "female", ylvl = NA_real_
  )
}