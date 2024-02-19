get_data <- function(src = "adult") {

  dat <- switch(src, adult = get_data_adult(), compas = get_data_compas(),
                census = get_data_census(), berkeley = get_data_berkeley())

  dat

}

get_metadata <- function(src = "adult") {

  mdat <- switch(src, adult = get_metadata_adult(),
                 compas = get_metadata_compas(),
                 census = get_metadata_census(),
                 berkeley = get_metadata_berkeley())

  mdat

}

get_mat <- function(src = "compas") {

  mat <- switch(src, adult = get_mat_adult(),
                 compas = get_mat_compas(),
                 census = get_mat_census(),
                 berkeley = get_mat_berkeley())

  mat

}

get_data_adult <- function() {

  root <- rprojroot::find_root(rprojroot::is_git_root)

  adult1 <- read_csv(file.path(root, "data", "adult-data", "adult.data"),
                     col_names = FALSE)

  adult2 <- read_csv(file.path(root, "data", "adult-data", "adult.test"),
                     col_names = FALSE)

  adult <- rbind(adult1, adult2)
  adult <- data.frame(adult)

  colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin',
                       'educatoin_num', 'marital_status', 'occupation',
                       'relationship', 'race', 'sex', 'capital_gain',
                       'capital_loss', 'hours_per_week', 'native_country',
                       'income')
  adult[, c("educatoin", "relationship", "fnlwgt",
            "capital_gain", "capital_loss")] <- NULL
  factor.columns <- c("workclass", "marital_status", "occupation", "race",
                      "sex", "native_country", "income")

  for(i in factor.columns) {

    adult[, i] <- as.factor(adult[, i])

  }

  ### recode the factors appropriately

  # variable workclass
  adult$workclass <- combineLevels(adult$workclass,
                                   levs = c("?", "Never-worked", "Without-pay"),
                                   newLabel = "Other/Unknown")

  adult$workclass <- combineLevels(adult$workclass,
                                   levs = c("Self-emp-inc", "Self-emp-not-inc"),
                                   newLabel = "Self-Employed")

  adult$workclass <- combineLevels(adult$workclass,
                                   levs = c("Federal-gov", "Local-gov",
                                            "State-gov"),
                                   newLabel = "Government")

  # variable marital_status
  adult$marital_status <- combineLevels(adult$marital_status,
                                        levs = c("Married-AF-spouse",
                                                 "Married-civ-spouse",
                                                 "Married-spouse-absent"),
                                        newLabel = "Married")

  adult$marital_status <- combineLevels(adult$marital_status,
                                        levs = c("Divorced", "Never-married",
                                                 "Separated", "Widowed"),
                                        newLabel = "Not-Married")

  # variable native_country
  non.US <- setdiff(levels(adult$native_country), "United-States")
  adult$native_country <- combineLevels(adult$native_country,
                                        levs = non.US,
                                        newLabel = "Not-United-States")

  # variable race
  adult$race <- combineLevels(adult$race,
                              levs = c("Amer-Indian-Eskimo",
                                       "Asian-Pac-Islander", "Other"),
                              newLabel = "Other")

  # variable income
  levels(adult$income) <- c("<=50K", "<=50K", ">50K", ">50K")

  adult$income <- as.integer(adult$income) - 1L

  adult
}

get_metadata_adult <- function() {

  list(
    X = "sex",
    W = c("educatoin_num", "marital_status", "workclass", "occupation",
          "hours_per_week"),
    Z = c("age", "race", "native_country"),
    Y = "income", x0 = "Male", x1 = "Female", ylvl = 1
  )

}

get_data_compas <- function() {

  root <- rprojroot::find_root(rprojroot::is_git_root)

  data <- read.csv(file.path(root, "data", "compas",
                             "compas-scores-two-years.csv"),
                   stringsAsFactors = TRUE)
  columns.keep <- which(names(data)
                        %in% c("age", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count",
                               "priors_count",
                               "c_charge_degree", "race", "two_year_recid")
  )
  data <- data[, columns.keep]
  levels(data$race) <- c("Non-White", "Non-White", "White", "Non-White",
                         "Non-White", "Non-White")
  data$race <- relevel(data$race, "White")

  data

}

get_data_census <- function() {

  root <- rprojroot::find_root(rprojroot::is_git_root)

  load(file.path(root, "data", "census", "gov_census.rda"))

  as.data.frame(gov_census[sample(nrow(gov_census), 20000), ])

}

get_data_berkeley <- function() {

  root <- rprojroot::find_root(rprojroot::is_git_root)
  load(file.path(root, "data", "berkeley", "ucbadmit.rda"))

  as.data.frame(ucbadmit)

}

get_mat_compas <- function() {


  vars <- c("age", "sex", "juv_fel_count","juv_misd_count",
            "juv_other_count", "priors_count",
            "c_charge_degree", "race", "two_year_recid")

  adj.mat <- array(0, dim = c(length(vars), length(vars)))
  colnames(adj.mat) <- vars
  rownames(adj.mat) <- colnames(adj.mat)

  # adding the edges to the matrix
  adj.mat[
    c("race", "sex", "age"), c("juv_fel_count", "juv_misd_count",
                               "juv_other_count", "priors_count",
                               "c_charge_degree", "two_year_recid")] <- 1
  adj.mat[c("juv_fel_count", "juv_misd_count", "juv_other_count"),
                   c("priors_count", "c_charge_degree", "two_year_recid")] <- 1
  adj.mat["priors_count", c("c_charge_degree", "two_year_recid")] <- 1
  adj.mat["c_charge_degree", "two_year_recid"] <- 1

  cfd.mat <- adj.mat
  cfd.mat[, ] <- 0L
  cfd.mat[c("sex", "age"), "race"] <- cfd.mat["race", c("sex", "age")] <- 1L

  list(adj.mat, cfd.mat)

}
