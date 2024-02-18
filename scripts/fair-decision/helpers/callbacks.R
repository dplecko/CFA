
eth_mim_callback <- function(x, val_var, env) {

  groups <- list(
    Caucasian = c("WHITE", "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN",
                  "WHITE - OTHER EUROPEAN", "WHITE - RUSSIAN"),
    Asian = c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CAMBODIAN",
              "ASIAN - CHINESE", "ASIAN - FILIPINO", "ASIAN - JAPANESE",
              "ASIAN - KOREAN", "ASIAN - OTHER", "ASIAN - THAI",
              "ASIAN - VIETNAMESE"),
    Hispanic = c("HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)",
                 "HISPANIC/LATINO - COLOMBIAN",
                 "HISPANIC/LATINO - CUBAN", "HISPANIC/LATINO - DOMINICAN",
                 "HISPANIC/LATINO - GUATEMALAN",
                 "HISPANIC/LATINO - HONDURAN", "HISPANIC/LATINO - MEXICAN",
                 "HISPANIC/LATINO - PUERTO RICAN",
                 "HISPANIC/LATINO - SALVADORAN", "HISPANIC OR LATINO"),
    `African American` = c("BLACK/AFRICAN AMERICAN"),
    Other = c("AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE",
              "UNABLE TO OBTAIN", "UNKNOWN/NOT SPECIFIED", "MIDDLE EASTERN",
              "MULTI RACE ETHNICITY",
              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER",
              "PATIENT DECLINED TO ANSWER",
              "PORTUGUESE", "SOUTH AMERICAN",
              "AMERICAN INDIAN/ALASKA NATIVE",
              "AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE",
              "CARIBBEAN ISLAND", "BLACK/AFRICAN", "BLACK/CAPE VERDEAN",
              "BLACK/HAITIAN")
  )
  map <- unlist(groups)
  names(map) <- rep(names(groups), times = lapply(groups, length))

  x[, race := names(map)[match(race, map)]]
}

eth_miiv_callback <- function(x, val_var, env) {

  groups <- list(
    Caucasian = c("WHITE", "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN",
                  "WHITE - OTHER EUROPEAN", "WHITE - RUSSIAN"),
    Asian = c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CHINESE",
              "ASIAN - KOREAN", "ASIAN - SOUTH EAST ASIAN"),
    Hispanic = c("HISPANIC OR LATINO", "HISPANIC/LATINO",
                 "HISPANIC/LATINO - CENTRAL AMERICAN",
                 "HISPANIC/LATINO - COLUMBIAN", "HISPANIC/LATINO - CUBAN",
                 "HISPANIC/LATINO - DOMINICAN", "HISPANIC/LATINO - GUATEMALAN",
                 "HISPANIC/LATINO - HONDURAN", "HISPANIC/LATINO - MEXICAN",
                 "HISPANIC/LATINO - PUERTO RICAN",
                 "HISPANIC/LATINO - SALVADORAN", "SOUTH AMERICAN"),
    `African American` = c("BLACK/AFRICAN", "BLACK/AFRICAN AMERICAN",
                           "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND"),
    Other = c("OTHER", "UNKNOWN", "UNABLE TO OBTAIN", "MULTIPLE RACE/ETHNICITY",
              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
              "PATIENT DECLINED TO ANSWER", "PORTUGUESE")
  )

  map <- unlist(groups)
  names(map) <- rep(names(groups), times = lapply(groups, length))

  x[, race := names(map)[match(race, map)]]


}

eth_eicu_cb <- function(x, val_var, env) {

  x[ethnicity %in% c("Native American", "Other/Unknown"), ethnicity := "Other"]
}

miiv_adm_epi_cb <- function(x, val_var, extra_var, env) {

  x <- merge(x, env$icustays[, c("stay_id", "intime")],
             by = "stay_id")
  x <- as_id_tbl(x, id_vars = "subject_id")
  x <- data.table::setorderv(x, cols = c("subject_id", "intime"))
  x[, adm_episode := seq_along(stay_id), by = "subject_id"]
  x <- as_id_tbl(x, id_vars = "stay_id")
  x[, subject_id := adm_episode]
}

acute_dayone <- function(sofa, ...) {

  ind_var <- index_var(sofa)
  sofa <- sofa[get(ind_var) == hours(24L)]
  sofa[, acu_24 := sofa]
  sofa[, c(ind_var, "sofa") := NULL]
  sofa
}

lact_dayone <- function(...) {

  tbl <- list(...)[[1]]

  ind_var <- index_var(tbl)
  val_var <- names(list(...)[1])
  tbl <- tbl[get(ind_var) >- hours(0L) & get(ind_var) <= hours(24L)]
  tbl[, list(lact_24 = max(lact)), by = c(id_vars(tbl))]
}

ast_dayone <- function(...) {

  tbl <- list(...)[[1]]

  ind_var <- index_var(tbl)
  val_var <- names(list(...)[1])
  tbl <- tbl[get(ind_var) >- hours(0L) & get(ind_var) <= hours(24L)]
  tbl[, list(ast_24 = max(ast)), by = c(id_vars(tbl))]
}

pafi_dayone <- function(...) {

  tbl <- list(...)[[1]]

  ind_var <- index_var(tbl)
  val_var <- names(list(...)[1])
  tbl <- tbl[get(ind_var) >- hours(0L) & get(ind_var) <= hours(24L)]
  tbl[, list(pafi_24 = min(pafi)), by = c(id_vars(tbl))]
}

safi_dayone <- function(...) {

  tbl <- list(...)[[1]]

  ind_var <- index_var(tbl)
  val_var <- names(list(...)[1])
  tbl <- tbl[get(ind_var) >- hours(0L) & get(ind_var) <= hours(24L)]
  tbl[, list(safi_24 = min(safi)), by = c(id_vars(tbl))]
}

sic_death_cb <- function(x, ...) {

  x[, HospitalDischargeType := HospitalDischargeType == 2028]
  x[, OffsetAfterFirstAdmission := NA]
  x[HospitalDischargeType == TRUE,
    OffsetAfterFirstAdmission := TimeOfStay]
  x[!is.na(OffsetOfDeath) & HospitalDischargeType == TRUE,
    OffsetAfterFirstAdmission := OffsetOfDeath]
  x
}

miiv_charlson_dir <- function(x, ...) {

  ch9 <- icd9_comorbid_charlson(x[icd_version == 9])
  ch10 <- icd10_comorbid_charlson(x[icd_version == 10])

  make_long <- function(x, id_name) {

    res <- id_tbl(id = as.integer(rownames(x)))
    res <- cbind(res, x)
    res <- rename_cols(res, id_name, "id")
    res <- melt.data.table(res, id.vars = id_name)
    as_id_tbl(res)
  }

  ch <- rbind(
    make_long(ch9, id_vars(x)),
    make_long(ch10, id_vars(x))
  )

  ch <- ch[, list(cmb = max(value, na.rm = TRUE)), by = c(id_vars(ch), "variable")]
  ch[, list(icd_code = sum(cmb, na.rm = TRUE)), by = c(id_vars(ch))]
}
