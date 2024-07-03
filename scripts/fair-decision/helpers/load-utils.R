
load_difftime.anzics_tbl <- function(x, rows, cols = colnames(x),
                                     id_hint = id_vars(x),
                                     time_vars = ricu::time_vars(x), ...) {
  
  ricu:::warn_dots(...)

  ricu:::load_mihi(x, {{ rows }}, cols, id_hint, time_vars)
}

id_win_helper.anzics_env <- function(x) {
  
  merge_inter <- function(x, y) {
    merge(x, y, by = intersect(colnames(x), colnames(y)))
  }
  
  get_id_tbl <- function(tbl, id, start, end, aux) {
    as_src_tbl(x, tbl)[, c(id, start, end, aux)]
  }
  
  cfg <- sort(as_id_cfg(x), decreasing = TRUE)
  
  ids  <- vctrs::field(cfg, "id")
  sta <- vctrs::field(cfg, "start")
  end  <- vctrs::field(cfg, "end")
  
  res <- Map(get_id_tbl, vctrs::field(cfg, "table"), ids, sta,
             end, c(as.list(ids[-1L]), list(NULL)))
  res <- Reduce(merge_inter, res)
  
  res <- res[, c(sta, end) := lapply(.SD, ricu:::as_dt_min, get(sta[1L])),
             .SDcols = c(sta, end)]
  
  ricu:::order_rename(res, ids, sta, end)
}


load_difftime.sic_tbl <- function(x, rows, cols = colnames(x),
                                  id_hint = id_vars(x),
                                  time_vars = ricu::time_vars(x), ...) {
  
  sec_as_mins <- function(x) ricu:::min_as_mins(as.integer(x / 60))
  ricu:::warn_dots(...)
  # TODO: consider renaming fun to reflect its use for SICdb
  ricu:::load_eiau(x, {{ rows }}, cols, id_hint, time_vars, sec_as_mins)
}

id_win_helper.sic_env <- function(x) {
  
  sec_as_mins <- function(x) ricu:::min_as_mins(as.integer(x / 60))
  
  cfg <- sort(as_id_cfg(x), decreasing = TRUE)
  
  ids <- vctrs::field(cfg, "id")
  sta <- c(unique(vctrs::field(cfg, "start")), "HospAdmTime")
  end <- vctrs::field(cfg, "end")
  
  tbl <- as_src_tbl(x, unique(vctrs::field(cfg, "table")))
  
  mis <- setdiff(sta, colnames(tbl))
  
  res <- load_src(tbl, cols = c(ids, intersect(sta, colnames(tbl)), end))
  
  if (length(mis) > 0L) {
    res[, c(mis) := 0L]
  }
  
  res <- res[, c(sta, end) := lapply(.SD, sec_as_mins), .SDcols = c(sta, end)]

  res <- setcolorder(res, c(ids, sta, end))
  res <- rename_cols(res, c(ids, paste0(ids, "_start"),
                            paste0(ids, "_end")), by_ref = TRUE)
  
  as_id_tbl(res, ids[2L], by_ref = TRUE)
}

