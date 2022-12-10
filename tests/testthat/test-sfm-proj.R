
test_that("SFM projection works", {
  
  compas <- SFM_proj("compas")
  expect_snapshot_json(compas)
  
  berkeley <- SFM_proj("berkeley")
  expect_snapshot_json(berkeley)
  
  census <- SFM_proj("census")
  expect_snapshot_json(census)
})