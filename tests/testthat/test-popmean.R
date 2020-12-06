context("output checks")
test_that("", {
  temp <- popmean(dat, intervention = IRS, years = 2020:2022)
  temp[["weighted_mean"]][temp$year==2020]
  expect_equal(temp[["weighted_mean"]][temp$year==2020], 0.077232642, tolerance = 0.001)

})
