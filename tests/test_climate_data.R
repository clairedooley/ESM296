# Test 1 - test that the combo function works when timestep = y      

test_that("combo.works.year", {
  clim.data=as.data.frame(cbind(month=c(3,3,3), day=c(1,1,1), year=c(1991,1992,1993), rain=c(1,2,3), tavg=c(1,2,2))) # created a small climate dataframe to run test on
  expect_that(Combo(dataset=clim.data, timestep="y")$Coldest.spring.temp.year[1,], equals("1991"))
}
)

# Test 2 - test that the combo function works when timestep = d    
test_that("combo.works.day", {
  clim.data=as.data.frame(cbind(month=c(3,3,3), day=c(1,1,1), year=c(1991,1992,1993), rain=c(1,2,3), tavg=c(1,2,2))) # created a small climate dataframe to run test on
  expect_that(Combo(dataset=clim.data, timestep="d")$Wettest.spring.rain.day[1,], equals("1"))
}
)

