expect_that((-4)**2, throws_error())

expect_that(sqrt(-4), gives_warning())

test_that("spring.avg.temp.works",
          {
          clim.data = as.data.frame(cbind(month=c(1:4), year=c(2000,2001,2002,2003), rain=rep(0, times=4), tavg=c(1,3,4,4)))
          expect_that(spring.avg.temp(clim.data, spring.months=1)[[2]][2], equals("0"))
          expect_that(spring.avg.temp(clim.data, spring.months=4)[[1]][2], equals("4"))
          expect_that(spring.avg.temp(clim.data, spring.months=1)[[3]][2]$year, equals(2000))
})