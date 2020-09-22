library(ERSA)

context("Test app Plots")


test_that("plotAnovaStats", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,])
  p <- plotAnovaStats(f)
  expect_is(p,"ggplot")
})


test_that("plottStats", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,])
  p <- plottStats(f)
  expect_is(p,"ggplot")
})


test_that("plotSeqSS", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,])
  p <- plotSeqSS(f)
  expect_is(p,"ggplot")
})


test_that("regPCPdata", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,])
  p <- regPCPdata(f)
  expect_is(p,"list")
  expect_is(p[[1]],"data.frame")
})


test_that("plotCIStats", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,])
  p <- plotCIStats(f)
  expect_is(p,"ggplot")
})

test_that("pcpPlot", {
  p <- pcpPlot(iris[1:10,],fit =lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,]))
  expect_is(p,"ggplot")
})
