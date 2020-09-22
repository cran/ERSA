library(ERSA)

context("Test helper functions")


test_that("extractModelData", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,])
  d <- extractModelData(f)
  expect_identical(d, iris[1:10,])
})


test_that("pvalOrder", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- pvalOrder(f)
  expect_is(f1,"lm")
})

test_that("bselOrder", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  preds <- bselOrder(f, refit=FALSE)
  expect_is(preds,"character")
  expect_equal(length(preds),3)
})


test_that("fselOrder", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- fselOrder(f)
  expect_is(f1,"lm")
})


test_that("randomPredOrder", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- randomPredOrder(f)
  expect_is(f1,"lm")
})


test_that("addPred", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,1:4])
  f1 <- addPred(f, "Petal.Length")
  expect_is(f1,"lm")
})

test_that("removePred", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- removePred(f, "Petal.Length")
  expect_is(f1,"lm")
})


test_that("upPredOrd", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- upPredOrd(f, "Petal.Length")
  expect_is(f1,"lm")
})


test_that("downPredOrd", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- downPredOrd(f, "Petal.Length")
  expect_is(f1,"lm")
})


test_that("regsubsetsOrder", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- regsubsetsOrder(f)
  expect_is(f1,"list")
})

test_that("drop1_models", {
  f <- lm(Sepal.Length ~ ., data=iris[1:10,1:4])
  f1 <- drop1_models(f, names(iris)[4])
  expect_is(f1,"list")
  expect_equal(length(f1),2)
})


test_that("add1_models", {
  f <- lm(Sepal.Length ~ Sepal.Width, data=iris[1:10,1:4])
  f1 <- add1_models(f, names(iris)[3:4])
  expect_is(f1,"list")
  expect_equal(length(f1),3)
})
