test_that("hsiao works", {

  library(poobly)
  library(plm)

  data("Gasoline", package = "plm")

  x <- hsiao(lgaspcar ~ lincomep + lrpmg + lcarpcap, Gasoline)
  expect_equal(x$Hypothesis, c("H1", "H2", "H3"))
  expect_equal(round(x$F.statistic, 5),  c(129.31658, 27.33519, 83.96080))
  expect_equal(round(x$p.value, 5), c(0, 0, 0))
  expect_equal(x$formula, lgaspcar ~ lincomep + lrpmg + lcarpcap)

  expect_output(print(x),"                  Hsiao Homogeneity Test

Hypothesis| Null |                Alternative
----------+------+-------------------------------------------
    H1    |Pooled|                    H2
    H2    |  H3  |      Heterogeneous intercept & slope
    H3    |Pooled|Heterogeneous intercept & Homogeneous slope
=============================================================

formula: lgaspcar ~ lincomep + lrpmg + lcarpcap

    Hypothesis  F-statistic   p-value
  1     H1       129.3166     < 0.001
  2     H2        27.3352     < 0.001
  3     H3        83.9608     < 0.001 ")

  x <- hsiao(lgaspcar ~ lincomep + lrpmg + lcarpcap,
             pdata.frame(Gasoline))
  expect_equal(x$Hypothesis, c("H1", "H2", "H3"))
  expect_equal(round(x$F.statistic, 5),  c(129.31658, 27.33519, 83.96080))
  expect_equal(round(x$p.value, 5), c(0, 0, 0))
  expect_equal(x$formula, lgaspcar ~ lincomep + lrpmg + lcarpcap)

  x <- hsiao(lgaspcar ~ lincomep + lrpmg + lcarpcap,
             Gasoline, index=c("country", "year"))
  expect_equal(x$Hypothesis, c("H1", "H2", "H3"))
  expect_equal(round(x$F.statistic, 5),  c(129.31658, 27.33519, 83.96080))
  expect_equal(round(x$p.value, 5), c(0, 0, 0))
  expect_equal(x$formula, lgaspcar ~ lincomep + lrpmg + lcarpcap)

  tryerror <- try(x <- hsiao(formula=lgaspcar ~ lincomep + lrpmg + lcarpcap,
                             data=Gasoline,
                             model="pooling"),
                  silent = TRUE)

  expect_true(class(tryerror) == "try-error")

  expect_equal(
    tryerror[[1]],
    paste0("Error in hsiao(formula = lgaspcar ~ lincomep + lrpmg +",
           " lcarpcap, data = Gasoline,  : \n  Remove 'model' arguement.\n"))

  tryerror <- try(x <- hsiao(formula=lgaspcar ~ lincomep + lrpmg + lcarpcap,
                             data=Gasoline,
                             effect="individual"),
                  silent = TRUE)

  expect_true(class(tryerror) == "try-error")

  expect_equal(
    tryerror[[1]],
    paste0("Error in hsiao(formula = lgaspcar ~ lincomep + lrpmg +",
           " lcarpcap, data = Gasoline,  : \n  Remove 'effect' arguement.\n"))



})
