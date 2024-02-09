context("Aslett et al (2015) functions")

test_that("Example in Section 4.1 computes correctly", {
  sys <- createSystem(s -- 1 -- 2:4:5, 2 -- 3 -- t, 4:5 -- 6 -- t, s -- 7 -- 8 -- t, s -- 9 -- 10 -- 11 -- t, 7 -- 10 -- 8,
                      types = list("T1" = c("1","6","11"),
                                   "T2" = c("2","3","9"),
                                   "T3" = c("4","5","10"),
                                   "T4" = c("7","8")))

  sig <- computeSystemSurvivalSignature(sys)

  set.seed(233)
  t1 <- rexp(100, rate=0.55)
  t2 <- rweibull(100, scale=1.8, shape=2.2)
  t3 <- rlnorm(100, 0.4, 0.9)
  t4 <- rgamma(100, scale=0.9, shape=3.2)

  test.data <- list("T1"=t1, "T2"=t2, "T3"=t3, "T4"=t4)

  yS <- nonParBayesSystemInference(c(0.5, 1, 1.5, 2, 2.5), sig, test.data)

  expect_equal(yS, c(0.998234874384741, 0.976341608804246, 0.842091867913212, 0.608406014322561, 0.361676850934916))
})
