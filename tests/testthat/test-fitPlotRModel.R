testthat::test_that("function fitPlotRModel", {
  testData <- data.frame(
    X = c(2002.5,1954.5,1906.5,1861,1817.5,
          1778,1741,1707.5,1674.5,1645,1637,1585,1552,1518,
          1465,1412.5,1379,1340.5,1306,1274.5,1249,1225,1203.5,
          1183.5,1165,1149,1134),
    XUncertainty = c(1.25,14.25,27.25,38.5,46.75,52.5,
                     55.5,56.75,56.75,55.5,55,50,47,43,37.5,32.25,
                     28.5,25.25,24,22.75,23,25,27.75,31.75,36,42.5,47.5),
    XUncertainty2 = c(0.00457265056727685,
                      0.0521282164669561,0.0996837823666354,0.140837637472127,0.171017131216154,
                      0.192051323825628,0.203025685187092,0.207598335754369,
                      0.207598335754369,0.203025685187092,0.201196624960182,
                      0.182906022691074,0.17193166132961,0.157299179514324,
                      0.137179517018306,0.117974384635743,0.104256432933912,
                      0.0923675414589925,0.0877948908917156,0.0832222403244387,
                      0.0841367704378941,0.0914530113455371,0.101512842593546,
                      0.116145324408832,0.131692336337573,0.155470119287413,
                      0.17376072155652),
    standardizedX = c(1.78854993003324,1.61296014824981,
                      1.43737036646637,1.2709258858175,1.11179764607626,
                      0.967301888150314,0.831951431358919,0.709404396155899,
                      0.58868642117979,0.480771867792057,0.451506904161485,
                      0.261284640562768,0.140566665586659,0.0161905701567282,-0.17768981389581,
                      -0.369741137721438,-0.492288172924458,-0.633125810396585,
                      -0.759330966053426,-0.874561760348803,-0.967843831921251,
                      -1.05563872281297,-1.13428831257013,-1.20745072164656,
                      -1.27512595004226,-1.3336558773034,-1.38852768411072),
    Y = c(543.9296188,1327.66702,1175.908707,
          678.5853659,129.6458527,1453.605016,254.6636156,
          1266.407563,453.7181996,2132.582547,3549.959401,1485.340344,
          2046.829856,1936.738956,1677.708543,224.2065934,288.20635,
          668.3963964,1790.182921,888.3141762,631.2422008,
          717.0618557,1233.244681,0,410.17249,602.2077922,88.4726683),
    YUncertainty = c(0,0,0,0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )

  testModelFit <- fitPlotRModel(data = testData,
                                K = 25,
                                burnin = 1000,
                                iter = 5000,
                                penalty = 2,
                                smoothConst = 1,
                                nChains = 1,
                                sdVar = FALSE,
                                progressMessage = "")

  testthat::expect_true(inherits(testModelFit$beta, "matrix"))
  testthat::expect_null(testModelFit$betaSigma)
  testthat::expect_true(typeof(testModelFit$beta) == "double")
  testthat::expect_true(typeof(testModelFit$betaSigma) == "NULL")
  testthat::expect_lt(max(testModelFit$range$mean), 1530 + 360) # max(mean) + mean(se)
  testthat::expect_gt(max(testModelFit$range$mean), 1530 - 360) # max(mean) - mean(se)
  testthat::expect_lt(min(testModelFit$range$mean), 460 + 360)  # min(mean) + mean(se)
  testthat::expect_gt(min(testModelFit$range$mean), 460 - 360)  # min(mean) - mean(se)

  testModelFit <- fitPlotRModel(data = testData,
                                K = 25,
                                burnin = 1000,
                                iter = 5000,
                                penalty = 2,
                                smoothConst = 1,
                                nChains = 1,
                                sdVar = TRUE,
                                progressMessage = "")

  testthat::expect_true(inherits(testModelFit$beta, "matrix"))
  testthat::expect_true(inherits(testModelFit$betaSigma, "matrix"))
  testthat::expect_true(typeof(testModelFit$beta) == "double")
  testthat::expect_true(typeof(testModelFit$betaSigma) == "double")
  testthat::expect_lt(max(testModelFit$range$mean), 2570 + 660) # max(mean) + mean(se)
  testthat::expect_gt(max(testModelFit$range$mean), 2579 - 660) # max(mean) - mean(se)
  testthat::expect_lt(min(testModelFit$range$mean), 240 + 550)  # min(mean) + mean(se)
  testthat::expect_gt(min(testModelFit$range$mean), 240 - 550)  # min(mean) - mean(se)
})

# used following loop to get typical values for the expects
#
# fitLoop <- lapply(1:20, function(i) {
#   tmp <- fitPlotRModel(data = testData,
#                        K = 25,
#                        burnin = 1000,
#                        iter = 5000,
#                        penalty = 2,
#                        smoothConst = 1,
#                        nChains = 1,
#                        sdVar = TRUE, # or FALSE
#                        progressMessage = "")
#
#   print(i)
#   data.frame(meanMin = tmp$range$mean %>% min(),
#              meanMax = tmp$range$mean %>% max(),
#              meanSD = tmp$range$se %>% mean())
# })
#
# fitLoop %>% bind_rows() %>% summary()
