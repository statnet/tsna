# tests for durations_censored methods

library(tsna)
require(testthat)
require(networkDynamicData)
data(moodyContactSim)

# check output structure
out<-edgeDurationEst(moodyContactSim,start=200,end=500,mode="left.truncation")
expect_equal(names(out),c("records","n.max","n.start","events","*rmean","*se(rmean)","median","0.95LCL","0.95UCL"))
expect_true(is.numeric(out))

out<-edgeDurationEst(moodyContactSim,start=200,end=500,rmean="extant")
expect_equal(names(out),c("records","n.max","n.start","events","*rmean","*se(rmean)","median","0.95LCL","0.95UCL"))
expect_true(is.numeric(out))

out<-edgeDurationEst(moodyContactSim,start=200,end=500,mode='right.censoring')
expect_equal(names(out),c("records","n.max","n.start","events","*rmean","*se(rmean)","median","0.95LCL","0.95UCL"))
expect_true(is.numeric(out))
