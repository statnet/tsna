# some tests for the basic durations function 
require(tsna)
require(testthat)
require(networkDynamicData)



data(moodyContactSim)
expect_equal(durations(moodyContactSim),c(32, 33, 32, 26, 30, 24, 32, 30, 26, 27, 27, 32, 31, 27, 26, 34, 44, 26))
# check with counts
expect_equal(durations(moodyContactSim,mode='counts'),rep(1,18))

# basic testing network
test<-network.initialize(5)
test[,]<-1
activate.edges(test,onset=0,terminus=2)
activate.edges(test,onset=5,terminus=6)

# spell level
expect_equal(durations(test,subject='spells'),c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1))
# edge level
expect_equal(durations(test,subject='edges'),rep(3,network.edgecount(test)))
expect_equal(durations(test,subject='dyads'),rep(3,network.edgecount(test)))


# test for dyads
test<-network.initialize(2)
add.edges(test,1,2)
add.edges(test,1,2)
add.edges(test,1,2)
expect_equal(durations(test,subject='edges',start=0,end=2),c(2,2,2))
expect_equal(durations(test,subject='dyads',start=0,end=2),6)
# test counts
expect_equal(durations(test,mode='count',subject='edges',start=0,end=2),c(1,1,1))
expect_equal(durations(test,mode='count',subject='dyads',start=0,end=2),3)



