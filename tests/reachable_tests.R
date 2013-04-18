# test the forward and backward componetnt functions
require(tsna)
require(testthat)
linegraph<-network.initialize(10)
add.edges(linegraph,tail=1:9,head=2:10)


# test non timed version error
test<-linegraph
expect_error(forward.reachable(test,v=1),'must be a networkDynamic')

activate.edges(test,onset=0,terminus=3)

expect_equal(forward.reachable(test,v=1,per.step.depth=Inf),1:10)

test<-linegraph
activate.edges(test,onset=10:0,terminus=11:1)
expect_equal(forward.reachable(test,v=5,per.step.depth=Inf),5:10)
expect_equal(forward.reachable(test,v=10,per.step.depth=Inf),10)

# test with two seeds
expect_equal(forward.reachable(test,v=c(1,5),per.step.depth=1),c(1,5,2,6))

# test on undirected case
test<-linegraph
set.network.attribute(test,'directed',FALSE)
activate.edges(test,onset=0,terminus=3)
expect_equal(forward.reachable(test,v=5,per.step.depth=Inf),)

# test on network with two components

# test on network with net.obs.period set
             