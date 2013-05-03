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

# reverse-ordered edge spells
test<-linegraph
activate.edges(test,onset=10:0,terminus=11:1)
expect_equal(forward.reachable(test,v=5,per.step.depth=Inf),5:6)
expect_equal(forward.reachable(test,v=10,per.step.depth=Inf),10)

# forward-ordred edge spells
test<-linegraph
activate.edges(test,onset=0:10,terminus=1:11)
expect_equal(forward.reachable(test,v=5,per.step.depth=Inf),5:10)
expect_equal(forward.reachable(test,v=10,per.step.depth=Inf),10)



# test with two seeds
expect_equal(forward.reachable(test,v=c(1,5)),c(1,5,2,3,4,6,7,8,9,10))

# test on undirected case
test<-linegraph
set.network.attribute(test,'directed',FALSE)
activate.edges(test,onset=0,terminus=3)
expect_equal(forward.reachable(test,v=5,per.step.depth=Inf),c(5,4,6,3,7,2,8,1,9,10))

# test on network with bounded time
test<-linegraph
activate.edges(test,onset=0:10,terminus=1:11)
expect_equal(forward.reachable(test,v=5,start=4,end=6),c(5,6,7,8))
expect_equal(forward.reachable(test,v=1,end=6),c(5,6,7,8))


# test with finite depthtest<-linegraph
test<-linegraph
activate.edges(test,onset=0,terminus=10)
expect_error(forward.reachable(test,v=1,per.step.depth=2,end=3),'currently only supports per.step.depth arguments of 1 and Inf')

# test on network with two components


# test on network with net.obs.period set
             