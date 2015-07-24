# tests for the time-projected functions

require(tsna)
library(networkDynamicData)
require(testthat)

# trivial example network, directed case
test<-network.initialize(3)
add.edges.active(test,tail=1,head=2,onset=0,terminus=1)
add.edges.active(test,tail=2,head=3,onset=1,terminus=2)
testProj<-timeProjectedNetwork(test,start=0,end=2)
expect_equal(as.matrix(testProj),
              matrix(
              c(0, 1, 0, 1, 0, 0,
                0, 0, 0, 0, 1, 0,
                0, 0, 0, 0, 0, 1,
                0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 1,
                0, 0, 0, 0, 0, 0),ncol=6,byrow=TRUE),check.attributes=FALSE)

# undirected case
test%n%'directed'<-FALSE
testProj<-timeProjectedNetwork(test,start=0,end=2)
expect_equal(as.matrix(testProj),
             matrix(
               c(0, 1, 0, 1, 0, 0,
                 1, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 1,
                 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 1,
                 0, 0, 0, 0, 1, 0),ncol=6,byrow=TRUE),check.attributes=FALSE)

data(harry_potter_support)
hpProj<-timeProjectedNetwork(harry_potter_support)
plot(hpProj,arrowhead.cex = 0,edge.col=ifelse(hpProj%e%'edge.type'=='within_slice','black','gray'),vertex.cex=0.7)

# check that specific slices copied correctly
# WHY DOES THIS FAIL?
#expect_equal(as.matrix(network.extract(harry_potter_support,at=5)),as.matrix(hpProj)[(64*4+1):(64*5),(64*4+1):(64*5)])

# check that vertex attributes copied
expect_equal((hpProj%v%'gender')[1:64],harry_potter_support%v%'gender')
expect_equal((hpProj%v%'gender')[65:128],harry_potter_support%v%'gender')

expect_equal(length(network.vertex.names(hpProj)),network.size(hpProj))

# check edge type added
expect_true("edge.type"%in%list.edge.attributes(hpProj))

data(moodyContactSim)
moodyProj<-timeProjectedNetwork(moodyContactSim,time.increment=100)

# correct size of new network?
expect_equal(network.size(moodyContactSim)*(moodyContactSim%n%'net.obs.period')$observations[[1]][2]/100,network.size(moodyProj))

# create network from changes
changes<-get.change.times(moodyContactSim)
moodyProjChange<-timeProjectedNetwork(moodyContactSim,onsets=changes,termini =changes)

# test for some vertex inactivity
data(windsurfers)
windProj<-timeProjectedNetwork(windsurfers,start=0,end=5)

# gplot3d(windProj,edge.col=ifelse(proj%e%'edge.type'=='within_slice','black','gray'))
