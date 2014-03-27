# tests for temporal density functions
require(tsna)
require(testthat)
require(networkDynamicData)

# density of a network with no edges
expect_equal(edge_event_density(as.networkDynamic(network.initialize(3))),0)
expect_equal(edge_event_density(as.networkDynamic(network.initialize(0))),0)

# complete network, explicit timing
complete<-as.networkDynamic(network.initialize(5))
complete[,]<-1
activate.edges(complete,onset=0,terminus=100)
expect_equal(edge_event_density(complete),0.01)
expect_equal(dyad_duration_density(complete),1)
expect_equal(edge_duration_density(complete),1)

# density of a complete network with no explicit timing
complete<-as.networkDynamic(network.initialize(5))
complete[,]<-1
expect_equal(edge_event_density(complete),1)
expect_equal(dyad_duration_density(complete),1)
expect_equal(edge_duration_density(complete),1)

# density of a complete network with no explicit timing, active.default=FALSE
expect_equal(dyad_duration_density(complete,active.default=FALSE),0)
expect_equal(edge_duration_density(complete,active.default=FALSE),0)

# density of a complete network with no explicit timing
complete<-as.networkDynamic(network.initialize(5))
complete[,]<-1
expect_equal(edge_event_density(complete),1)
expect_equal(dyad_duration_density(complete),1)
expect_equal(edge_duration_density(complete),1)


# single edge, implicit timing
half<-as.networkDynamic(network.initialize(2))
half[1,2]<-1
expect_equal(edge_event_density(half),1)
expect_equal(dyad_duration_density(half),0.5)
expect_equal(edge_duration_density(half),1)

# single undirected edge, implicit timing
half<-as.networkDynamic(network.initialize(2,directed=FALSE))
half[1,2]<-1
expect_equal(edge_event_density(half),1)
expect_equal(dyad_duration_density(half),1)
expect_equal(edge_duration_density(half),1)

# two edges, each half active
half<-network.initialize(2)
add.edges.active(half,tail=1:2,head=2:1,onset=0:1,terminus=1:2)
expect_equal(edge_event_density(half),0.5)
expect_equal(dyad_duration_density(half),0.5)
expect_equal(edge_duration_density(half),0.5)

# two edges, each half active in a network of 4
half<-network.initialize(4)
add.edges.active(half,tail=1:2,head=2:1,onset=0:1,terminus=1:2)
expect_equal(edge_event_density(half),0.5)
expect_equal(dyad_duration_density(half),2/24)
expect_equal(edge_duration_density(half),0.5)

# single edge in range defined by net.obs
obs<-as.networkDynamic(network.initialize(2))
add.edges.active(obs,tail=1,head=2,onset=1,terminus=2)
obs%n%'net.obs.period'<-list(observations=list(c(0,3)),mode="discrete", time.increment=1,time.unit="step")
expect_equal(edge_event_density(obs),1/3)
expect_equal(dyad_duration_density(obs),1/6)
expect_equal(edge_duration_density(obs),1/3)

# single edge in network with loop
loop<-network.initialize(2,loops=TRUE)
loop[1,2]<-1
expect_equal(edge_event_density(loop),1)
expect_equal(dyad_duration_density(loop),0.25) # loop increases possible dyads to 4
expect_equal(edge_duration_density(loop),1)


# test on example networks 
# have no valid baseline to compare, so just comparing to previous output
data(concurrencyComparisonNets)
expect_equal(edge_event_density(base),0.009809139,tolerance=0.0000001)
expect_equal(edge_duration_density(base),0.1996973,tolerance=0.0000001)
expect_equal(dyad_duration_density(base),0.000766006,tolerance=0.0000001)

expect_equal(edge_event_density(middle),0.00981934,tolerance=0.0000001)
expect_equal(edge_duration_density(middle),0.1921979,tolerance=0.0000001)
expect_equal(dyad_duration_density(middle),0.0007387788,tolerance=0.0000001)

expect_equal(edge_event_density(monog),0.009819324,tolerance=0.0000001)
expect_equal(edge_duration_density(monog),0.1979761,tolerance=0.000001)
expect_equal(dyad_duration_density(monog),0.0007617818,tolerance=0.0000001)



