# test the forward and backward componetnt functions
require(tsna)
require(testthat)
require(networkDynamicData)
linegraph<-network.initialize(10)
add.edges(linegraph,tail=1:9,head=2:10)

# ---- forward.reachable tests -----
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
expect_equal(forward.reachable(test,v=5,start=4,end=6),c(5,6,7))
expect_equal(forward.reachable(test,v=1,end=6,per.step.depth=1),c(1,2,3,4,5,6,7))


# test with finite depthtest<-linegraph
test<-linegraph
activate.edges(test,onset=0,terminus=10)
expect_equal(forward.reachable(test,v=1,per.step.depth=2,end=3),1:7)
expect_equal(forward.reachable(test,v=1,per.step.depth=1.5,end=3),1:5)

test_that("test on network with two components",{
  test<-network.initialize(10)
  activate.vertices(test)
  test[1:5,5:1]<-1
  test[6:10,10:6]<-1
  expect_equal(forward.reachable(test,v=1),1:5)
  expect_equal(forward.reachable(test,v=6),6:10)
})


test_that("network with at spell durations",{
  test<-linegraph
  activate.edges(test,onset=0:10,terminus=0:10)
  expect_equal(forward.reachable(test,v=5,per.step.depth=Inf),5:10)
  expect_equal(forward.reachable(test,v=10,per.step.depth=Inf),10)
  
})

line<-network.initialize(4)
add.edges.active(line,tail=1:3,head=2:4,onset=c(1,0,2),terminus=c(Inf,1,2))
forward.reachable(line,v=1)
paths.fwd.earliest(line,v=1)

# test on network with net.obs.period set

# test on network with way too many time steps
data(hospital_contact)
#forward.reachable(hospital,v=1,start=120, end=347640,interval=300,per.step.depth=1)


# test that it matches infction in example network

data(concurrencyComparisonNets)
which(get.vertex.attribute.active(base,'status',at=1)==1)
# size of forward component in base from v 24
sum(get.vertex.attribute.active(base,'status',at=102)==1)

epiFound<-which(get.vertex.attribute.active(base,'status',at=102)==1)
fwdFound<-forward.reachable(base,v=24,per.step.depth=1,end=100)
expect_equal(length(setdiff(fwdFound,epiFound)),0)

# --- profiling -----#
# fiveRuns1<-function(){
#   forward.reachable1(base,v=1,per.step.depth=1)
#   forward.reachable1(base,v=2,per.step.depth=1)
#   forward.reachable1(base,v=3,per.step.depth=1)
#   forward.reachable1(base,v=4,per.step.depth=1)
#   forward.reachable1(base,v=5,per.step.depth=1)
# }
# fiveRuns2<-function(){
#   forward.reachable2(base,v=1,per.step.depth=1)
#   forward.reachable2(base,v=2,per.step.depth=1)
#   forward.reachable2(base,v=3,per.step.depth=1)
#   forward.reachable2(base,v=4,per.step.depth=1)
#   forward.reachable2(base,v=5,per.step.depth=1)
# }
# 
# # memory profiling
# Rprof(filename='fwdReachable.before')
# fiveRuns2()
# Rprof(NULL)
# summaryRprof(filename='fwdReachable.before')

# # time profiling
# library(microbenchmark)
# timing<-microbenchmark(fiveRuns1(),fiveRuns2(),times=1)


# ----- tests for paths.fwd.earliest ----

test_that('paths.fwd.earliest basic tests',{
  line<-network.initialize(4)
  add.edges.active(line,tail=1:3,head=2:4,onset=0:2,terminus=1:3)
  # check return format
  expect_equal(names(paths.fwd.earliest(line,v=1)),c('distance','previous'))
  
  # check basic line
  expect_equal(paths.fwd.earliest(line,v=1)$distance,c(0, 0, 1, 2))
  expect_equal(paths.fwd.earliest(line,v=2)$distance,c(Inf,0,1,2))
  
  # test starting and ending flags
  expect_equal(paths.fwd.earliest(line,v=1,start=0.5)$distance, c(0,0,0.5,1.5))
  expect_equal(paths.fwd.earliest(line,v=1,start=2)$distance, c(0,Inf,Inf,Inf))
  expect_equal(paths.fwd.earliest(line,v=1,end=2)$distance, c(0,0,1,Inf))
  
  line<-network.initialize(4)
  add.edges.active(line,tail=1:3,head=2:4,onset=c(2,1,3),terminus=c(3,2,4))
  expect_equal(paths.fwd.earliest(line,v=1)$distance,c(0,1,Inf,Inf))
  
  # test active default
  test<-as.networkDynamic(network.initialize(4))
  add.edges(test,1:3,2:4)
  expect_equal(paths.fwd.earliest(test,v=1,start=0)$distance,c(0,0,0,0))
  expect_equal(paths.fwd.earliest(test,v=1,active.default=FALSE,start=0)$distance,c(0,Inf,Inf,Inf))
  
  test<-network.initialize(4)
  add.edges(test,1:3,3:4)
  activate.edges(test,e=1,at=2)
  
  # test start message
  test<-as.networkDynamic(network.initialize(4))
  expect_message(paths.fwd.earliest(test,v=1),regexp="'start' time parameter for paths was not specified")
  
  # test wrong object
  expect_error(paths.fwd.earliest(network.initialize(3)),regexp='first argument must be a networkDynamic object')
  
  # test no v specified
  expect_error(paths.fwd.earliest(as.networkDynamic(network.initialize(2)),regexp='argument with valid vertex ids was not given'))
  
  # test on network size 0
  expect_equal(paths.fwd.earliest(as.networkDynamic(network.initialize(0)),start=0,v=numeric(0))$distance,numeric(0))
})

test_that("path in large base network matches",{
  fwdDFS<-paths.fwd.earliest(base,v=24)
  expect_equal(sum(fwdDFS$distance<Inf),772) # should find 772 vertices, because that is what we found with BFS search
})

data(moodyContactSim)


# tests with moody's example network
test_that("test of moody's example network",{
 
  paths<-paths.fwd.earliest(moodyContactSim,v=10)
  
  expect_equal(paths$distance,c(543, 454, 594,   0, 672, 661, 184, 679, 634,   0, 709, 581, 413, 625, 669, 535))
  expect_equal(paths$previous,c(16,13,13,10,13,16,10,13,1,0,8,1,4,4,2,2))
  
  # render a pdf for visual inspection of correctness
  # tree<-create_tree(paths)
  # pdf(file="MoodyTestNetTree.pdf",width=10,height=5)
  # par(mfcol=c(1,2))
  # plot(moodyContactSim,displaylabels=TRUE,
  #      edge.label=lapply(get.edge.activity(moodyContactSim),
  #                         function(spl){
  #                           paste("(",spl[,1],"-",spl[,2],")",sep=
  #                                   ''
  #                           )
  #                         }),
  #      edge.label.col='blue',
  #      edge.label.cex=0.6,
  #      main="moody example net")
  # 
  # plot(tree,
  #      coord=layout.normalize(network.layout.animate.Graphviz(tree,layout.par=list(gv.engine='dot')),keep.aspect.ratio=FALSE),
  #      displaylabels=TRUE,
  #      jitter=FALSE,
  #      label.pos=2,
  #      main='earliest paths from v10',
  #      edge.label=lapply(get.edge.activity(tree),
  #                        function(spl){
  #                          paste("(",spl[,1],")",sep=
  #                                  ''
  #                          )
  #                        }),
  #      edge.label.col='blue',
  #      edge.label.cex=0.6)
  # par(mfcol=c(1,1))
  # dev.off()
})



test_that("test on network with two components",{
  test<-network.initialize(10)
  activate.vertices(test)
  test[1:5,5:1]<-1
  test[6:10,10:6]<-1
  expect_equal(which(paths.fwd.earliest(test,v=1)$distance!=Inf),1:5)
  expect_equal(which(paths.fwd.earliest(test,v=6)$distance!=Inf),6:10)
})



# test path distance
test_that("graph step time param works",{
  test<-network.initialize(4)
  add.edges.active(line,tail=1:3,head=2:4,onset=0:2,terminus=1:3)
  # count each geodesic step as 1
  expect_equal(paths.fwd.earliest(line,v=1,graph.step.time=1)$distance,c(0, 1, 2, 3))
  # count each geodesic step as 2
  expect_equal(paths.fwd.earliest(line,v=1,graph.step.time=2)$distance,c(0, 2, Inf, Inf))
  
})


# test for a later-leaving path arriving earlier
paths.fwd.earliest(test,v=1,start=0)

# ----- tests for paths.bkwd.latest -----



# reverse-ordered edge spells
test<-network.initialize(10)
add.edges(test,tail=1:9,head=2:10)
activate.edges(test,onset=10:0,terminus=11:1)
results<-paths.bkwd.latest(test,v=5)
expect_equal(results$distance,c(Inf, Inf, Inf,   3,   0, Inf, Inf, Inf, Inf, Inf))
expect_equal(results$previous,c(0, 0, 0, 5, 0, 0, 0, 0, 0, 0))

# forward-ordred edge spells
test<-network.initialize(10)
add.edges(test,tail=1:9,head=2:10)
activate.edges(test,onset=0:10,terminus=1:11)
results<-paths.bkwd.latest(test,v=10)
expect_equal(results$distance,c(8,7,6,5,4,3,2,1,0,0))
expect_equal(results$previous,c(2,3,4,5,6,7,8,9,10,0))

# moody sim
results<-paths.bkwd.latest(moodyContactSim,v=10)
expect_equal(results$distance,c(Inf, Inf, Inf, 723, Inf, Inf, 539, Inf, Inf,   0, Inf, Inf, Inf, Inf, Inf, Inf))
expect_equal(results$previous,c(0,  0,  0, 10,  0,  0, 10,  0,  0,  0,  0,  0,  0,  0,  0,  0))

results<-paths.bkwd.latest(moodyContactSim,v=16)
expect_equal(results$distance,c(180, 196, Inf,  13, Inf,  62, Inf, Inf, Inf, 723, 548, Inf, 271, 103, Inf,   0))
expect_equal(results$previous,c(16, 16,  0, 16,  0, 16,  0,  0,  0,  4,  1,  0,  2,  4,  0,  0))


# --------- tests for paths.fwd.latest ---------
# two paths, does it 
test<-network.initialize(2)
add.edges.active(test,tail=1,head=2,onset=0,terminus=1)
activate.edges(test,onset=2,terminus=3)
paths.fwd.earliest(test,v=2,start=0,end=3)
paths.fwd.latest(test,v=2,start=0,end=3)


# create a network in which the latest-starting path and
# the latest ending path are not the same
test<-network.initialize(5,direct=FALSE)
add.edges(test,tail=c(1,1,2,4),head=c(3,2,4,3))
activate.edges(test,at=c(1,2,3,4))
plot(test,displaylabels=TRUE,edge.label=get.edge.activity(test))
# latest starting path v1 to v4 should be at time 2 (via edge 2)
# latest ending path v1 to v4 should be at time 4 (via edge 3)
paths.fwd.latest(test,v=1,start=0,end=4)


# create a network in which the latest-starting path and
# the latest ending path are not the same
test<-network.initialize(5,direct=FALSE)
add.edges(test,tail=c(1,1,2,4),head=c(3,2,4,3))
activate.edges(test,at=c(1,2,3,4))
plot(test,displaylabels=TRUE,edge.label=get.edge.activity(test))
paths.fwd.earliest(test,v=1,start=0)
paths.fwd.latest(test,v=1,start=0)

# create a network in which an early-leaving path arrives latest
# the latest path from v1 to v3 should arrive at t4 via v4
test<-network.initialize(4,directed=FALSE)
add.edges(test,tail=c(1,1,2,4),head=c(3,2,4,3))
activate.edges(test,at=c(2,1,3,4))
plot(test,displaylabels=TRUE,edge.label=get.edge.activity(test))



# the network below illustrates the various possible paths
# tests ability to distinguish paths
# howver, it is not a great complex test case since there are no
# indirect paths 
paths5<-network.initialize(7)
network.vertex.names(paths5)<-LETTERS[1:7]
add.edges.active(paths5,tail=c(1,2),head=c(2,7),onset=c(1,4),terminus=c(2,5))
add.edges.active(paths5,tail=c(1,3),head=c(3,7),onset=c(0,6),terminus=c(2,7))
add.edges.active(paths5,tail=c(1,4),head=c(4,7),onset=c(4,5),terminus=c(5,6))
add.edges.active(paths5,tail=c(1,5),head=c(5,7),onset=c(6,9),terminus=c(7,10))
add.edges.active(paths5,tail=c(1,6),head=c(6,7),onset=c(4,10),terminus=c(5,11))
plot(paths5, mode='circle',displaylabels=TRUE,edge.label=get.edge.activity(paths5),edge.label.col='blue',edge.label.cex=0.6)
as.data.frame(paths5)
# FORWARDS
# earliest leaving ACG @ 6
# earliest arriving ABG @ 4
res2<-paths.fwd.earliest(paths5,v=1)
expect_equal(res2$distance[7],4)
# latest leaving AEG @ 10


# quickest ADG @ 5
# latest ariving  AFG @ 11

#BACKWARDS
paths.bkwd.latest(paths5,v=7)
             