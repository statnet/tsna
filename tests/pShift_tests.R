library(tsna)
library(testthat)


data(McFarland_cls33_10_16_96)
output<-pShiftCount(cls33_10_16_96)

expect_equal(ncol(output),13)

# check expected values for clss 33
expect_equal(output, matrix(c(247,2,45,3,2,5,4,7,8,155,0,1,29),ncol=13, byrow=TRUE),check.attributes=FALSE)

# check alternate outputformat
output <-pShiftCount(cls33_10_16_96,output='full')
expect_equal(dim(output),c(691,18))
expect_equal(colnames(output),c("AB-BA","AB-B0","AB-BY","A0-X0","A0-XA","A0-XY","AB-X0","AB-XA","AB-XB","AB-XY","A0-AY","AB-A0","AB-AY"   , "onset","terminus", "tail","head","group"))


# test for non-directed network
expect_error(pShiftCount(as.networkDynamic(network.initialize(10,directed=FALSE))),regexp = 'only appropriate for directed networks')

# test for empty network
expect_equal(nrow(pShiftCount(as.networkDynamic(network.initialize(10)))),0)
# test for zero network
expect_equal(nrow(pShiftCount(as.networkDynamic(network.initialize(0)))),0)

# test for time range
expect_equal(pShiftCount(cls33_10_16_96,start=10,end=11), matrix(c(4,0,2,0,0,0,0,0,0,1,0,0,1),ncol=13, byrow=TRUE),check.attributes=FALSE)



# TODO: construct network that tests each shift
