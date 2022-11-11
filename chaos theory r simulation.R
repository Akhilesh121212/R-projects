rd <- seq(2.5,4,0.01)
plot(c(2.5,4),c(0,1),type = "n",pch=".",xlab="r",ylab="population")
for(r in rd){x <- 0.1
for(i in 1:100){x <- r*x*(1-x)}
for(i in 1:400){x <- r*x*(1-x)
points(r,x,pch="*")}
}