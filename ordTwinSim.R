########
# Load the necessary packages
library(MASS)

# Set a value for the sample sizes for MZ and DZ twins.
nmz=1000000
ndz=1000000

# Specify the A, C and E variance components.

a2=.5; c2=.2; e2=.3

# Generate the expected covariance matrices
smz=matrix(c(1,a2+c2, a2+c2,1),2,2)
sdz=matrix(c(1,.5*a2+c2, .5*a2+c2,1),2,2)

# Use the expected covariance matrices to simulate data for the MZ and DZ twins
cmz=mvrnorm(nmz,mu=rep(0,2),Sigma=smz, emp=T)
cdz=mvrnorm(ndz,mu=rep(0,2),Sigma=sdz, emp=T)

# Check to ensure the MZ and DZ correlations are correct
rmz1=cor(cmz)[2,1]
rdz1=cor(cdz)[2,1]

# Use the Falconer formulas to calculate expected proportions of variance.
a2_=2*(rmz1-rdz1)    # Q&D
c2_ =2*rdz1-rmz1
e2_= 1 - a2_ - c2_

## Set up the parameters for the simulation study
nvalth=50
prev <- seq(.01,.5,by=.01)
nth=qnorm(prev)
res=matrix(0,nvalth,6)
colnames(res) <- c("Th", "A", "C", "E", "rMZ", "rDZ")
nrep <- 1000
resP=matrix(0,nrep,6)


ii=0

for (ith in nth) {

ii=ii+1
print(ii)
print(Sys.time())
for (rep in 1:nrep){
	
if (rep/nrep == .25)	print("1/4")
	if (rep/nrep == .50)	print("1/2")
		if (rep/nrep == .75)	print("3/4")
	
	cmz=mvrnorm(nmz,mu=rep(0,2),Sigma=smz, emp=T)
	cdz=mvrnorm(ndz,mu=rep(0,2),Sigma=sdz, emp=T)


	dmz=matrix(0,nmz,2)
	ddz=matrix(0,ndz,2)

	dmz[cmz[,1]>ith,1]=1
	ddz[cdz[,1]>ith,1]=1
	dmz[cmz[,2]>ith,2]=1
	ddz[cdz[,2]>ith,2]=1

	rmz=cor(dmz)[2,1]
	rdz=cor(ddz)[2,1]
	a2_est=2*(rmz-rdz)
	c2_est=2*rdz-rmz
	e2_est=1 - a2_est - c2_est

	resP[rep,1:6]=c(ith, a2_est,c2_est,e2_est,rmz,rdz)

}


res[ii,1:6] <- apply(resP, 2, mean)
	
}

res <- as.data.frame(cbind(res, prev))

# Graph the results

par(mfrow = c(1,2))

plot(res$prev,res$A, type='l', ylim=c(-.1,1), col= "red", ylab="Proportion of Variance", xlab="Prevalence", main = "a) Variance Components")
abline(h=a2_, lwd=1, lty = 2, col="red")
abline(h=c2_, lwd=1, lty = 2, col="darkgreen")
abline(h=e2_, lwd=1, lty = 2, col="blue")

lines(res$prev,res$A , type='l', lwd = 3, col="red")
lines(res$prev,res$C , type='l', lwd = 3, col="darkgreen")
lines(res$prev,res$E , type='l', lwd = 3, col="blue")

legend("topright", fill = c("red", "darkgreen", "blue"), 
       legend = c("Additive Genetic", "Common Environment", 
	              "Unique Environment"), title = "Variance Components",
				  cex = .7, box.col = NA, border = "white")

plot(res$prev,res$rMZ, type='l', lwd = 3, ,ylim=c(-.1,1), col= "purple", ylab="Correlation (or Difference)", xlab="Prevalence", main = "b) MZ & DZ Correlations \n (and the difference)" )
lines(res$prev,res$rDZ , type='l', lwd = 3, col="orange")
lines(res$prev,res$rMZ - res$rDZ , type='l', lwd = 3, col= "black")

abline(h=rmz1, lwd=1, lty = 2, col="purple")
abline(h=rdz1, lwd=1, lty = 2, col="orange")

legend("topright", fill = c("purple", "orange", "black"), 
       legend = c("MZ Correlation", "DZ Correlation", 
	              "Difference"), 
				  cex = .7, box.col = NA, border = "white")

