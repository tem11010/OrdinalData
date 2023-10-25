# The is the script to conduct simulation study 2 & 3 and figure 3 in Verhulst & Neale "Best Practices for Binary or Ordinal Data Analysis"
# The code was written by Brad Verhulst on April 15th, 2020 and has been modestly edited since.

# Note: for publication, we set the number of repitions to 1000.  This means that it takes a long time to run.
# Thus, if you are looking to poke around with the simulation, you will probably want to reduce the reps.
# Your choice, but caveat emptor!


## Simulation to demonstrate the bias in Pearson product-moment correlations with joint binary-ordinal and continuous data 
#  across a wide range of correlations.
# There are two conditions in this analysis.
# 1) The magnitude of the correlation, ranging from .95 to .05 by increments of .05
# 2) The prevalence of the binary trait, ranging from .50 to .01 in increments of .01

##############################################################################################################
##############################################################################################################
##############################################################################################################
# Require the necessary R packages

require(MASS)
require(polycor)


## Simulation for joint continuous ordinal data sections
## Figure 3

##############################################################################################################
##############################################################################################################
##############################################################################################################

# One Binary and One Continuous variable

# Set up the basic parameters for the simulation
# The values below are the values that we used for the paper.
# These values can easily be changed without any loss of interpretability.

r <- 1                            # The initial correlation (it is immediately reduced by .05 before the first simulation)
N <- 100000                       # Sample size - to ensure coverage of binary trait when the prevalence is low
Ni <- 19                          # number of increments between 1 and .05
Nj <- 50                          # number of increments between .51 and .01
rep <- 1000                       # Number of repetitions

## Matrices to store the values
zero     <- matrix(NA, Ni, Nj)
poly     <- matrix(NA, Ni, Nj)
logit    <- matrix(NA, Ni, Nj)
or       <- matrix(NA, Ni, Nj)



for (i in 1:Ni) { # Beginning of the correlation loop
r <- r - .05      # The correlation starts at 1, so this reduces it by .05 every time it moves to the next loop
print(r)          # Prints the value of the correlation (it's going to run for a lot of reps, so it's nice to have the status)

sigma <- matrix(c(1,r,r,1), 2, 2)      # Put the value of the correlation into a correlation matrix
prev1 <- .51                           # Specify the initial prevalence (it is immediately reduced by .01 before the first simulation)

for(j in 1:Nj){
prev1 <- prev1 - 0.01  # The prevalence starts at .51, so this reduces it by .01 every time it moves to the next loop
print(prev1)           # Prints the value of the prevalence (again, for tracking)

# Set up temporary matrices to store the values
zeroRep    <- matrix(NA, rep, 1)   
polyRep    <- matrix(NA, rep, 1)
logitRep   <- matrix(NA, rep, 1)
orRep      <- matrix(NA, rep, 1)


for(k in 1:rep){ # Beginning of the prevalence loop
dat <- as.data.frame(mvrnorm(N, c(0,0), sigma, empirical = T ))                    # Simulate the data
colnames(dat) <- c("v1", "v2")                                                     # Name the varibles 
datBin <- dat                                                                      # Put the data into a new object
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf, qnorm(prev1) ,Inf), labels = 0:1)     # Chop v1 into a binary variable with a specific prevalence
                                                                                   #
zeroRep[k,]  <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))                  # Product-moment analysis
polyRep[k,]  <- polyserial(as.numeric(datBin$v2), as.numeric(datBin$v1))           # Poly-serial analysis
LogitModel <- glm(v1~v2, data=datBin, family=binomial(link="logit"))               # Logit model for odds ratios
logitRep[k,] <- LogitModel$coefficients[2]                                         # Grab the logit coefficient
orRep[k,]    <- exp(LogitModel$coefficients)[2]                                    # Grab the OR

} # Close the prevalence loop

zero[i,j]  <- mean(zeroRep)   # Take the mean of the estimates across all of the runs
poly[i,j]  <- mean(polyRep)   # Take the mean of the estimates across all of the runs
logit[i,j] <- mean(logitRep)  # Take the mean of the estimates across all of the runs
or[i,j]    <- mean(orRep)     # Take the mean of the estimates across all of the runs

}
} # close correlation loop

##############################################################################################################
##############################################################################################################
##############################################################################################################

### Generate the figures for the point-biserial and odds ratios graphs.

colors <- rainbow(Ni)

par(xpd=FALSE)
par(mfcol = c(1,2))
par(mar = c(5, 4, 4, 3) + 0.1) 


# Point-biserial subfigure

plot(1:50, rev(zero[1,]), type = "l", col = colors[1], lwd = 2, ylim = c(0,.8), xaxt = "n", yaxt = "n", 
ylab = "Observed Product-Moment Correlation", xlab = "Prevalence (%)", cex.axis = .8, main = "a) Product-Moment Correlations")

	 abline(h = (0:20)/20, col = "lightgrey")
	 abline(v = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), col = "lightgrey")

for (i in 1:19){
	lines(1:50, rev(zero[i,]), type = "l", col = colors[i], lwd = 2)
}

axis(1, at = c(1, 5, 10, 15,20,25,30,35,40,45,50) , tick = T, cex.axis = .75)
axis(2, at = seq(0, 1, .05), tick = T, las = 1, cex.axis = .75)



polygon(c(.45,.51,.51,.45), c(.39,.39,.62,.62), col = "white", border = NA)

par(xpd=TRUE)

legend(53, .7,  legend =  c(.95,.9,.85,.8,.75,.7, .65,.6,.55,.5,.45,.4,.35,.3,.25,.2,.15,.1,.05), col = colors, 
      bty = "n",lwd = 3, title= "\n Simulated \n Correlation", cex = .45)


################################
# Odds Ratio subfigure

par(xpd=FALSE)
plot(1:50, rev(log10(or[1,])), type = "l", col = colors[1], lwd = 2, ylim = c(0,2.6), 
xaxt = "n", yaxt = "n", 
ylab = expression('Odds Ratio (log'[10]*" scale)"), xlab = "Prevalence (%)", cex.axis = .8, main = "b) Odds Ratios")

abline(h = log10(c(1, 2, 4, 8, 16, 32, 64, 128, 256)), col = "lightgrey")
abline(v = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), col = "lightgrey")
#	 abline(h = 1, col = "black", lwd = 3)

for (i in 1:19){
	lines(1:50, rev(log10(or[i,])), type = "l", col = colors[i], lwd = 2)
}

axis(1, at = c(1, 5, 10, 15,20,25,30,35,40,45,50) , tick = T, cex.axis = .75)
axis(2, at = log10(c(1, 2, 4, 8, 16, 32, 64, 128, 256)) , labels = c(1, 2, 4, 8, 16, 32, 64, 128, 256), tick = T, las = 1, cex.axis = .75)

