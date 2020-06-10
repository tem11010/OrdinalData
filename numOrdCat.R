# The is the script to conduct simulation study 1 and figure 2 in Verhulst & Neale "Best Practices for Binary or Ordinal Data Analysis"
# The code was written by Brad Verhulst on April 15th, 2020 and has been modestly edited since.

# Note: When this code was originally written, I didn't think that anyone would ever see it, and I figured 
# that I would run it once. As such, rather write a more elegant set of functions and loops, I cobbled together
# a rambling set of copy and pasted code with minor differences (which is why the code is approximately 800
# lines). The silver lining in this, is that it is very easy to figure out what was done.

# Also, for publication, we set the number of repitions to 1000.  This means that it takes a long time to run.
# Thus, if you are looking to poke around with the simulation, you will probably want to reduce the reps.
# Your choice, but caveat emptor!


## Simulation to demonstrate the bias in Pearson product-moment correlations witth binary Data 
# There are two primary conditions in this figure.
# 1) Using a product moment correlation vs using a polychoric correlation
# 2) Using symmetrically distributed ordinal data vs using asymmetrically distributed ordinal data


##############################################################################################################
##############################################################################################################
##############################################################################################################
# Require the necessary R packages

require(MASS)
require(polycor)
require(vioplot)

##############################################################################################################
##############################################################################################################
##############################################################################################################

# Set up the basic parameters for the simulation
# The values below are the values that we used for the paper.
# These values can easily be changed without any loss of interpretability.

rep   <- 1000                         # Number of times each correlation was fit
r     <- .7                           # Magnitude of the simulated correlation
sigma <- matrix(c(1,r,r,1), 2, 2)     # Basic correlation matrix (probably shouldn't change this)


############################################################################################
############################################################################################
############################################################################################

# In the code that follows we run essentially the same analysis sequentially increasing the number
# of categories for the ordinal variable from 2 (1 threshold) to 15 (14 thresholds). 


## 2 Categories
# I am commenting the first section, but as all other sections are very similar, the comments will become 
# redundant quickly

# Symmetrical Prevalence

nTh <- 1                              # Number of thresholds (2 categories requires 1 threshold)
prev <- c((1:nTh)/(nTh+1))            # The makes sure that each category is equal

# These empty matrices are where we are going to put the results
zeroR.n1000.c2 <- matrix(NA, rep, 1)   
polyR.n1000.c2 <- matrix(NA, rep, 1)

# This is the beginning of the loop to generate and evaluate the data
for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T ))         # Simulate the continuous data
colnames(dat) <- c("v1", "v2")                                             # Name the variables
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)                       # Specify the exact values for the thresholds

datBin <- dat    
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)    # Chop the continuous data into ordinal data
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)    # 
                                                                            
zeroR.n1000.c2  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))      # estimate the product-moment correlation
polyR.n1000.c2  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2)) # estimate the polychoric correlation
} # Close the loop

# Asymmetrical Prevalence
# This is generally the same process as the Symmetrical prevalence, but the prevalence is asymetrical
# For the 1 threshold case, we arbitrarily set it to a .75/.25 split. For all subsequent asymetrical classifications
# we set the first threshold at 50% and spread the other thresholds so the categories capture and equal proportion of 
# of the sample using this command: prevs <- c(-Inf, seq(.5, 1, th)); prevs[prevs==1] <- Inf

nTh <- 1
prev <- .75     

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c2 <- matrix(NA, rep, 1)
SpolyR.n1000.c2 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,prev,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,prev,Inf), labels = 0:nTh)

SzeroR.n1000.c2  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c2  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

############################################################################################
############################################################################################
############################################################################################

## 3 Categories

# Symmetrical Prevalence

nTh <- 2
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
prevs <- c(-Inf, seq(.5, 1, th)); prevs[prevs==1] <- Inf


zeroR.n1000.c3 <- matrix(NA, rep, 1)
polyR.n1000.c3 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c3  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c3  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c3 <- matrix(NA, rep, 1)
SpolyR.n1000.c3 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c3  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c3  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}



############################################################################################
############################################################################################
############################################################################################

## 3 Categories

# Symmetrical Prevalence

nTh <- 2
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c3 <- matrix(NA, rep, 1)
polyR.n1000.c3 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c3  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c3  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c3 <- matrix(NA, rep, 1)
SpolyR.n1000.c3 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c3  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c3  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 4 Categories

# Symmetrical Prevalence

nTh <- 3
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c4 <- matrix(NA, rep, 1)
polyR.n1000.c4 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c4  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c4  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c4 <- matrix(NA, rep, 1)
SpolyR.n1000.c4 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c4  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c4  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 5 Categories

# Symmetrical Prevalence

nTh <- 4
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c5 <- matrix(NA, rep, 1)
polyR.n1000.c5 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c5  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c5  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c5 <- matrix(NA, rep, 1)
SpolyR.n1000.c5 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c5  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c5  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 6 Categories

# Symmetrical Prevalence

nTh <- 5
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c6 <- matrix(NA, rep, 1)
polyR.n1000.c6 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c6  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c6  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c6 <- matrix(NA, rep, 1)
SpolyR.n1000.c6 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c6  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c6  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 7 Categories

# Symmetrical Prevalence

nTh <- 6
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c7 <- matrix(NA, rep, 1)
polyR.n1000.c7 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c7  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c7  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c7 <- matrix(NA, rep, 1)
SpolyR.n1000.c7 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c7  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c7  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

############################################################################################
############################################################################################
############################################################################################

## 8 Categories

# Symmetrical Prevalence

nTh <- 7
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c8 <- matrix(NA, rep, 1)
polyR.n1000.c8 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c8  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c8  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c8 <- matrix(NA, rep, 1)
SpolyR.n1000.c8 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c8  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c8  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 9 Categories

# Symmetrical Prevalence

nTh <- 8
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c9 <- matrix(NA, rep, 1)
polyR.n1000.c9 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c9  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c9  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c9 <- matrix(NA, rep, 1)
SpolyR.n1000.c9 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c9  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c9  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 10 Categories

# Symmetrical Prevalence

nTh <- 9
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c10 <- matrix(NA, rep, 1)
polyR.n1000.c10 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c10  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c10  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c10 <- matrix(NA, rep, 1)
SpolyR.n1000.c10 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c10  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c10  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 11 Categories

# Symmetrical Prevalence

nTh <- 10
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c11 <- matrix(NA, rep, 1)
polyR.n1000.c11 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c11  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c11  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c11 <- matrix(NA, rep, 1)
SpolyR.n1000.c11 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c11  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c11  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 12 Categories

# Symmetrical Prevalence

nTh <- 11
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c12 <- matrix(NA, rep, 1)
polyR.n1000.c12 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c12  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c12  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c12 <- matrix(NA, rep, 1)
SpolyR.n1000.c12 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c12  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c12  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}



############################################################################################
############################################################################################
############################################################################################

## 13 Categories

# Symmetrical Prevalence

nTh <- 12
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c13 <- matrix(NA, rep, 1)
polyR.n1000.c13 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c13  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c13  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c13 <- matrix(NA, rep, 1)
SpolyR.n1000.c13 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c13  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c13  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 14 Categories

# Symmetrical Prevalence

nTh <- 13
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c14 <- matrix(NA, rep, 1)
polyR.n1000.c14 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c14  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c14  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c14 <- matrix(NA, rep, 1)
SpolyR.n1000.c14 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c14  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c14  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################

## 15 Categories

# Symmetrical Prevalence

nTh <- 14
prev <- c((1:nTh)/(nTh+1))
quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

th <- (.5)/(nTh)
probs <- c(0, seq(.5, 1, th))
prevs <- qnorm(probs)

zeroR.n1000.c15 <- matrix(NA, rep, 1)
polyR.n1000.c15 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,quants,Inf), labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,quants,Inf), labels = 0:nTh)

zeroR.n1000.c15  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
polyR.n1000.c15  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}

# Asymmetrical Prevalence

quants<-quantile(c(dat[,1], dat[,2]),  probs = prev)

SzeroR.n1000.c15 <- matrix(NA, rep, 1)
SpolyR.n1000.c15 <- matrix(NA, rep, 1)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(1000, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat
datBin$v1 <- cut(datBin$v1, breaks = prevs, labels = 0:nTh)
datBin$v2 <- cut(datBin$v2, breaks = prevs, labels = 0:nTh)

SzeroR.n1000.c15  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
SpolyR.n1000.c15  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
}


############################################################################################
############################################################################################
############################################################################################


### Constructing Figure 1

par(xaxt = "n")

vioplot(zeroR.n1000.c2, zeroR.n1000.c3, zeroR.n1000.c4, zeroR.n1000.c5, zeroR.n1000.c6, zeroR.n1000.c7, 
	    zeroR.n1000.c8, zeroR.n1000.c9, zeroR.n1000.c10, zeroR.n1000.c11, zeroR.n1000.c12, zeroR.n1000.c13, 
		zeroR.n1000.c14, zeroR.n1000.c15, col = "blue", side = "left" , 
		plotCentre = "line", ylim = c(.4, .8), ylab = "Correlation", xlab = "Number of Categories")
				
		mtext("|", 1, line = -.2, at = 1:14)
		mtext("|", 1, line = .4, at = 1:14, col = "white")

		mtext(2:15, 1, line = .75, at = 1:14)
		
vioplot(SzeroR.n1000.c2 , SzeroR.n1000.c3, SzeroR.n1000.c4, SzeroR.n1000.c5, SzeroR.n1000.c6, SzeroR.n1000.c7, 
	    SzeroR.n1000.c8, SzeroR.n1000.c9, SzeroR.n1000.c10, SzeroR.n1000.c11, SzeroR.n1000.c12, SzeroR.n1000.c13, 
		SzeroR.n1000.c14, SzeroR.n1000.c15, col = "red", side = "right" , 
		plotCentre = "line", ylim = c(.4, .8),  add = T)

abline(h = .7, col = "red", lwd = 3)


vioplot(zeroR.n1000.c2, zeroR.n1000.c3, zeroR.n1000.c4, zeroR.n1000.c5, zeroR.n1000.c6, zeroR.n1000.c7, 
	    zeroR.n1000.c8, zeroR.n1000.c9, zeroR.n1000.c10, zeroR.n1000.c11, zeroR.n1000.c12, zeroR.n1000.c13, 
		zeroR.n1000.c14, zeroR.n1000.c15, col = "blue", side = "left" , xaxt = "n", 
		plotCentre = "line", ylim = c(.4, .8),  add = T)

vioplot(SzeroR.n1000.c2, SzeroR.n1000.c3, SzeroR.n1000.c4, SzeroR.n1000.c5, SzeroR.n1000.c6, SzeroR.n1000.c7, 
	    SzeroR.n1000.c8, SzeroR.n1000.c9, SzeroR.n1000.c10, SzeroR.n1000.c11, SzeroR.n1000.c12, SzeroR.n1000.c13, 
		SzeroR.n1000.c14, SzeroR.n1000.c15, col = "red", side = "right" , 
		plotCentre = "line", ylim = c(.4, .8),  add = T)


vioplot(polyR.n1000.c2, polyR.n1000.c3, polyR.n1000.c4,  polyR.n1000.c5,  polyR.n1000.c6,  polyR.n1000.c7, 
	    polyR.n1000.c8, polyR.n1000.c9, polyR.n1000.c10, polyR.n1000.c11, polyR.n1000.c12, polyR.n1000.c13, 
		polyR.n1000.c14,polyR.n1000.c15, col = "lightblue", side = "left" , 
		plotCentre = "line", ylim = c(.4, .8),  add = T)

vioplot( polyR.n1000.c2,  SpolyR.n1000.c3, SpolyR.n1000.c4,  SpolyR.n1000.c5,  SpolyR.n1000.c6,  SpolyR.n1000.c7, 
	    SpolyR.n1000.c8,  SpolyR.n1000.c9, SpolyR.n1000.c10, SpolyR.n1000.c11, SpolyR.n1000.c12, SpolyR.n1000.c13, 
		SpolyR.n1000.c14, SpolyR.n1000.c15, col = "palevioletred", side = "right" , 
		plotCentre = "line", ylim = c(.4, .8),  add = T)


legend("bottomright", fill = c("lightblue", "palevioletred", "blue", "red"), 
       legend = c("Polychoric Correlation - Symmetric Categories", "Polychoric Correlation - Asymmetric Categories", 
	              "Pearson Correlation - Symmetric Categories", "Pearson Correlation - Asymmetric Categories"), title = " ",
				  cex = .7, box.col = "white", border = "white")


