# Function to calculate the bias in product moment correlations and non-linearities in odds ratios.

wrongCor <- function(r, rep, N, thresh1, thresh2 = NULL){

sigma     <- matrix(c(1,r,r,1), 2, 2)	
zeroR     <- matrix(NA, rep, 1)
polyR     <- matrix(NA, rep, 1)
logitEst  <- matrix(NA, rep, 1)
logitZ    <- matrix(NA, rep, 1)
or        <- matrix(NA, rep, 1)

nTh1 <- length(thresh1)
nTh2 <- length(thresh2)

for(k in 1:rep){
	
dat <- as.data.frame(mvrnorm(N, c(0,0), sigma, empirical = T )); colnames(dat) <- c("v1", "v2")
datBin <- dat

datBin$v1 <- cut(datBin$v1, breaks = c(-Inf,thresh1,Inf), labels = 0:nTh1) 
if (is.null(thresh2) ) {datBin$v2 <- datBin$v2} else {datBin$v2 <- cut(datBin$v2, breaks = c(-Inf,thresh2,Inf), labels = 0:nTh2) }
 


zeroR  [k,] <- cor(as.numeric(datBin$v1), as.numeric(datBin$v2))
if (!is.null(thresh2) )  polyR  [k,] <- polychor(as.numeric(datBin$v1), as.numeric(datBin$v2))
if ( is.null(thresh2) )  polyR  [k,] <- polyserial(as.numeric(datBin$v2), as.numeric(datBin$v1))


if	(nTh1 >1 ){
LogitModel <- polr(v1~as.numeric(v2), data=datBin, method="logistic")
logitEst[k,] <- coef(LogitModel)
or[k,]       <- exp(coef(LogitModel))
}



if	(nTh1 ==1 ){
LogitModel <- glm(v1~v2, data=datBin, family=binomial(link="logit"))
logitEst[k,] <- summary(LogitModel)$coefficients[2,1]
or[k,]       <- exp(summary(LogitModel)$coefficients[2,1])
}

}

tpoly  <-  t.test(polyR,mu=r)
tpear  <-  t.test(zeroR,mu=r)
logitZ <- mean(logitEst)/sd(logitEst)

res <- as.data.frame( cbind(
		   round(rbind( r , mean(polyR),mean(zeroR),mean(logitEst),mean(or)) , 4) ,
        as.data.frame(rbind("", round(rbind(sd(polyR), sd(zeroR), sd(logitEst), sd(or)), 4))), 
		as.data.frame(rbind(" ",round(rbind(abs(tpoly$statistic), abs(tpear$statistic), mean(logitZ)), 4), " " ) ),
		as.data.frame(rbind(" ",round(rbind(tpoly$p.value, tpear$p.value, 1-pnorm(mean(logitZ))), 4), " " ) ),
		 rbind(" ", "est diff from sim", "est diff from sim", "est diff from zero", " ")))
		
		
			   
colnames(res) <- c("Value", "Std. Dev.", "t Stat", "P value", "H0")
rownames(res) <- c("Simulated Value", "Polychoric Estimate", "Product-Moment Estimate", "Logit", "Odds Ratio")

res
}


