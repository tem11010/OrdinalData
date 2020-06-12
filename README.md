# Best Practices for Binary or Ordinal Data Analysis.

This page is a companion website to the manuscript: 

Verhulst, B., & Neale, M.C. (Under Review) Best Practices for Binary or Ordinal Data Analysis.

In the paper, we conduct a series of simualation studies to assess the bias that accrues in estimates of Pearson product-moment correlations when they are applied to ordinal data.

All of the simulation scripts that were used in the paper are provided, including scripts to construct the figures.

The script for the liability threshold model Figure (Figure 1) can be found [here](https://github.com/bradverhulst/OrdinalData/blob/master/LTMfig.R).
The script for the first simulation study and Figure 2 can be found [here](https://github.com/bradverhulst/OrdinalData/blob/master/numOrdCat.R).
The script for the second and thirds simulation stuies and Figure 3 can be found [here](https://github.com/bradverhulst/OrdinalData/blob/master/polySerialSim.R).


As we cannot possibly anticipate every combination of simulated correlations, number of categories, threshold values, the included R function simulates data, then calculates Pearson product-moment, logit and maximum likelihood correlations and odds ratios.

Below is a short tutorial demonstrating how to use the function.

```ruby
# Load the required packages
require(MASS)
require(polycor)

#load the function into R
source("wrongCorFun.R")


### Now we are ready to play with the function.


wrongCor(r = .7, rep = 10, N = 1000, thresh1 = c(-1, -.5, 0, 0.5, 1) , thresh2 = c(-1, -.5, 0, 0.5, 1))
wrongCor(r = .7, rep = 10, N = 1000,  thresh1 = c(-1, -.5, 0, 0.5, 1))


wrongCor(r = .7, rep = 10, N = 1000, thresh1 = 0 , thresh2 = 0)
wrongCor(r = .7, rep = 12, N = 1000,  thresh1 = 0)


# Testing a few different sets of thresholds


# Example 1
nTh <- 6
prev <- c((1:nTh)/(nTh+1))
thr <- qnorm(prev)

wrongCor(r = .7, rep = 10, N = 1000, thresh1 = thr , thresh2 = thr)


# Example 2
nTh <- 6
th <- (.5)/(nTh)
probs <- c(seq(.5, 1-th, th))
thr <- qnorm(probs)

wrongCor(r = .7, rep = 10, N = 1000, thresh1 = thr , thresh2 = thr)


# Example 3
nTh <- 2
prev <- c((1:nTh)/(nTh+1))
thr <- qnorm(prev)

wrongCor(r = .7, rep = 10, N = 1000, thresh1 = thr , thresh2 = thr)


# Example 4
nTh <- 2
th <- (.5)/(nTh)
probs <- c(seq(.5, 1-th, th))
thr <- qnorm(probs)

wrongCor(r = .7, rep = 10, N = 1000, thresh1 = thr , thresh2 = thr)




```


