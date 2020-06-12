# Best Practices for Binary or Ordinal Data Analysis.

This page is a companion website to the manuscript: 

Verhulst, B., & Neale, M.C. (Under Review) Best Practices for Binary or Ordinal Data Analysis.

In the paper, we conduct a series of simualation studies to assess the bias that accrues in estimates of Pearson product-moment correlations when they are applied to ordinal data.

All of the simulation scripts that were used in the paper are provided, including scripts to construct the figures.

The script for the liability threshold model Figure (Figure 1) can be found [here](https://github.com/bradverhulst/OrdinalData/blob/master/LTMfig.R).
The script for the first simulation study and Figure 2 can be found [here](https://github.com/bradverhulst/OrdinalData/blob/master/numOrdCat.R).
The script for the second and thirds simulation stuies and Figure 3 can be found [here](https://github.com/bradverhulst/OrdinalData/blob/master/polySerialSim.R).

--- 
# Description of a Function to Calculate the Bias in the Product-Moment correlation with ordinal data

As we cannot possibly anticipate every combination of simulated correlations, number of categories, threshold values, the included R function simulates data, then calculates Pearson product-moment, logit and maximum likelihood correlations and odds ratios.

Below is a short tutorial demonstrating how to use the function.
The first thing that we need to do is load the require packages and functions.  You can download the function [here](https://github.com/bradverhulst/OrdinalData/blob/master/polySerialSim.R), and save it in your working directory. (While sourcing the file direclty from github should be possible, it requires several additional steps so this is easier).

```ruby
# Load the required packages
require(MASS)
require(polycor)

#load the function into R
source("wrongCorFun.R")

```

Now that everything is loaded we are ready to play with the function. 

The *wrongCor* function has 5 arguments:

_r_ indicates the value of the simulated correlation

_rep_ indicates the number of times to repeat the analysis to get a more stable correlation based on the central limit theorem. 

_N_ indicates the sample size.  Larger samples will provide etimates that are more similar to the expected value based on the law of large numbers

_thresh1_ is the set of thresholds for the first variable.  Note that to be valid, the thresholds must be increasing along the liability dimension from -Inf to Inf.

_thresh2_ is the set of thresholds for the second variable, with the same requirements. If thresh2 is missing, the function will assume that the second variable is continuous and conduct at point-biserial correlation. Otherwise, a polychoric correlation will be presented.

Let's start with an simple example where there are 6 categories thresholds at -1, -.5, 0, .5, and 1 (for both variables).  While the thresholds are evenly spaced, they don't  reflect any intentional proportions of the population, and we could have easily chosen different values. The results from this analysis are pasted below.

```ruby
wrongCor(r = .7, rep = 10, N = 1000, thresh1 = c(-1, -.5, 0, 0.5, 1) , thresh2 = c(-1, -.5, 0, 0.5, 1))

#                         Value Std. Dev.  t Stat P value                 H0
# Simulated Value         0.7000                                             
# Polychoric Estimate     0.6979    0.0089  0.7562  0.4688  est diff from sim
# Product-Moment Estimate 0.6580    0.0092 14.3901       0  est diff from sim
# Logit                   0.9215     0.025 36.9221       0 est diff from zero
# Odds Ratio              2.5137    0.0629                                   

```
The function returns a table of results with the simulated value, the polychoric correlation, the product-moment correlation, the logit coefficient, and the odds ratio.  These parameters were the focus of the paper.

As we can see from the analysis, the estimate of the polychoric correlation reflects the simulated value, with the p-value suggesting that there is no difference between the estimated and simulated values. By contrasts, the product-moment correlation is slightly biased towards zero, and is signifacntly lower than the simulated value. As expected, the logit coefficient and the odd ratio are quite large,  but must be interpreted in conjunction with the prevalences.


The next example is for a joint binary continuous scenario, with a correlation of .5 and a threshold of 0 (prevalence of .50).

```ruby

wrongCor(r = .5, rep = 12, N = 1000,  thresh1 = 0)

#                         Value Std. Dev.  t Stat P value                 H0
# Simulated Value         0.5000                                             
# Polychoric Estimate     0.4931    0.0108  2.2183  0.0485  est diff from sim
# Product-Moment Estimate 0.3934    0.0087 42.6586       0  est diff from sim
# Logit                   0.9321     0.029  32.153       0 est diff from zero
# Odds Ratio              2.5408    0.0732                               

```

# Testing a few different sets of thresholds

### Example with evenly, symmetrically spaced thresholds

```ruby
nTh <- 4
prev <- c((1:nTh)/(nTh+1))         #[1] 0.2 0.4 0.6 0.8
thr <- qnorm(prev)                 #[1] -0.8416212 -0.2533471  0.2533471  0.8416212

wrongCor(r = .7, rep = 10, N = 1000, thresh1 = thr , thresh2 = thr)

#                          Value Std. Dev.  t Stat P value                 H0
# Simulated Value         0.7000                                             
# Polychoric Estimate     0.7044     0.009  1.5315    0.16  est diff from sim
# Product-Moment Estimate 0.6499    0.0089  17.704       0  est diff from sim
# Logit                   1.0635    0.0263 40.5067       0 est diff from zero
# Odds Ratio              2.8974    0.0754                                   


```

### Example with skewed, asymmetrically spaced thresholds

```ruby
nTh <- 6
th <- (.5)/(nTh)
probs <- c(seq(.5, 1-th, th))     #[1] 0.5000000 0.5833333 0.6666667 0.7500000 0.8333333 0.9166667
thr <- qnorm(probs)               #[1] 0.0000000 0.2104284 0.4307273 0.6744898 0.9674216 1.3829941


wrongCor(r = .7, rep = 10, N = 1000, thresh1 = thr , thresh2 = thr)

#                          Value Std. Dev.  t Stat P value                 H0
# Simulated Value         0.7000                                             
# Polychoric Estimate     0.6992    0.0149  0.1607  0.8758  est diff from sim
# Product-Moment Estimate 0.6153    0.0218 12.3057       0  est diff from sim
# Logit                   0.6209    0.0324 19.1894       0 est diff from zero
# Odds Ratio              1.8615    0.0606                                   

```

As can be seen across all of the examples, the polychoric correlation is consistent with the simulated value, and the product-moment correlation is consistently biased towards zero.

While these functions were not used to conduct the simulations in the manuscript, by putting them in loops with specified values, it will be possible to obtain comparable estimates to those that we present.

---

