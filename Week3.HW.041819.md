---
title: "Week3.HW.041819"
author: "Rie"
date: "4/18/2019"
output: 
  html_document: 
    keep_md: yes
---

```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## rstan (Version 2.18.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.88)
```



```r
p_grid <- seq(from=0, to=1, length.out = 1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data*prob_p
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
```

###3E1 How much posterior probability lies below p = 0.2?

```r
sum( samples <0.2)/1e4
```

```
## [1] 4e-04
```

###3E2 How much posterior probability lies above p = 0.8?

```r
sum( samples > 0.8)/1e4
```

```
## [1] 0.1267
```

###3E3 How much posterior probability lies between p = 0:2 and p = 0:8?

```r
sum( samples > 0.2 & samples <0.8)/1e4
```

```
## [1] 0.8729
```

###3E4 20% of the posterior probability lies below which value of p?

```r
quantile(samples, 0.2)
```

```
##       20% 
## 0.5225225
```
###3E5 20% of the posterior probability lies above which value of p?

```r
quantile(samples, c(0.2, 1))
```

```
##       20%      100% 
## 0.5225225 0.9769770
```

###3E6 Which values of p contain the narrowest interval equal to 66% of the posterior probability?


```r
HPDI(samples, prob = 0.66)
```

```
##     |0.66     0.66| 
## 0.5305305 0.7987988
```

###3E7 Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?


```r
PI(samples, prob = 0.66)
```

```
##       17%       83% 
## 0.5045045 0.7787788
```

###3M1 Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same flat prior as before.

```r
p_grid <- seq(from=0, to=1, length.out = 1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(8, size = 15, prob = p_grid)
posterior <- prob_data*prob_p
posterior <- posterior / sum(posterior)
plot(posterior)
```

![](Week3.HW.041819_files/figure-html/Rcode.HW3M1-1.png)<!-- -->
###3M2 Draw 10,000 samples from the grid approximation from above. Then use the samples to calculate the 90% HPDI for p.


```r
samples <- sample(p_grid, prob= posterior, size = 1e4, replace = T)
HPDI(samples, prob=0.9)
```

```
##      |0.9      0.9| 
## 0.3453453 0.7317317
```
###3M3 Construct a posterior predictive check for this model and data. This means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing


```r
w <- rbinom(1e4, size = 15, prob = samples)
simplehist(w, xlab = "water count")
```

![](Week3.HW.041819_files/figure-html/Rcode.HW3M3-1.png)<!-- -->
###3M4 Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.


```r
prob_data2 <- dbinom(6, size = 9, prob = posterior)
posterior2 <- prob_data2*posterior
posterior2 <- posterior2 / sum(posterior2)
plot(posterior2)
```

![](Week3.HW.041819_files/figure-html/Rcode.HW3M4-1.png)<!-- -->

###3M5 Start over at 3M1, but now use a prior that is zero below p = 0.5 and a constant above p = 0.5.This corresponds to prior information that a majority of the Earth’s surface is water. Repeat each problem above and compare the inferences. What difference does the better prior make? If it helps,compare inferences (using both priors) to the true value p = 0.7.


```r
p_grid <- seq(from=0, to=1, length.out = 1000)
prob_p <- rep(0,1000)
prob_data <- dbinom(8, size = 15, prob = p_grid)
posterior <- prob_data*prob_p
posterior <- posterior / sum(posterior)


#samples <- sample(p_grid, prob= posterior, size = 1e4, replace = T)
HPDI(samples, prob=0.5)
```

```
##      |0.5      0.5| 
## 0.4584585 0.6216216
```

```r
w <- rbinom(1e4, size = 15, prob = samples)
simplehist(w, xlab = "water count")
```

![](Week3.HW.041819_files/figure-html/Rcode.HW3M5-1-1.png)<!-- -->

```r
prob_data2 <- dbinom(6, size = 9, prob = posterior)
posterior2 <- prob_data2*posterior
posterior2 <- posterior2 / sum(posterior2)
```

```r
p_grid <- seq(from=0, to=1, length.out = 1000)
prob_p <- rep(0,1000)
prob_data <- dbinom(8, size = 15, prob = p_grid)
posterior <- prob_data*prob_p
posterior <- posterior / sum(posterior)


samples <- sample(p_grid, size = 1e4, replace = T)
HPDI(samples, prob=0.7)
```

```
##       |0.7       0.7| 
## 0.07807808 0.76976977
```

```r
w <- rbinom(1e4, size = 15, prob = samples)
simplehist(w, xlab = "water count")
```

![](Week3.HW.041819_files/figure-html/Rcode.HW3M5-2-1.png)<!-- -->

```r
prob_data2 <- dbinom(6, size = 9, prob = posterior)
posterior2 <- prob_data2*posterior
posterior2 <- posterior2 / sum(posterior2)
```
