---
title: "Week4 HW"
author: "Rie"
date: "4/25/2019"
output: 
  html_document: 
    keep_md: yes
---



###4E1 In the model definition below, which line is the likelyhood?
y1

###4E2 In the model definition just above, how many parameters are in the post distribution?
2, mu and sigma

###4E3 Using the model definition above, write down the appropriate form of Bayes' theorem include the proper likelihood and priors.


###4E4 In the model definition below, which line is the linear model?
mu = alpha + beta*xi

###4E5 In the model definition above, how many parameters are in the posterior distribution?
3, alpha and beta, sigma



###4M1 For the model definition below, simulate observed heights from the prior (not the posterior)


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
data("Howell1")
d <- Howell1
d2 <- d[d$age>=18,]
head(d2)
```

```
##    height   weight age male
## 1 151.765 47.82561  63    1
## 2 139.700 36.48581  63    0
## 3 136.525 31.86484  65    0
## 4 156.845 53.04191  41    1
## 5 145.415 41.27687  51    0
## 6 163.830 62.99259  35    1
```

```r
sample_mu <- rnorm(1e4,178, 20) #1000 samples, average178, sd 20
head(sample_mu)
```

```
## [1] 208.6032 186.2421 137.9937 131.2181 187.7106 162.8900
```

```r
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e1, sample_mu, sample_sigma)
dens(prior_h)
```

![](Week4.HW.042519_files/figure-html/HW4M1-1.png)<!-- -->
###4M2 Translate the model just above into a quap formula

```r
flist <- alist(
  height <- dnorm(mu , sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0,50)
)
m4.1 <- quap(flist , data=d2)
precis(m4.1)
```

```
##             mean        sd       5.5%      94.5%
## mu    154.607023 0.4119947 153.948576 155.265470
## sigma   7.731333 0.2913860   7.265642   8.197024
```

###4M3 Translate the quap model formula below into a mathematical model definition

yi ~ Normal(mu, sigma) 
mu-i = alph + beta*x
alpha ~ Normal(0, 50)
beta ~uniform (0, 10)
sigma ~ uniform(-0, 50)

