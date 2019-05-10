---
title: "Week4.042319"
author: "Rie"
date: "4/22/2019"
output: 
  html_document: 
    keep_md: yes
---


```r
knitr::opts_chunk$set(echo = TRUE)
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
pos <- replicate(1000, sum(runif(16, -1,1)))
plot(density(pos))
```

![](Week4.042319_files/figure-html/Rcode4.1-1.png)<!-- -->




```r
prod(1 + runif(12, 0, 0.1))
```

```
## [1] 2.152662
```

```r
growth <- replicate(10000, prod(1+runif(12, 0, 0.1)))
dens(growth, norm.comp = T)
```

![](Week4.042319_files/figure-html/Rcode4.3-1.png)<!-- -->

```r
big <- replicate(10000, prod(1+runif(12,0,0.5)))
small <- replicate(10000, prod(1+runif(12,0,0.01)))
dens(big, norm.comp = T)
```

![](Week4.042319_files/figure-html/Rcode4.4-1.png)<!-- -->

```r
dens(small, norm.comp = T)
```

![](Week4.042319_files/figure-html/Rcode4.4-2.png)<!-- -->

```r
log.big <- replicate(10000,log(prod(1+runif(12,0,0.5))))
dens(log.big, norm.comp = T)
```

![](Week4.042319_files/figure-html/Rcode4.5-1.png)<!-- -->

```r
curve( exp(-x^2), from = -3, to = 3)
```

![](Week4.042319_files/figure-html/Rcode4 extra-1.png)<!-- -->

```r
dnorm(0, 0, 0.1)
```

```
## [1] 3.989423
```


```r
w <- 6 ; n <- 9;
p_grid <- seq(from=0, to=1, length.out = 100)
posterior <- dbinom(w, n, p_grid)*dunif(p_grid, 0, 1)
posterior <- posterior/sum(posterior)
dens(posterior)
```

![](Week4.042319_files/figure-html/Rcode4.6-1.png)<!-- -->

```r
data("Howell1")
d <- Howell1
head(d)
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
precis(d)
```

```
##               mean         sd      5.5%     94.5%     histogram
## height 138.2635963 27.6024476 81.108550 165.73500 ▁▁▁▁▁▁▁▂▁▇▇▅▁
## weight  35.6106176 14.7191782  9.360721  54.50289 ▁▂▃▂▂▂▂▅▇▇▃▂▁
## age     29.3443934 20.7468882  1.000000  66.13500     ▇▅▅▃▅▂▂▁▁
## male     0.4724265  0.4996986  0.000000   1.00000    ▇▁▁▁▁▁▁▁▁▇
```

```r
d2 <- d[d$age >= 18,]
dens(d2$height)
```

![](Week4.042319_files/figure-html/Rcode4.11-1.png)<!-- -->

```r
curve(dnorm(x, 178, 20), from = 100, to = 250)
```

![](Week4.042319_files/figure-html/Rcode4.12-1.png)<!-- -->


```r
curve(dunif(x, 0, 50), from = -10, to = 60)
```

![](Week4.042319_files/figure-html/Rcode4.13-1.png)<!-- -->


```r
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)
```

![](Week4.042319_files/figure-html/Rcode4.14-1.png)<!-- -->


```r
sample_mu <- rnorm(1e4, 178, 100)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)
```

![](Week4.042319_files/figure-html/Rcode4.15-1.png)<!-- -->

```r
mu.list <- seq(from=140, to=160, length.out=200)
sigma.list <- seq(from=4, to=9, length.out = 200)
post <- expand.grid(mu=mu.list, sigma=sigma.list)
post$LL <- sapply(1:nrow(post), function(i) sum( dnorm(
                  d2$height, 
                  mean = post$mu[i],
                  sd = post$sigma[i],
                  log = TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))
head(post)
```

```
##         mu sigma        LL      prod prob
## 1 140.0000     4 -3812.776 -3822.407    0
## 2 140.1005     4 -3780.612 -3790.234    0
## 3 140.2010     4 -3748.670 -3758.283    0
## 4 140.3015     4 -3716.951 -3726.554    0
## 5 140.4020     4 -3685.454 -3695.047    0
## 6 140.5025     4 -3654.179 -3663.763    0
```


```r
contour_xyz(post$mu, post$sigma, post$prob)
```

![](Week4.042319_files/figure-html/Rcode4.17&18-1.png)<!-- -->

```r
image_xyz(post$mu, post$sigma, post$prob)
```

![](Week4.042319_files/figure-html/Rcode4.17&18-2.png)<!-- -->


```r
sample.rows <- sample(1:nrow(post), size = 1e4, replace = T ,
                      prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]
plot(sample.mu, sample.sigma, cex=0.5, pch=16, col=col.alpha(rangi2,0.1))
```

![](Week4.042319_files/figure-html/Rcode4.19&20-1.png)<!-- -->


```r
dens(sample.mu)
```

![](Week4.042319_files/figure-html/Rcode4.21-1.png)<!-- -->

```r
dens(sample.sigma)
```

![](Week4.042319_files/figure-html/Rcode4.21-2.png)<!-- -->

```r
HPDI(sample.mu)
```

```
##    |0.89    0.89| 
## 153.8693 155.1759
```

```r
HPDI(sample.sigma)
```

```
##    |0.89    0.89| 
## 7.341709 8.246231
```


```r
d3 <- sample(d2$height, size=20)
d3
```

```
##  [1] 147.320 142.113 154.305 158.750 158.115 157.480 144.145 141.605
##  [9] 145.415 156.210 160.020 153.670 163.195 158.115 142.875 163.195
## [17] 150.495 156.210 140.970 154.940
```


```r
mu.list <- seq(from=150, to=170, length.out=200)
sigma.list <- seq(from=4, to=20, length.out = 200)
post2 <- expand.grid(mu=mu.list, sigma=sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) sum( dnorm(
                  d3, 
                  mean = post2$mu[i],
                  sd = post$sigma[i],
                  log = TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) +
  dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))
sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = T,
                       prob = post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex=0.5, pch=16, col=col.alpha(rangi2,0.1),
     xlab="mu", ylab="sigma")
```

![](Week4.042319_files/figure-html/Rcode4.24-1.png)<!-- -->


```r
dens(sample2.sigma, norm.comp = T)
```

![](Week4.042319_files/figure-html/Rcode4.25-1.png)<!-- -->


```r
library(rethinking)
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
flist <- alist(
  height ~dnorm(mu, sigma), 
  mu~ dnorm(178, 20),
  sigma ~ dunif(0,50)
)
```


```r
m4.1 <- quap(flist , data=d2)
```


```r
precis(m4.1)
```

```
##             mean        sd       5.5%      94.5%
## mu    154.607029 0.4119937 153.948583 155.265474
## sigma   7.731315 0.2913843   7.265627   8.197003
```


```r
HPDI(sample.mu)
```

```
##    |0.89    0.89| 
## 153.8693 155.1759
```

```r
HPDI(sample.sigma)
```

```
##    |0.89    0.89| 
## 7.341709 8.246231
```


```r
start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)
m4.1 <- quap(flist, data = d2, start = start)
```


```r
m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ), data = d2)
precis(m4.2)
```

```
##            mean        sd     5.5%     94.5%
## mu    177.86375 0.1002354 177.7036 178.02394
## sigma  24.51701 0.9288715  23.0325  26.00153
```


```r
vcov(m4.1)
```

```
##                 mu        sigma
## mu    0.1697396109 0.0002180307
## sigma 0.0002180307 0.0849058224
```


```r
diag(vcov(m4.1))
```

```
##         mu      sigma 
## 0.16973961 0.08490582
```

```r
cov2cor(vcov(m4.1))
```

```
##                mu       sigma
## mu    1.000000000 0.001816174
## sigma 0.001816174 1.000000000
```


```r
library(rethinking)
post <- extract.samples(m4.1, n=1e4)
head(post)
```

```
##         mu    sigma
## 1 154.6397 7.395144
## 2 154.5938 7.968838
## 3 154.3804 8.150512
## 4 154.0907 7.965406
## 5 154.0073 8.075733
## 6 154.5937 7.643338
```


```r
precis(post)
```

```
##             mean        sd       5.5%     94.5%    histogram
## mu    154.610312 0.4143448 153.944827 155.26634      ▁▁▅▇▂▁▁
## sigma   7.730807 0.2913540   7.275056   8.19928 ▁▁▁▂▅▇▇▃▁▁▁▁
```

```r
plot(post)
```

![](Week4.042319_files/figure-html/Rcode3.35-1.png)<!-- -->


```r
library(MASS)
post <- mvrnorm(n=1e4, mu=coef(m4.1), Sigma = vcov(m4.1))
head(post)
```

```
##            mu    sigma
## [1,] 155.0767 7.623520
## [2,] 154.9255 7.952491
## [3,] 155.0605 7.391281
## [4,] 154.8819 7.757065
## [5,] 154.7626 7.863887
## [6,] 154.4070 7.546795
```


```r
plot(d2$height ~d2$weight)
```

![](Week4.042319_files/figure-html/Rcode4.37-1.png)<!-- -->


```r
set.seed(2971)
N <- 100 #100 lines
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)
```


```r
plot(NULL, xlim = range(d2$weight), ylim=c(-100,400),
     xlab="weight", ylab="height")
abline(h=0, lty=2)
abline(h=272, lty=1, lwd=0.5)
mtext("b~dnorm(0,10)")
xbar <- mean(d2$weight)
for (i in 1:N) curve(a[i] +b[i]*(x -xbar),
    from=min(d2$weight), to=max(d2$weight), add = T ,
    col=col.alpha("black", 0.2))
```

![](Week4.042319_files/figure-html/Rcode4.39-1.png)<!-- -->


```r
b <- rlnorm(1e4, 0, 1)
dens(b, xlim=c(0,5), adj = 0.1)
```

![](Week4.042319_files/figure-html/Rcode4.40-1.png)<!-- -->


```r
set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) , 
xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
col=col.alpha("black",0.2) )
```

![](Week4.042319_files/figure-html/Rcode4.41-1.png)<!-- -->


```r
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >=18,]
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
#define the average weight, x-bar
xbar <- mean(d2$weight)

#fit model
m4.3 <- quap(
  alist(
    height ~dnorm(mu, sigma),
    mu <- a+b*(weight -xbar),
    a ~ dnorm(178,20),
    b ~ dlnorm(0 , 1),
    sigma ~ dunif( 0, 50)
  ),
  data = d2
)
```


```r
m4.3b <- quap(
  alist(
    height ~dnorm(mu, sigma),
    mu <- a+exp(log_b)*(weight - xbar),
    a ~ dnorm( 178, 100), 
    log_b ~ dnorm(0,1),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.3b)
```

```
##               mean         sd        5.5%        94.5%
## a     154.59726390 0.27033038 154.1652237 155.02930405
## log_b  -0.09957433 0.04626318  -0.1735118  -0.02563683
## sigma   5.07186596 0.19115311   4.7663664   5.37736554
```


```r
precis(m4.3)
```

```
##              mean         sd        5.5%       94.5%
## a     154.6013671 0.27030766 154.1693633 155.0333710
## b       0.9032807 0.04192363   0.8362787   0.9702828
## sigma   5.0718809 0.19115478   4.7663786   5.3773831
```

```r
round(vcov(m4.3), 3)
```

```
##           a     b sigma
## a     0.073 0.000 0.000
## b     0.000 0.002 0.000
## sigma 0.000 0.000 0.037
```


```r
plot(height ~ weight, data=d2, col=rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x-xbar), add = T)
```

![](Week4.042319_files/figure-html/Rcode4.46-1.png)<!-- -->


```r
post <- extract.samples(m4.3)
post[1:5,]
```

```
##          a         b    sigma
## 1 154.6673 0.9579937 5.242168
## 2 154.9493 0.9075067 4.873812
## 3 154.8834 0.9578025 5.120553
## 4 154.4967 0.8987900 5.161813
## 5 154.2916 0.8414611 4.755758
```


```r
N <- 10
dN <- d2[ 1:N , ]
mN <- quap(
alist(
height ~ dnorm( mu , sigma ) ,
mu <- a + b*( weight - mean(weight) ) ,
a ~ dnorm( 178 , 20 ) ,
b ~ dlnorm( 0 , 1 ) ,
sigma ~ dunif( 0 , 50 )
) , data=dN )
```


```r
#extract 20 samples from the posterior

post <- extract.samples(mN, n=20)

#display raw data and smaple size
plot( dN$weight,dN$height, 
      xlim=range(d2$weight) ,  ylim=range(d2$height) , 
      col=rangi2, xlab="weight", ylab="height")
mtext(concat("N =", N))

#plot the lines with transparency
for(i in 1:20)
  curve(post$a[i]+post$b[i]*(x-mean(dN$weight)), 
        col= col.alpha("black",0.3), add = T)
```

![](Week4.042319_files/figure-html/Rcode4.49-1.png)<!-- -->


```r
post <- extract.samples(m4.3)
mu_at_50 <- post$a+post$b*(50-xbar)
```


```r
dens( mu_at_50, col=rangi2, lwd = 2, xlab="mu|weight=50")
```

![](Week4.042319_files/figure-html/Rcode4.51-1.png)<!-- -->


```r
HPDI(mu_at_50, prob = 0.89)
```

```
##    |0.89    0.89| 
## 158.5710 159.6676
```

```r
mu <- link(m4.3)
str(mu)
```

```
##  num [1:1000, 1:352] 157 158 157 157 157 ...
```


```r
#define sequence of weights to compute precition for 
#these valuses will be on the the horizontal axis

weight.seq <- seq(from=25, to=70, by=1)

#Use link to compute mu
#for each sample from posterior
#and for each weight in weight.seq

mu <- link(m4.3, data = data.frame(weight=weight.seq))
str(mu)
```

```
##  num [1:1000, 1:46] 137 136 136 137 136 ...
```


```r
plot(height ~ weight, d2, type="n")

#loop over samples and plot each mu value
for(i in 1:100)
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))
```

![](Week4.042319_files/figure-html/Rcode4.55-1.png)<!-- -->

```r
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
```


```r
#plot raw data
#fading out points to make line and interval more visible
plot(height ~weight, data = d2, col= col.alpha(rangi2, 0.5))

#plot the MAP line aka the mean mu for each weight
lines(weight.seq, mu.mean)

#plot a shaded regiong for 89 % HPDI
shade(mu.HPDI, weight.seq)
```

![](Week4.042319_files/figure-html/Rcode4.57-1.png)<!-- -->


```r
post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b*(weight - xbar)
weight.seq <- seq(from=25, to=70, by=1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
```


```r
sim.height <- sim(m4.3, data = list(weight = weight.seq))
str(sim.height)
```

```
##  num [1:1000, 1:46] 142 142 126 133 134 ...
```


```r
height.PI <- apply(sim.height, 2, PI, prob=0.89)
```


```r
#plot raw data
plot(height ~ weight, d2, col=col.alpha(rangi2, 0.5))

#draw MAP line
lines(weight.seq, mu.mean)

#draw HPDI region for line
shade(mu.HPDI, weight.seq)

#drw PI region for simulated heights
shade(height.PI, weight.seq)
```

![](Week4.042319_files/figure-html/Rcode4.61-1.png)<!-- -->


```r
sim.height <- sim(m4.3, data = list(weight=weight.seq), n=1e4)
height.PI <-apply(sim.height, 2, PI, prob=0.89)

#plot raw data
plot(height ~ weight, d2, col=col.alpha(rangi2, 0.5))

#draw MAP line
lines(weight.seq, mu.mean)

#draw HPDI region for line
shade(mu.HPDI, weight.seq)

#drw PI region for simulated heights
shade(height.PI, weight.seq)
```

![](Week4.042319_files/figure-html/Rcode4.62-1.png)<!-- -->


```r
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply(weight.seq, function(weight)
  rnorm(
    n=nrow(post),
    mean = post$a+post$b*(weight-xbar),
    sd=post$sigma))
height.PI <- apply(sim.height, 2, PI, prob=0.89)
height.PI
```

```
##         [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]
## 5%  128.3579 129.2084 130.2045 131.0268 131.8857 132.8668 133.7501
## 94% 144.7252 145.6272 146.5812 147.4911 148.2288 149.4802 150.2792
##         [,8]     [,9]    [,10]    [,11]    [,12]    [,13]    [,14]
## 5%  134.5551 135.5478 136.5851 137.4367 138.2628 139.2582 140.2732
## 94% 150.9139 151.8334 152.7157 153.7198 154.5365 155.5329 156.4610
##        [,15]    [,16]    [,17]    [,18]    [,19]    [,20]    [,21]
## 5%  141.0107 141.9222 142.9010 143.8285 144.6437 145.4352 146.7091
## 94% 157.3694 158.2304 159.1991 160.1449 160.8773 161.7158 162.8095
##        [,22]    [,23]    [,24]    [,25]    [,26]    [,27]    [,28]
## 5%  147.3283 148.3555 149.2770 149.9689 150.9397 151.8408 152.8993
## 94% 163.4781 164.4951 165.7156 166.3013 167.1430 168.0712 169.1570
##        [,29]    [,30]    [,31]    [,32]    [,33]    [,34]    [,35]
## 5%  153.7961 154.7003 155.4757 156.5078 157.1192 158.0711 158.9102
## 94% 170.0083 170.9768 171.9074 172.7840 173.7286 174.5840 175.2914
##        [,36]    [,37]    [,38]    [,39]    [,40]    [,41]    [,42]
## 5%  160.0589 160.7297 161.8822 162.7758 163.6369 164.4593 165.3669
## 94% 176.3318 177.1715 178.1490 179.1221 179.9791 180.8743 181.7898
##        [,43]    [,44]    [,45]    [,46]
## 5%  166.3949 167.1145 168.0682 168.9956
## 94% 182.7304 183.7170 184.6278 185.4948
```


```r
library(rethinking)
data("Howell1")
d <- Howell1
str(d)
```

```
## 'data.frame':	544 obs. of  4 variables:
##  $ height: num  152 140 137 157 145 ...
##  $ weight: num  47.8 36.5 31.9 53 41.3 ...
##  $ age   : num  63 63 65 41 51 35 32 27 19 54 ...
##  $ male  : int  1 0 0 1 0 1 0 1 0 1 ...
```

```r
plot( height ~ weight, d, col=col.alpha(rangi2, 0.5) )
```

![](Week4.042319_files/figure-html/Rcode4.64-1.png)<!-- -->


```r
d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm( mu, sigma),
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,1) , 
    sigma ~ dunif(0,50) 
  ),
  data = d)
```



```r
precis(m4.5)
```

```
##             mean        sd       5.5%      94.5%
## a     146.057415 0.3689755 145.467721 146.647109
## b1     21.733063 0.2888890  21.271363  22.194764
## b2     -7.803268 0.2741838  -8.241467  -7.365069
## sigma   5.774474 0.1764651   5.492448   6.056499
```


```r
weight.seq <- seq(from=2.2, to=2, length.out = 30)
pred_dat <- list(weight_s=weight.seq, weight_s2=weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)
```



```r
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)
```

![](Week4.042319_files/figure-html/Rcode4.68-1.png)<!-- -->


```r
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a+b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
    a ~ dnorm(178 , 20),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    sigma ~ dunif(0, 50)
  ), data = d)
```


```r
plot( height ~ weight_s, d, col=col.alpha(rangi2, 0.5), xaxt = "n")
at <- c(-2,-1, 0, 1, 2)
labels <- at*sd(d$weight)+ mean(d$weight)
axis(side=1, at=at, labels=round(labels, 1))
at <- c(-2,-1, 0, 1, 2)
labels <- at*sd(d$weight)+ mean(d$weight)
axis(side=1, at=at, labels=round(labels, 1))
```

![](Week4.042319_files/figure-html/Rcode4.70&71-1.png)<!-- -->


```r
library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
precis(d)
```

```
##                   mean          sd      5.5%      94.5%       histogram
## year       1408.000000 350.8845964 867.77000 1948.23000   ▇▇▇▇▇▇▇▇▇▇▇▇▁
## doy         104.540508   6.4070362  94.43000  115.00000        ▁▂▅▇▇▃▁▁
## temp          6.141886   0.6636479   5.15000    7.29470        ▁▃▅▇▃▂▁▁
## temp_upper    7.185151   0.9929206   5.89765    8.90235 ▁▂▅▇▇▅▂▂▁▁▁▁▁▁▁
## temp_lower    5.098941   0.8503496   3.78765    6.37000 ▁▁▁▁▁▁▁▃▅▇▃▂▁▁▁
```


```r
d2 <- d[complete.cases(d$temp), ]
num_knots <- 15
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))
knot_list
```

```
##        0% 7.142857% 14.28571% 21.42857% 28.57143% 35.71429% 42.85714% 
##  839.0000  937.2143 1017.4286 1097.6429 1177.8571 1258.0714 1338.2857 
##       50% 57.14286% 64.28571% 71.42857% 78.57143% 85.71429% 92.85714% 
## 1418.5000 1498.7143 1578.9286 1659.1429 1739.3571 1819.5714 1899.7857 
##      100% 
## 1980.0000
```

```r
library(splines)
head(d2)
```

```
##    year doy temp temp_upper temp_lower
## 39  839  NA 5.87      10.99       0.75
## 40  840  NA 5.99      10.78       1.21
## 41  841  NA 6.12      10.60       1.64
## 42  842  NA 6.25      10.47       2.02
## 43  843  NA 6.37      10.39       2.35
## 44  844  NA 6.50      10.37       2.63
```

```r
B <- bs(d2$year,
        knots = knot_list[-c(1, num_knots)],
        degree = 3, intercept = T)
```


```r
plot(NULL, xlim = range(d2$year), ylim=c(0, 1), xlab="year", ylab="basis value")
for(i in 1:ncol(B)) lines(d2$year, B[,i])
```

![](Week4.042319_files/figure-html/Rcode4.75-1.png)<!-- -->


```r
m4.7 <- quap(
  alist(
    T ~ dnorm(mu, sigma),
    mu <- a +B %*% w,
    a ~ dnorm(6,10),
    w ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data = list(T=d2$temp, B=B),
  start = list(w=rep(0, ncol(B)))
)
```


```r
post <- extract.samples(m4.7)
w <- apply( post$w, 2, mean)
plot(NULL, xlim=range(d2$year), ylim = c(-2, 2), 
     xlab="year", ylab="basis  * weight")
for(i in 1:ncol(B)) lines(d2$year, w[i]*B[,i])
```

![](Week4.042319_files/figure-html/Rcode4.77-1.png)<!-- -->


```r
mu <- link(m4.7)
mu.PI <- apply(mu, 2, PI, 0.97)
plot(d2$year, d2$temp, col=col.alpha(rangi2, 0.3), pch=16)
shade(mu.PI, d2$year, col=col.alpha("black", 0.5))
```

![](Week4.042319_files/figure-html/Rcode4.78-1.png)<!-- -->

