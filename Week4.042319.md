---
title: "Week4.042319"
author: "Rie"
date: "4/22/2019"
output: 
  html_document: 
    keep_md: yes
---




```r
pos <- replicate(1000, sum(runif(16, -1,1)))
plot(density(pos))
```

![](Week4.042319_files/figure-html/Rcode4.1-1.png)<!-- -->




```r
prod(1 + runif(12, 0, 0.1))
```

```
## [1] 1.946883
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
## 7.266332 8.195980
```

```r
d3 <- sample(d2$height, size=20)
d3
```

```
##  [1] 151.7650 144.1450 167.0050 162.5600 161.9250 152.7048 148.2852
##  [8] 142.8750 146.6850 153.6700 142.8750 157.4800 141.6050 158.1150
## [15] 154.3050 144.7800 160.0200 158.7500 152.4000 151.7650
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
## mu    154.606874 0.4119610 153.948481 155.265267
## sigma   7.730702 0.2913266   7.265106   8.196298
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
## 7.266332 8.195980
```

```r
start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)
m4.1 <- quap(flist, data = d2, start = start)
```

