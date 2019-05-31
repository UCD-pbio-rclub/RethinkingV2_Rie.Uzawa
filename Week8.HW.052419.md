---
title: "Week8.HW.052419"
author: "Rie"
date: "5/24/2019"
output: 
  html_document: 
    keep_md: yes
---



###6H1. Use the Waffle House data, data(WaffleDivorce), to find the total causal influence of number of Waffle Houses on divorce rate. Justify your model or models with a causal graph.


```r
data("WaffleDivorce")
d <- WaffleDivorce
d$WH <- scale(d$WaffleHouses)
d$D <- scale(d$Divorce)

hw1.a <- quap(
  alist(
    WH ~ dnorm(mu, sigma),
    mu <- a + bD*D, 
    a ~ dnorm(0, 0.2), 
    bD ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.a)
```

```
##               mean         sd        5.5%     94.5%
## a     1.424615e-07 0.11140719 -0.17805006 0.1780503
## bD    2.370630e-01 0.13082947  0.02797228 0.4461538
## sigma 9.485596e-01 0.09356201  0.79902946 1.0980898
```

###Divorce rate is higher in the states where more WHs are located


###6H2. Build a series of models to test the implied conditional independencies of the causal graph you used in the previous problem. If any of the tests fail, how do you think the graph needs to be amended? Does the graph need more or fewer arrows? Feel free to nominate variables that aren’t in the data.


```r
hw1.south <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bSouth*South, 
    a ~ dnorm(0, 0.2), 
    bSouth ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.south)
```

```
##              mean         sd       5.5%      94.5%
## a      -0.1021384 0.12009631 -0.2940755 0.08979871
## bSouth  0.5217511 0.24383132  0.1320615 0.91144062
## sigma   0.9277619 0.09236023  0.7801525 1.07537143
```


```r
#south+ waffle house vs divorce

hw1.SW <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bWH*WH + bSouth*South,
    a ~ dnorm(0, 0.2), 
   c(bWH, bSouth) ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.SW)
```

```
##               mean        sd        5.5%     94.5%
## a      -0.07724529 0.1238455 -0.27517426 0.1206837
## bWH     0.12322349 0.1533215 -0.12181389 0.3682609
## bSouth  0.39416446 0.2908969 -0.07074504 0.8590740
## sigma   0.92663778 0.0919878  0.77962351 1.0736521
```
###This shows Pipe (south - WH - D)


```r
#age vs divorce, any relationship between age and divorce?
d$A <- scale(d$MedianAgeMarriage)

hw1.AD <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2), 
    bA ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.AD)
```

```
##                mean         sd       5.5%      94.5%
## a      1.191874e-09 0.09737878 -0.1556301  0.1556301
## bA    -5.684034e-01 0.10999982 -0.7442044 -0.3926024
## sigma  7.883258e-01 0.07801137  0.6636486  0.9130030
```
###Divorce rate is higher in younger people(anticorrelated)



```r
# marriage age vs south, any relationship between age and divorce?

hw1.AS <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a + bSouth*South,
    a ~ dnorm(0, 0.2), 
    bSouth ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.AS)
```

```
##               mean         sd       5.5%      94.5%
## a       0.07099361 0.12180104 -0.1236680 0.26565520
## bSouth -0.36888491 0.24770226 -0.7647610 0.02699114
## sigma   0.95382254 0.09448857  0.8028115 1.10483353
```
###marriage age is younger in south 


```r
#marriage age + south vs divorce 

hw1.ASD <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A + bSouth*South,
    a ~ dnorm(0, 0.2), 
   c(bA, bSouth) ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.ASD)
```

```
##               mean         sd        5.5%       94.5%
## a      -0.07721005 0.10604880 -0.24669651  0.09227641
## bA     -0.53178661 0.10918688 -0.70628834 -0.35728489
## bSouth  0.35626012 0.21490135  0.01280626  0.69971397
## sigma   0.76438469 0.07588348  0.64310824  0.88566115
```


```r
#age vs marriage rate, any relationship between age and divorce?
d$M <- scale(d$Marriage)

hw1.AM <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2), 
    bA ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.AM)
```

```
##                mean         sd       5.5%      94.5%
## a     -7.723433e-06 0.08684728 -0.1388064  0.1387910
## bA    -6.947458e-01 0.09572611 -0.8477346 -0.5417570
## sigma  6.817315e-01 0.06757870  0.5737276  0.7897353
```
###Marriage rate is higher for older people


```r
# marriage rate vs south, any relationship between age and divorce?

hw1.MS <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bSouth*South,
    a ~ dnorm(0, 0.2), 
    bSouth ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.MS)
```

```
##               mean         sd       5.5%     94.5%
## a      -0.02270483 0.12329763 -0.2197583 0.1743486
## bSouth  0.11983220 0.25109246 -0.2814620 0.5211264
## sigma   0.97755593 0.09640094  0.8234886 1.1316233
```


```r
#marriage age + south vs divorce 

hw1.ASM <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bA*A + bSouth*South,
    a ~ dnorm(0, 0.2), 
   c(bA, bSouth) ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d
)

precis(hw1.ASM)
```

```
##               mean         sd       5.5%      94.5%
## a       0.03818221 0.09698987 -0.1168263  0.1931908
## bA     -0.71338663 0.09722375 -0.8687689 -0.5580043
## bSouth -0.16744717 0.19485046 -0.4788558  0.1439615
## sigma   0.67519405 0.06702176  0.5680803  0.7823078
```

###Collider!!
###A, S, M are failed in multiregression.

###1. Use a model to infer the total causal influence of area on weight. Would increasing the area available to each fox make it heavier (healthier)? You might want to standardize the variables. Regardless, use prior predictive simulation to show that your model’s prior predictions stay within the possible outcome range.


```r
data("foxes")
d2 <- foxes
d2$W <- scale(d2$weight)
d2$A <- scale(d2$area)

m1.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bA*A, 
    a ~ dnorm(0, 0.2), 
    bA ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d2
)

precis(m1.1)
```

```
##               mean         sd       5.5%     94.5%
## a     9.156506e-09 0.08360868 -0.1336228 0.1336228
## bA    1.883366e-02 0.09089583 -0.1264354 0.1641027
## sigma 9.912661e-01 0.06466649  0.8879166 1.0946157
```



###2. Now infer the causal impact of adding food to a territory. Would this make foxes heavier? Which covariates do you need to adjust for to estimate the total causal influence of food?


```r
d2$FD <- scale(d2$avgfood)
m2.1 <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a + bFD*FD, 
    a ~ dnorm(0, 0.2), 
    bFD ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d2
)

precis(m2.1)
```

```
##               mean         sd        5.5%      94.5%
## a     5.722923e-06 0.04231533 -0.06762235 0.06763379
## bFD   8.764758e-01 0.04332837  0.80722868 0.94572288
## sigma 4.663066e-01 0.03053247  0.41750978 0.51510336
```

###No relationship between area and food

###3. Now infer the causal impact of group size. Which covariates do you need to adjust for? Looking at the posterior distribution of the resulting model,what do you think explains these data? That is, can you explain the estimates for all three problems? How do they go together?


```r
d2$GS <- scale(d2$groupsize)
m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bGS*GS + bFD*FD, 
    a ~ dnorm(0, 0.2), 
    c(bGS, bFD) ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), data = d2
)

precis(m3.1)
```

```
##                mean         sd       5.5%      94.5%
## a      4.989088e-08 0.08013805 -0.1280760  0.1280761
## bGS   -5.735259e-01 0.17914169 -0.8598289 -0.2872229
## bFD    4.772530e-01 0.17912319  0.1909795  0.7635264
## sigma  9.420437e-01 0.06175252  0.8433513  1.0407362
```
###Group size has anticorrelation with weight whereas food is now correlate with weight.
