ways <- c(0,3,8,9,0)
ways/sum(ways)
#[1] 0.00 0.15 0.40 0.45 0.00

dbinom(6, size=9, prob=0.5) ###Got 6 water, probability 0.5, total sampling is 9

#Define grid
p_grid <- seq(from=0, to=1, length.out = 20)

# [1] 0.00000000 0.05263158 0.10526316 0.15789474 0.21052632 0.26315789 0.31578947
# [8] 0.36842105 0.42105263 0.47368421 0.52631579 0.57894737 0.63157895 0.68421053
# [15] 0.73684211 0.78947368 0.84210526 0.89473684 0.94736842 1.00000000

#Defind prior
prior <-rep(1,20)
#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

#compute likelihood at each value in grid
likelihood <- dbinom(6,size = 9, prob=p_grid)
# [1] 0.000000e+00 1.518149e-06 8.185093e-05 7.772923e-04 3.598575e-03
# [6] 1.116095e-02 2.668299e-02 5.292110e-02 9.082698e-02 1.383413e-01
# [11] 1.897686e-01 2.361147e-01 2.666113e-01 2.714006e-01 2.450051e-01
# [16] 1.897686e-01 1.179181e-01 5.026670e-02 8.853845e-03 0.000000e+00

#compute product of likelihood and prior
unstd.posterior <- likelihood*prior
# [1] 0.000000e+00 1.518149e-06 8.185093e-05 7.772923e-04 3.598575e-03
# [6] 1.116095e-02 2.668299e-02 5.292110e-02 9.082698e-02 1.383413e-01
# [11] 1.897686e-01 2.361147e-01 2.666113e-01 2.714006e-01 2.450051e-01
# [16] 1.897686e-01 1.179181e-01 5.026670e-02 8.853845e-03 0.000000e+00

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior /sum(unstd.posterior)
# [1] 0.000000e+00 7.989837e-07 4.307717e-05 4.090797e-04 1.893887e-03
# [6] 5.873873e-03 1.404294e-02 2.785174e-02 4.780115e-02 7.280739e-02
# [11] 9.987296e-02 1.242643e-01 1.403143e-01 1.428349e-01 1.289433e-01
# [16] 9.987296e-02 6.205890e-02 2.645477e-02 4.659673e-03 0.000000e+00

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
mtext("20 points")


##########5 points
#Define grid
p_grid <- seq(from=0, to=1, length.out = 5)
#[1] 0.00 0.25 0.50 0.75 1.00

#Defind prior
prior <-rep(1,5)

#compute likelihood at each value in grid
likelihood <- dbinom(6,size = 9, prob=p_grid)
#[1] 0.000000000 0.008651733 0.164062500 0.233596802 0.000000000

#compute product of likelihood and prior
unstd.posterior <- likelihood*prior

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior /sum(unstd.posterior)
#[1] 0.00000000 0.02129338 0.40378549 0.57492114 0.00000000

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
mtext("5 points")

##########1000 points
#Define grid
p_grid <- seq(from=0, to=1, length.out = 1000)

#Defind prior
prior <-rep(1,1000)

#compute likelihood at each value in grid
likelihood <- dbinom(6,size = 9, prob=p_grid)


#compute product of likelihood and prior
unstd.posterior <- likelihood*prior

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior /sum(unstd.posterior)
#[1] 0.00000000 0.02129338 0.40378549 0.57492114 0.00000000

#C plot
plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
mtext("1000 points")

#####Replacement1

#Define grid
p_grid <- seq(from=0, to=1, length.out = 20)

# [1] 0.00000000 0.05263158 0.10526316 0.15789474 0.21052632 0.26315789 0.31578947
# [8] 0.36842105 0.42105263 0.47368421 0.52631579 0.57894737 0.63157895 0.68421053
# [15] 0.73684211 0.78947368 0.84210526 0.89473684 0.94736842 1.00000000

#Defind prior
prior <- ifelse(p_grid<0.5,0,1)
#[1] 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1

#compute likelihood at each value in grid
likelihood <- dbinom(6,size = 9, prob=p_grid)
# [1] 0.000000e+00 1.518149e-06 8.185093e-05 7.772923e-04 3.598575e-03
# [6] 1.116095e-02 2.668299e-02 5.292110e-02 9.082698e-02 1.383413e-01
# [11] 1.897686e-01 2.361147e-01 2.666113e-01 2.714006e-01 2.450051e-01
# [16] 1.897686e-01 1.179181e-01 5.026670e-02 8.853845e-03 0.000000e+00

#compute product of likelihood and prior
unstd.posterior <- likelihood*prior
# [1] 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
# [7] 0.000000000 0.000000000 0.000000000 0.000000000 0.189768623 0.236114658
# [13] 0.266611252 0.271400562 0.245005089 0.189768623 0.117918118 0.050266702
# [19] 0.008853845 0.000000000

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior /sum(unstd.posterior)

#D plot
plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
mtext("20 points")

#####Replacement2

#Define grid
p_grid <- seq(from=0, to=1, length.out = 20)

#Defind prior
prior <- exp(-5*abs(p_grid-0.5))
# [1] 0.0820850 0.1067952 0.1389440 0.1807706 0.2351884 0.3059877 0.3980998
# [8] 0.5179406 0.6738573 0.8767101 0.8767101 0.6738573 0.5179406 0.3980998
# [15] 0.3059877 0.2351884 0.1807706 0.1389440 0.1067952 0.0820850

#compute likelihood at each value in grid
likelihood <- dbinom(6,size = 9, prob=p_grid)


#compute product of likelihood and prior
unstd.posterior <- likelihood*prior


#standardize the posterior, so it sums to 1
posterior <- unstd.posterior /sum(unstd.posterior)

#E plot
plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
mtext("20 points")


#R code 2.6
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom(W+L ,p) ,  #binomial likelihood
    p ~ dunif(0,1)        #uniform prior
    ) ,
  data=list(W=6, L=3) )

#disply summary of quadratic approximation 

precis(globe.qa)

# mean   sd 5.5% 94.5%
#   p 0.67 0.16 0.42  0.92


#analytical calculation
W <- 6
L <- 3
curve( dbeta(x, W+1, L+1), from = 0, to = 1)
#qadratic approximation
curve( dnorm(x, 0.67, 0.16), lty=2, add = T)

#MCMC
n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for (i in 2:n_samples){
  p_new <- rnorm(1, p[i-1], 0.1)
  if (p_new < 0) p_new <- abs(p_new)
  if (p_new >1) p_new <- 2 - p_new
  q0 <- dbinom(W, W+L, p[i-1])
  q1 <- dbinom(W, W+L, p_new)
  p[i] <- ifelse( runif(1) < q1/q0, p_new, p[i-1])
}

dens(p, xlim=c(0,1))
curve(dbeta(x, W+1, L+1), lty = 2, add = T)


###########################################################
#######Problems
###########################################################
#2E1 2
#2E2 3
#2E3 1

#2M 1-1
p_grid <- seq(from=0, to=1, length.out = 20)
prior <- rep(0.5, 20)
likelihood <- dbinom(3, size=3, prob=p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior) 
plot(p_grid, posterior, type = "b",
     xlab = "probablility of water", ylab = "posterior probablity")
mtext("2M 1-1")

#2M 1-2
p_grid <- seq(from=0, to=1, length.out = 20)
prior <- rep(0.5, 20)
likelihood <- dbinom(3, size=4, prob=p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior) 
plot(p_grid, posterior, type = "b",
     xlab = "probablility of water", ylab = "posterior probablity")
mtext("2M 1-2")

#2M 1-3
p_grid <- seq(from=0, to=1, length.out = 20)
prior <- rep(0.5, 20)
likelihood <- dbinom(5, size=7, prob=p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior) 
plot(p_grid, posterior, type = "b",
     xlab = "probablility of water", ylab = "posterior probablity")
mtext("2M 1-3")



#2M 2-1
p_grid <- seq(from=0, to=1, length.out = 20)
#prior <- rep(0.5, 20)
prior <- ifelse( p_grid < 0.5 , 0 , 0.5 )
likelihood <- dbinom(3, size=3, prob=p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior) 
plot(p_grid, posterior, type = "b",
     xlab = "probablility of water", ylab = "posterior probablity")
mtext("2M 2-1")

#2M 2-2
p_grid <- seq(from=0, to=1, length.out = 20)
prior <- ifelse( p_grid < 0.5 , 0 , 0.5 )
likelihood <- dbinom(3, size=4, prob=p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior) 
plot(p_grid, posterior, type = "b",
     xlab = "probablility of water", ylab = "posterior probablity")
mtext("2M 2-2")

#2M 2-3
p_grid <- seq(from=0, to=1, length.out = 20)
prior <- ifelse( p_grid < 0.5 , 0 , 0.5 )
likelihood <- dbinom(5, size=7, prob=p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior) 
plot(p_grid, posterior, type = "b",
     xlab = "probablility of water", ylab = "posterior probablity")
mtext("2M 2-3")


#2M 3
proDat <- 0.5*0.3 
post <- proDat*1/0.23
#[1] 0.6521739


#2M 4 and 5 answeres are in TextEd





