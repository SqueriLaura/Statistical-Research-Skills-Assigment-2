# Statistical-Research-Skills-Assigment-2
---
title: "trial"
author: "Laura Squeri"
date: "06/02/2021"
output:
  word_document: default
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(sfsmisc)
set.seed(1)
library(np)
library(fitdistrplus)
library(tidyr)
library(data.table)
library(dplyr)
library(formattable)
```

### The simulated data
Here id say that the kernel density estimator shows better the shape of a normal distribution (from which we sampled)
on high numbers they seem both equally good but on lower numbers of n the KDE seems better norm distr
```{r, figures-side, fig.show="hold", out.width="10%"}
set.seed(1)
n = 70

norm_dist = rnorm(n)
hist(norm_dist, freq = FALSE, main = "Normal Distribution" )
lines(density(norm_dist))

chi_dist = rchisq(n,5)
hist(chi_dist, freq = FALSE, main = "Chi Distribution")
lines(density(chi_dist))


logistic_dist = rlogis(n)
hist(logistic_dist, freq = FALSE, main = "Logistic Distribution")
lines(density(logistic_dist))
```



```{r }
set.seed(1)
n1 <- 250
n2 <- 500
n3 <- 1000
R <- 1000
### NORMAL
#n = 250
ynorm1 <- matrix(NA, R, n1) # matrix with simulated datasets
mu1 <- numeric(R) # vector with mean for each simulated dataset
med1 <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ynorm1[r, ] <- rnorm(n1)
mu1[r] <- mean(ynorm1[r, ])
med1[r] <- median(ynorm1[r, ])
}
MCMmu1 <- mean(mu1)
print('MC mean of normal distribution' )
print("n = 250")
MCMmu1

# n = 500
ynorm2 <- matrix(NA, R, n2) # matrix with simulated datasets
mu2 <- numeric(R) # vector with mean for each simulated dataset
med2 <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ynorm2[r, ] <- rnorm(n2)
mu2[r] <- mean(ynorm2[r, ])
med2[r] <- median(ynorm2[r, ])
}
MCMmu2 <- mean(mu2)
print("n = 500" )
MCMmu2

# n = 1000
ynorm3 <- matrix(NA, R, n3) # matrix with simulated datasets
mu3 <- numeric(R) # vector with mean for each simulated dataset
med3 <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ynorm3[r, ] <- rnorm(n3)
mu3[r] <- mean(ynorm3[r, ])
med3[r] <- median(ynorm3[r, ])
}
MCMmu3 <- mean(mu3)
print("n = 1000" )
MCMmu3

###CHI
#n = 250
ychi <- matrix(NA, R, n1) # matrix with simulated datasets
mu1_c <- numeric(R) # vector with mean for each simulated dataset
med1_c <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ychi[r, ] <- rchisq(n1,5)
mu1_c[r] <- mean(ychi[r, ])
med1_c[r] <- median(ychi[r, ])
}
MCMmu1_c <- mean(mu1_c)
print("MC mean of chi distribution" )
print("n = 250")
MCMmu1_c

#n = 500
ychi <- matrix(NA, R, n2) # matrix with simulated datasets
mu2_c <- numeric(R) # vector with mean for each simulated dataset
med2_c <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ychi[r, ] <- rchisq(n2,5)
mu2_c[r] <- mean(ychi[r, ])
med2_c[r] <- median(ychi[r, ])
}
MCMmu2_c <- mean(mu2_c)
print("n = 500")
MCMmu2_c

#n = 1000
ychi <- matrix(NA, R, n3) # matrix with simulated datasets
mu3_c <- numeric(R) # vector with mean for each simulated dataset
med3_c <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ychi[r, ] <- rchisq(n3,5)
mu3_c[r] <- mean(ychi[r, ])
med3_c[r] <- median(ychi[r, ])
}
MCMmu3_c <- mean(mu3_c)
print("n = 1000")
MCMmu3_c

###logistic
#n = 250
ylogistic <- matrix(NA, R, n1) # matrix with simulated datasets
mu1_l <- numeric(R) # vector with mean for each simulated dataset
med1_l <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ylogistic[r, ] <- rlogis(n1)
mu1_l[r] <- mean(ylogistic[r, ])
med1_l[r] <- median(ylogistic[r, ])
}
MCMmu1_l <- mean(mu1_l)
print("MC mean of logistic distribution" )
print("n = 250")
MCMmu1_l

#n = 500
ylogistic <- matrix(NA, R, n2) # matrix with simulated datasets
mu2_l <- numeric(R) # vector with mean for each simulated dataset
med2_l <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ylogistic[r, ] <- rlogis(n2)
mu2_l[r] <- mean(ylogistic[r, ])
med2_l[r] <- median(ylogistic[r, ])
}
MCMmu2_l <- mean(mu2_l)
print("n = 500")
MCMmu2_l

#n = 1000
ylogistic <- matrix(NA, R, n3) # matrix with simulated datasets
mu3_l <- numeric(R) # vector with mean for each simulated dataset
med3_l <- numeric(R) # vector with median for each simulated dataset

for(r in 1:R) {
ylogistic[r, ] <- rlogis(n3)
mu3_l[r] <- mean(ylogistic[r, ])
med3_l[r] <- median(ylogistic[r, ])
}
MCMmu3_l <- mean(mu3_l)
print("n = 1000")
MCMmu3_l

```

https://stats.stackexchange.com/questions/390777/how-to-compute-integrated-squared-error-for-kernel-density-estimation-in-r


########### ISE for KDE on normal distribution
```{r}
# norm, KDE, 250
set.seed(1)
n1 <- 250
R <- 1000

ynorm1 <- matrix(NA, R, n1) # matrix with simulated datasets
ise_sum1 = 0

for(r in 1:R) {
ynorm1[r, ] <- rnorm(n1)

est = density(ynorm1[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dnorm(est$x))^2)
integrate(splxy, lower = min(ynorm1[r, ]), upper = max(ynorm1[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dnorm(est$x))^2)
ise_sum1 = ise_sum1 + ise
}
norm1 = ise_sum1/1000
```

```{r}
# norm, KDE, 500
n2 = 500
ynorm2 <- matrix(NA, R, n2) # matrix with simulated datasets
ise_sum2 = 0

for(r in 1:R) {
ynorm2[r, ] <- rnorm(n2)

est = density(ynorm2[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dnorm(est$x))^2)
integrate(splxy, lower = min(ynorm1[r, ]), upper = max(ynorm1[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dnorm(est$x))^2)
ise_sum2 = ise_sum2 + ise
}
norm2 = ise_sum2/1000
```

```{r}
# norm, KDE, 1000
n3 = 1000
ynorm3 <- matrix(NA, R, n3) # matrix with simulated datasets
ise_sum3 = 0

for(r in 1:R) {
ynorm3[r, ] <- rnorm(n3)

est = density(ynorm3[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dnorm(est$x))^2)
integrate(splxy, lower = min(ynorm1[r, ]), upper = max(ynorm1[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dnorm(est$x))^2)
ise_sum3 = ise_sum3 + ise
}
norm3 = ise_sum3/1000
```




####### ISE for KDE in chi square distribution
```{r}
# Chi, KDE, 250
set.seed(1)
n1 <- 250
R <- 1000

chi_d1 <- matrix(NA, R, n1) # matrix with simulated datasets
ise_sum1_c = 0

for(r in 1:R) {
chi_d1[r, ] <- rchisq(n1,5)

est = density(chi_d1[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dchisq(est$x,5))^2)
integrate(splxy, lower = min(chi_d1[r, ]), upper = max(chi_d1[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dchisq(est$x,5))^2)
ise_sum1_c = ise_sum1_c + ise
}
chi1 = ise_sum1_c/1000
```

```{r}
# Chi, KDE, 500
set.seed(1)
n2 <- 500
R <- 1000

chi_d2 <- matrix(NA, R, n2) # matrix with simulated datasets
ise_sum2_c = 0

for(r in 1:R) {
chi_d2[r, ] <- rchisq(n2,5)

est = density(chi_d2[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dchisq(est$x,5))^2)
integrate(splxy, lower = min(chi_d2[r, ]), upper = max(chi_d2[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dchisq(est$x,5))^2)
ise_sum2_c = ise_sum2_c + ise
}
chi2 = ise_sum2_c/1000
```

```{r}
# Chi, KDE, 1000
set.seed(1)
n3 <- 1000
R <- 1000

chi_d3 <- matrix(NA, R, n3) # matrix with simulated datasets
ise_sum3_c = 0

for(r in 1:R) {
chi_d3[r, ] <- rchisq(n3,5)

est = density(chi_d3[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dchisq(est$x,5))^2)
integrate(splxy, lower = min(chi_d3[r, ]), upper = max(chi_d3[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dchisq(est$x,5))^2)
ise_sum3_c = ise_sum3_c + ise
}
chi3 = ise_sum3_c/1000
```

####### ISE in KDE in logistic distribution
```{r}
# logistic, KDE, 250
set.seed(1)
n1 <- 250
R <- 1000

logistic_d1 <- matrix(NA, R, n1) # matrix with simulated datasets
ise_sum1_l = 0

for(r in 1:R) {
logistic_d1[r, ] = rlogis(n1)

est = density(logistic_d1[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dlogis(est$x))^2)
integrate(splxy, lower = min(logistic_d1[r, ]), upper = max(logistic_d1[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dlogis(est$x))^2)
ise_sum1_l = ise_sum1_l + ise
}
lt1 = ise_sum1_l/1000
```

```{r}
# logistic, KDE, 500
set.seed(1)
n2 <- 500
R <- 1000

logistic_d2 <- matrix(NA, R, n2) # matrix with simulated datasets
ise_sum2_l = 0

for(r in 1:R) {
logistic_d2[r, ] = rlogis(n2)

est = density(logistic_d2[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dlogis(est$x))^2)
integrate(splxy, lower = min(logistic_d2[r, ]), upper = max(logistic_d2[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dlogis(est$x))^2)
ise_sum2_l = ise_sum2_l + ise
}
lt2 = ise_sum2_l/1000
```



```{r}
# logistic, KDE, 1000
set.seed(1)
n3 <- 1000
R <- 1000

logistic_d3 <- matrix(NA, R, n3) # matrix with simulated datasets
ise_sum3_l = 0

for(r in 1:R) {
logistic_d3[r, ] = rlogis(n3)

est = density(logistic_d3[r, ])

# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dlogis(est$x))^2)
integrate(splxy, lower = min(logistic_d3[r, ]), upper = max(logistic_d3[r, ]))

ise = sfsmisc::integrate.xy(x = est$x, (est$y - dlogis(est$x))^2)
ise_sum3_l = ise_sum3_l + ise
}
lt3 = ise_sum3_l/1000

```


#######HIST
```{r}
# normal, Histogram, 250
set.seed(1)
n1 <- 250
R <- 1000

ynorm1 <- matrix(NA, R, n1) # matrix with simulated datasets
ise_sum1 = 0

for(r in 1:R) {
ynorm1[r, ] <- rnorm(n1)

est_h = hist(ynorm1[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dnorm(est_h$mids))^2)
integrate(splxy, lower = min(ynorm1[r, ]), upper = max(ynorm1[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dnorm(est_h$mids))^2)
ise_sum1 = ise_sum1 + ise
}
norm1_h = ise_sum1/1000
norm1_h
```


```{r}
# normal, Histogram, 500
n2 = 500
ynorm2 <- matrix(NA, R, n2) # matrix with simulated datasets
ise_sum2 = 0

for(r in 1:R) {
ynorm2[r, ] <- rnorm(n2)

est_h = hist(ynorm2[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dnorm(est_h$mids))^2)
integrate(splxy, lower = min(ynorm1[r, ]), upper = max(ynorm1[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dnorm(est_h$mids))^2)
ise_sum2 = ise_sum2 + ise
}
norm2_h = ise_sum2/1000
norm2_h
```


```{r}
# normal, Histogram, 1000
n3 = 1000
ynorm3 <- matrix(NA, R, n3) # matrix with simulated datasets
ise_sum3 = 0

for(r in 1:R) {
ynorm3[r, ] <- rnorm(n3)

est_h = hist(ynorm3[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dnorm(est_h$mids))^2)
integrate(splxy, lower = min(ynorm1[r, ]), upper = max(ynorm1[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dnorm(est_h$mids))^2)
ise_sum3 = ise_sum3 + ise
}
norm3_h = ise_sum3/1000
norm3_h
```


####### ISE for HIST in chi square distribution
```{r}
# Chi, Histogram, 250
set.seed(1)
n1 <- 250
R <- 1000

chi_d1 <- matrix(NA, R, n1) # matrix with simulated datasets
ise_sum1_c = 0

for(r in 1:R) {
chi_d1[r, ] <- rchisq(n1,5)

est_h = hist(chi_d1[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dchisq(est_h$mids,5))^2)
integrate(splxy, lower = min(chi_d1[r, ]), upper = max(chi_d1[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dchisq(est_h$mids,5))^2)
ise_sum1_c = ise_sum1_c + ise
}
chi1_h = ise_sum1_c/1000
chi1_h
```

```{r}
# Chi, Histogram, 500
set.seed(1)
n2 <- 500
R <- 1000

chi_d2 <- matrix(NA, R, n2) # matrix with simulated datasets
ise_sum2_c = 0

for(r in 1:R) {
chi_d2[r, ] <- rchisq(n2,5)

est_h = hist(chi_d2[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dchisq(est_h$mids,5))^2)
integrate(splxy, lower = min(chi_d2[r, ]), upper = max(chi_d2[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dchisq(est_h$mids,5))^2)
ise_sum2_c = ise_sum2_c + ise
}
chi2_h = ise_sum2_c/1000
chi2_h
```

```{r}
# Chi, Histogram, 1000
set.seed(1)
n3 <- 1000
R <- 1000

chi_d3 <- matrix(NA, R, n3) # matrix with simulated datasets
ise_sum3_c = 0

for(r in 1:R) {
chi_d3[r, ] <- rchisq(n3,5)

est_h = hist(chi_d3[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dchisq(est_h$mids,5))^2)
integrate(splxy, lower = min(chi_d3[r, ]), upper = max(chi_d3[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dchisq(est_h$mids,5))^2)
ise_sum3_c = ise_sum3_c + ise
}
chi3_h = ise_sum3_c/1000
chi3_h
```

####### ISE in HIST in logistic distribution
```{r}
#logistic, Histogram, 250
set.seed(1)
n1 <- 250
R <- 1000

logistic_d1 <- matrix(NA, R, n1) # matrix with simulated datasets
ise_sum1_l = 0

for(r in 1:R) {
logistic_d1[r, ] = rlogis(n1)

est_h = hist(logistic_d1[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dlogis(est_h$mids))^2)
integrate(splxy, lower = min(logistic_d1[r, ]), upper = max(logistic_d1[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dlogis(est_h$mids))^2)
ise_sum1_l = ise_sum1_l + ise
}
lt1_h = ise_sum1_l/1000
lt1_h
```

```{r}
#logistic, Histogram, 500
set.seed(1)
n2 <- 500
R <- 1000

logistic_d2 <- matrix(NA, R, n2) # matrix with simulated datasets
ise_sum2_l = 0

for(r in 1:R) {
logistic_d2[r, ] = rlogis(n2)

est_h = hist(logistic_d2[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dlogis(est_h$mids))^2)
integrate(splxy, lower = min(logistic_d2[r, ]), upper = max(logistic_d2[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dlogis(est_h$mids))^2)
ise_sum2_l = ise_sum2_l + ise
}
lt2_h = ise_sum2_l/1000
lt2_h
```



```{r }
#logistic, Histogram, 1000
set.seed(1)
n3 <- 1000
R <- 1000

logistic_d3 <- matrix(NA, R, n3) # matrix with simulated datasets
ise_sum3_l = 0

for(r in 1:R) {
logistic_d3[r, ] = rlogis(n3)

est_h = hist(logistic_d3[r, ], plot = FALSE)

# Create approx func obj and integrate()
splxy = splinefun(est_h$mids, (est_h$density - dlogis(est_h$mids))^2)
integrate(splxy, lower = min(logistic_d3[r, ]), upper = max(logistic_d3[r, ]))

ise = sfsmisc::integrate.xy(x = est_h$mids, (est_h$density - dlogis(est_h$mids))^2)
ise_sum3_l = ise_sum3_l + ise
}
lt3_h = ise_sum3_l/1000
lt3_h
```

```{r, figures-side, fig.show="hold", out.width="30%"}
n = c(250,500,1000)
#KDE 
#normal
ISE_n = c(norm1, norm2, norm3)
#chi
ISE_c = c(chi1, chi2, chi3)
#logistic
ISE_l = c(lt1, lt2, lt3)

#HISTOGRAM
#normal green is KDE and red is histogram
ISE_n_h = c(norm1_h, norm2_h, norm3_h)

plot(n, ISE_n_h, "b", xlim = c(250,1000), xgap.axis = 250, col="red", main = "Normal Distribution" ,ylab = "ISE")
lines(n,ISE_n,col="green")
axis(side = 1, at = n, labels = c(250,500,1000))
legend("bottomleft", 
  legend = c("KDE", "Histogram"), 
  col = c("green","red"), lty=1:2, cex=0.8)

#chi
ISE_c_h = c(chi1_h, chi2_h, chi3_h)
?plot
plot(n, ISE_c_h, "b", xlim = c(250,1000), xgap.axis = 250, col="red", main = "Chi Distribution" ,ylab = "ISE")
lines(n,ISE_c,col="green")
axis(side = 1, at = n, labels = c(250,500,1000))
legend("bottomleft", 
  legend = c("KDE", "Histogram"), 
  col = c("green","red"), lty=1:2, cex=0.8)

#logistic
ISE_l_h = c(lt1_h, lt2_h, lt3_h)

plot(n, ISE_l_h, "b", xlim = c(250,1000), xgap.axis = 250, col="red", main = "Logistic Distribution" ,ylab = "ISE")
lines(n,ISE_l,col="green")
axis(side = 1, at = n, labels = c(250,500,1000))
legend("bottomleft", 
  legend = c("KDE", "Histogram"), 
  col = c("green","red"), lty=1:2, cex=0.8)
```

```{r}
n = c(250, 500, 1000)
tabKDE = data.frame(n, ISE_n, ISE_c, ISE_l)
tabH = data.frame(n, ISE_n_h, ISE_c_h, ISE_l_h)
formattable(tabKDE)
formattable(tabH)

```

