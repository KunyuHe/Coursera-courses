---
title: "Statistical Inference Quiz 3"
author: "Quinn He"
date: "04/28/2018"
output:
        prettydoc::html_pretty:
          theme: architect
          highlight: github
          keep_md: true
          pandoc_args: [
                 "--number-sections",
          ]
---

# Calculating t Confidence Interval


```r
mu <- 1100
sd <- 30
n <- 9
cl <- 0.95
error <- qt(1-(1-cl)/2, df = n-1)*sd/sqrt(n)
left <- round(mu - error, digits = 3)
right <- round(mu + error, digits = 3)
print(paste("[", left, " , ", right, "]", sep = ""), quote = F)
```

```
## [1] [1076.94 , 1123.06]
```

# Calculating Other Parameters for a t-distributed RV


```r
mu <- -2
n <- 9
cl <- 0.95
right <- 0
erro <- right - mu
sd_min <- erro*sqrt(n)/qt(1-(1-cl)/2,df = n-1)
round(sd_min, digits = 3)
```

```
## [1] 2.602
```

# Calculating t Confidence Interval for Comparing Means


```r
mu1 <- 3
sv <- 0.60
n1 <- 10

mu2 <- 5
sv <- 0.68
n2 <- 10

cl <- 0.95

# pooled variance estimator
sp.sq <- ((n1-1)*sv + (n2-1)*sv)/(n1+n2-2)
error <- qt(1-(1-cl)/2, df = (n1+n2-2))*sqrt(sp.sq)*sqrt(1/n1+1/n2)
left <- round((mu1 - mu2) - error, digits = 3)
right <- round((mu1 - mu2) + error, digits = 3)
print(paste("[", left, " , ", right, "]", sep = ""), quote = F)
```

```
## [1] [-2.775 , -1.225]
```

# Calculating Normal Confidence Interval for Comparing Means


```r
mu1 <- -3
sd1 <- 1.5
n1 <- 9

mu2 <- 1
sd2 <- 1.8
n2 <- 9

cl <- 0.90

# pooled variance estimator
sp.sq <- ((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2)
error <- qt(1-(1-cl)/2, df = (n1+n2-2))*sqrt(sp.sq)*sqrt(1/n1+1/n2)
left <- round((mu1 - mu2) - error, digits = 3)
right <- round((mu1 - mu2) + error, digits = 3)
print(paste("[", left, " , ", right, "]", sep = ""), quote = F)
```

```
## [1] [-5.364 , -2.636]
```

