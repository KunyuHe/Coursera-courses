---
title: "Statistical Inference Quiz 4"
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

# t-test on Paired Data


```r
q1 <- data.frame(baseline = c(140, 138, 150, 148, 135), 
                 week2 = c(132, 135, 151, 146, 130))
with(q1, t.test(week2, baseline, alternative = "two.sided", paired = T))
```

```
## 
## 	Paired t-test
## 
## data:  week2 and baseline
## t = -2.2616, df = 4, p-value = 0.08652
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -7.5739122  0.7739122
## sample estimates:
## mean of the differences 
##                    -3.4
```

```r
# or
# with(q1, t.test(week2 - baseline, alternative = "two.sided", paired = T))
```

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

# Binomial Test


```r
# Coke as 1 and Pepsi as 0
binom.test(x = 3, n = 4, alternative = "greater")
```

```
## 
## 	Exact binomial test
## 
## data:  3 and 4
## number of successes = 3, number of trials = 4, p-value = 0.3125
## alternative hypothesis: true probability of success is greater than 0.5
## 95 percent confidence interval:
##  0.2486046 1.0000000
## sample estimates:
## probability of success 
##                   0.75
```

# Poisson Test


```r
poisson.test(10, 1787, r = 0.01, alternative = "less")
```

```
## 
## 	Exact Poisson test
## 
## data:  10 time base: 1787
## number of events = 10, time base = 1787, p-value = 0.03237
## alternative hypothesis: true event rate is less than 0.01
## 95 percent confidence interval:
##  0.000000000 0.009492009
## sample estimates:
##  event rate 
## 0.005595971
```

```r
# or ppois(10, lambda = 0.01 * 1787) where lamda = rate*monitoring time (entities)
```

# P-value for Comparing Statistic of two Samples


```r
mu1 <- -3
sd1 <- 1.5
n1 <- 9

mu2 <- 1
sd2 <- 1.8
n2 <- 9

sp.sq <- ((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2)
t <- (mu1-mu2)/(sqrt(sp.sq)*sqrt(1/n1+1/n2))
2*pt(t, n1+n2-2)
```

```
## [1] 0.0001025174
```

# Calculate Power with Known Statistics(mean, sd, n)


```r
power.t.test(n = 100, delta = 0.01, sd = 0.04,  type= "one.sample" , alt= "one.sided" )
```

```
## 
##      One-sample t test power calculation 
## 
##               n = 100
##           delta = 0.01
##              sd = 0.04
##       sig.level = 0.05
##           power = 0.7989855
##     alternative = one.sided
```

# Calculate Minimum Sample Size with Known Statistics(mean, sd, power)


```r
power.t.test(power = 0.9, delta = .01, sd = .04,  type= "one.sample" , alt= "one.sided" )
```

```
## 
##      One-sample t test power calculation 
## 
##               n = 138.3856
##           delta = 0.01
##              sd = 0.04
##       sig.level = 0.05
##           power = 0.9
##     alternative = one.sided
```

