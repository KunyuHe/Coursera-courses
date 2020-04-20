---
  title: "R Project Example-Ames Housing Price Prediction, Kunyu HE"
output:
  html_document:
  code_folding: hide 
pandoc_args: [
  "--number-sections",
  ]
---
 
* * *
  
As an assumed consultant, I was asked to work on the given data sets `ames_train` to build a Multiple Linear Regression Model (MLR) to predict sale price of houses in Ames, IA. The whole project contains five parts and several sections under each of them.  

As a start, load relevant data set `ames_train`

```{r load, message = FALSE}
load("ames_train.Rdata")
```

Use the code block below to load necessary packages.

```{r packages, message = FALSE}
library(statsr)
library(dplyr)
library(ggplot2)
library(BAS)
library(GGally)
library(car)
library(conover.test)
library(MASS)
```

* * *
  
  # Part 1 - Exploratory Data Analysis (EDA)
  
  * * *
  * * *
  
  ## Data Set Structure and the Dependent Variable
  
  * * *
  
  Explore the data set `ames_train` by pringting a summary table as a start.

```{r data_set_summary}
dim(ames_train)
str(ames_train)
```

As observed, there are 1000 observations for 81 variables in the data set. It contains information from the Ames Assessor??s Office used in computing assessed values for individual residential properties sold in Ames, IA from 2006 to 2010, including sale price of each house along with many of its physical features[1].

*([1]Source: Codebook for Ames Housing, https://ww2.amstat.org/publications/jse/v19n3/decock/datadocumentation.txt, accessed at 09/15/2017)*
  
  Among the variables, the one of interest is `price`, as we are interested in using the information to help assess whether the asking price of a house is higher or lower than the true value of the house. It's a numerical variable that denotes the sale price of houses in US dollars. Perform a summary and several visualizations of the dependent variable `price`. 

```{r summary_statistics_price}
summary(ames_train$price)
ggplot(ames_train, aes(x=price)) + geom_histogram(binwidth = 10000,fill = "Steelblue", color="brown") + scale_x_continuous(name = "Price of Houses", breaks = seq(0,650000, 50000)) + ggtitle("Frequency Distribution of Price", subtitle = "(pre-adjustment)") + theme(plot.title = element_text(hjust=0.5, vjust=3)) + theme(plot.subtitle = element_text(hjust=0.5, vjust=3))
```

The distribution of `price` is highly right-skewed, with a sample mean approximately $21,700 larger than the smaple median. Hence, consider transforming it into log-scale and create a new variable `log_price` correspondingly.

```{r log_transform_price}
ames_train <- ames_train %>%
mutate(log_price=log(ames_train$price))
ggplot(ames_train, aes(x=log_price)) + geom_histogram(binwidth = 0.1,fill = "Yellow", color= "brown") + xlab("Log(price) of Houses") + ggtitle("Frequency Distribution of Log(price)", subtitle = "(post-adjustment)") + theme(plot.title = element_text(hjust=0.5, vjust=3)) + theme(plot.subtitle = element_text(hjust=0.5, vjust=3))
```

__When we refer to sales price of houses as dependent variable for estimation, use `log_price` instead of `price`.__

* * *

## Elimination of Variables and Outliers

* * *

Some variables in `ames_train` have `"NA"` values, we should eliminate those with too many in order to ensure the efficiency of our model. The following chunk list the top 10 variables with the most `"NA"s`. 

```{r most_NAs}
most_NA_10 <- colSums(is.na(ames_train))
most_NA_10 <- sort(most_NA_10, decreasing = TRUE)
head(most_NA_10,10)
rm(most_NA_10)
```

Remove `Pool.QC`, `Misc.Feature`, `Alley`, `Fence`, `Fireplace.Qu`, and `Lot.Frontage`. Also, remove their relevant variables, in other words, variables that take value `"0"` all the time given frequent `"NA"` values of `Pool.QC`, `Misc.Feature`, and `Fireplace.Qu`. These variables include `Fireplaces`, `Misc.Val`, and `Pool.Area`.

From the summary table above, variable `utilities` has only one value `"AllPub"`, which makes it irrelevant for having any possible impacts on `price` or `log_price`, so we also remove that from the data set.

```{r remove_variables}
ames_train$Pool.QC <- NULL
ames_train$Misc.Feature <- NULL
ames_train$Alley <- NULL 
ames_train$Fence <- NULL
ames_train$Fireplace.Qu <- NULL 
ames_train$Lot.Frontage <- NULL
ames_train$Fireplaces <- NULL
ames_train$Misc.Val <- NULL
ames_train$Pool.Area <- NULL
ames_train$Utilities <- NULL
```

According to special notes in the codebook, there are 5 observations that should be eliminated from the data set. Three of them are true outliers (partial sales that likely don??t represent actual market values), and two of them are simply unusual sales (very large houses priced relatively appropriately) [2]. As recommended, remove any houses with more than 4000 square feet from the data set.

*([2]Source: Codebook for Ames Housing, https://ww2.amstat.org/publications/jse/v19n3/decock/datadocumentation.txt, accessed at 09/15/2017)*

```{r remove_outliers}
ames_train <- ames_train %>%
filter(area<4000)
```

It's also suggested that effective observations typically do not include those take abnormal values for the variable `Sale.Condition`, such as foreclosures, or adjoining land purchase, or allocation, or family sales. Check their frequency and exclude the observations with abnormal values for `Sale.Condition`.

```{r remove_observations}
ggplot(ames_train, aes(x=area/100, y= log_price)) + geom_point(size=1, shape=21) + xlab("Area of Houses") + ylab("Log(price)") + facet_grid(.~ Sale.Condition) + ggtitle("Log(price) against Area", subtitle="(grouped by Sale.Condition)") + theme(plot.title = element_text(hjust=0.5, vjust=3)) + theme(plot.subtitle = element_text(hjust=0.5, vjust=3))
ames_train <- ames_train %>% 
  filter(Sale.Condition=="Normal"|Sale.Condition=="Partial")
dim(ames_train)
```

__After elimination of variables, observations and outliers, the final version of data set `ames_train` to work with contains 915 observations of 72 variables.__

* * *
  
  ## Log(price) versus House Area
  
  * * *
  
  It's natural to expect a positive correlation between sale price of houses and their area. Plot these variables to check.

```{r log(price)_against_area}
ggplot(ames_train, aes(x=area, y=log_price)) + geom_point(size=1.5, shape=23) + scale_x_continuous(name = "Area of Houses", breaks = seq(0,5000, 500)) + scale_y_continuous(name = "Log(price)")+ ggtitle("Log(price) against Area") + theme(plot.title = element_text(hjust=0.5, vjust=3))
```

The graph above show a slight trend towards fan-like distribution as area of houses increases over 2,000 square feet. Make a summary and some visualizations to further check.

```{r summary_statistics_area}
summary(ames_train$area)
ggplot(ames_train, aes(x=area)) + geom_histogram(binwidth = 250,fill = "Steelblue", color="brown") + scale_x_continuous(name = "Area of Houses", breaks = seq(0,5000, 500)) + ggtitle("Frequency Distribution of Area", subtitle = "(pre-adjustment)") + theme(plot.title = element_text(hjust=0.5, vjust=3)) + theme(plot.subtitle = element_text(hjust=0.5, vjust=3))
```

Likewise, the distribution of `area` is highly right-skewed, with a sample mean approximately 65 square feet larger than the smaple median. Hence, consider transforming it into log-scale and create a new variable `log_area` correspondingly.

```{r log_transformation_area}
ames_train <- ames_train %>%
mutate(log_area=log(ames_train$area))
ggplot(ames_train, aes(x=log_area)) + geom_histogram(binwidth = 0.1,fill = "Yellow", color= "brown") + xlab("Log(area) of Houses") + ggtitle("Frequency Distribution of Log(area)", subtitle = "(post-adjustment)") + theme(plot.title = element_text(hjust=0.5, vjust=3)) + theme(plot.subtitle = element_text(hjust=0.5, vjust=3))
```

__When we refer to area of houses as independent variable for estimation, use `log_area` instead of `area`__.

```{r log(price)_against_log(area)}
ggplot(ames_train, aes(x=log_area, y=log_price)) + geom_point(size=1.5, shape=23) + scale_x_continuous(name = "Log(area)") + scale_y_continuous(name = "Log(price)")+ ggtitle("Log(price) against Area") + theme(plot.title = element_text(hjust=0.5, vjust=3))
```

__Undoubtedly there is a linear association, in other words correlation between the two calculated variables. We should definitely take `log_area` into our model.__

* * *

## Log(price) versus Neiborhood

* * *

In the last EDA project (for Week 4), we've scrutinized the relationship between sale price of houses and neighborhood categories, while using median of `log_price` to measure the neighborhood sale price of houses "on average". Use box plot to visualize the potential association in order.

```{r log(price)_against_neighborhood}
log_price_neighbor <- order(as.numeric(by(ames_train$log_price, ames_train$Neighborhood, median)))
ames_train$Neighborhood <- ordered(ames_train$Neighborhood, levels=levels(ames_train$Neighborhood)[log_price_neighbor])
ggplot(ames_train, aes(y=log_price, x=Neighborhood)) + geom_boxplot() +ylab("Log(price)")+ ggtitle("Log(price) against Neighborhood", subtitle="(ordered by median of log(price))") + theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust=0.5, vjust=3), plot.subtitle = element_text(hjust=0.5, vjust=3))
rm(log_price_neighbor)
```

The box plot shows how log(price) increases across neighborhoods. The neighborhoods are ordered by the median for log(price) of house sold within them. By median of log(price), the most "expensive" neighborhood is *Stone Brook*, and the cheapest is *Meadow Village*. 

In order to test whether the two variables are dependent, in other words whether the median of log(price) significantly changes across neighborhoods, conduct a Kruskal-Wallis test. 

Check for the conditions of the test first. As a non-parametric test, it doesn't assume normal distribution of each grouped population, but it assumes that different groups have the same distribution, and groups with different standard deviations have different distributions; in other words, homoscedasticity. In order to test for homoscedasticity, use a Levene??s test in advance. The null hypothesis is that the standard deviations across neighborhoods are identical.

```{r log_price_against_neighborhood_Levene}
leveneTest(log_price ~ Neighborhood, data=ames_train)
```

As the test results shows, p-value turns out to be nearly zero (2.471e-12). Hence we reject the null hypothesis, and conclude that at least a pair of distributions of log(price) in neighborhoods have heteroscedasticity problems, at a significance level of 5%.

```{r log_price_against_neighborhood_Krusal}
kruskal.test(log_price~Neighborhood, data=ames_train)
```

As the test results shows, p-value turns out to be nearly zero (2.2e-16). Hence we reject the null hypothesis, and conclude that a significance level of 5%, the log(price) of houses in ames across at least 2 out of 28 neiborhoods from *Meadow Village* to *Stone Brook* are non-identical populations.

__However, Kruskal-Wallis test is very vulnerable to heteroscedasticity across groups. With its assumption of homoscedasticity violated, we should be really careful to apply the conclusions to our sample.__ It's not encouraged to further conduct a Conover-Iman test to find out which pair of populations diverge. 

In conclusion for 1.4, the median of log(price) might differ across neiborhoods, but as heteroscedasticity exsists between the distributions of log(price) of least two neighborhoods, at a significance level of 5%, we should be really careful to apply the conclusion of Kruskal-Wallis test that mean ranks, or medians, are not the same across the distributions of log(price) of at least two neighborhoods, at a significance level of 5%. 

To be brief and direct, __we shouldn't include `Neighborhood` in our model to predict `log_price`, as long as there's no better statistical tools than Kruskal-Wallis test, to overcome heteroscedasticity, suggested by Levene??s test, across at least a pair of distribution of log(price) of two neighborhoods at a significance level of 5%.__

* * *
  
  ## Log(price) versus Basement Condition
  
  * * *
  
  Large houses tend to be more expensive, but for houses with basements, the quality of their basements is of concern. During rigid winters of Canada, a nice basement can provide residences of the house a warm and dry place to stay. One of the most interesting results of EDA process is about the association between `log_price` and `Bsmt.Cond`, when the latter refers to "Basement Condition".

`Bsmt.Cond` is a categorical variable with multiple categories, extend the abbreviations to present the potential association better.

```{r extend_abbreviation}
levels(ames_train$Bsmt.Cond) <- c("No Basement","Excellent", "Fair", "Good",  "Poor", "Typical")
ames_train$Bsmt.Cond <- as.character(ames_train$Bsmt.Cond)
ames_train$Bsmt.Cond[is.na(ames_train$Bsmt.Cond)] <- "No Basement"
ames_train$Bsmt.Cond <- factor(ames_train$Bsmt.Cond)
```
```{r log_price_against_basement_condition}
ggplot(data = ames_train, aes(x = Bsmt.Cond, y = log_price, fill = Bsmt.Cond)) + geom_boxplot(outlier.color = "purple") + ggtitle("Log(price) against Basement Condition")+ xlab("Basement Condition of Houses")+ylab( "Log(price)") + theme(axis.text.x = element_text(angle = 45, size = 8), plot.title = element_text(hjust=0.5, vjust=3))
```

The visulization is impressive in that it somehow contradicts our initial guess. Make a summary of the variable `Bsmt.Cond`.

```{r Bsmt.Cond_summary}
summary(ames_train$Bsmt.Cond)
```

Now perform a Levene??s test in advance, to test the homoscedasticity across groups. The null hypothesis is that the standard deviations across basement condition groups are identical.

```{r log_price_against_Bsmt.Cond_Levene}
leveneTest(log_price ~ Bsmt.Cond, data=ames_train)
```

As the test results shows, p-value turns out to be large enough (>0.05). Hence we fail to reject the null hypothesis, and conclude that our sample `ames_Bsmt` fail to provide convincing evidence to refuse that the standard deviations across basement condition groups are identical. __In other words, there is no significant heteroscedasticity problem for our sample__. 

Conditions of Kruskal-Wallis rank sum test satisfied, we can proceed to test the null hypothesis that mean ranks, or medians, are the same across basement condition groups. 

```{r log_price_against_basement_Krusal}
kruskal.test(log_price~Bsmt.Cond, data=ames_train)
```

The reported p-value turns out to be nearly zero (1.67e-09). Hence we reject the null hypothesis, and conclude that mean ranks, or medians, are not the same across the distributions of log(price) of at least two basement condition groups, at a significance level of 5%.

With a corresponding Kruskal-Wallis null hypothesis rejected, further conduct a Conover-Iman test, with Bonferroni adjustments to p-values, to find out which pair of groups significantly diverge.

```{r log_price_against_basement_Conover}
conover.test(ames_train$log_price, g=ames_train$Bsmt.Cond, method="bonferroni", label=TRUE,  wrap=TRUE, table=TRUE)
```

From the test report above, we can conclude that at a significance level of 5%, 
distributions of log(price) significantly diverge between paired populations of basements categorized `"Good"` and `"Fair"`, `"Good"` and `"No Basement"`, `"Typical"` and `"Fair"`, as well as `"Typical"` and `"No Basement"`.

__In other words, if we are to take `Bsmt.Cond` as an independable variable into our model to predict `log_price`, we should eliminate the categories of `"Poor"` and `"Excellent"`, as they make no significant contribution to the variation of `log_price`.__

```{r eliminate_categories}
ames_train <- ames_train %>%
  filter(Bsmt.Cond != "Poor" & Bsmt.Cond != "Excellent")
```


* * *
  
  # Part 2 - Development and assessment of an initial model
  
  * * *
  * * *
  
  ## Initial Model
  
  * * *
  
  The independent variable is `log_price`. Additionally, based on the discussion from __Section 1.3__ to __Section 1.5__, __we are sure to take `log_area` and `Bsmt.Cond` as independent variables__ *(find out the reasons there)*. The following paragraphs list other independent variables to include in our initial model and briefly explain why each of them should be included. 

We would consider `Lot.Area` as an important predictor, in that we expect houses with larger lot areas to, on average, have higher sale prices. We should transform it into log-scale for the same reason as for `Area`. 

In addition, we would take the age of house into our model, as there could be a negative relationship between age of houses and their price. As long as there is no exsiting variable, we need to create a caculated variable `Age`. It's a reasonable proxy for the condition of houses, `Overall.Cond` in other words, either.

We are going to take `Garage.Cars` as a predictor, as consumers will need a place for their cars and therefore parking capacity of garages should be valued.

Consumers will value the number of bedrooms above ground, as people are less likely to sleep underground. Hence `Bedroom.AbvGr` seems to be something they look for. 

Likewise, consumers will also value the number of bathrooms above grade, especially full-bathrooms. We would consider half-bathrooms as half of a full-bathroom, and calculate a new variable named `Bathroom`.

In addition, as a substitue of `Neighborhood`, for which we decided not to include for the initial model, we take `MS.Zoning` as a predictor to scrutinize whether surroundings and residential density make a difference.

For the last two variables, consider `Bldg.Type` and `Central.Air`. Building type matters for households, and single-family detached houses are expected to have higher sale prices. For the latter, central air conditioning makes houses comfortable in summer and winter. 

```{r calculate_variables}
ames_train$log_Lot.Area <- log(ames_train$Lot.Area)
ames_train$Age <- ames_train$Yr.Sold - ames_train$Year.Built
ames_train$Bathroom <- ames_train$Full.Bath + (ames_train$Half.Bath/2)
```

Also, extend the abbreviations for `MS.Zoning`, `Central.Air`, and `Bldg.Type` in favor of presentation.

```{r extend_abbreviation_2}
levels(ames_train$MS.Zoning) <- c("Agriculture","Commercial", "Village Residential", "Industrial",  "R High Density", "R Low Density", "R Medium Density")
levels(ames_train$Central.Air) <- c("Not Central Air", "Central Air")
levels(ames_train$Bldg.Type) <- c("Single Detached","Two Conversion", "Duplex", "End Unit",  "Inside Unit")
ames_train <- ames_train %>%
filter(MS.Zoning != "Industrial")
```

In short, the dependent variable is `log_price`, and the 10 independent variables include `log_area`, `Bsmt.Cond`, `log_Lot.Area`, `Age`, `Bathroom`, `Garage.Cars`, `Bedroom.AbvGr`, `MS.Zoning`, `Central.Air`, and `Bldg.Type`.

Following is the initial MLR model and a summary table for it.

```{r initial model}
initial_m <- lm(log_price ~ log_area + Bsmt.Cond + log_Lot.Area + Age + Bathroom + Garage.Cars + Bedroom.AbvGr + MS.Zoning + Central.Air + Bldg.Type, data = ames_train)
summary(initial_m)
```


* * *

## Model Selection

* * *

The initial model seems to work well. Now use stepwise AIC test to conduct a model selection.

```{r model_selection_AIC}
AIC_m <- stepAIC(initial_m, k = 2)
summary(AIC_m)
```

Stepwise AIC test suggests that we should drop no variable from the model. So the resulting model `AIC_m` keeps identical with the initial model. 

Continue with a stepwise BIC test and set `k` to `log(n)` where `n` denotes the observations in our initial model, which is 912.

```{r model_selection_BIC}
BIC_m <- stepAIC(initial_m, k= log(911))
summary(BIC_m)
```

Stepwise BIC test suggests that we should drop `Bldg.Type` from the initial model. __However, as the summary above shows, the adjusted R-squared falls when we do so. Keep with the initial model, or the `AIC_m`.__

```{r remove}
rm(initial_m, BIC_m)
```

* * *

## Initial Model Residuals

* * *

```{r model_resid}
layout(matrix(c(1,2,3,4),2,2))
plot(AIC_m)
```

The "Residual vs Fitted" plot shows almost no pattern in distribution of the residuals, which denotes there is no non-linear relationship between the aggregate of the predictors and the dependent variable.

The "Scale-Location" plot confirms that there is no heteroscedasticity problem. In other words, residuals are equally spread along the predicted values.

The "Normal Q-Q" plot shows a slight deviation, at the upper right corner, from normality in the distribution of the residuals. Non-normal residuals might decrease the credibility of predictions based on the assumption of normal residuals. It also indicates a potential presence of outliers.

According to the "Residuals vs Leverage" plot, none of the observations is beyond cook??s distance lines. However, there are potential influential outliers with relatively high absolute residual and high leverage. For example, __observation 670__, which may hamper the prediction power of our model.

* * *

## Initial Model RMSE

* * *

Calculate the RMSE value for `AIC_m`.

```{r RMSE_AIC_m}
AIC_m_resid <- ames_train$price - exp(predict(AIC_m, ames_train))
AIC_m_RMSE <- sqrt(mean(AIC_m_resid^2))
AIC_m_RMSE
```

RMSE value for `AIC_m` is $34,350, approximately. 

Less than $100,000, RMSE for our model seems reasonable.

* * *

## Overfitting 

* * *

The `AIC_m` model may do well on training data but perform poorly out-of-sample, in other words, on a dataset other than the original training data, for the model is overly-tuned to specifically fit the training data. 

To determine whether overfitting is occurring, compare the performance of `AIC_m` on both in-sample and out-of-sample data sets. Use `ames_test` as an out-of-sample data set.

```{r loadtest, message = FALSE}
load("ames_test.Rdata")
```

* * *

Before we get down to work with `ames_test`, we should calculate variables, level variables, and exclude obsevations same to what we've done on `ames_train`. We don't have to exclude any variables this time, as those excluded for `ames_train` were not taken as predictors for `AIC_m`.

```{r caculate_relabel_and_eliminate}
ames_test $log_price<- log(ames_test$price)
ames_test $log_area <- log(ames_test$area)
ames_test$log_Lot.Area <- log(ames_test$Lot.Area)
ames_test$Age <- ames_test$Yr.Sold - ames_test$Year.Built
ames_test$Bathroom <- ames_test$Full.Bath + (ames_test$Half.Bath/2)
levels(ames_test$Bsmt.Cond) <- c("No Basement", "Excellent", "Fair", "Good",  "Poor", "Typical")
ames_test$Bsmt.Cond <- as.character(ames_test$Bsmt.Cond)
ames_test$Bsmt.Cond[is.na(ames_test$Bsmt.Cond)] <- "No Basement"
ames_test$Bsmt.Cond <- factor(ames_test$Bsmt.Cond)
levels(ames_test$MS.Zoning) <- c("Agriculture","Commercial", "Village Residential", "Industrial",  "R High Density", "R Low Density", "R Medium Density")
levels(ames_test$Central.Air) <- c("Not Central Air", "Central Air")
levels(ames_test$Bldg.Type) <- c("Single Detached","Two Conversion", "Duplex", "End Unit",  "Inside Unit")
ames_test <- ames_test %>%
filter(MS.Zoning != "Industrial")
ames_test <- ames_test %>%
mutate(log_price=log(ames_test$price))
ames_test <- ames_test %>%
filter(area<4000)
ames_test <- ames_test %>%
filter(Bsmt.Cond != "Poor" & Bsmt.Cond != "Excellent")
```

Calculate RMSE value and coverage probability for `AIC_m` on `ames_test`.

```{r RMSE_coverage_AIC_m_test}
AIC_m_resid_test <- ames_test$price - exp(predict(AIC_m, ames_test))
AIC_m_RMSE_test <- sqrt(mean(AIC_m_resid_test^2))
AIC_m_RMSE_test
predict.test <- exp(predict(AIC_m, ames_test, interval = "prediction"))
coverage.prob.test <- mean(ames_test$price > predict.test[,"lwr"] &
ames_test$price < predict.test[,"upr"])
coverage.prob.test
```

The RMSE value is even lower for the test data set, at the coverage probability is higher than 0.95, which in combination means that our model perform even better for the test data set. __In conclusion, our model can pass the overfitting test.__

* * *

# Part 3 Development of a Final Model

* * *

* * *

## Final Model

* * *

For the final model, further include the variable `Land.Contour`, which indicates the flatness of the property; `Land.Slope`, which indicates the slope of property; `Overall.Qual`, which indicates the overall rates of material and finish of the house; `Overall.Cond`, which indicates the overall condition of the house; `Heating.QC`, which denotes the heating quality; `Foundation`, which implies the type of foundation; `House.Style`, which indicates the style of dwelling.

Fit the final model and make a summary table.

```{r final_model}
final_m <- lm(log_price ~ log_area + Bsmt.Cond + log_Lot.Area + Age + Bathroom + Garage.Cars + Bedroom.AbvGr + MS.Zoning + Central.Air + Bldg.Type + House.Style + Overall.Cond + Overall.Qual + Land.Slope + Land.Contour + Foundation + Heating.QC, data = ames_train)
summary(final_m)
```

Conduct a stepwise AIC and BIC test for model selection. Calculate the RMSE value for each on `ames_train`. Also, calculate their RMSE value on `ames_test` as a measure of their performance on the test data set. Notice that as `House.Style` in `ames_test` has a new level of `"2.5Fin"`, we need to filter it out for comparison before caculating RMSE.

```{r filter_2.5Fin}
ames_test <- ames_test %>%
filter(House.Style != "2.5Fin")
```


```{r final_AIC_m}
final_AIC_m <-stepAIC(final_m,k=2)
summary(final_AIC_m)
final_AIC_m_resid <- ames_train$price - exp(predict(final_AIC_m, ames_train))
final_AIC_m_RMSE <- sqrt(mean(final_AIC_m_resid^2))
final_AIC_m_RMSE
final_AIC_m_resid_test <- ames_test$price - exp(predict(final_AIC_m, ames_test))
final_AIC_m_RMSE_test <- sqrt(mean(final_AIC_m_resid_test^2))
final_AIC_m_RMSE_test
```

__For the AIC approach, `Central.Air`, `Heating.QC`, `Foundation` are dropped from the model; the adjusted R-squared is 0.915; the RMSE value on `ames_train` is $25,432; the RMSE value on `ames_test` is $22,723.__  

```{r final_BIC_m}
final_BIC_m <-stepAIC(final_m,k=log(911))
summary(final_BIC_m)
final_BIC_m_resid <- ames_train$price - exp(predict(final_BIC_m, ames_train))
final_BIC_m_RMSE <- sqrt(mean(final_BIC_m_resid^2))
final_BIC_m_RMSE
final_BIC_m_resid_test <- ames_test$price - exp(predict(final_BIC_m, ames_test))
final_BIC_m_RMSE_test <- sqrt(mean(final_BIC_m_resid_test^2))
final_BIC_m_RMSE_test
```

__For the BIC approach, `Central.Air`, `Heating.QC`, `Foundation`,`Land.Contour`, `Bldg.Type`, `Land.Slope`, `Bsmt.Cond`, `Bathroom`  are dropped from the model; the adjusted R-squared is 0.911; the RMSE value on `ames_train` is $26,462; the RMSE value on `ames_test` is $23020.__  

Compare the two model after selection. `final_AIC_m` has higher adjusted R-squared, lower `RMSE` on both `ames_train` and `ames_test` against `final_BIC_m`. Obviously we should choose `final_AIC_m` as our final model. 

* * *

## Transformation

* * *

Previously, `price` was transformed to `log_price` as the distribution is highly right-skewed; for the same reason, `area` and `Lot.Area` are transformed to `log_area`, `log_Lot.Area` respectively.

* * *

## Variable Interaction

* * *

Variable interactions refers to significant correlations between numerical varaibles and strong associations between categorical varaibles; it also include significant relationship between numerical and categorical variables, respectively. Show the potential interactions in the data set `ames_train` as examples.

The former is a example for interaction between numerical variables, `log_area` against `log_Surface_Area`, when the latter is a calculated variable by adding first floor and second floor area in square feet and tranform it to log-scale. The two variables both despict above ground area of houses.

```{r potential_interactions_numerical}
ames_train$log_Surface_Area = log(ames_train$X1st.Flr.SF+ames_train$X2nd.Flr.SF)
ggplot(data = ames_train, aes(x = log_Surface_Area, y = log_area))+ geom_jitter(size=1, shape=3, color="red") + ggtitle("Log(area) against Log(surface area)", subtitle="(interaction between numerical variables)") + xlab("Log(surface area)") + ylab("Log(area)") + theme(plot.title = element_text(hjust=0.5, vjust=3), plot.subtitle = element_text(hjust=0.5, vjust=3))
```

The latter is a example for interaction between categorical variables, `Bsmt.Cond` against `Bsmt.Qual`, both of them depict the overall condition of the basements.

```{r potential_interactions_categorical}
levels(ames_train$Bsmt.Qual) <- c("No Basement","Excellent", "Fair", "Good",  "Poor", "Typical")
ames_train$Bsmt.Qual <- as.character(ames_train$Bsmt.Qual)
ames_train$Bsmt.Qual[is.na(ames_train$Bsmt.Qual)] <- "No Basement"
ames_train$Bsmt.Qual <- factor(ames_train$Bsmt.Qual)
ames_train <- ames_train %>%
filter(Bsmt.Qual != "Excellent" & Bsmt.Qual != "Poor") 
ggplot(data = ames_train, aes(x = Bsmt.Qual, y =Bsmt.Cond))+ geom_jitter(size=1, shape=3, color="steelblue") + ggtitle("Basement Condition against Basement Quality", subtitle="(interaction between categorical variables)") + xlab("Basement Quality of Houses") + ylab("Basement Condition") + theme(plot.title = element_text(hjust=0.5, vjust=3), plot.subtitle = element_text(hjust=0.5, vjust=3))
```

__It's obvious that when the model includes pairs of variables that tend to describe a particular feature of houses, there might be variable ineraction problems. However, we didn't take such pairs of variables into the final model fortunately.__

* * *

## Variable Selection

* * *

By EDA process, we define the possible correlations among variables and `log_price`, and undoubtedly include numerical variables that have strong correlations with it. 

With Kruskal-Wallis rank sum test, we define the possible associations among categorical variables and `log_price`, and take those in the final model. However, we excluded some variables, for example `Neighborhood`, in order to aviod uncertainty in estimations due to heteroscedasticity.

We also include those possibly contribute to the dependent variable intuitively.

* * *

## Model Testing


* * *

Originally, test data set allowed us to identify an issue my model was having: including `"NA"` values which would cause significant problems with the final model??s accuracy. This issue was addressed, using the test data set allowed us to ensure that my model was not over-fitted, which is covered further along in the analysis.

* * *

# Part 4 Final Model Assessment

* * *
* * *

##  Final Model Residual

* * *



```{r residual_final_model}
layout(matrix(c(1,2,3,4),2,2))
plot(final_AIC_m)
```

The "Residual vs Fitted" plot shows almost no pattern in distribution of the residuals, which denotes there is no non-linear relationship between the aggregate of the predictors and the dependent variable.

The "Scale-Location" plot confirms that there is no heteroscedasticity problem. In other words, residuals are equally spread along the predicted values.

The "Normal Q-Q" plot shows a slight deviation, at the upper right corner, from normality in the distribution of the residuals. Non-normal residuals might decrease the credibility of predictions based on the assumption of normal residuals. It also indicates a potential presence of outliers.

According to the "Residuals vs Leverage" plot, none of the observations is beyond cook??s distance lines. However, there are potential influential outliers with relatively high absolute residual and high leverage. For example, __observation 670__, which may hamper the prediction power of our model.

The __observation 717__ has a high leverage (approximately one), but the residual for that outlier is relatively low (within Cook's distance line), so it's just a leverage outlier.

* * *

## Final Model RMSE

* * *

```{r final_AIC_m_RMSE}
final_AIC_m_RMSE
```

RMSE value of the final model on `ames_train` was calculated above, it's around $25,432.

Less than $100,000, RMSE for our final model seems reasonable.

* * *
  
  ## Final Model Evaluation
  
  
  * * *
  
  Compare the two models after selection. `final_AIC_m`, which is our final model, has higher adjusted R-squared, lower `RMSE` on both `ames_train` and `ames_test` against `final_BIC_m`. It has exactly the same advantages over our initial model, `AIC_m`, too.  

For the weakness part, the adjusted R-squared, compared with that of the one before stepwise AIC test, is lower. Moreover, not including the variable `Neighborhood`due to potential heteroscedasticity problem might be excessively careful and a unnecessary sacrifice, for a model particular for AMES. 

* * *
  
  ## Final Model Validation
  
  Test our final model on a separate, validation data set to determine how the model perform in real-life practice. Load the data set.

```{r loadvalidation, message = FALSE}
load("ames_validation.Rdata")
```

* * *
  
  Likewise, we should calculate variables, level variables, and exclude obsevations same to what we've done on `ames_test`. We don't have to exclude any variables this time, as those excluded for `ames_test` were not taken as predictors for `final_AIC_m`.

```{r caculate_relabel_and_eliminate_2}
ames_validation $log_price<- log(ames_validation$price)
ames_validation $log_area <- log(ames_validation$area)
ames_validation$log_Lot.Area <- log(ames_validation$Lot.Area)
ames_validation$Age <- ames_validation$Yr.Sold - ames_validation$Year.Built
ames_validation$Bathroom <- ames_validation$Full.Bath + (ames_validation$Half.Bath/2)
levels(ames_validation$Bsmt.Cond) <- c("No Basement", "Excellent", "Fair", "Good",  "Poor", "Typical")
ames_validation$Bsmt.Cond <- as.character(ames_validation$Bsmt.Cond)
ames_validation$Bsmt.Cond[is.na(ames_validation$Bsmt.Cond)] <- "No Basement"
ames_validation$Bsmt.Cond <- factor(ames_validation$Bsmt.Cond)
levels(ames_validation$MS.Zoning) <- c("Agriculture","Commercial", "Village Residential", "Industrial",  "R High Density", "R Low Density", "R Medium Density")
levels(ames_validation$Central.Air) <- c("Not Central Air", "Central Air")
levels(ames_validation$Bldg.Type) <- c("Single Detached","Two Conversion", "Duplex", "End Unit",  "Inside Unit")
ames_validation <- ames_validation %>%
  filter(MS.Zoning != "Industrial" & MS.Zoning != "Agriculture")
ames_validation <- ames_validation %>%
  mutate(log_price=log(ames_validation$price))
ames_validation <- ames_validation %>%
  filter(area<4000)
ames_validation <- ames_validation %>%
  filter(Bsmt.Cond != "Poor" & Bsmt.Cond != "Excellent")
ames_validation <- ames_validation %>%
  filter(House.Style != "2.5Fin")
```

Then, calculate RMSE and a coverage probability of `final_AIC_m` on `ames_validation`. Show the RMSE values for `final_AIC_m` on `ames_train` and `ames_test` for comparison.

```{r validate_final_AIC_m_RMSE}
final_AIC_m_resid_valid <- ames_validation$price - exp(predict(final_AIC_m, ames_validation))
final_AIC_m_RMSE_valid <- sqrt(mean(final_AIC_m_resid_valid^2))
final_AIC_m_RMSE_valid
final_AIC_m_RMSE
final_AIC_m_RMSE_test
predict.validation <- exp(predict(final_AIC_m, ames_validation, interval = "prediction"))
coverage.prob.validation <- mean(ames_validation$price > predict.validation[,"lwr"] &
                                   ames_validation$price < predict.validation[,"upr"])
coverage.prob.validation
```

As the report above shows, RMSE for `final_AIC_m` on `ames_validation` is even lower, at a level of $20,881, relatively lower than both of those on `ames_train` and `ames_test`. The coverage probability is significantly higher than 0.95, which in combination means that our final model perform even better for the test and validation data set. __In conclusion, our final model `final_AIC_m` can pass the overlifting test.__

Print a summary table for it in the following chunk in favor of interpretation.

```{r final_model_validated}
summary(final_AIC_m)
```

* * *
  
  # Part 5 Conclusion
  
  
  * * *
  
  In conclusion, the following things impact the price of houses in Ames: __area and lot area; age, overall condition of the house and the overall quality of the house; land slope and contour; building type and house style; number of bathrooms and bedrooms above ground; general zoning classification; parking capacity of garage; basement condition; as well as heating quality.__
