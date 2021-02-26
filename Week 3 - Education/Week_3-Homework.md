Week 3 Homework, Unit 1
================
Lemi Daba
2/25/2021

``` r
# Some basic set-up
library(dummies)
library(AER)
library(tidyverse)

# Load the data
inpres <- read_csv("inpres_data_corrected.csv")
```

Question 1
----------

Run a regression of log monthly earnings on education. What is the
estimated impact of an extra year of education on log monthly earnings?

### Answer

``` r
lm(log_wage ~ education, data = inpres) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = log_wage ~ education, data = inpres)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5021 -0.3444  0.0533  0.3526  4.3300 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.402893   0.007123    1601   <2e-16 ***
    ## education    0.077033   0.000700     110   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6011 on 45622 degrees of freedom
    ## Multiple R-squared:  0.2098, Adjusted R-squared:  0.2097 
    ## F-statistic: 1.211e+04 on 1 and 45622 DF,  p-value: < 2.2e-16

Question 3
----------

First we will estimate the impact of the program on educational
attainment using difference in differences. People born in 1968 or later
were exposed to the program. Generate a dummy variable that takes the
value 1 if the person was born in these years and 0 otherwise. We will
construct a difference-in-differences table with four cells across
low/high program intensity and old/young (unexposed/exposed). Fill in
the table with the average education level for that group. Your table
should be similar to Table 3 Panel A in the paper. For now, do not worry
about standard errors. (Please round to the nearest hundredth. For
instance, 67.89 would be accepted if the correct answer is 67.8912.)

### Answer

``` r
summ <- inpres %>%
  mutate(born_aft68 = (birth_year >= 68)) %>%
  group_by(born_aft68, high_intensity) %>%
  summarise(mean_educ = mean(education))

summ
```

    ## # A tibble: 4 x 3
    ## # Groups:   born_aft68 [2]
    ##   born_aft68 high_intensity mean_educ
    ##   <lgl>               <dbl>     <dbl>
    ## 1 FALSE                   0      9.73
    ## 2 FALSE                   1      8.48
    ## 3 TRUE                    0     10.1 
    ## 4 TRUE                    1      8.94

``` r
X <- cbind(
  c(
    summ$mean_educ[4],
    summ$mean_educ[2],
    summ$mean_educ[4] - summ$mean_educ[2]
  ),
  c(
    summ$mean_educ[3],
    summ$mean_educ[1],
    summ$mean_educ[3] - summ$mean_educ[1]
  ),
  c(
    summ$mean_educ[4] - summ$mean_educ[3],
    summ$mean_educ[2] - summ$mean_educ[1],
    (summ$mean_educ[4] - summ$mean_educ[2]) - (summ$mean_educ[3] - summ$mean_educ[1])
  )
)

colnames(X) <- c(
  "High Intensity (Treatment)",
  "Low Intensity (Control)",
  "Difference (Treatment-Control)"
)

rownames(X) <- c(
  "**Young (Born 1968 or later)**",
  "**Old (Born before 1968)**",
  "**Difference (Young - Old)**"
)

knitr::kable(X, digits = 2)
```

|                                | High Intensity (Treatment) | Low Intensity (Control) | Difference (Treatment-Control) |
|:-------------------------------|---------------------------:|------------------------:|-------------------------------:|
| **Young (Born 1968 or later)** |                       8.94 |                   10.12 |                          -1.18 |
| **Old (Born before 1968)**     |                       8.48 |                    9.73 |                          -1.26 |
| **Difference (Young - Old)**   |                       0.46 |                    0.39 |                           0.08 |

Question 4
----------

Consider only the regions where many schools were build due to INPRES
(high intensity). What is the average difference in outcomes between
people who were young enough to take advantage of the schools and those
who were too old to benefit from the schools? (Please round to the
nearest hundredth).

*Thought exercise: Under what assumptions would this be an unbiased
estimate of the causal effect of the program on education? Do you think
this is an unbiased estimate in this case?*

### Answer

``` r
inpres %>% 
  mutate(born_aft68 = (birth_year >= 68)) %>% 
  filter(high_intensity == 1) %>% 
  group_by(born_aft68) %>% 
  summarise(avg_educ = mean(education) %>% round(2)) %>% 
  mutate( diff = (.$avg_educ[2] - .$avg_educ[1]) %>% round(2) )
```

    ## # A tibble: 2 x 3
    ##   born_aft68 avg_educ  diff
    ##   <lgl>         <dbl> <dbl>
    ## 1 FALSE          8.48  0.46
    ## 2 TRUE           8.94  0.46

Question 5
----------

Now consider only people who were young enough to be exposed to the
INPRES program. What is the average difference in education between such
people in regions with high and low program intensity? (Please round to
the nearest hundredth).

*Thought exercise: Under what assumptions would this be an unbiased
estimate of the causal effect of the program on education? Do you think
this is an unbiased estimate in this case?*

### Answer

``` r
inpres %>% 
  mutate(born_aft68 = (birth_year >= 68)) %>% 
  filter(born_aft68 == TRUE) %>% 
  group_by(high_intensity) %>% 
  summarise(avg_educ = mean(education) %>% round(2)) %>% 
  mutate(diff = (.$avg_educ[2] - .$avg_educ[1]) %>% round(2))
```

    ## # A tibble: 2 x 3
    ##   high_intensity avg_educ  diff
    ##            <dbl>    <dbl> <dbl>
    ## 1              0    10.1  -1.18
    ## 2              1     8.94 -1.18

Question 6
----------

What is the DD estimate of the effect of the program on education?
(Please round to the nearest hundredth).

*Thought exercise: Under what assumptions would this be an unbiased
estimate of the causal effect of the program on education? Do you think
this is an unbiased estimate in this case?*

### Answer

``` r
summ2 <- inpres %>% 
  mutate(born_aft68 = (birth_year >= 68)) %>%
  group_by(high_intensity, born_aft68) %>%
  summarise(mean_educ = mean(education)) 
  
DD_est <- (summ2$mean_educ[4] - summ2$mean_educ[3]) - (summ2$mean_educ[2] - summ2$mean_educ[1])
DD_est %>% round(2)
```

    ## [1] 0.08
