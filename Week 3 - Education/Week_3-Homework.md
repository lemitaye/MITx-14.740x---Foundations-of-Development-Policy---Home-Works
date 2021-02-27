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
  group_by(high_intensity, born_aft68) %>%
  summarise(mean_educ = mean(education))

summ
```

    ## # A tibble: 4 x 3
    ## # Groups:   high_intensity [2]
    ##   high_intensity born_aft68 mean_educ
    ##            <dbl> <lgl>          <dbl>
    ## 1              0 FALSE           9.73
    ## 2              0 TRUE           10.1 
    ## 3              1 FALSE           8.48
    ## 4              1 TRUE            8.94

``` r
X <- cbind(
  c(
    summ$mean_educ[4],
    summ$mean_educ[3],
    summ$mean_educ[4] - summ$mean_educ[3]
  ),
  c(
    summ$mean_educ[2],
    summ$mean_educ[1],
    summ$mean_educ[2] - summ$mean_educ[1]
  ),
  c(
    summ$mean_educ[4] - summ$mean_educ[2],
    summ$mean_educ[3] - summ$mean_educ[1],
    (summ$mean_educ[4] - summ$mean_educ[3]) - (summ$mean_educ[2] - summ$mean_educ[1])
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
# You need the 'summ' tibble from Question 3
DD_est <- (summ$mean_educ[4] - summ$mean_educ[3]) - (summ$mean_educ[2] - summ$mean_educ[1])
DD_est %>% round(2)
```

    ## [1] 0.08

Question 7
----------

Now create a similar table, this time for log earnings instead of
education. What is the DD estimate of the effect of the program on log
earnings? (Please round to the nearest thousandth. For example, 67.891
would be accepted if the correct answer is 67.8912.)

### Answer

``` r
summ2 <- inpres %>%
  mutate(born_aft68 = (birth_year >= 68)) %>%
  group_by(high_intensity, born_aft68) %>%
  summarise(mean_lwage = mean(log_wage))

summ2
```

    ## # A tibble: 4 x 3
    ## # Groups:   high_intensity [2]
    ##   high_intensity born_aft68 mean_lwage
    ##            <dbl> <lgl>           <dbl>
    ## 1              0 FALSE            12.3
    ## 2              0 TRUE             12.0
    ## 3              1 FALSE            12.1
    ## 4              1 TRUE             11.8

``` r
Y <- cbind(
  c(
    summ2$mean_lwage[4],
    summ2$mean_lwage[3],
    summ2$mean_lwage[4] - summ2$mean_lwage[3]
  ),
  c(
    summ2$mean_lwage[2],
    summ2$mean_lwage[1],
    summ2$mean_lwage[2] - summ2$mean_lwage[1]
  ),
  c(
    summ2$mean_lwage[4] - summ2$mean_lwage[2],
    summ2$mean_lwage[3] - summ2$mean_lwage[1],
    (summ2$mean_lwage[4] - summ2$mean_lwage[3]) - (summ2$mean_lwage[2] - summ2$mean_lwage[1])
  )
)

colnames(Y) <- c(
  "High Intensity (Treatment)",
  "Low Intensity (Control)",
  "Difference (Treatment-Control)"
)

rownames(Y) <- c(
  "**Young (Born 1968 or later)**",
  "**Old (Born before 1968)**",
  "**Difference (Young - Old)**"
)

knitr::kable(Y, digits = 3)
```

|                                | High Intensity (Treatment) | Low Intensity (Control) | Difference (Treatment-Control) |
|:-------------------------------|---------------------------:|------------------------:|-------------------------------:|
| **Young (Born 1968 or later)** |                     11.839 |                  11.975 |                         -0.135 |
| **Old (Born before 1968)**     |                     12.142 |                  12.279 |                         -0.137 |
| **Difference (Young - Old)**   |                     -0.303 |                  -0.304 |                          0.001 |

The difference-in-difference estimate is 0.001.

Question 10
-----------

Run the DD regressions for estimating the impact of the program on
education. Run the DD regression for estimating the impact of the
program on log earnings. Using the resulting output, what is the Wald
estimate of the effect of education on log earnings, using INPRES
exposure as the instrument? (Please round to three decimal places.)

*Thought exercise: What additional assumptions (in addition to those
mentioned in question 6) are required to interpret the Wald estimate as
a causal relationship? Do you think this is an unbiased estimate in this
case?*

### Answer

``` r
inpres <- inpres %>% 
  mutate(young = (birth_year >= 68))

mod1 <- lm(education ~ high_intensity*young, data = inpres)
mod2 <- lm(log_wage ~ high_intensity*young, data = inpres)

stargazer::stargazer(mod1, mod2, type = "text")
```

    ## 
    ## =============================================================
    ##                                      Dependent variable:     
    ##                                  ----------------------------
    ##                                    education      log_wage   
    ##                                       (1)            (2)     
    ## -------------------------------------------------------------
    ## high_intensity                     -1.257***      -0.137***  
    ##                                     (0.046)        (0.008)   
    ##                                                              
    ## young                               0.386***      -0.304***  
    ##                                     (0.052)        (0.009)   
    ##                                                              
    ## high_intensity:young                 0.076          0.001    
    ##                                     (0.080)        (0.013)   
    ##                                                              
    ## Constant                            9.733***      12.279***  
    ##                                     (0.029)        (0.005)   
    ##                                                              
    ## -------------------------------------------------------------
    ## Observations                         45,624        45,624    
    ## R2                                   0.025          0.055    
    ## Adjusted R2                          0.025          0.055    
    ## Residual Std. Error (df = 45620)     3.970          0.657    
    ## F Statistic (df = 3; 45620)        388.783***    884.753***  
    ## =============================================================
    ## Note:                             *p<0.1; **p<0.05; ***p<0.01

``` r
wald_est <- coef(mod2)[["high_intensity:youngTRUE"]]/coef(mod1)[["high_intensity:youngTRUE"]]
```

The Wald estimate is 0.015.

Question 11
-----------

We can also compute the Wald estimate using IV regression. In an IV-DD
setup, the two main effect terms (i.e. program intensity and age of
exposure) are included as controls, and the variable representing the
treatment (i.e. the interaction term) is used as the instrument.

where $ \_{2} $ is the first-stage effect of the program on schooling
and δ1 is our IV estimate of the causal effect of schooling on log
earnings. Hint: use ivreg() in R.

Run the regression corresponding to this Wald estimate and confirm that
it returns the same value. What is the coefficient, and what is the
standard error? (Please provide your answer to three decimal places.)

### Answer

``` r
modiv <- ivreg(
  log_wage ~ education + high_intensity + young | high_intensity + young + high_intensity:young, data = inpres
)

stargazer::stargazer(modiv, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                              log_wage          
    ## -----------------------------------------------
    ## education                      0.015           
    ##                               (0.167)          
    ##                                                
    ## high_intensity                -0.117           
    ##                               (0.206)          
    ##                                                
    ## young                        -0.310***         
    ##                               (0.070)          
    ##                                                
    ## Constant                     12.130***         
    ##                               (1.623)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                  45,624           
    ## R2                             0.130           
    ## Adjusted R2                    0.129           
    ## Residual Std. Error     0.631 (df = 45620)     
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

As can be seen, the coefficient on education from this iv regression is
the same as the Wald estimate obtained from question 10.
