Week 4 Homework, Unit 1
================
Lemi Daba
3/7/2021

``` r
library(tidyverse)
library(lfe)
library(stargazer)

tea_data <- read_csv("tea_data_corrected.csv")
```

Question 1
----------

First, lets understand the data structure. What describes the unit of
observation in the data?

### Answer

``` r
tea_data 
```

    ## # A tibble: 46,398 x 7
    ##     admin biryr   han   sex teasown  orch cashcrop
    ##     <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>    <dbl>
    ##  1 320102  1962     1  0.5        0     0     2.79
    ##  2 320102  1964     1  0          0     0     2.79
    ##  3 320102  1966     1  1          0     0     2.79
    ##  4 320102  1967     1  1          0     0     2.79
    ##  5 320102  1968     1  0.75       0     0     2.79
    ##  6 320102  1970     1  1          0     0     2.79
    ##  7 320102  1971     1  0          0     0     2.79
    ##  8 320102  1985     1  0.5        0     0     2.79
    ##  9 320102  1986     1  0.5        0     0     2.79
    ## 10 320102  1987     1  0          0     0     2.79
    ## # ... with 46,388 more rows

It appears each observation represents a cohort in each county.

``` r
tea_data %>% count(admin, biryr)
```

    ## # A tibble: 46,398 x 3
    ##     admin biryr     n
    ##     <dbl> <dbl> <int>
    ##  1 320102  1962     1
    ##  2 320102  1964     1
    ##  3 320102  1966     1
    ##  4 320102  1967     1
    ##  5 320102  1968     1
    ##  6 320102  1970     1
    ##  7 320102  1971     1
    ##  8 320102  1985     1
    ##  9 320102  1986     1
    ## 10 320102  1987     1
    ## # ... with 46,388 more rows

The above result strengthens our suspicion (the number of observations
and the number of unique county-cohort combination is the same). To
confirm, let’s see if there is more than one observation in any
county-cohort combination:

``` r
tea_data %>% 
    count(admin, biryr) %>% 
    filter(n > 1)
```

    ## # A tibble: 0 x 3
    ## # ... with 3 variables: admin <dbl>, biryr <dbl>, n <int>

Hence, we can now be sure that each observation represents a particular
cohort in a given county.

Question 4a
-----------

Now, generate the variable `teaDum`, which takes value 1 if any amount
of tea is sown and 0 otherwise. Generate the variable `post`, which
takes a value of 1 if the cohort is born on or after 1979. To be sure
that you generated this variables correctly, the mean of `teaDum` is
0.209 and mean of `post` is 0.412. Now, also generate the interaction
term.

Estimate $ $ from the data. Give its point estimate and the standard
error.

### Answer

``` r
tea_data <- tea_data %>%
  mutate(
    teaDum = (teasown > 0),
    post = (biryr >= 1979)
  ) 

tea_data %>%
  summarise(
    mean_tea = mean(teaDum, na.rm = TRUE),
    mean_post = mean(post, na.rm = TRUE)
  )
```

    ## # A tibble: 1 x 2
    ##   mean_tea mean_post
    ##      <dbl>     <dbl>
    ## 1    0.209     0.412

We now estimate the model in Question 2.

``` r
reg1 <- lm(sex ~ teaDum*post, data = tea_data)

stargazer(reg1, type = "text", digits = 5)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                 sex            
    ## -----------------------------------------------
    ## teaDum                      0.00601***         
    ##                              (0.00188)         
    ##                                                
    ## post                        0.01828***         
    ##                              (0.00134)         
    ##                                                
    ## teaDumTRUE:post             -0.01018***        
    ##                              (0.00292)         
    ##                                                
    ## Constant                    0.50463***         
    ##                              (0.00086)         
    ##                                                
    ## -----------------------------------------------
    ## Observations                  46,398           
    ## R2                            0.00426          
    ## Adjusted R2                   0.00419          
    ## Residual Std. Error    0.12599 (df = 46394)    
    ## F Statistic         66.12587*** (df = 3; 46394)
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

Question 6a
-----------

Now generate variables `orchardDum` and `cashcropDum` in a way similar
to `teaDum` (that is, they take value 1 if the crop is grown and 0
otherwise). Also, generate the interactions with post. Estimate the
following regression:

*S*<sub>*i**j*</sub> = *α* + *β*<sub>1</sub>*T**e**a**D**u**m*<sub>*i*</sub> + *β*<sub>2</sub>*O**r**c**h**a**r**d**D**u**m*<sub>*i*</sub> + *β*<sub>3</sub>*c**a**s**h**c**r**o**p**D**u**m*<sub>*i*</sub> + *γ*<sub>1</sub>(*T**e**a**D**u**m*<sub>*i*</sub> × *P**o**s**t*<sub>*i*</sub>) + ...

What are the point estimates for each of $ \_1 $, $ \_2 $, and $ \_3 $?

### Answer

``` r
tea_data <- tea_data %>%
  mutate(
    orchardDum = (orch > 0),
    cashcropDum = (cashcrop > 0)
  )

reg2 <- lm(
  sex ~ (teaDum + orchardDum + cashcropDum) * post,
  data = tea_data
)

stargazer(reg2, type = "text", digits = 6)
```

    ## 
    ## =================================================
    ##                          Dependent variable:     
    ##                      ----------------------------
    ##                                  sex             
    ## -------------------------------------------------
    ## teaDum                        0.005167**         
    ##                               (0.002016)         
    ##                                                  
    ## orchardDum                    -0.003481          
    ##                               (0.002228)         
    ##                                                  
    ## cashcropDum                  0.006666***         
    ##                               (0.002435)         
    ##                                                  
    ## post                         0.021307***         
    ##                               (0.002264)         
    ##                                                  
    ## teaDumTRUE:post              -0.008679***        
    ##                               (0.003135)         
    ##                                                  
    ## orchardDumTRUE:post            0.000701          
    ##                               (0.003475)         
    ##                                                  
    ## cashcropDumTRUE:post          -0.005162          
    ##                               (0.003802)         
    ##                                                  
    ## Constant                     0.501963***         
    ##                               (0.001450)         
    ##                                                  
    ## -------------------------------------------------
    ## Observations                    46,398           
    ## R2                             0.004446          
    ## Adjusted R2                    0.004296          
    ## Residual Std. Error     0.125986 (df = 46390)    
    ## F Statistic          29.598920*** (df = 7; 46390)
    ## =================================================
    ## Note:                 *p<0.1; **p<0.05; ***p<0.01

Question 8
----------

Now instead of grouping all the pre- and post-cohorts and grouping all
the counties into two groups (those who grow and don’t), let’s exploit
all the variation in the data. That is, let us estimate:

…

What is the estimate of $ \_1 $ from this model? (Please provide your
answer to 3 significant figures.) Does this change the conclusion from
the previous regressions?

### Answer

``` r
reg3a <- felm(
  sex ~ I(post * teaDum) + I(post * orchardDum) + I(post * cashcropDum) | admin + biryr,
  data = tea_data
)

stargazer(reg3a, type = "text")
```

    ## 
    ## =================================================
    ##                           Dependent variable:    
    ##                       ---------------------------
    ##                                   sex            
    ## -------------------------------------------------
    ## I(post * teaDum)               -0.009***         
    ##                                 (0.003)          
    ##                                                  
    ## I(post * orchardDum)            -0.0002          
    ##                                 (0.003)          
    ##                                                  
    ## I(post * cashcropDum)           -0.003           
    ##                                 (0.004)          
    ##                                                  
    ## -------------------------------------------------
    ## Observations                    46,398           
    ## R2                               0.104           
    ## Adjusted R2                      0.070           
    ## Residual Std. Error       0.122 (df = 44719)     
    ## =================================================
    ## Note:                 *p<0.1; **p<0.05; ***p<0.01

Let’s try it the old way (this takes longer).

``` r
reg3b <- lm(
  sex ~ I(post * teaDum) + I(post * orchardDum) + I(post * cashcropDum) + as.factor(admin) + as.factor(biryr),
  data = tea_data
)

stargazer(reg3b, type = "text", keep = c("tea", "orch", "cash"))
```

    ## 
    ## =================================================
    ##                           Dependent variable:    
    ##                       ---------------------------
    ##                                   sex            
    ## -------------------------------------------------
    ## I(post * teaDum)               -0.009***         
    ##                                 (0.003)          
    ##                                                  
    ## I(post * orchardDum)            -0.0002          
    ##                                 (0.003)          
    ##                                                  
    ## I(post * cashcropDum)           -0.003           
    ##                                 (0.004)          
    ##                                                  
    ## -------------------------------------------------
    ## Observations                    46,398           
    ## R2                               0.104           
    ## Adjusted R2                      0.070           
    ## Residual Std. Error       0.122 (df = 44719)     
    ## F Statistic           3.092*** (df = 1678; 44719)
    ## =================================================
    ## Note:                 *p<0.1; **p<0.05; ***p<0.01

Comparing the two:

``` r
stargazer(reg3a, reg3b, type = "text", keep = c("tea", "orch", "cash"))
```

    ## 
    ## ======================================================================
    ##                                           Dependent variable:         
    ##                                  -------------------------------------
    ##                                                   sex                 
    ##                                    felm                OLS            
    ##                                     (1)                (2)            
    ## ----------------------------------------------------------------------
    ## I(post * teaDum)                 -0.009***          -0.009***         
    ##                                   (0.003)            (0.003)          
    ##                                                                       
    ## I(post * orchardDum)              -0.0002            -0.0002          
    ##                                   (0.003)            (0.003)          
    ##                                                                       
    ## I(post * cashcropDum)             -0.003             -0.003           
    ##                                   (0.004)            (0.004)          
    ##                                                                       
    ## ----------------------------------------------------------------------
    ## Observations                      46,398             46,398           
    ## R2                                 0.104              0.104           
    ## Adjusted R2                        0.070              0.070           
    ## Residual Std. Error (df = 44719)   0.122              0.122           
    ## F Statistic                                3.092*** (df = 1678; 44719)
    ## ======================================================================
    ## Note:                                      *p<0.1; **p<0.05; ***p<0.01
