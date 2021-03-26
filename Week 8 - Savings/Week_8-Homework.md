Week 8 Homework, Unit 1
================
Lemi Daba
3/25/2021

``` r
# Load necessary packages
library(tidyverse)
library(haven)
library(stargazer)
library(fastDummies)
library(broom)
library(car)
```

``` r
# Read and view the data
paxson <- read_dta("paxson.dta")
paxson
```

    ## # A tibble: 4,855 x 44
    ##       hid   save3  year region   inc  save1  save2   m12   m18   m65 m18elem
    ##     <dbl>   <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
    ##  1 1.21e8 -2998.     76     19 3263. -577.  -397.      0     0     1       1
    ##  2 2.21e8   251.     76     32 1891.   31.3  289.      1     2     1       0
    ##  3 2.11e8   677.     76     26  864. -623.    58.8     1     0     0       1
    ##  4 3.21e8   758.     76     28 7454. 2340.  2691.      0     1     0       2
    ##  5 2.11e8   -45.0    76     26 1110.  232.   236.      0     0     0       1
    ##  6 1.21e8  5523.     76     19 5375. 1000.  1697.      0     0     0       1
    ##  7 3.20e8 -1421.     76     29 2481. -447.   566.      1     0     1       1
    ##  8 2.11e8   225.     76     14 1158.   84.4  309.      0     0     0       1
    ##  9 2.11e8   663.     76     26 1475.  -59.1  194.      0     0     0       1
    ## 10 2.22e8  -594.     76     26  810. -405.   -62.9     0     0     0       1
    ## # ... with 4,845 more rows, and 33 more variables: m18sec <dbl>, m18pos <dbl>,
    ## #   f12 <dbl>, f18 <dbl>, f65 <dbl>, f18elem <dbl>, f18sec <dbl>, f18pos <dbl>,
    ## #   of1 <dbl>, of2 <dbl>, of3 <dbl>, of4 <dbl>, of5 <dbl>, p0to5 <dbl>,
    ## #   p6to11 <dbl>, p12to17 <dbl>, p18to64 <dbl>, p65 <dbl>, of0 <dbl>,
    ## #   month <dbl>, cpi <dbl>, dev1 <dbl>, dev2 <dbl>, dev3 <dbl>, dev4 <dbl>,
    ## #   sd1 <dbl>, sd2 <dbl>, sd3 <dbl>, sd4 <dbl>, dvsq1 <dbl>, dvsq2 <dbl>,
    ## #   dvsq3 <dbl>, dvsq4 <dbl>

Question 4
----------

Using the coefficients estimated in the regression for Table 3, Column
1, Paxson constructs a predicted value for permanent income as follows.
She multiples the permanent characteristics by their respective
coefficients, and adds them up to form **incperm** (see equation 2 on
page 17 of the paper.) Generate the variable **incperm**. What is the
standard deviation of **incperm**?

**R Hint: You can store the *summary()* of a linear model, then use
*coef()* to generate a table of coefficients. To get a specific
coefficient, use *coef(reg\_sum)\[‘’x’’,‘‘Estimate’’\]*.**

### Answer

Let’s first replicate the regression results on Table 3. I’ll use the
`dummy_cols()` function from the `fastDummies` package to generate
region and year dummies.

``` r
# Selecting variables in the order they are in Table 3
paxson4reg <- paxson %>% 
  select( inc, year, region, p0to5, 
          starts_with( c("dev", "dvsq", "m", "f", "of")), -month ) %>% 
  dummy_cols( c("year", "region") , remove_first_dummy = TRUE) %>% 
  select(inc, year_81, year_86, dev1, dvsq1, dev2, dvsq2, 
         dev3, dvsq3, dev4, dvsq4, p0to5, m12, f12, m18, f18, m18elem,
         m18sec, m18pos, f18elem, f18sec, f18pos, m65, f65, of0, of1:of5,
         everything(), -year, -region)

# Run the regression
reginc <- lm( inc ~ ., data = paxson4reg )  
stargazer(reginc, type = "text", keep = "^(region)") # ??
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                 inc            
    ## -----------------------------------------------
    ## region_5                       5.804           
    ##                              (175.190)         
    ##                                                
    ## region_6                      266.638          
    ##                              (167.260)         
    ##                                                
    ## region_8                    -454.243**         
    ##                              (220.231)         
    ##                                                
    ## region_9                    -478.454***        
    ##                              (175.532)         
    ##                                                
    ## region_10                     27.105           
    ##                              (205.468)         
    ##                                                
    ## region_11                   -888.137***        
    ##                              (161.285)         
    ##                                                
    ## region_13                     -99.447          
    ##                              (141.516)         
    ##                                                
    ## region_14                   -561.007***        
    ##                              (158.365)         
    ##                                                
    ## region_15                     206.353          
    ##                              (198.253)         
    ##                                                
    ## region_17                    -167.521          
    ##                              (166.808)         
    ##                                                
    ## region_19                    236.748*          
    ##                              (141.100)         
    ##                                                
    ## region_20                  1,071.856***        
    ##                              (219.134)         
    ##                                                
    ## region_24                    340.178**         
    ##                              (141.380)         
    ##                                                
    ## region_26                   -596.278***        
    ##                              (133.889)         
    ##                                                
    ## region_27                    -127.223          
    ##                              (150.421)         
    ##                                                
    ## region_28                  1,059.911***        
    ##                              (152.806)         
    ##                                                
    ## region_29                   808.084***         
    ##                              (153.840)         
    ##                                                
    ## region_31                     -16.169          
    ##                              (146.587)         
    ##                                                
    ## region_32                   -415.647***        
    ##                              (129.802)         
    ##                                                
    ## region_34                  1,245.083***        
    ##                              (228.453)         
    ##                                                
    ## -----------------------------------------------
    ## Observations                   4,855           
    ## R2                             0.343           
    ## Adjusted R2                    0.336           
    ## Residual Std. Error    1,242.721 (df = 4805)   
    ## F Statistic          51.107*** (df = 49; 4805) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

Let’s use the `tidy()` command from the package `broom` to extract the
coefficients of the above regression.

``` r
coefs <- tidy(reginc) %>% 
  select(term, estimate)
coefs
```

    ## # A tibble: 50 x 2
    ##    term           estimate
    ##    <chr>             <dbl>
    ##  1 (Intercept) 2456.      
    ##  2 year_81      302.      
    ##  3 year_86     -402.      
    ##  4 dev1           1.91    
    ##  5 dvsq1         -0.0450  
    ##  6 dev2           1.25    
    ##  7 dvsq2          0.000877
    ##  8 dev3           0.228   
    ##  9 dvsq3          0.000445
    ## 10 dev4           1.61    
    ## # ... with 40 more rows

We can compute the estimate of permanent income using matrix
multiplication.

``` r
# Select the appropriate variables according to equation 2 on the paper
X <- paxson4reg %>% 
  select(-inc, -(dev1:dvsq4)) %>% 
  as.matrix()

b <- coefs %>%
  filter( !str_detect(term, "^\\(|dev|dvsq") ) %>%
  column_to_rownames(var = "term") %>% 
  as.matrix()

# Check compatibility
identical(colnames(X), rownames(b))
```

    ## [1] TRUE

``` r
# Generate "incperm"
paxson <- paxson %>% 
  mutate(incperm = X %*% b)
```

We can now compute the standard deviation of `incperm`.

``` r
sd(paxson$incperm)
```

    ## [1] 906.9714

Question 6
----------

What is the standard deviation of **inctrans**?

### Answer

We can generate `inctrans` in a similar way. As before, we need to
extract the relevant variables according to equation 3 on the paper.

``` r
Y <- paxson4reg %>%
  select(year_81:dvsq4) %>%
  as.matrix()

c <- coefs %>%
  filter( str_detect(term, "^year|dev|dvsq") ) %>%
  column_to_rownames(var = "term") %>% 
  as.matrix()

# Check compatibility
identical(colnames(Y), rownames(c))
```

    ## [1] TRUE

``` r
# Generate "inctrans"
paxson <- paxson %>% 
  mutate(inctrans = Y %*% c)

sd(paxson$inctrans)
```

    ## [1] 325.4108

Question 7
----------

Paxson also has a category called unexplained income, defined as **inc –
incperm – inctrans**. Form this variable and call it **incunexp.** What
is the standard deviation of **incunexp**?

### Answer

``` r
paxson <- paxson %>% 
  mutate(incunexp = inc - incperm - inctrans)

sd(paxson$incunexp)
```

    ## [1] 1258.259

Question 8
----------

You will now run a regression to estimate the effect of income on
savings. Use the variable **save2** as your measure of savings. Include
the variables that you included in the matrix for the final regression
(see the explanation to question 4 above.) What do you estimate for the
marginal propensity to save out of each additional dollar of transitory
income (i.e. **inctrans**)?

### Answer

``` r
save2reg <- lm(
  save2 ~ incperm + inctrans + incunexp + p0to5 + p6to11 + p12to17 + p18to64 +
    p65 + sd1 + sd2 + sd3 + sd4 + factor(year), data = paxson
)

stargazer(save2reg, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                save2           
    ## -----------------------------------------------
    ## incperm                      0.440***          
    ##                               (0.049)          
    ##                                                
    ## inctrans                     0.804***          
    ##                               (0.163)          
    ##                                                
    ## incunexp                     0.693***          
    ##                               (0.023)          
    ##                                                
    ## p0to5                         -52.854          
    ##                              (34.537)          
    ##                                                
    ## p6to11                         7.832           
    ##                              (29.497)          
    ##                                                
    ## p12to17                       -49.733          
    ##                              (33.880)          
    ##                                                
    ## p18to64                       -38.812          
    ##                              (37.248)          
    ##                                                
    ## p65                          -122.774*         
    ##                              (64.008)          
    ##                                                
    ## sd1                            1.738           
    ##                               (2.958)          
    ##                                                
    ## sd2                           -3.075*          
    ##                               (1.588)          
    ##                                                
    ## sd3                           4.007*           
    ##                               (2.177)          
    ##                                                
    ## sd4                           3.473*           
    ##                               (2.037)          
    ##                                                
    ## factor(year)81                -69.835          
    ##                              (82.729)          
    ##                                                
    ## factor(year)86              -288.071**         
    ##                              (137.386)         
    ##                                                
    ## Constant                   -1,694.077***       
    ##                              (435.163)         
    ##                                                
    ## -----------------------------------------------
    ## Observations                   4,855           
    ## R2                             0.187           
    ## Adjusted R2                    0.184           
    ## Residual Std. Error    2,008.196 (df = 4840)   
    ## F Statistic          79.391*** (df = 14; 4840) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

Hence, the marginal propensity to save out of transitory income is
0.804. This is the same result as in Table 4 in the paper.

Question 9
----------

What do you estimate for the marginal propensity to save out of each
additional dollar of permanent income (i.e. **incperm**)?

### Answer

From the regression in Question 8, the MPS for permanent income is
0.440.

Question 10
-----------

Test the null hypothesis that the marginal propensity to save out of
each dollar of transitory income from question 8 equals the marginal
propensity to save out of each dollar of permanent income from question
9. Can we reject the null hypothesis at 95% significance level?

*R Hint: Use the **linearHypothesis()** command from the “car” library,
which you were asked to install in the explanation to question 3 above.
Note that testing whether two coefficients are equal is equivalent to
testing whether one minus the other equals 0.*

### Answer

``` r
linearHypothesis(save2reg, "incperm = inctrans")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## incperm - inctrans = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: save2 ~ incperm + inctrans + incunexp + p0to5 + p6to11 + p12to17 + 
    ##     p18to64 + p65 + sd1 + sd2 + sd3 + sd4 + factor(year)
    ## 
    ##   Res.Df        RSS Df Sum of Sq      F  Pr(>F)  
    ## 1   4841 1.9538e+10                              
    ## 2   4840 1.9519e+10  1  19460266 4.8254 0.02809 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Since the p-value is 0.028 which is less than 0.05, we reject the
equality of their coefficients at the 95 percent significance level.
