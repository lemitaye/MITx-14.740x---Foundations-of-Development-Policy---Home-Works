Week 2 Homework, Unit 1
================
Lemi Daba
2/18/2021

``` r
# Some basic set-up
library(tidyverse)
nutrition <- read_csv("nutrition_csv.csv")
```

Question 6
----------

For most pupils, how many observations are there per pupil? (Enter a
whole number of 0 or higher)

### Answer

``` r
nutrition %>% 
    count(pupid, name = "Times_appeared") %>% 
    count(Times_appeared, name = "count")
```

    ## # A tibble: 3 x 2
    ##   Times_appeared count
    ##            <int> <int>
    ## 1              1 38471
    ## 2              2    12
    ## 3             76     1

Question 7
----------

What percentage of the pupils are boys? (Answers within 0.50 percentage
points of the correct answer will be accepted. For instance, 67 would be
accepted if the correct answer is 67.45%)

### Answer

``` r
nutrition %>% 
    count(sex) %>% 
    filter(!is.na(sex)) %>% 
    mutate("prop (%)" = (n*100/sum(n)) %>% round(2) )
```

    ## # A tibble: 2 x 3
    ##     sex     n `prop (%)`
    ##   <dbl> <int>      <dbl>
    ## 1     0 14123       47.9
    ## 2     1 15347       52.1

Question 8
----------

What percentage of pupils took the deworming pill in 1998? (Answers
within 0.50 percentage points of the correct answer will be accepted.
For instance, 67 would be accepted if the correct answer is 67.45%)

### Answer

``` r
nutrition %>% 
    count(pill98) %>% 
    filter(!is.na(pill98)) %>% 
    mutate("prop (%)" = (n*100/sum(n)) %>% round(2) )
```

    ## # A tibble: 2 x 3
    ##   pill98     n `prop (%)`
    ##    <dbl> <int>      <dbl>
    ## 1      0 25801       78.6
    ## 2      1  7025       21.4

Question 9
----------

Was the percentage of schools assigned to treatment in 1998 greater than
or less than the percentage of pupils that actually took the deworming
pill in 1998?

### Answer

``` r
nutrition %>%
  summarise(
    sch_assign98 = (mean(treat_sch98, na.rm = TRUE) * 100) %>% round(2),
    pupil_deworm98 = (mean(pill98, na.rm = TRUE) * 100) %>% round(2),
  )
```

    ## # A tibble: 1 x 2
    ##   sch_assign98 pupil_deworm98
    ##          <dbl>          <dbl>
    ## 1         33.4           21.4

Question 10
-----------

Which of the following variables from the dataset are dummy variables?
(Check all that apply.)

### Answer

``` r
summary(nutrition)
```

    ##      pupid             pill98          pill99         grade98      
    ##  Min.   :1071714   Min.   :0.000   Min.   :0.000   Min.   : 0.000  
    ##  1st Qu.:1670050   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 1.000  
    ##  Median :2082400   Median :0.000   Median :0.000   Median : 3.000  
    ##  Mean   :2142327   Mean   :0.214   Mean   :0.303   Mean   : 5.676  
    ##  3rd Qu.:2707002   3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.: 6.000  
    ##  Max.   :9146209   Max.   :1.000   Max.   :1.000   Max.   :88.000  
    ##  NA's   :76        NA's   :5745    NA's   :5745    NA's   :3784    
    ##       sex          old_girl98       totpar98          wgrp      
    ##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :1.000  
    ##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.600   1st Qu.:1.000  
    ##  Median :1.000   Median :0.000   Median :1.000   Median :2.000  
    ##  Mean   :0.521   Mean   :0.113   Mean   :0.764   Mean   :1.986  
    ##  3rd Qu.:1.000   3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.:3.000  
    ##  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :3.000  
    ##  NA's   :9101    NA's   :4980    NA's   :11195   NA's   :3779   
    ##   treat_sch98    infect_early99 
    ##  Min.   :0.000   Min.   :0.00   
    ##  1st Qu.:0.000   1st Qu.:0.00   
    ##  Median :0.000   Median :0.00   
    ##  Mean   :0.335   Mean   :0.43   
    ##  3rd Qu.:1.000   3rd Qu.:1.00   
    ##  Max.   :1.000   Max.   :1.00   
    ##  NA's   :3779    NA's   :36150

From the above summary, variable with a minimum of 0 and a maximum of 1
are the following:

    * `pill98`

    * `pill99`

    * `sex`

    * `old_girl98`

    * `treat_sch98`

    * `infect_early99`

The variable `totpar98` also has a minimum of 0 and a maximum of 1, but
it is not a dummy variable (it is a proportion, expressed as a fraction
between 0 and 1).

Question 11
-----------

Using the data, find and enter the difference in outcomes (Y: school
participation) between students who took the pill and students who did
not in 1998. (Enter your answer as a difference in proportions. For
instance, if the proportion in one group is 0.61 and the proportion in
the other group is 0.54, enter 0.07. Answers within 0.05 of the correct
answer will be accepted. For instance, 0.28 would be accepted if the
correct answer is 0.33.)

### Answer

``` r
nutrition %>%
  filter(!is.na(totpar98) & !is.na(pill98)) %>%
  group_by(pill98) %>%
  summarise(avg_partic = mean(totpar98)) %>%
  ungroup() %>%
  mutate(diff = round(avg_partic[pill98 == 1] - avg_partic[pill98 == 0], 3))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   pill98 avg_partic  diff
    ##    <dbl>      <dbl> <dbl>
    ## 1      0      0.746 0.131
    ## 2      1      0.877 0.131

Question 12
-----------

Since schools were randomly assigned to the deworming treatment group,
the estimate calculated in the previous answer is an unbiased estimate
of taking the pill on school attendance.

### Answer

There are students in schools assigned to deworming but did not get the
pill:

``` r
nutrition %>% 
    filter((pill98 == 0) & (treat_sch98 == 1))
```

    ## # A tibble: 3,461 x 10
    ##     pupid pill98 pill99 grade98   sex old_girl98 totpar98  wgrp treat_sch98
    ##     <dbl>  <dbl>  <dbl>   <dbl> <dbl>      <dbl>    <dbl> <dbl>       <dbl>
    ##  1 1.08e6      0      0       4    NA          0    NA        1           1
    ##  2 1.09e6      0      0       1    NA          0    NA        1           1
    ##  3 1.09e6      0      0       4     1          0     0        1           1
    ##  4 1.09e6      0      0       5     1          0    NA        1           1
    ##  5 1.09e6      0      0       5     1          0    NA        1           1
    ##  6 1.09e6      0      0       5     0          0    NA        1           1
    ##  7 1.09e6      0      0       5     1          0    NA        1           1
    ##  8 1.09e6      0      0       4     1          0    NA        1           1
    ##  9 1.09e6      0      0       4     0          1     0.75     1           1
    ## 10 1.09e6      0      1       3     0          0     1        1           1
    ## # ... with 3,451 more rows, and 1 more variable: infect_early99 <dbl>

Hence, the above estimate didn’t take into the account the spillover
effect and hence is likely biased.
