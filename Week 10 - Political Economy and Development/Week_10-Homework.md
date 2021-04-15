Week 10 Homework
================
Lemi Daba
4/15/2021

``` r
# Load required packages
library(tidyverse)
library(stargazer)
```

``` r
# Load the data:
mitadata <- read_csv("mitaData_corrected.csv")
mitadata
```

    ## # A tibble: 1,478 x 48
    ##    conglome vivienda hogar district male    age  ccdd  ccpp  ccdi hid   hhmem
    ##       <dbl>    <dbl> <dbl>    <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <chr> <dbl>
    ##  1      174       36    11    30101 male     34     3     1     1 174 ~     4
    ##  2      176       53    11    30101 fema~    60     3     1     1 176 ~     3
    ##  3      168      130    11    30101 male     45     3     1     1 1681~     6
    ##  4      174       14    11    30101 male     35     3     1     1 174 ~     4
    ##  5      173       68    11    30101 fema~    46     3     1     1 173 ~     3
    ##  6      172       28    11    30101 male     39     3     1     1 172 ~     4
    ##  7      173       32    11    30101 male     54     3     1     1 173 ~     5
    ##  8      169       45    11    30101 male     49     3     1     1 169 ~     7
    ##  9      175       27    11    30101 male     38     3     1     1 175 ~     4
    ## 10      173       57    11    30101 fema~    69     3     1     1 173 ~     1
    ## # ... with 1,468 more rows, and 37 more variables: lhhmem <dbl>, kids <dbl>,
    ## #   k_hhmem <dbl>, infants <dbl>, children <dbl>, adults <dbl>, ces <dbl>,
    ## #   depprov <dbl>, hconsump <dbl>, hconsumplm <dbl>, hhequiv <dbl>,
    ## #   lhhequiv <dbl>, lhhconsplm <dbl>, CAST <chr>, QUE <dbl>, AYM <dbl>,
    ## #   near_x <dbl>, near_y <dbl>, cusco <dbl>, pothuan_mita <dbl>, border <dbl>,
    ## #   d_bnd <dbl>, bfe4_1 <dbl>, bfe4_2 <dbl>, bfe4_3 <dbl>, elv_sh <dbl>,
    ## #   slope <dbl>, near_dist <dbl>, dpot <dbl>, lat <dbl>, lon <dbl>,
    ## #   dbnd_sh <dbl>, dbnd_sh2 <dbl>, dbnd_sh3 <dbl>, dbnd_sh4 <dbl>, x <dbl>,
    ## #   y <dbl>

The following function enables us to run regressions with clustered
standard errors for different running variables and different cutoff
points. The packages `lmtest` and `multiwayvcov` need to be installed
first to define and use this function.

``` r
mita_cluster_regs <- function(vars, cutoff) {
  require(multiwayvcov, quietly = TRUE)
  require(lmtest, quietly = TRUE)

  # Run OLS without clustering 
  if (all(vars == c("x", "y"))) {
    mitareg <- lm(
      formula = lhhequiv ~ pothuan_mita + poly(x, 3, raw = TRUE) +
        poly(y, 3, raw = TRUE) + I(x * y) + I((x^2) * y) + I(x * (y^2)) +
        elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3,
      data = mitadata,
      subset = (d_bnd < cutoff)
    )
  } else if (vars == "dpot") {
    mitareg <- lm(
      formula = lhhequiv ~ pothuan_mita + poly(dpot, 3, raw = TRUE) +
        elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3,
      data = mitadata,
      subset = (d_bnd < cutoff)
    )
  } else {
    stop('Enter either `c("x", "y")` or `"dpot"` for `vars` ', call. = FALSE
    )
  }

  # Get the clustered Variance-covariance matrix:
  mita.vcovCL <- cluster.vcov(
    mitareg,
    mitadata %>% filter(d_bnd < cutoff) %>% select(district)
  )

  # Return the result with clustered standard errors:
  res <- coeftest(mitareg, mita.vcovCL)
  return(res)
}
```

Let’s run the regressions.

``` r
xy_100 <- mita_cluster_regs(c("x", "y"), 100)
xy_75 <- mita_cluster_regs(c("x", "y"), 75)
xy_50 <- mita_cluster_regs(c("x", "y"), 50)
```

We summarize the results together in the following table.

``` r
stargazer(xy_100, xy_75, xy_50,
  type = "text",
  keep = "pothuan_mita",
  title = "Cubic Polynomial in Latitude and Longitude",
  column.labels = c("<100 km", "<75 km", "<50 km"),
  style = "aer"
)
```

    ## 
    ## Cubic Polynomial in Latitude and Longitude
    ## =====================================================
    ##                                                      
    ##                 <100 km       <75 km        <50 km   
    ##                   (1)           (2)          (3)     
    ## -----------------------------------------------------
    ## pothuan_mita    -0.284        -0.216        -0.331   
    ##                 (0.199)       (0.207)      (0.219)   
    ##                                                      
    ## -----------------------------------------------------
    ## Notes:       ***Significant at the 1 percent level.  
    ##              **Significant at the 5 percent level.   
    ##              *Significant at the 10 percent level.

As can be seen, all coefficients, corresponding to the different cutoff
values, are statistically insignificant at the 5% level.

Question 3
----------

What is the coefficient estimate for Mita on the regression using 100km
as the relevant cutoff? (Please input your answer to 3 decimal places).

### Answer

`-0.284`

Question 4
----------

Is this result significant at the 5% level?

### Answer

No.

Question 5
----------

What is the coefficient estimate for Mita on the regression using 75km
as the relevant cutoff? (Please input your answer to 3 decimal places).

### Answer

`-0.216`

Question 6
----------

Is this result significant at the 5% level?

### Answer

No

Question 7
----------

What is the coefficient estimate for Mita on the regression using 50km
as the relevant cutoff? (Please input your answer to 3 decimal places).

### Answer

`-0.331`

Question 8
----------

Is this result significant at the 5% level?

### Answer

No

------------------------------------------------------------------------

Run the same regressions as before, but instead of polynomial terms in
longitude and latitude, use a cubic polynomial in distance to Potosi
dpot. That is, include the first, second and third powers of this
variable in the regressions. Again, cluster the standard errors by
district and run the regression in 3 ways: first, for observations where
the distance to Mita boundary (d\_bnd) is less than 100km, next when it
is less than 75km, and lastly when it is less than 50km.

------------------------------------------------------------------------

Let’s run the corresponding regressions with `dpot` as the running
variable.

``` r
dpot_100 <- mita_cluster_regs("dpot", 100)
dpot_75 <- mita_cluster_regs("dpot", 75)
dpot_50 <- mita_cluster_regs("dpot", 50)
```

The following table summarizes the relevant results.

Question 9
----------

What is the coefficient estimate for Mita on the regression using 100km
as the relevant cutoff? (Please input your answer to 3 decimal places).

``` r
stargazer(dpot_100, dpot_75, dpot_50, type = "text", 
          keep = "pothuan_mita",
          title = "Cubic Polynomial in Distance to Potosi",
          column.labels = c("<100 km", "<75 km", "<50 km"),
          style = "aer")
```

    ## 
    ## Cubic Polynomial in Distance to Potosi
    ## =====================================================
    ##                                                      
    ##                 <100 km       <75 km        <50 km   
    ##                   (1)           (2)          (3)     
    ## -----------------------------------------------------
    ## pothuan_mita   -0.337***     -0.307***    -0.329***  
    ##                 (0.087)       (0.101)      (0.096)   
    ##                                                      
    ## -----------------------------------------------------
    ## Notes:       ***Significant at the 1 percent level.  
    ##              **Significant at the 5 percent level.   
    ##              *Significant at the 10 percent level.

All results are significant at the 1% (hence, at the 5%) level.

Question 9
----------

What is the coefficient estimate for Mita on the regression using 100km
as the relevant cutoff? (Please input your answer to 3 decimal places).

### Answer

`-0.337`

Question 10
-----------

Is this result significant at the 5% level?

### Answer

Yes

Question 11
-----------

What is the coefficient estimate for Mita on the regression using 75km
as the relevant cutoff? (Please input your answer to 3 decimal places).

### Answer

`-0.307`

Question 12
-----------

Is this result significant at the 5% level?

### Answer

Yes

Question 13
-----------

What is the coefficient estimate for Mita on the regression using 50km
as the relevant cutoff? (Please input your answer to 3 decimal places).

### Answer

`-0.329`

Question 14
-----------

Is this result significant at the 5% level?

### Answer

Yes
