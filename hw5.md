Homework 5
================
Natalie Boychuk
11/15/2020

## Problem 1

Read in the data

``` r
  homicide_df = 
    read_csv("data/homicide-data.csv") %>% 
    mutate(
      city_state = str_c(city, state, sep = "_"),
      resolved = case_when(
        disposition == "Closed without arrest" ~ "unsolved",
        disposition == "Open/No arrest" ~ "unsolved",
        disposition == "Closed by arrest" ~ "solved"
        )
    ) %>% select(city_state,resolved) %>% 
  filter(city_state != "Tulsa_AL")
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_double(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
aggregate_df = 
  homicide_df %>% 
  group_by(city_state) %>% 
  summarize(
    hom_total = n(),
    hom_unsolved = sum(resolved == "unsolved")
  ) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
prop.test(
  aggregate_df %>%  filter(city_state == "Baltimore_MD") %>% pull(hom_unsolved),
   aggregate_df %>%  filter(city_state == "Baltimore_MD") %>% pull(hom_total)) %>% 
  broom::tidy()
```

    ## # A tibble: 1 x 8
    ##   estimate statistic  p.value parameter conf.low conf.high method    alternative
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
    ## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample… two.sided

``` r
results_df = 
  aggregate_df %>% 
  mutate(
    prop_tests = map2(.x = hom_unsolved, .y = hom_total, ~prop.test(x = .x, n = .y)),
    tidy_tests = map(.x = prop_tests, ~broom::tidy(.x))
  ) %>% 
  select(-prop_tests) %>% 
  unnest(tidy_tests) %>% 
  select(city_state, estimate, conf.low,conf.high)
```

``` r
results_df %>% 
  mutate(city_state = fct_reorder(city_state, estimate)) %>% 
  ggplot(aes(x = city_state, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))
```

<img src="hw5_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

    ## List of 1
    ##  $ axis.text.x:List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : num 0.5
    ##   ..$ angle        : num 90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

\#\#Problem 2

## Problem 3

``` r
simfun = function(samp_size = 30, mu = 0, sigma = 5){
  
  prob3_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    ) 
  
    prob3_ttest = 
     t.test(prob3_data, conf.level = .95) %>% 
      broom::tidy()
}
```

``` r
mu_sim =
  tibble(
    mu = c(0, 1, 2, 3, 4, 5, 6)
  ) %>% 
  mutate(
    result = map(.x = mu, ~rerun(5000, simfun(mu = .x))),
    result_df = map(result, bind_rows)
  ) 
```

``` r
unnest(mu_sim, cols = c("result_df"))
```

    ## # A tibble: 35,000 x 10
    ##       mu result estimate statistic p.value parameter conf.low conf.high method
    ##    <dbl> <list>    <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl> <chr> 
    ##  1     0 <list…    0.412     0.489  0.629         29   -1.31     2.14   One S…
    ##  2     0 <list…    0.664     0.914  0.368         29   -0.821    2.15   One S…
    ##  3     0 <list…    0.551     0.629  0.534         29   -1.24     2.34   One S…
    ##  4     0 <list…    0.567     0.704  0.487         29   -1.08     2.21   One S…
    ##  5     0 <list…   -1.65     -1.96   0.0599        29   -3.37     0.0731 One S…
    ##  6     0 <list…    1.19      1.23   0.229         29   -0.786    3.16   One S…
    ##  7     0 <list…    0.334     0.337  0.738         29   -1.69     2.36   One S…
    ##  8     0 <list…   -1.19     -1.29   0.209         29   -3.08     0.703  One S…
    ##  9     0 <list…    0.122     0.144  0.887         29   -1.62     1.86   One S…
    ## 10     0 <list…    0.684     0.728  0.472         29   -1.24     2.60   One S…
    ## # … with 34,990 more rows, and 1 more variable: alternative <chr>

``` r
power_list = 
  mu_sim %>% 
mutate(decision = case_when(
  "p.value" < .05 ~ "reject",
  "p.value" > .05 ~ "fail.reject")) %>% 
  mutate(power = 
           (mean(decision == "reject"))
  ) %>% view()
```
