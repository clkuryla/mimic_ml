EDA
================
Christine Lucille Kuryla
2024-11-24

## Data from online article

``` r
# Downloaded from: https://translational-medicine.biomedcentral.com/articles/10.1186/s12967-020-02620-5#Sec14

# This data has all of the patients included in the study, after they filtered out certain patients due to the exclusion criteria in the paper
# n = 4559

data <- read_csv("data/12967_2020_2620_MOESM1_ESM.csv")
```

    ## New names:
    ## Rows: 4559 Columns: 106
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (10): intime, outtime, dbsource, suspected_infection_time_poe, specimen_... dbl
    ## (96): icustay_id...1, hadm_id...2, suspected_infection_time_poe_days, po...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `icustay_id` -> `icustay_id...1`
    ## • `hadm_id` -> `hadm_id...2`
    ## • `hadm_id` -> `hadm_id...102`
    ## • `icustay_id` -> `icustay_id...103`

# Distributions

``` r
# Create histograms for each variable
data %>%
  #select(-c(icustay_id...1, hadm_id...2, icustay_id...103, hadm_id...102,
        #    intime, outtime, dbsource, suspected_infection_time_poe)) %>% #filter irrelevant variables
  select(
    age, icu_los, hosp_los, sofa, lods
  ) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram( fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](EDA_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
data %>%
  select(aniongap_min:hematocrit_max) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 46 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
data %>%
  select(hemoglobin_min, hemoglobin_max,
      #   lactate_mean, lactate_min, lactate_max,
         platelet_min, platelet_max,
         potassium_min, potassium_max,
         inr_min, inr_max,
      sodium_min, sodium_max
      ) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 558 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
data %>%
  select(lactate_min:lactate_mean,
         bun_min:wbc_mean) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
data %>%
  select(heartrate_min:diasbp_mean) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 48 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
data %>%
  select(meanbp_min:tempc_mean) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 312 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
data %>%
  select(spo2_min:glucose_mean) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 93 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

# S
