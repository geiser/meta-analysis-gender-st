Metanalysis of performance in condition of Stereotype Threat
(`stThreat`)
================
Geiser C. Challco <geiser@alumni.usp.br>

-   [Initial Variables and Loading
    Data](#initial-variables-and-loading-data)
-   [Perform meta-analyses](#perform-meta-analyses)
    -   [Subgroup analysis by “country”](#subgroup-analysis-by-country)
    -   [Subgroup analysis by “age”](#subgroup-analysis-by-age)
    -   [Subgroup analysis by “ed.level”](#subgroup-analysis-by-edlevel)
    -   [Subgroup analysis by
        “intervention”](#subgroup-analysis-by-intervention)
    -   [Subgroup analysis by
        “country:age”](#subgroup-analysis-by-countryage)
    -   [Subgroup analysis by
        “country:ed.level”](#subgroup-analysis-by-countryedlevel)
    -   [Subgroup analysis by
        “country:intervention”](#subgroup-analysis-by-countryintervention)
    -   [Subgroup analysis by
        “age:intervention”](#subgroup-analysis-by-ageintervention)
    -   [Subgroup analysis by
        “ed.level:intervention”](#subgroup-analysis-by-edlevelintervention)
    -   [Subgroup analysis by
        “country:age:intervention”](#subgroup-analysis-by-countryageintervention)
    -   [Subgroup analysis by
        “country:ed.level:intervention”](#subgroup-analysis-by-countryedlevelintervention)
-   [Funnel Plot](#funnel-plot)

## Initial Variables and Loading Data

``` r
cond <- "stThreat"
to_remove <- c('S11')
sub.groups <- c("country","age","ed.level","intervention",
                "country:age","country:ed.level","country:intervention",
                "age:intervention","ed.level:intervention",
                "country:age:intervention","country:ed.level:intervention")
```

``` r
dat <- read_excel("../data/data-without-outliers.xlsx", sheet = "perform-cond-descriptive")
dat <- dat[!dat$study %in% to_remove, ]

leg <- read_excel("../data/data-without-outliers.xlsx", sheet = "legend")
```

    ## New names:
    ## • `` -> `...10`

``` r
leg <- leg[!leg$study %in% to_remove, ]

idx.e <- which(dat$condition==cond)
idx.c <- which(dat$condition=="control")

data <- data.frame(
  study = dat$study[idx.c],
  n.e = dat$N[idx.e], mean.e = dat$M[idx.e], sd.e = dat$SD[idx.e],
  n.c = dat$N[idx.c], mean.c = dat$M[idx.c], sd.c = dat$SD[idx.c]
)
for (cgroups in strsplit(sub.groups,":")) {
  data[[paste0(cgroups, collapse = ":")]] <- sapply(data$study, FUN = function(x) {
    paste0(sapply(cgroups, FUN = function(namecol) leg[[namecol]][which(x == leg$study)]), collapse = ":")
  })
}
data[["lbl"]] <- sapply(data$study, FUN = function(x) leg$Note[which(x == leg$study)])
```

## Perform meta-analyses

``` r
m.cont <- metacont(
  n.e = n.e, mean.e = mean.e, sd.e = sd.e, n.c = n.c, mean.c = mean.c, sd.c = sd.c,
  studlab = lbl, data = data, sm = "SMD", method.smd = "Hedges",
  fixed = F, random = T, method.tau = "REML", hakn = T, title = paste("Performance in",cond)
)
summary(m.cont)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.cont, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Subgroup analysis by “country”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = country, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random) country
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2  Brazil
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8  Brazil
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4  Brazil
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2  Brazil
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0  Brazil
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8  Brazil
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2  Brazil
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2   China
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9  Brazil
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3  Brazil
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                    k     SMD            95%-CI  tau^2    tau     Q   I^2
    ## country = Brazil   9 -0.0667 [-0.2873; 0.1539] 0.0272 0.1650 11.85 32.5%
    ## country = China    1  0.1809 [-0.3141; 0.6759]     --     --  0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   0.84    1  0.3592
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Subgroup analysis by “age”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = country, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random) country
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2  Brazil
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8  Brazil
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4  Brazil
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2  Brazil
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0  Brazil
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8  Brazil
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2  Brazil
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2   China
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9  Brazil
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3  Brazil
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                    k     SMD            95%-CI  tau^2    tau     Q   I^2
    ## country = Brazil   9 -0.0667 [-0.2873; 0.1539] 0.0272 0.1650 11.85 32.5%
    ## country = China    1  0.1809 [-0.3141; 0.6759]     --     --  0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   0.84    1  0.3592
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Subgroup analysis by “ed.level”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = ed.level, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)         ed.level
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2  upper-secundary
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8  upper-secundary
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4  upper-secundary
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2 higher-education
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0 higher-education
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8 higher-education
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2          unknown
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2          unknown
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9          unknown
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3  upper-secundary
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                               k     SMD            95%-CI  tau^2    tau    Q   I^2
    ## ed.level = upper-secundary    4 -0.1725 [-0.5126; 0.1676]      0      0 2.44  0.0%
    ## ed.level = higher-education   3  0.1507 [-0.3279; 0.6294]      0      0 1.20  0.0%
    ## ed.level = unknown            3 -0.0353 [-1.0048; 0.9342] 0.1022 0.3197 6.04 66.9%
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   4.40    2  0.1110
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Subgroup analysis by “intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ##                                                                        intervention
    ## S1                             Gender-stereotype color, ranking, badges, and avatar
    ## S2                             Gender-stereotype color, ranking, badges, and avatar
    ## S3                             Gender-stereotype color, ranking, badges, and avatar
    ## S4                             Gender-stereotype color, ranking, badges, and avatar
    ## S5                             Gender-stereotype color, ranking, badges, and avatar
    ## S6                             Gender-stereotype color, ranking, badges, and avatar
    ## S7                             Gender-stereotype color, ranking, badges, and avatar
    ## S8: Conducted by BNU           Gender-stereotype color, ranking, badges, and avatar
    ## S9: Albuquerque, et al. (2017) Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs           Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                                      k     SMD            95%-CI  tau^2    tau     Q   I^2
    ## intervention = Gender-stereotype color, rankin ...   9 -0.0290 [-0.2575; 0.1994] 0.0313 0.1768 12.30 34.9%
    ## intervention = Gender-stereotyped motivational ...   1 -0.1663 [-0.5939; 0.2614]     --     --  0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   0.33    1  0.5668
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Subgroup analysis by “country:age”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `country:age`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)           country:age
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2     Brazil:adolescent
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8     Brazil:adolescent
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4     Brazil:adolescent
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2          Brazil:adult
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0          Brazil:adult
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8          Brazil:adult
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2          Brazil:adult
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2  China:no-restriction
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9 Brazil:no-restriction
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3    Brazil:adolescence
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                       k     SMD             95%-CI  tau^2    tau    Q   I^2
    ## country:age = Brazil:adolescent       3 -0.1887 [-0.8543;  0.4770] 0.0191 0.1382 2.43 17.8%
    ## country:age = Brazil:adult            4  0.1671 [-0.0733;  0.4075]      0      0 1.24  0.0%
    ## country:age = China:no-restriction    1  0.1809 [-0.3141;  0.6759]     --     -- 0.00    --
    ## country:age = Brazil:no-restriction   1 -0.4832 [-0.9228; -0.0436]     --     -- 0.00    --
    ## country:age = Brazil:adolescence      1 -0.1663 [-0.5939;  0.2614]     --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                      Q d.f. p-value
    ## Between groups   11.66    4  0.0200
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Subgroup analysis by “country:ed.level”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `country:ed.level`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)        country:ed.level
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2  Brazil:upper-secundary
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8  Brazil:upper-secundary
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4  Brazil:upper-secundary
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2 Brazil:higher-education
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0 Brazil:higher-education
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8 Brazil:higher-education
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2          Brazil:unknown
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2           China:unknown
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9          Brazil:unknown
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3  Brazil:upper-secundary
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                              k     SMD            95%-CI  tau^2    tau    Q   I^2
    ## country:ed.level = Brazil:upper-secundary    4 -0.1725 [-0.5126; 0.1676]      0      0 2.44  0.0%
    ## country:ed.level = Brazil:higher-education   3  0.1507 [-0.3279; 0.6294]      0      0 1.20  0.0%
    ## country:ed.level = Brazil:unknown            2 -0.1353 [-4.4768; 4.2061] 0.1874 0.4329 5.06 80.3%
    ## country:ed.level = China:unknown             1  0.1809 [-0.3141; 0.6759]     --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   5.10    3  0.1643
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Subgroup analysis by “country:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `country:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ##                                                                       country:intervention
    ## S1                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S2                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S3                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S4                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S5                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S6                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S7                             Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S8: Conducted by BNU            China:Gender-stereotype color, ranking, badges, and avatar
    ## S9: Albuquerque, et al. (2017) Brazil:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs           Brazil:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                                              k     SMD            95%-CI  tau^2    tau     Q
    ## country:intervention = Brazil:Gender-stereotype color, ...   8 -0.0546 [-0.3105; 0.2013] 0.0377 0.1942 11.58
    ## country:intervention = China:Gender-stereotype color,  ...   1  0.1809 [-0.3141; 0.6759]     --     --  0.00
    ## country:intervention = Brazil:Gender-stereotyped motiv ...   1 -0.1663 [-0.5939; 0.2614]     --     --  0.00
    ##                                                              I^2
    ## country:intervention = Brazil:Gender-stereotype color, ... 39.6%
    ## country:intervention = China:Gender-stereotype color,  ...    --
    ## country:intervention = Brazil:Gender-stereotyped motiv ...    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   1.12    2  0.5722
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Subgroup analysis by “age:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `age:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ##                                                                                   age:intervention
    ## S1                                 adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S2                                 adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S3                                 adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S4                                      adult:Gender-stereotype color, ranking, badges, and avatar
    ## S5                                      adult:Gender-stereotype color, ranking, badges, and avatar
    ## S6                                      adult:Gender-stereotype color, ranking, badges, and avatar
    ## S7                                      adult:Gender-stereotype color, ranking, badges, and avatar
    ## S8: Conducted by BNU           no-restriction:Gender-stereotype color, ranking, badges, and avatar
    ## S9: Albuquerque, et al. (2017) no-restriction:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs              adolescence:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                                          k     SMD            95%-CI  tau^2    tau    Q
    ## age:intervention = adolescent:Gender-stereotype co ...   3 -0.1887 [-0.8543; 0.4770] 0.0191 0.1382 2.43
    ## age:intervention = adult:Gender-stereotype color,  ...   4  0.1671 [-0.0733; 0.4075]      0      0 1.24
    ## age:intervention = no-restriction:Gender-stereotyp ...   2 -0.1613 [-4.3784; 4.0558] 0.1635 0.4043 3.87
    ## age:intervention = adolescence:Gender-stereotyped  ...   1 -0.1663 [-0.5939; 0.2614]     --     -- 0.00
    ##                                                          I^2
    ## age:intervention = adolescent:Gender-stereotype co ... 17.8%
    ## age:intervention = adult:Gender-stereotype color,  ...  0.0%
    ## age:intervention = no-restriction:Gender-stereotyp ... 74.1%
    ## age:intervention = adolescence:Gender-stereotyped  ...    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   6.10    3  0.1067
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Subgroup analysis by “ed.level:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `ed.level:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ##                                                                                ed.level:intervention
    ## S1                              upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S2                              upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S3                              upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S4                             higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S5                             higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S6                             higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S7                                      unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S8: Conducted by BNU                    unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S9: Albuquerque, et al. (2017)          unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs            upper-secundary:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                                               k     SMD            95%-CI  tau^2    tau    Q
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...   3 -0.1887 [-0.8543; 0.4770] 0.0191 0.1382 2.43
    ## ed.level:intervention = higher-education:Gender-stereot ...   3  0.1507 [-0.3279; 0.6294]      0      0 1.20
    ## ed.level:intervention = unknown:Gender-stereotype color ...   3 -0.0353 [-1.0048; 0.9342] 0.1022 0.3197 6.04
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...   1 -0.1663 [-0.5939; 0.2614]     --     -- 0.00
    ##                                                               I^2
    ## ed.level:intervention = upper-secundary:Gender-stereoty ... 17.8%
    ## ed.level:intervention = higher-education:Gender-stereot ...  0.0%
    ## ed.level:intervention = unknown:Gender-stereotype color ... 66.9%
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   3.93    3  0.2693
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Subgroup analysis by “country:age:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `country:age:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ##                                                                                  country:age:intervention
    ## S1                                 Brazil:adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S2                                 Brazil:adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S3                                 Brazil:adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S4                                      Brazil:adult:Gender-stereotype color, ranking, badges, and avatar
    ## S5                                      Brazil:adult:Gender-stereotype color, ranking, badges, and avatar
    ## S6                                      Brazil:adult:Gender-stereotype color, ranking, badges, and avatar
    ## S7                                      Brazil:adult:Gender-stereotype color, ranking, badges, and avatar
    ## S8: Conducted by BNU            China:no-restriction:Gender-stereotype color, ranking, badges, and avatar
    ## S9: Albuquerque, et al. (2017) Brazil:no-restriction:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs              Brazil:adolescence:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                                                  k     SMD             95%-CI  tau^2    tau
    ## country:age:intervention = Brazil:adolescent:Gender-stereo ...   3 -0.1887 [-0.8543;  0.4770] 0.0191 0.1382
    ## country:age:intervention = Brazil:adult:Gender-stereotype  ...   4  0.1671 [-0.0733;  0.4075]      0      0
    ## country:age:intervention = China:no-restriction:Gender-ste ...   1  0.1809 [-0.3141;  0.6759]     --     --
    ## country:age:intervention = Brazil:no-restriction:Gender-st ...   1 -0.4832 [-0.9228; -0.0436]     --     --
    ## country:age:intervention = Brazil:adolescence:Gender-stere ...   1 -0.1663 [-0.5939;  0.2614]     --     --
    ##                                                                   Q   I^2
    ## country:age:intervention = Brazil:adolescent:Gender-stereo ... 2.43 17.8%
    ## country:age:intervention = Brazil:adult:Gender-stereotype  ... 1.24  0.0%
    ## country:age:intervention = China:no-restriction:Gender-ste ... 0.00    --
    ## country:age:intervention = Brazil:no-restriction:Gender-st ... 0.00    --
    ## country:age:intervention = Brazil:adolescence:Gender-stere ... 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                      Q d.f. p-value
    ## Between groups   11.66    4  0.0200
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Subgroup analysis by “country:ed.level:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `country:ed.level:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                                    SMD             95%-CI %W(random)
    ## S1                             -0.3302 [-0.8631;  0.2026]        8.2
    ## S2                              0.0653 [-0.3468;  0.4773]       11.8
    ## S3                             -0.4150 [-0.9402;  0.1103]        8.4
    ## S4                             -0.0924 [-0.6256;  0.4408]        8.2
    ## S5                              0.1959 [-0.2701;  0.6619]       10.0
    ## S6                              0.2959 [-0.1777;  0.7695]        9.8
    ## S7                              0.2003 [-0.2012;  0.6017]       12.2
    ## S8: Conducted by BNU            0.1809 [-0.3141;  0.6759]        9.2
    ## S9: Albuquerque, et al. (2017) -0.4832 [-0.9228; -0.0436]       10.9
    ## S10: Only use prompt msgs      -0.1663 [-0.5939;  0.2614]       11.3
    ##                                                                               country:ed.level:intervention
    ## S1                              Brazil:upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S2                              Brazil:upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S3                              Brazil:upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S4                             Brazil:higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S5                             Brazil:higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S6                             Brazil:higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S7                                      Brazil:unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S8: Conducted by BNU                     China:unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S9: Albuquerque, et al. (2017)          Brazil:unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs            Brazil:upper-secundary:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 10
    ## Number of observations: o = 741
    ## 
    ##                          SMD            95%-CI     t p-value
    ## Random effects model -0.0434 [-0.2450; 0.1583] -0.49  0.6382
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0233 [0.0000; 0.2097]; tau = 0.1525 [0.0000; 0.4579]
    ##  I^2 = 29.0% [0.0%; 66.0%]; H = 1.19 [1.00; 1.72]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  12.68    9  0.1775
    ## 
    ## Results for subgroups (random effects model):
    ##                                                                       k     SMD            95%-CI  tau^2
    ## country:ed.level:intervention = Brazil:upper-secundary:Gender-s ...   3 -0.1887 [-0.8543; 0.4770] 0.0191
    ## country:ed.level:intervention = Brazil:higher-education:Gender- ...   3  0.1507 [-0.3279; 0.6294]      0
    ## country:ed.level:intervention = Brazil:unknown:Gender-stereotyp ...   2 -0.1353 [-4.4768; 4.2061] 0.1874
    ## country:ed.level:intervention = China:unknown:Gender-stereotype ...   1  0.1809 [-0.3141; 0.6759]     --
    ## country:ed.level:intervention = Brazil:upper-secundary:Gender-s ...   1 -0.1663 [-0.5939; 0.2614]     --
    ##                                                                        tau    Q   I^2
    ## country:ed.level:intervention = Brazil:upper-secundary:Gender-s ... 0.1382 2.43 17.8%
    ## country:ed.level:intervention = Brazil:higher-education:Gender- ...      0 1.20  0.0%
    ## country:ed.level:intervention = Brazil:unknown:Gender-stereotyp ... 0.4329 5.06 80.3%
    ## country:ed.level:intervention = China:unknown:Gender-stereotype ...     -- 0.00    --
    ## country:ed.level:intervention = Brazil:upper-secundary:Gender-s ...     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   4.54    4  0.3381
    ## 
    ## Details on meta-analytical method:
    ## - Inverse variance method
    ## - Restricted maximum-likelihood estimator for tau^2
    ## - Q-profile method for confidence interval of tau^2 and tau
    ## - Hartung-Knapp adjustment for random effects model
    ## - Hedges' g (bias corrected standardised mean difference; using exact formulae)

``` r
forest(m.sg4sub, digits=2, digits.sd = 2, test.overall = T, label.e = cond)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## Funnel Plot

``` r
m.cont <- update.meta(m.cont, studlab = data$study)
summary(eggers.test(x = m.cont))
```

    ## Eggers' test of the intercept 
    ## ============================= 
    ## 
    ##  intercept        95% CI      t    p
    ##     -3.205 -10.62 - 4.21 -0.847 0.42
    ## 
    ## Eggers' test does not indicate the presence of funnel plot asymmetry.

``` r
funnel(m.cont, xlab = "Hedges' g", studlab = T, legend=T, addtau2 = T)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-perform_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
