Metanalysis of flow state in condition of Stereotype Boost (`stBoost`)
================
Geiser C. Challco <geiser@alumni.usp.br>

-   [Initial Variables and Loading
    Data](#initial-variables-and-loading-data)
-   [Perform meta-analyses](#perform-meta-analyses)
    -   [Subgroup analysis by “age”](#subgroup-analysis-by-age)
    -   [Subgroup analysis by “ed.level”](#subgroup-analysis-by-edlevel)
    -   [Subgroup analysis by
        “intervention”](#subgroup-analysis-by-intervention)
    -   [Subgroup analysis by
        “age:intervention”](#subgroup-analysis-by-ageintervention)
    -   [Subgroup analysis by
        “ed.level:intervention”](#subgroup-analysis-by-edlevelintervention)
    -   [Subgroup analysis by
        “age:ed.level:intervention”](#subgroup-analysis-by-ageedlevelintervention)
-   [Funnel Plot](#funnel-plot)

## Initial Variables and Loading Data

``` r
cond <- "stBoost"
to_remove <- c('S11')
sub.groups <- c("age","ed.level","intervention","age:intervention",
                "ed.level:intervention","age:ed.level:intervention")
```

``` r
dat <- read_excel("../data/data-without-outliers.xlsx", sheet = "fss-cond-descriptive")
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
  n.e = dat$N[idx.e], mean.e = dat$M.emms[idx.e], sd.e = dat$SD.emms[idx.e],
  n.c = dat$N[idx.c], mean.c = dat$M.emms[idx.c], sd.c = dat$SD.emms[idx.c]
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

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
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

![](02-flow_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Subgroup analysis by “age”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = age, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)         age
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1  adolescent
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6  adolescent
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2  adolescent
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5       adult
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3       adult
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1       adult
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8       adult
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4 adolescence
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
    ## 
    ## Results for subgroups (random effects model):
    ##                     k     SMD            95%-CI  tau^2    tau    Q   I^2
    ## age = adolescent    3  0.1498 [-0.8478; 1.1473] 0.0917 0.3028 4.71 57.5%
    ## age = adult         4  0.2406 [-0.0406; 0.5218]      0      0 1.83  0.0%
    ## age = adolescence   1 -0.0106 [-0.4241; 0.4028]     --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   1.25    2  0.5343
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

![](02-flow_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Subgroup analysis by “ed.level”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = ed.level, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)         ed.level
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1  upper-secundary
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6  upper-secundary
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2  upper-secundary
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5 higher-education
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3 higher-education
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1 higher-education
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8          unknown
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4  upper-secundary
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
    ## 
    ## Results for subgroups (random effects model):
    ##                               k    SMD            95%-CI  tau^2    tau    Q   I^2
    ## ed.level = upper-secundary    4 0.1025 [-0.4182; 0.6233] 0.0397 0.1993 5.10 41.2%
    ## ed.level = higher-education   3 0.2767 [-0.2494; 0.8028]      0      0 1.60  0.0%
    ## ed.level = unknown            1 0.1612 [-0.2355; 0.5580]     --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   0.78    2  0.6761
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

![](02-flow_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Subgroup analysis by “intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = intervention, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4
    ##                                                                   intervention
    ## S1                        Gender-stereotype color, ranking, badges, and avatar
    ## S2                        Gender-stereotype color, ranking, badges, and avatar
    ## S3                        Gender-stereotype color, ranking, badges, and avatar
    ## S4                        Gender-stereotype color, ranking, badges, and avatar
    ## S5                        Gender-stereotype color, ranking, badges, and avatar
    ## S6                        Gender-stereotype color, ranking, badges, and avatar
    ## S7                        Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs      Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
    ## 
    ## Results for subgroups (random effects model):
    ##                                                      k     SMD            95%-CI   tau^2    tau    Q   I^2
    ## intervention = Gender-stereotype color, rankin ...   7  0.2078 [-0.0265; 0.4421] <0.0001 0.0013 6.76 11.3%
    ## intervention = Gender-stereotyped motivational ...   1 -0.0106 [-0.4241; 0.4028]      --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   0.89    1  0.3457
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

![](02-flow_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Subgroup analysis by “age:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `age:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4
    ##                                                                          age:intervention
    ## S1                        adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S2                        adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S3                        adolescent:Gender-stereotype color, ranking, badges, and avatar
    ## S4                             adult:Gender-stereotype color, ranking, badges, and avatar
    ## S5                             adult:Gender-stereotype color, ranking, badges, and avatar
    ## S6                             adult:Gender-stereotype color, ranking, badges, and avatar
    ## S7                             adult:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs     adolescence:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
    ## 
    ## Results for subgroups (random effects model):
    ##                                                          k     SMD            95%-CI  tau^2    tau    Q
    ## age:intervention = adolescent:Gender-stereotype co ...   3  0.1498 [-0.8478; 1.1473] 0.0917 0.3028 4.71
    ## age:intervention = adult:Gender-stereotype color,  ...   4  0.2406 [-0.0406; 0.5218]      0      0 1.83
    ## age:intervention = adolescence:Gender-stereotyped  ...   1 -0.0106 [-0.4241; 0.4028]     --     -- 0.00
    ##                                                          I^2
    ## age:intervention = adolescent:Gender-stereotype co ... 57.5%
    ## age:intervention = adult:Gender-stereotype color,  ...  0.0%
    ## age:intervention = adolescence:Gender-stereotyped  ...    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   1.25    2  0.5343
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

![](02-flow_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Subgroup analysis by “ed.level:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `ed.level:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4
    ##                                                                           ed.level:intervention
    ## S1                         upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S2                         upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S3                         upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S4                        higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S5                        higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S6                        higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S7                                 unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs       upper-secundary:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
    ## 
    ## Results for subgroups (random effects model):
    ##                                                               k     SMD            95%-CI  tau^2    tau    Q
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...   3  0.1498 [-0.8478; 1.1473] 0.0917 0.3028 4.71
    ## ed.level:intervention = higher-education:Gender-stereot ...   3  0.2767 [-0.2494; 0.8028]      0      0 1.60
    ## ed.level:intervention = unknown:Gender-stereotype color ...   1  0.1612 [-0.2355; 0.5580]     --     -- 0.00
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...   1 -0.0106 [-0.4241; 0.4028]     --     -- 0.00
    ##                                                               I^2
    ## ed.level:intervention = upper-secundary:Gender-stereoty ... 57.5%
    ## ed.level:intervention = higher-education:Gender-stereot ...  0.0%
    ## ed.level:intervention = unknown:Gender-stereotype color ...    --
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   1.46    3  0.6921
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

![](02-flow_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Subgroup analysis by “age:ed.level:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `age:ed.level:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stBoost
    ## 
    ##                               SMD            95%-CI %W(random)
    ## S1                         0.6017 [ 0.0897; 1.1136]       10.1
    ## S2                         0.0200 [-0.4562; 0.4962]       11.6
    ## S3                        -0.1774 [-0.7134; 0.3587]        9.2
    ## S4                         0.5120 [-0.0442; 1.0682]        8.5
    ## S5                         0.0845 [-0.3315; 0.5005]       15.3
    ## S6                         0.3476 [-0.1021; 0.7973]       13.1
    ## S7                         0.1612 [-0.2355; 0.5580]       16.8
    ## S10: Only use prompt msgs -0.0106 [-0.4241; 0.4028]       15.4
    ##                                                                                 age:ed.level:intervention
    ## S1                        adolescent:upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S2                        adolescent:upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S3                        adolescent:upper-secundary:Gender-stereotype color, ranking, badges, and avatar
    ## S4                            adult:higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S5                            adult:higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S6                            adult:higher-education:Gender-stereotype color, ranking, badges, and avatar
    ## S7                                     adult:unknown:Gender-stereotype color, ranking, badges, and avatar
    ## S10: Only use prompt msgs     adolescence:upper-secundary:Gender-stereotyped motivational message prompts
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 597
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1741 [-0.0312; 0.3793] 2.01  0.0849
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 < 0.0001 [0.0000; 0.2358]; tau = 0.0006 [0.0000; 0.4856]
    ##  I^2 = 8.8% [0.0%; 70.4%]; H = 1.05 [1.00; 1.84]
    ## 
    ## Test of heterogeneity:
    ##     Q d.f. p-value
    ##  7.67    7  0.3625
    ## 
    ## Results for subgroups (random effects model):
    ##                                                                   k     SMD            95%-CI  tau^2    tau
    ## age:ed.level:intervention = adolescent:upper-secundary:Gend ...   3  0.1498 [-0.8478; 1.1473] 0.0917 0.3028
    ## age:ed.level:intervention = adult:higher-education:Gender-s ...   3  0.2767 [-0.2494; 0.8028]      0      0
    ## age:ed.level:intervention = adult:unknown:Gender-stereotype ...   1  0.1612 [-0.2355; 0.5580]     --     --
    ## age:ed.level:intervention = adolescence:upper-secundary:Gen ...   1 -0.0106 [-0.4241; 0.4028]     --     --
    ##                                                                    Q   I^2
    ## age:ed.level:intervention = adolescent:upper-secundary:Gend ... 4.71 57.5%
    ## age:ed.level:intervention = adult:higher-education:Gender-s ... 1.60  0.0%
    ## age:ed.level:intervention = adult:unknown:Gender-stereotype ... 0.00    --
    ## age:ed.level:intervention = adolescence:upper-secundary:Gen ... 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   1.46    3  0.6921
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

![](02-flow_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Funnel Plot

``` r
m.cont <- update.meta(m.cont, studlab = data$study)
summary(eggers.test(x = m.cont))
```

    ## Eggers' test of the intercept 
    ## ============================= 
    ## 
    ##  intercept       95% CI     t    p
    ##      2.745 -3.54 - 9.03 0.856 0.42
    ## 
    ## Eggers' test does not indicate the presence of funnel plot asymmetry.

``` r
funnel(m.cont, xlab = "Hedges' g", studlab = T, legend=T, addtau2 = T)
```

![](02-flow_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
