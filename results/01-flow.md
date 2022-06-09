Metanalysis of flow state in condition of Stereotype Threat (`stThreat`)
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
cond <- "stThreat"
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

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Subgroup analysis by “age”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = age, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)         age
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0  adolescent
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3  adolescent
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9  adolescent
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5       adult
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4       adult
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3       adult
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7       adult
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8 adolescence
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
    ## 
    ## Results for subgroups (random effects model):
    ##                     k     SMD             95%-CI  tau^2    tau    Q   I^2
    ## age = adolescent    3  0.1247 [-0.6949;  0.9444] 0.0403 0.2008 3.43 41.7%
    ## age = adult         4  0.2839 [ 0.1999;  0.3680]      0      0 0.15  0.0%
    ## age = adolescence   1 -0.4296 [-0.8583; -0.0010]     --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                      Q d.f. p-value
    ## Between groups   11.09    2  0.0039
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Subgroup analysis by “ed.level”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = ed.level, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)         ed.level
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0  upper-secundary
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3  upper-secundary
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9  upper-secundary
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5 higher-education
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4 higher-education
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3 higher-education
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7          unknown
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8  upper-secundary
    ## 
    ## Number of studies combined: k = 8
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
    ## 
    ## Results for subgroups (random effects model):
    ##                               k     SMD            95%-CI  tau^2    tau    Q   I^2
    ## ed.level = upper-secundary    4 -0.0251 [-0.6469; 0.5968] 0.0953 0.3088 8.09 62.9%
    ## ed.level = higher-education   3  0.2748 [ 0.1118; 0.4377]      0      0 0.14  0.0%
    ## ed.level = unknown            1  0.3025 [-0.1003; 0.7053]     --     -- 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   2.30    2  0.3160
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Subgroup analysis by “intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = intervention, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8
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
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
    ## 
    ## Results for subgroups (random effects model):
    ##                                                      k     SMD             95%-CI tau^2 tau    Q  I^2
    ## intervention = Gender-stereotype color, rankin ...   7  0.2202 [ 0.0337;  0.4067]     0   0 4.28 0.0%
    ## intervention = Gender-stereotyped motivational ...   1 -0.4296 [-0.8583; -0.0010]    --  -- 0.00   --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                     Q d.f. p-value
    ## Between groups   7.87    1  0.0050
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Subgroup analysis by “age:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `age:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8
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
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
    ## 
    ## Results for subgroups (random effects model):
    ##                                                          k     SMD             95%-CI  tau^2    tau    Q
    ## age:intervention = adolescent:Gender-stereotype co ...   3  0.1247 [-0.6949;  0.9444] 0.0403 0.2008 3.43
    ## age:intervention = adult:Gender-stereotype color,  ...   4  0.2839 [ 0.1999;  0.3680]      0      0 0.15
    ## age:intervention = adolescence:Gender-stereotyped  ...   1 -0.4296 [-0.8583; -0.0010]     --     -- 0.00
    ##                                                          I^2
    ## age:intervention = adolescent:Gender-stereotype co ... 41.7%
    ## age:intervention = adult:Gender-stereotype color,  ...  0.0%
    ## age:intervention = adolescence:Gender-stereotyped  ...    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                      Q d.f. p-value
    ## Between groups   11.09    2  0.0039
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Subgroup analysis by “ed.level:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `ed.level:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8
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
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
    ## 
    ## Results for subgroups (random effects model):
    ##                                                               k     SMD             95%-CI  tau^2    tau
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...   3  0.1247 [-0.6949;  0.9444] 0.0403 0.2008
    ## ed.level:intervention = higher-education:Gender-stereot ...   3  0.2748 [ 0.1118;  0.4377]      0      0
    ## ed.level:intervention = unknown:Gender-stereotype color ...   1  0.3025 [-0.1003;  0.7053]     --     --
    ## ed.level:intervention = upper-secundary:Gender-stereoty ...   1 -0.4296 [-0.8583; -0.0010]     --     --
    ##                                                                Q   I^2
    ## ed.level:intervention = upper-secundary:Gender-stereoty ... 3.43 41.7%
    ## ed.level:intervention = higher-education:Gender-stereot ... 0.14  0.0%
    ## ed.level:intervention = unknown:Gender-stereotype color ... 0.00    --
    ## ed.level:intervention = upper-secundary:Gender-stereoty ... 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                      Q d.f. p-value
    ## Between groups   10.58    3  0.0142
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Subgroup analysis by “age:ed.level:intervention”

``` r
m.sg4sub <- update.meta(m.cont, subgroup = `age:ed.level:intervention`, random = T, fixed = F)
summary(m.sg4sub)
```

    ## Review:     Performance in stThreat
    ## 
    ##                               SMD             95%-CI %W(random)
    ## S1                         0.4174 [-0.0990;  0.9338]       11.0
    ## S2                         0.1925 [-0.2204;  0.6053]       14.3
    ## S3                        -0.2607 [-0.7823;  0.2610]       10.9
    ## S4                         0.2676 [-0.2677;  0.8028]       10.5
    ## S5                         0.3398 [-0.1285;  0.8082]       12.4
    ## S6                         0.2142 [-0.2581;  0.6865]       12.3
    ## S7                         0.3025 [-0.1003;  0.7053]       14.7
    ## S10: Only use prompt msgs -0.4296 [-0.8583; -0.0010]       13.8
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
    ## Number of observations: o = 600
    ## 
    ##                         SMD            95%-CI    t p-value
    ## Random effects model 0.1274 [-0.1287; 0.3835] 1.18  0.2778
    ## 
    ## Quantifying heterogeneity:
    ##  tau^2 = 0.0393 [0.0000; 0.3290]; tau = 0.1981 [0.0000; 0.5736]
    ##  I^2 = 40.8% [0.0%; 73.9%]; H = 1.30 [1.00; 1.96]
    ## 
    ## Test of heterogeneity:
    ##      Q d.f. p-value
    ##  11.83    7  0.1063
    ## 
    ## Results for subgroups (random effects model):
    ##                                                                   k     SMD             95%-CI  tau^2    tau
    ## age:ed.level:intervention = adolescent:upper-secundary:Gend ...   3  0.1247 [-0.6949;  0.9444] 0.0403 0.2008
    ## age:ed.level:intervention = adult:higher-education:Gender-s ...   3  0.2748 [ 0.1118;  0.4377]      0      0
    ## age:ed.level:intervention = adult:unknown:Gender-stereotype ...   1  0.3025 [-0.1003;  0.7053]     --     --
    ## age:ed.level:intervention = adolescence:upper-secundary:Gen ...   1 -0.4296 [-0.8583; -0.0010]     --     --
    ##                                                                    Q   I^2
    ## age:ed.level:intervention = adolescent:upper-secundary:Gend ... 3.43 41.7%
    ## age:ed.level:intervention = adult:higher-education:Gender-s ... 0.14  0.0%
    ## age:ed.level:intervention = adult:unknown:Gender-stereotype ... 0.00    --
    ## age:ed.level:intervention = adolescence:upper-secundary:Gen ... 0.00    --
    ## 
    ## Test for subgroup differences (random effects model):
    ##                      Q d.f. p-value
    ## Between groups   10.58    3  0.0142
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

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Funnel Plot

``` r
m.cont <- update.meta(m.cont, studlab = data$study)
summary(eggers.test(x = m.cont))
```

    ## Eggers' test of the intercept 
    ## ============================= 
    ## 
    ##  intercept        95% CI     t    p
    ##      0.883 -8.52 - 10.28 0.184 0.86
    ## 
    ## Eggers' test does not indicate the presence of funnel plot asymmetry.

``` r
funnel(m.cont, xlab = "Hedges' g", studlab = T, legend=T, addtau2 = T)
```

![](/Users/gcc/Insync/geiser@alumni.usp.br/Google%20Drive/Workspace/nature/results/01-flow_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
