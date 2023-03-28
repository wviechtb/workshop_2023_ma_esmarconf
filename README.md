# Meta-Analysis with R Workshop

### Wolfgang Viechtbauer

#### March 24, 2023

## Overview

This repo contains the materials for the "Meta-Analysis with R" workshop given on March 24, 2023, as part of the [2023 Evidence Synthesis & Meta-Analysis in R Conference](https://esmarconf.org). Further details about the workshop can be found [here](https://www.wvbauer.com/doku.php/workshop_2023_ma_esmarconf).

## Structure

The lecture slides are provided [here](workshop_meta-analysis.pdf). We loosely followed the slides, but the focus in this workshop was on the application of the methods using R. We started with [code_r_bcg.r](code_r_bcg.r), which illustrates some of the basic functionality of the [`metafor`](https://www.metafor-project.org) package (in particular, the use of the [`escalc()`](https://wviechtb.github.io/metafor/reference/escalc.html) function for computing various effect size or outcome measures and the [`rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html) function for fitting equal/random-effects model to these values and for conducting meta-regression analyses). This is also the same example as used in [Viechtbauer (2010)](https://www.jstatsoft.org/article/view/v036i03). The [exercises.r](exercises.r) script provides further examples of this.

Next, we looked at the issue of publication bias and related methods. These are illustrated in [code_r_magnesium.r](code_r_magnesium.r). The exercise script again contains further examples. Due to lack of time, we did not really get into multilevel/multivariate models, but some applications are provided in [code_r_ml_mv.r](code_r_ml_mv.r). More examples thereof are again part of the exercises.

Finally, the [code_r_other.r](code_r_other.r) script covers various topics, including improved methods for testing / constructing confidence intervals, specialized methods for meta-analyzing 2x2 table data, model diagnostics, and Bayesian models.
