############################################################################

# materials from the "Meta-Analysis with R" workshop given by Wolfgang
# Viechtbauer on March 24, 2003, as part of the 2023 "Evidence Synthesis &
# Meta-Analysis in R Conference" (https://esmarconf.org)
#
# workshop details: https://www.wvbauer.com/doku.php/workshop_2023_ma_esmarconf
# author website:   https://www.wvbauer.com

############################################################################

# load metafor package
library(metafor)

############################################################################

# meta-analysis examining the effectiveness of school-based writing-to-learn
# interventions on academic achievement
#
# source: Bangert-Drowns, R. L., Hurley, M. M., & Wilkinson, B. (2004). The
# effects of school-based writing-to-learn interventions on academic
# achievement: A meta-analysis. Review of Educational Research, 74(1), 29-58.
# https://doi.org/10.3102/00346543074001029 https://www.jstor.org/stable/3516060
#
# one of the included studies: Ganguli, A. B. (1989). Integrating writing in
# developmental mathematics. College Teaching, 37(4), 140-142.
# https://www.jstor.org/stable/27558364
#
# note: the standardized mean differences are already computed for the studies
# in this dataset, so there is no need to use the escalc() function; positive
# values indicate a higher mean level of academic achievement in the group
# receiving a writing-to-learn intervention compared to the control group

# for a description of the dataset, see:
help(dat.bangertdrowns2004)

# copy data to 'dat'
dat <- dat.bangertdrowns2004
dat

# note: NA = not available (i.e., missing values)

# illustrate the calculation of the SMD for study 14: Ganguli (1989)
escalc(measure="SMD", m1i=342, sd1i=68, n1i=27,
                      m2i=303, sd2i=75, n2i=23)

# fit a random-effects model (use either the DL or REML estimator)

# obtain the 95% prediction interval

# do the results suggest that writing-to-learn interventions have on average a
# positive effect on academic achievement? if so, how consistent is the effect
# across studies?

# obtain a 95% CIs for tau^2 and I^2

# fit an equal-effects model (as in Bangert-Drowns et al., 2004)

# find the results provided by this model in Bangert-Drowns et al. (2004); do
# you think these are the results that should have been reported in the paper?
# why or why not?

# like Figure 1 in the paper, but without a normal distribution superimposed
# on the histogram, because we do NOT assume that the observed outcomes follow
# a normal distribution (the sampling distributions are assumed to be normal,
# but this does not imply that the observed outcomes are normally distributed)
hist(dat$yi, breaks=seq(-1.5, 2, by=0.25), xlab="Standardized Mean Difference",
     main="Histogram of the SMD Values")
abline(v=0, lwd=3)

# fit mixed-effects meta-regression models with the following moderators (one
# at a time): grade (treated categorically!), length (continuously), wic,
# feedback, info, pers, imag, and meta

# for grade, compute the estimated average SMD for each level

# for length, compute the estimated average SMD for length equal to 1 and 24

# for meta, compute the estimated average SMD for meta=0 and meta=1

# fit a model with multiple moderators and compute some predicted average SMDs
# for various combinations of the moderator values; do you see differences in
# the relevance of particular moderator variables in the model compared to the
# models where each moderator was examined individually?

############################################################################

# meta-analysis on the relationship between class attendance and class
# performance / grade point average in college students
#
# source: personal communication
#
# original meta-analysis: Crede, M., Roch, S. G., & Kieszczynka, U. M. (2010).
# Class attendance in college: A meta-analytic review of the relationship of
# class attendance with grades and student characteristics. Review of
# Educational Research, 80(2), 272-295. https://doi.org/10.3102/0034654310362998
# https://www.jstor.org/stable/40658464
#
# one of the included studies: Culler, R. E., & Holahan, C. J. (1980). Test
# anxiety and academic performance: The effects of study-related behaviors.
# Journal of Educational Psychology, 72(1), 16-20.
# https://doi.org/10.1037/0022-0663.72.1.16 (note: study 12 in the dataset)
#
# note: the data used in the meta-analysis by Crede et al. (2010) are slightly
# different than the data included in this dataset (but just slightly)

# for a description of the dataset, see:
help(dat.crede2010)

# copy data to 'dat'
dat <- dat.crede2010
dat

# we will focus on the relationship between class attendance and performance
# (i.e., the grade) within the class (i.e., when the criterion is 'grade')

# calculate r-to-z transformed correlations and corresponding sampling variances
# (note: using 'subset' to select those rows where criterion is 'grade')
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat, subset=criterion=="grade")
dat

# fit a random-effects model

# compute the estimated average true correlation, 95% CI, and 95% PI
# note: use the back-transformation transf=transf.ztor

# is there on average a correlation between class attendance and performance?
# if so, how strong is that correlation? how consistent is the correlation
# across studies? what was the estimated average correlation reported in Crede
# et al. (2004)?

# fit a mixed-effects meta-regression model with class as moderator and
# compute the predicted average correlation for non-science and science
# classes

# fit a mixed-effects meta-regression model with source as moderator

# compute the predicted average correlations for the three source types; for
# which type is the estimated average correlation the highest / the lowest?

# fit a mixed-effects meta-regression model with the year of publication as
# moderator; does it appear as if the strength of the correlation has changed
# over time? if so, is it getting stronger or weaker?

# compute the predicted average correlations for 1973, 1999, and 2009

############################################################################

# note: wait with this part until we have talked about publication bias

# meta-analysis of studies examining the risk of lung cancer due to
# environmental tobacco smoke (ETS) exposure
#
# source: Hackshaw, A. K., Law, M. R., & Wald, N. J. (1997). The accumulated
# evidence on lung cancer and environmental tobacco smoke. British Medical
# Journal, 315(7114), 980–988. https://doi.org/10.1136/bmj.315.7114.980
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2127653/
#
# see also: Hackshaw, A. K. (1998). Lung cancer and passive smoking.
# Statistical Methods in Medical Research, 7(2), 119–136.
# https://doi.org/10.1177/096228029800700203
#
# one of the included studies: Fontham, E. T., Correa, P., Reynolds, P.,
# Wu-Williams, A., Buffler, P. A., Greenberg, R. S., Chen, V. W., Alterman,
# T., Boyd, P., Austin, D. F., & Liff, J. (1994). Environmental tobacco smoke
# and lung cancer in nonsmoking women: A multicenter study. Journal of the
# American Medical Association, 271(22), 1752-1759.
# https://doi.org/10.1001/jama.1994.03510460044031
#
# note: this meta-analysis was conducted with log odds ratios (and the values
# are already included in the dataset); in this particular example, you can
# think of these values as more or less the same as log risk ratios (but
# that's not true in general!); the log odds ratios were computed so that
# values greater than 0 indicate an increased risk of cancer in exposed women
# compared to women not exposed to ETS from their spouse

# for a description of the dataset, see:
help(dat.hackshaw1998)

# copy data to 'dat'
dat <- dat.hackshaw1998
dat

# fit a random-effects model

# compute the estimated average true odds ratio, 95% CI, and 95% PI

# examine the funnel plot for these data; does the plot suggest that some
# studies may be missing? if so, what kinds of studies? (i.e., what kinds of
# effects would those missing studies show?)

# conduct a failsafe-N analysis; what do the results suggest?

# conduct the regression test for funnel plot asymmetry; does the test suggest
# that there may be an association between the standard errors of the studies
# and the observed effects?

# conduct the test of excess significance; what do the results suggest?

# apply the trim and fill method; does the method suggest that some studies
# may be missing (and on which side)? if so (i.e., after their imputation),
# what happens to the estimated effect?

# draw a funnel plot with the 'filled-in' studies added

# apply the PET and PEESE methods; what are the estimated effects according to
# these methods?

# fit a 'logistic' selection model (think about the possible direction of the
# selection; what should the 'alternative' argument be set to?); does the
# model suggest a possible selection effect?

############################################################################

# note: wait with this part until we have talked about multilevel /
# multivariate models

# back to the meta-analysis by Crede et al. (2010) on the relationship between
# class attendance and performance (i.e., the grade) within the class

# copy data to 'dat'
dat <- dat.crede2010
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat, subset=criterion=="grade")
dat

# note: some studies included multiple samples; the different samples within
# studies presumably included different subjects and hence the sampling errors
# of the (r-to-z transformed) correlation coefficients can be assumed to be
# independent, but the underlying true outcomes might be correlated

# fit a standard random-effects model

# fit a multilevel model with random effects for studies and samples within studies

# which one is the larger source of heterogeneity? differences between studies
# or differences in the outcomes within studies? and what is the estimated
# correlation between the true outcomes of different samples within the same
# study?

# fit the same model using the multivariate parameterization

# meta-analysis on the difference between schizophrenia patients and healthy
# controls with respect to their performance on the tower of London test
# (https://en.wikipedia.org/wiki/Tower_of_London_test), a cognitive tasks
# measuring planning ability
#
# source: Knapp, F., Viechtbauer, W., Leonhart, R., Nitschke, K., & Kaller, C.
# P. (2017). Planning performance in schizophrenia patients: A meta-analysis
# of the influence of task difficulty and clinical and sociodemographic
# variables. Psychological Medicine, 47(11), 2002-2016.
# https://doi.org/10.1017/S0033291717000459
#
# note: this meta-analysis was conducted with standardized mean differences
# (the values are already included in the dataset); positive values indicate
# better performance by healthy controls compared to schizophrenia patients

# for a description of the dataset, see:
help(dat.knapp2017)

# copy data to 'dat'
dat <- dat.knapp2017
dat

# note: this is a more complex dataset
#
# 1. studies 2, 3, 9, and 20 included more than one schizophrenia patient
#    group and the standardized mean differences were computed by comparing
#    these groups against a single healthy control group
# 2. studies 6, 12, 14, 15, 18, 19, 22, and 26 had the patients and controls
#    complete different tasks of varying complexity (essentially the average
#    number of moves required to complete a task); study 6 also included two
#    different task types
# 3. study 24 provides two standardized mean differences, one for men and the
#    other for women
# 4. study 29 provides three standardized mean differences, corresponding to
#    the three different COMT Val158Met genotypes (val/val, val/met, met/met)

# all 4 issues described above lead to a multilevel structure in the dataset,
# with multiple standardized mean differences nested within some of the studies;
# issues 1. and 2. also lead to correlated sampling errors

# fit a standard random-effects model ignoring these issues

# fit a multilevel model with random effects for studies (variable 'study')
# and comparisons within studies (variable 'comp')

# construct an approximate V matrix assuming a correlation of 0.4 for sampling
# errors of different comparisons within the same study

# fit again the same multilevel model, but now use the V matrix in the model

# use cluster-robust inference methods based on this model

# examine if task difficulty is a potential moderator of the effect

############################################################################
