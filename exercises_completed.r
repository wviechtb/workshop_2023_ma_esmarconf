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
res <- rma(yi, vi, data=dat)
res

# obtain the 95% prediction interval
predict(res, digits=2)

# do the results suggest that writing-to-learn interventions have on average a
# positive effect on academic achievement? if so, how consistent is the effect
# across studies?

# yes, the estimated average standardized mean difference is positive (0.22;
# 95% CI: 0.13 to 0.31) and significantly different from 0; however, there
# does appear to be heterogeneity in the true effects (Q(47)=107.11, p<.0001)
# and the 95% prediction interval (-0.23 to 0.67) indicates that the effect in
# individual studies can vary quite a bit, so there could be circumstances
# where such interventions are ineffective (or even counterproductive) and
# circumstances where such interventions are quite a bit more effective

# obtain a 95% CIs for tau^2 and I^2
confint(res)

# fit an equal-effects model (as in Bangert-Drowns et al., 2004)
res <- rma(yi, vi, data=dat, method="EE", digits=2)
res

# find the results provided by this model in Bangert-Drowns et al. (2004); do
# you think these are the results that should have been reported in the paper?
# why or why not?

# we can find these results on page 43; however, it does not seem sensible to
# use an equal-effects model here, since there is pretty clear indication of
# heterogeneity in the true effects, so a random-effects model seems more
# appropriate for these data

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
res <- rma(yi, vi, mods = ~ factor(grade), data=dat)
res
predict(res, newmods=c(0,0,0), digits=2)
predict(res, newmods=c(1,0,0), digits=2)
predict(res, newmods=c(0,1,0), digits=2)
predict(res, newmods=c(0,0,1), digits=2)

# can also do this with a single call to predict()
predict(res, newmods=rbind(0,diag(3)), digits=2)

# for length, compute the estimated average SMD for length equal to 1 and 24
res <- rma(yi, vi, mods = ~ length, data=dat)
res
predict(res, newmods=1,  digits=2)
predict(res, newmods=24, digits=2)

# same as this
predict(res, newmods=c(1,24), digits=2)

# scatterplot of the SMDs against length with the regression line added
regplot(res, xlim=c(0,25), xlab="Length", las=1, digits=1, bty="l")

# other moderators
rma(yi, vi, mods = ~ wic, data=dat)
rma(yi, vi, mods = ~ feedback, data=dat)
rma(yi, vi, mods = ~ info, data=dat)
rma(yi, vi, mods = ~ pers, data=dat)
rma(yi, vi, mods = ~ imag, data=dat)
rma(yi, vi, mods = ~ meta, data=dat)

# for meta, compute the estimated average SMD for meta=0 and meta=1
res <- rma(yi, vi, mods = ~ meta, data=dat)
res
predict(res, newmods=0, digits=2)
predict(res, newmods=1, digits=2)

# again can also just use this
predict(res, newmods=c(0,1), digits=2)

# fit a model with multiple moderators and compute some predicted average SMDs
# for various combinations of the moderator values; do you see differences in
# the relevance of particular moderator variables in the model compared to the
# models where each moderator was examined individually?
res <- rma(yi, vi, mods = ~ factor(grade) + length + meta, data=dat)
res

# predicted average SMDs for grade = 2, length = 15, and meta=0/1
predict(res, newmods=c(1,0,0, 15, 0), digits=2)
predict(res, newmods=c(1,0,0, 15, 1), digits=2)

# test the grade factor as a whole (use btt to specify the coefficient numbers
# that you want to include in the omnibus test; here, we want to test the 2nd,
# 3rd, and 4th coefficient)
anova(res, btt=2:4)

# can also specify a string that identifies all coefficients to be tested
anova(res, btt="grade")

# can also specify multiple strings
anova(res, btt=c("grade","length","meta"))

# or when specifying coefficient numbers, use a list
anova(res, btt=list(2:4,5,6))

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
res <- rma(yi, vi, data=dat)
res

# compute the estimated average true correlation, 95% CI, and 95% PI
# note: use the back-transformation transf=transf.ztor
predict(res, transf=transf.ztor, digits=2)

# is there on average a correlation between class attendance and performance?
# if so, how strong is that correlation? how consistent is the correlation
# across studies? what was the estimated average correlation reported in Crede
# et al. (2004)?

# yes, the estimated average correlation is 0.43 (95% CI: 0.38 to 0.47),
# although the prediction interval (0.01 to 0.72) indicates substantial
# heterogeneity in how strong the correlation may be in individual studies

# fit a mixed-effects meta-regression model with class as moderator and
# compute the predicted average correlation for non-science and science
# classes
res <- rma(yi, vi, mods = ~ class, data=dat)
res
predict(res, newmods=0, transf=transf.ztor, digits=2)
predict(res, newmods=1, transf=transf.ztor, digits=2)

# fit a mixed-effects meta-regression model with source as moderator
res <- rma(yi, vi, mods = ~ source, data=dat)
res

# compute the predicted average correlations for the three source types; for
# which type is the estimated average correlation the highest / the lowest?
predict(res, newmods=c(0,0), transf=transf.ztor, digits=2)
predict(res, newmods=c(1,0), transf=transf.ztor, digits=2)
predict(res, newmods=c(0,1), transf=transf.ztor, digits=2)

# the estimated average correlation is highest for source = 'dissertation'
# (0.55) and lowest for source = 'journal' (0.40)

# fit a mixed-effects meta-regression model with the year of publication as
# moderator; does it appear as if the strength of the correlation has changed
# over time? if so, is it getting stronger or weaker?
res <- rma(yi, vi, mods = ~ year, data=dat)
res

# while the slope for 'year' is positive (suggesting an increase in the
# correlation over time), it is not significantly different from 0 (p = .24)

# compute the predicted average correlations for 1973, 1999, and 2009
predict(res, newmods=1973, transf=transf.ztor, digits=2)
predict(res, newmods=1999, transf=transf.ztor, digits=2)
predict(res, newmods=2009, transf=transf.ztor, digits=2)

# scatterplot of r-to-z transformed correlations against year of publication
regplot(res, xlab="Year of Publication", las=1, digits=1, bty="l", psize="seinv")

# in Crede et al. (2004), the relationship between the year of publication and
# the correlations was tested with a simple correlation test (but this ignores
# differences in the size of the studies, so not a recommended approach)
cor.test(dat$ri, dat$year)

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
res <- rma(yi, vi, data=dat)
res

# compute the estimated average true odds ratio, 95% CI, and 95% PI
predict(res, transf=exp, digits=2)

# examine the funnel plot for these data; does the plot suggest that some
# studies may be missing? if so, what kinds of studies? (i.e., what kinds of
# effects would those missing studies show?)
funnel(res)

# there does appear to be a bit of a gap on the bottom left of the plot; this
# area corresponds to small studies where the log odds ratio happens to be
# negative (which would correspond to a lower risk in exposed women)

# can you spot the actual funnel plot among these 20 simulated ones?

set.seed(1234)
sim <- replicate(20, simulate(res))
obs <- sample(20, 1) # random number between 1 and 20
sim[[obs]] <- dat$yi # replace simulated dataset 'obs' with the real data

par(mfrow=c(4,5), cex=0.5, mar=c(3,3,3,3))
invisible(sapply(1:20, function(i)
   funnel(sim[[i]], dat$vi, refline=coef(res), main=paste("Plot:", i))))

# again, this can be quite difficult!

# close plot (to reset par() adjustments)
dev.off()

# conduct a failsafe-N analysis; what do the results suggest?
fsn(yi, vi, data=dat)

# close to 400 studies with null results would be needed to overturn the
# conclusion that the risk of cancer is elevated in exposed women (in at least
# one study); it seems implausible that so many studies with null results are
# sitting in file drawers, so this suggests that the finding is to some extent
# robust to potential publication bias

# failsafe-N analysis using the Rosenberg method
fsn(yi, vi, data=dat, type="Rosenberg")

# this would suggest around 200 studies with 0 effects would be needed to
# reduce the estimate from an equal-effects model to non-significance; that is
# still a rather large (and not very plausible) number

# conduct the regression test for funnel plot asymmetry; does the test suggest
# that there may be an association between the standard errors of the studies
# and the observed effects?
regtest(res)

# yes, the test is (just) significant (p=.0352)

# conduct the test of excess significance; what do the results suggest?
tes(yi, vi, data=dat)

# there is a higher number of significant tests in this dataset than we would
# expect based on the power of the tests; however, the difference is not quite
# significant (p = 0.1419)

# apply the trim and fill method; does the method suggest that some studies
# may be missing (and on which side)? if so (i.e., after their imputation),
# what happens to the estimated effect?
taf <- trimfill(res)
taf

# the method estimates that 7 studies on the left side of the funnel plot are
# missing; after their imputation, the estimated effect (0.1745) is pulled
# towards 0 (cf. 0.2189 from the standard random-effects model), although it
# remains statistically significant

# draw a funnel plot with the 'filled-in' studies added
funnel(taf)

# apply the PET and PEESE methods; what are the estimated effects according to
# these methods?
regtest(res)
regtest(res, predictor="vi")

# according to PET, the limit estimate is essentially zero (0.0216)); on the
# other hand, PEESE yields an estimate (0.1405) that is quite similar to the
# one from the trim and fill method (and the 95% CI still excludes 0)

# fit a 'logistic' selection model (think about the possible direction of the
# selection; what should the 'alternative' argument be set to?); does the
# model suggest a possible selection effect?
sel <- selmodel(res, type="logistic", alternative="greater")
sel
plot(sel)

# the (Wald-type) test of the selection parameter is significant (p=.0379),
# although the likelihood ratio test just fails to be significant (p=.0548);
# the estimate of the 'adjusted' effect is very close to 0 (0.0324) and no
# longer significant

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
res <- rma(yi, vi, data=dat)
res

# fit a multilevel model with random effects for studies and samples within studies
res <- rma.mv(yi, vi, random = ~ 1 | studyid/sampleid, data=dat)
res

# which one is the larger source of heterogeneity? differences between studies
# or differences in the outcomes within studies? and what is the estimated
# correlation between the true outcomes of different samples within the same
# study?
round(res$sigma2[1] / sum(res$sigma2), digits=4)

# the variance component for between-study heterogeneity is the larger one
# (0.0376 vs 0.0159); the estimated correlation between the true outcomes
# within studies is 0.7033

# fit the same model using the multivariate parameterization
res <- rma.mv(yi, vi, random = ~ sampleid | studyid, data=dat)
res

# LRT comparing a standard RE model with the multilevel model
dat$id <- 1:nrow(dat)
res0 <- rma.mv(yi, vi, random = ~ 1 | id, data=dat)
res1 <- rma.mv(yi, vi, random = ~ 1 | studyid/sampleid, data=dat)
anova(res0, res1)

# the LRT is significant (p=.0237), indicating that the multilevel model
# provides a better fit than the standard RE model

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
res <- rma(yi, vi, data=dat)
res

# fit a multilevel model with random effects for studies (variable 'study')
# and comparisons within studies (variable 'comp')
res <- rma.mv(yi, vi, random = ~ 1 | study/comp, data=dat)
res

# construct an approximate V matrix assuming a correlation of 0.4 for sampling
# errors of different comparisons within the same study
V <- vcalc(vi, cluster=study, obs=comp, data=dat, rho=0.4)

# fit again the same multilevel model, but now use the V matrix in the model
res <- rma.mv(yi, V, random = ~ 1 | study/comp, data=dat)
res

# use cluster-robust inference methods based on this model
robust(res, cluster=dat$study)
robust(res, cluster=dat$study, clubSandwich=TRUE)

# examine if task difficulty is a potential moderator of the effect
res <- rma.mv(yi, V, mods = ~ difficulty, random = ~ 1 | study/comp, data=dat)
res
robust(res, cluster=dat$study, clubSandwich=TRUE)

# draw a bubble plot (CI for regression line based on the robust() results)
sav <- robust(res, cluster=dat$study, clubSandwich=TRUE)
regplot(sav, xlab="Task Difficulty", ylab="Standardized Mean Difference",
        las=1, digits=1, bty="l")

############################################################################
