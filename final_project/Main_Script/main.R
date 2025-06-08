#LOADING LIBRARIES
#-------------------------------------------------------------------------------

library(spida2)
library(p3d)
library(nlme)
library(car)
library(lattice)
library(latticeExtra)
library(dplyr)
library(glmmTMB)
library(DHARMa)
library(splines)
library(performance)

#DATA LOADING AND CLEANING
#-------------------------------------------------------------------------------

df <- read.csv("D:/Daniel/Documents/MATH6642/final_project/oasis_longitudinal.csv")
df
dc <- df[!is.na(df$MMSE) & !is.na(df$SES), ]
dc

#Note: SES is ordered lowest = 5 and highest = 1; we will reverse the ordering
dc$SES <- 5 - dc$SES
dc

dc_orig <- dc
dc_orig

table_per_subject <- table(dc$Subject.ID)
table_per_subject[table_per_subject < 2]

dc <- dc[order(dc$Subject.ID, dc$Age), ]

#DATA VISUALIZATION
#-------------------------------------------------------------------------------

xqplot(dc)

par(mfrow = c(1, 3))

hist(dc$EDUC, 
     breaks = 10, 
     main = "Histogram of Education Level", 
     xlab = "Years of Education",
     ylab = "Number of Individuals",
     col = "lightblue",
     border = "white")

barplot(table(dc$SES),
        main = "Barplot of Socioeconomic Status",
        xlab = "SES",
        ylab = "Number of Individuals",
        col = "lightblue",
        border = "white")

barplot(table(dc$M.F), 
        main = "Bar Chart of Sex", 
        xlab = "Sex", 
        ylab = "Number of Individuals", 
        col = c("pink", "lightblue"))

par(mfrow = c(1, 1))

initial_visits <- dc[ave(dc$Visit, dc$Subject.ID, FUN = min) == dc$Visit, ]
final_visits <- dc[ave(dc$Visit, dc$Subject.ID, FUN = max) == dc$Visit, ]

#Set layout: 1 row, 2 columns
par(mfrow = c(1, 2))

#Histogram for initial visits
hist(initial_visits$MMSE,
     main = "Initial Visit MMSE", 
     breaks = 0:30,
     xlab = "MMSE",
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

#Histogram for final visits
hist(final_visits$MMSE,
     main = "Final Visit MMSE", 
     breaks = 0:30,
     xlab = "MMSE",
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

#Reset layout
par(mfrow = c(1, 1))

#Get initial visits per subject
initial_visits <- dc %>%
  group_by(Subject.ID) %>%
  filter(Visit == min(Visit)) %>%
  ungroup()

#Filter subjects who had CDR = 0 at initial visit
subjects_cdr0 <- initial_visits %>%
  filter(CDR == 0) %>%
  pull(Subject.ID)

#Initial visits for subjects with CDR = 0 at initial visit
initial_cdr0 <- dc %>%
  filter(Subject.ID %in% subjects_cdr0) %>%
  group_by(Subject.ID) %>%
  filter(Visit == min(Visit)) %>%
  ungroup()

#Final visits for subjects with CDR = 0 at initial visit
final_cdr0 <- dc %>%
  filter(Subject.ID %in% subjects_cdr0) %>%
  group_by(Subject.ID) %>%
  filter(Visit == max(Visit)) %>%
  ungroup()

par(mfrow = c(1, 2))

hist(initial_cdr0$MMSE, 
     breaks = 0:30, 
     main = "Initial Visit MMSE (CDR=0)", 
     xlab = "MMSE", 
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

hist(final_cdr0$MMSE, 
     breaks = 0:30, 
     main = "Final Visit MMSE (CDR=0)", 
     xlab = "MMSE", 
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

par(mfrow = c(1, 1))

#Get initial visits per subject
initial_visits <- dc %>%
  group_by(Subject.ID) %>%
  filter(Visit == min(Visit)) %>%
  ungroup()

#Filter subjects who had CDR = 0.5 at initial visit
subjects_cdr05 <- initial_visits %>%
  filter(CDR == 0.5) %>%
  pull(Subject.ID)

#Initial visits for subjects with CDR = 0.5 at initial visit
initial_cdr05 <- dc %>%
  filter(Subject.ID %in% subjects_cdr05) %>%
  group_by(Subject.ID) %>%
  filter(Visit == min(Visit)) %>%
  ungroup()

#Final visits for subjects with CDR = 0.5 at initial visit
final_cdr05 <- dc %>%
  filter(Subject.ID %in% subjects_cdr05) %>%
  group_by(Subject.ID) %>%
  filter(Visit == max(Visit)) %>%
  ungroup()

par(mfrow = c(1, 2))

#Histogram for initial visits
hist(initial_cdr05$MMSE, 
     breaks = 0:30, 
     main = "Initial Visit MMSE (CDR=0.5)", 
     xlab = "MMSE", 
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

#Histogram for final visits
hist(final_cdr05$MMSE, 
     breaks = 0:30, 
     main = "Final Visit MMSE (CDR=0.5)", 
     xlab = "MMSE", 
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

par(mfrow = c(1, 1))

#Get initial visits per subject
initial_visits <- dc %>%
  group_by(Subject.ID) %>%
  filter(Visit == min(Visit)) %>%
  ungroup()

#Filter subjects who had CDR = 1 at initial visit
subjects_cdr1 <- initial_visits %>%
  filter(CDR == 1) %>%
  pull(Subject.ID)

#Initial visits for subjects with CDR = 1 at initial visit
initial_cdr1 <- dc %>%
  filter(Subject.ID %in% subjects_cdr1) %>%
  group_by(Subject.ID) %>%
  filter(Visit == min(Visit)) %>%
  ungroup()

#Final visits for subjects with CDR = 1 at initial visit
final_cdr1 <- dc %>%
  filter(Subject.ID %in% subjects_cdr1) %>%
  group_by(Subject.ID) %>%
  filter(Visit == max(Visit)) %>%
  ungroup()

par(mfrow = c(1, 2))

#Histogram for initial visits
hist(initial_cdr1$MMSE, 
     breaks = 0:30, 
     main = "Initial Visit MMSE (CDR=1)", 
     xlab = "MMSE", 
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

#Histogram for final visits
hist(final_cdr1$MMSE, 
     breaks = 0:30, 
     main = "Final Visit MMSE (CDR=1)", 
     xlab = "MMSE", 
     ylab = "Number of Individuals",
     col = "lightblue", 
     border = "white", 
     xlim = c(0, 30))

par(mfrow = c(1, 1))

#LONGITUDINAL PLOTS
#-------------------------------------------------------------------------------

cdr_colors <- c("0" = "lightblue", "0.5" = "violet", "1" = "darkblue")

#nWBV trajectories
xyplot(nWBV ~ Age, data = dc,
       groups = Subject.ID,
       type = "b",
       lwd = 1,
       pch = 16,
       col = cdr_colors[as.character(dc$CDR)],
       xlab = "Age (years)",
       ylab = "nWBV (%)",
       main = "Longitudinal trajectories of nWBV per subject by CDR group",
       key = list(text = list(c("CDR 0", "CDR 0.5", "CDR 1")),
                  points = list(pch = 16, col = c("lightblue", "violet", "darkblue")),
                  columns = 3))

#MMSE trajectories
xyplot(MMSE ~ Age, data = dc,
       groups = Subject.ID,
       type = "b",
       lwd = 1,
       pch = 16,
       col = cdr_colors[as.character(dc$CDR)],
       xlab = "Age (years)",
       ylab = "MMSE",
       main = "Longitudinal trajectories of MMSE per subject by CDR group",
       key = list(text = list(c("CDR 0", "CDR 0.5", "CDR 1")),
                  points = list(pch = 16, col = c("lightblue", "violet", "darkblue")),
                  columns = 3))

#Within-stage MMSE trajectories
xyplot(MMSE ~ Age | factor(CDR), data = dc, 
       groups = Subject.ID, 
       lwd = 1, 
       pch = 16, 
       xlab = "Age (years)", 
       ylab = "MMSE", 
       main = "Within-stage MMSE trajectories by CDR group", 
       strip = strip.custom(bg = "lightgray"), 
       panel = function(x, y, groups, subscripts, ...) { 
         # Individual trajectories
         panel.superpose(x, y, groups = groups, subscripts = subscripts, 
                         type = "b", lwd = 1, pch = 16)
       })

# Copy dc to avoid modifying original
dc_copy <- dc

# Calculate slopes per subject manually
subject_slopes <- dc_copy %>%
  group_by(Subject.ID, CDR) %>%
  filter(n() > 1) %>%  # need at least two points per subject
  summarise(
    slope = {
      fit <- lm(MMSE ~ Age, data = cur_data())
      coef(fit)["Age"]
    },
    .groups = "drop"
  )

# Average slopes by CDR group
avg_slopes_by_CDR_old <- subject_slopes %>%
  group_by(CDR) %>%
  summarise(
    avg_slope = mean(slope, na.rm = TRUE),
    n = n()
  )

print(avg_slopes_by_CDR_old)

#LONGITUDINAL MODELLING
#-------------------------------------------------------------------------------

#Try random slope Age since it's time-variant

fit1 <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC, 
                  data = dc, 
                  random = ~ 1 + Age | Subject.ID,
                  correlation = corAR1(form = ~ 1 | Subject.ID))
summary(fit1)

#Try intercept only

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC, 
            data = dc, 
            random = ~ 1 | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID))
summary(test)

anova(fit1, test)

fit1 <- update(fit1, random = ~ 1 | Subject.ID)

#Try random slope nWBV since it's time-variant

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Test to see which is better

anova(fit1, test)

#Lower AIC, BIC so test is better

fit2 <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(fit2)

wald( fit1 )
wald( fit2 )

#Currently fit2 is our best model

#3-way Interaction terms:

#nWBV * Age * SES, SES influences access to healthcare, slowing decay in
#nWBV during aging

#nWBV * Age * M.F possible differences in nWBV decay by sex as you age

#nWBV * Age * EDUC education acts as a buffer to slow loss of brain volume
#as you age

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * Age * SES
            + nWBV * Age + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Switch to ML so we can use ANOVA

test_ml <- update(test, method = "ML")
fit2_ml <- update(fit2, method = "ML")

anova(test_ml, fit2_ml)
#test_ml better, but has all p-values > 0.05 (???)
#For now keep as fit3

fit3 <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * Age * SES
            + nWBV * Age + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(fit3)

#Proceed with next interaction

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * Age * SES
            + nWBV * Age + nWBV * SES + Age * SES + nWBV * Age * M.F
            + nWBV * M.F + Age * M.F, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Switch to ML so we can use ANOVA
test_ml <- update(test, method = "ML")
fit3_ml <- update(fit3, method = "ML")

anova(test_ml, fit3_ml)

#lower AIC, log likelihood, p-value close to 0.05, proceed with fit3

anova(test_ml, fit2_ml)

#test has lower log likelihood, p-value < 0.05

#Next interaction versus fit3

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * Age * SES
            + nWBV * Age + nWBV * SES + Age * SES + nWBV * Age * EDUC
            + nWBV * EDUC + Age * EDUC, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Switch to ML for ANOVA
test_ml <- update(test, method = "ML")

anova(test_ml, fit3_ml)
#fit3 better model

#ML for anova
fit2 <- update(fit2, method = "ML")
fit3 <- update(fit3, method = "ML")
anova(fit2, fit3)

#REML to compare wald
fit2 <- update(fit2, method = "REML")
fit3 <- update(fit3, method = "REML")

wald(fit2)
wald(fit3)

anova(fit2, fit3)

#fit3 appears to be better, but all terms insignificant. Try 2-way interactions...

#nWBV * Age : brain volume changes with age
#nWBV * EDUC: education acts as a buffer against brain volume loss
#nWBV * SES : higher SES allows for better healthcare
#nWBV * M.F : sex affects change in brain volume
#Age * EDUC : education acts as a buffer against age effects
#Age * M.F : aging affects women and men differently
#Age * SES : higher SES acts as a buffer against age effects (healthcare access)

dc$nWBV <- scale(dc$nWBV, center = TRUE, scale = FALSE)

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * Age, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#centered within person value
test2 <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * Age, 
            data = dc, 
            random = ~ 1 + dvar(nWBV, Subject.ID) | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test2)

anova(test, test2)

#switch to ML for anova
ml_test <- update(test, method = "ML")
ml_fit2 <- update(fit2, method = "ML")
anova(ml_fit2, ml_test)

#high p-value, keep fit2 and proceed with next interaction

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * EDUC, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
anova(ml_fit2, ml_test)

#high p-value with minimal improvement, keep fit2

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
anova(ml_fit2, ml_test)

#low p-value, lower log-likelihood. keep as ideal model fit4

fit4 <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))

#next interaction

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES + nWBV * M.F, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
ml_fit4 <- update(fit4, method = "ML")
anova(ml_fit4, ml_test)

#high p-value, keep fit4. proceed with next interaction

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES + Age * EDUC, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
anova(ml_fit4, ml_test)

#high p-value, keep fit4 and proceed with next interaction

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES + Age * M.F, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
anova(ml_fit4, ml_test)

#high p-value, keep fit4 and proceed with next interaction

test <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
anova(ml_fit4, ml_test)

#low p-value, lower log likelihood. note in summary, many p-values are
#significant. keep as fit5

fit5 <- lme(MMSE ~ nWBV + Age + M.F + SES + EDUC + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(fit5)

#best candidates so far: fit2, fit3, fit5

#for fit5: can we drop sex?

test <- lme(MMSE ~ nWBV + Age + SES + EDUC + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_fit5 <- update(fit5, method = "ML")
ml_test <- update(test, method = "ML")
anova(ml_fit5, ml_test)

#high p-value - drop sex. can we drop education?

fit5 <- lme(MMSE ~ nWBV + Age + SES + EDUC + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
ml_test <- update(test, method = "ML")
ml_fit5 <- update(fit5, method ="ML")
anova(ml_fit5, ml_test)

#high p-value - drop education

fit5 <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(fit5)
ml_fit5 <- update(fit5, method = "ML")


#anova for fit2 vs fit3, fit2 vs fit5, fit3 vs fit5

anova(ml_fit2, fit3_ml)
#fit3 is better than fit2

anova(ml_fit2, ml_fit5)
#fit5 is better than fit2

anova(fit3_ml, ml_fit5)
#fit5 is better than fit3 but high p-value...

#"best" model appears to be fit5
summary(fit3)
summary(fit5)

dc$fit_vals2 <- fitted(fit2)
dc$fit_vals3 <- fitted(fit3)
dc$fit_vals5 <- fitted(fit5)

xyplot(MMSE ~ Age | factor(Subject.ID), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories per Subject",
       panel = function(x, y, subscripts, ...) {

         panel.xyplot(x, y, col = "black", pch = 16, ...)
        
         fit_vals <- dc$fit_vals2[subscripts]
         
         ord <- order(x)
         
         panel.lines(x[ord], fit_vals[ord], col = "blue", lwd = 2)
       })

xyplot(MMSE ~ Age | factor(Subject.ID), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories per Subject",
       panel = function(x, y, subscripts, ...) {

         panel.xyplot(x, y, col = "black", pch = 16, ...)
         
         fit_vals <- dc$fit_vals3[subscripts]
         
         ord <- order(x)
         
         panel.lines(x[ord], fit_vals[ord], col = "blue", lwd = 2)
       })

xyplot(MMSE ~ Age | factor(Subject.ID), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories per Subject",
       panel = function(x, y, subscripts, ...) {

         panel.xyplot(x, y, col = "black", pch = 16, ...)
         
         fit_vals <- dc$fit_vals5[subscripts]
         
         ord <- order(x)
         
         panel.lines(x[ord], fit_vals[ord], col = "blue", lwd = 2)
       })

#These visualizations reaffirm that so far, fit5 is the "best" model

#GROUPING BY CDR, VISUALIZATIONS: PART 1
#-------------------------------------------------------------------------------

dc_0 <- subset(dc_orig, CDR == 0)
dc_0.5 <- subset(dc_orig, CDR == 0.5)
dc_1 <- subset(dc_orig, CDR == 1)

dc$fit_vals2 <- fitted(fit2)
dc$fit_vals3 <- fitted(fit3)
dc$fit_vals5 <- fitted(fit5)

xqplot(dc)

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {
         
         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$fit_vals2[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {
         
         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$fit_vals3[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {

         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$fit_vals5[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

#DIAGNOSTICS FOR G-MATRIX
#-------------------------------------------------------------------------------

summary(fit2)
summary(fit3)
summary(fit5)
#G-matrix: correlation close to -1, high stddev for nWBV
#NEEDS TO BE FIGURED OUT
#try: centering

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + dvar(nWBV, Subject.ID) | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(fit5, test)
dc$test <- fitted(test)
xqplot(dc)

#model is worse but, corr goes down, stddev goes up. try dropping nWBV?

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(fit5, test)
dc$test <- fitted(test)
xqplot(dc)

#low p-value - maybe try re-introducing age and then center?

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + Age | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(fit5, test)

#corr is close to 0, AIC goes up

dc$test <- fitted(test)
xqplot(dc)

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + dvar(Age, Subject.ID) | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(fit5, test)

#corr goes up close to 1, AIC goes up


dc$test <- fitted(test)
xqplot(dc)

#Model becomes worse, correlation becomes 1 instead of -1 as it was with
#nWBV. Maybe include both Age anD nWBV in random effects?

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ nWBV + Age | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(test, fit5)

#correlation for nWBV goes to -1, correlation for age goes to 0? 
#stddevs all go to 0

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ dvar(nWBV, Subject.ID) + dvar(Age, Subject.ID) | Subject.ID,
            correlation = corAR1(form = ~ 1 | Subject.ID),
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(fit5, test)

#low p-value, fit5 still fits better. correlations are more reasonable but stddev
#for nWBV shoots up

#try removing correlation from fit5?

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + dvar(nWBV, Subject.ID) | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

anova(test, fit5)

#fit5 is still preferred...

#try removing correlations from centered model?

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ dvar(nWBV, Subject.ID) + dvar(Age, Subject.ID) | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)
anova(fit5, test)

#fit5 is still preferred, but Age SES interaction is insignificant. What if we
#drop it?

test <- lme(MMSE ~ nWBV + Age + SES + nWBV * SES, 
            data = dc, 
            random = ~ dvar(nWBV, Subject.ID) + dvar(Age, Subject.ID) | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
fit5_ml <- update(fit5, method = "ML")
test_ml <- update(test, method = "ML")
anova(fit5_ml, test_ml)

#so we still prefer fit5. What if we drop nWBV*SES?

test <- lme(MMSE ~ nWBV + Age + SES, 
            data = dc, 
            random = ~ dvar(nWBV, Subject.ID) + dvar(Age, Subject.ID) | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#ML for anova
test_ml <- update(test, method = "ML")
anova(fit5_ml, test_ml)

#fit5 is still preferred...

#DIAGNOSTICS WITH RESIDUALS: PART 1
#-------------------------------------------------------------------------------

hist(resid(fit5), main = "Histogram of Residuals (fit5)")
qqnorm(resid(fit5)); qqline(resid(fit5), col = "red")
plot(fit5)


fit5_tmb <- glmmTMB(
  MMSE ~ nWBV + Age + SES + nWBV:SES + Age:SES + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

#No normality in QQ plot, quantile deviations
res <- simulateResiduals(fittedModel = fit5_tmb)
plot(res)

#Square term for Age
fit_age_poly <- glmmTMB(
  MMSE ~ poly(Age, 2) + nWBV + SES + nWBV:SES + poly(Age, 2):SES + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

#Similar issue to fit5
sim_age_poly <- simulateResiduals(fit_age_poly)
plot(sim_age_poly)

dc$nWBV_sq <- dc$nWBV^2
dc$Age_sq <- dc$Age^2

#Square term for nWBV
fit_nwbv_poly_raw <- glmmTMB(
  MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV:SES + nWBV_sq:SES + Age:SES + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

#Same issue with residuals
sim_nwbv_poly <- simulateResiduals(fit_nwbv_poly_raw)
plot(sim_nwbv_poly)

#Drop slope from random effects
fit_simple_re <- glmmTMB(
  MMSE ~ nWBV + Age + SES + nWBV:SES + Age:SES + (1 | Subject.ID),
  data = dc,
  REML = TRUE
)

#Deviations get worse
sim_simple <- simulateResiduals(fit_simple_re)
plot(sim_simple)

#Fit5 better AIC
AIC(fit5_tmb, fit_simple_re)

#Squared nWBV better AIC
AIC(fit_age_poly, fit_nwbv_poly_raw)

#Squared nWBV better AIC
AIC(fit_nwbv_poly_raw, fit5_tmb)

#Squared Age better AIC
AIC(fit_age_poly, fit5_tmb)

#Squared age and squared nWBV
fit_both <- glmmTMB(
  MMSE ~ nWBV + nWBV_sq + Age + Age_sq + SES + nWBV:SES + nWBV_sq:SES + Age:SES + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

#Residuals get worse including both squared terms
sim_both <- simulateResiduals(fit_both)
plot(sim_both)

#AIC is worse than just nWBV
AIC(fit_both, fit_nwbv_poly_raw)

#Including squared nWBV in lme
test <- lme(MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Note that all terms are still significant in this new model. New model performs
#slightly worse than fit5, but correlation is slightly improved (-0.961)
test_ml <- update(test, method = "ML")
fit5_ml <- update(fit5, method = "ML")
anova(test_ml, fit5_ml)


test <- lme(MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Dropping nWBV from random effects significantly reduces performance of this
#new model
test_ml <- update(test, method = "ML")
anova(fit5_ml, test_ml)

test <- lme(MMSE ~ nWBV + Age_sq + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

#Squared age also sees improvement in correlation and performs slightly worse than
#fit5; not as much improvement as squared nWBV
test_ml <- update(test, method = "ML")
anova(fit5_ml, test_ml)

#We will proceed with the model that includes squared nWBV; call it fit6
fit6 <- lme(MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + nWBV | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(fit6)

#What if we do within subject-centering for nWBV?
fit7 <- lme(MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES + Age * SES, 
            data = dc, 
            random = ~ 1 + dvar(nWBV, Subject.ID) | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(fit7)

#Age:SES becomes insignificant. Can we drop it?

test <- lme(MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES, 
            data = dc, 
            random = ~ 1 + dvar(nWBV, Subject.ID) | Subject.ID,
            control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
summary(test)

ml_test <- update(test, method = "ML")
ml_fit7 <- update(fit7, method = "ML")
anova(ml_test, ml_fit7)
#High p-value; but dropping makes other terms insignificant (nWBV:SES, nWBV_sq).
#Let's keep it as is for now.

#DIAGNOSTICS WITH RESIDUALS: PART 2
#-------------------------------------------------------------------------------

#fit5 has best performance followed closely by fit6
ml_fit5 <- update(fit5, method = "ML")
ml_fit6 <- update(fit6, method = "ML")
ml_fit7 <- update(fit7, method = "ML")
anova(ml_fit5, ml_fit6, ml_fit7)

summary(fit5)
summary(fit6)
summary(fit7)

fit5_tmb <- glmmTMB(
  MMSE ~ nWBV + Age + SES + nWBV:SES + Age:SES + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

res5 <- simulateResiduals(fittedModel = fit5_tmb)
plot(res5)

fit6_tmb <- glmmTMB(
  MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES + Age * SES + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

res6 <- simulateResiduals(fittedModel = fit6_tmb)
plot(res6)

fit7_tmb <- glmmTMB(
  MMSE ~ nWBV + nWBV_sq + Age + SES + nWBV * SES + Age * SES + 
  (1 + dvar(nWBV, Subject.ID) | Subject.ID),
  data = dc,
  REML = TRUE
)

res7 <- simulateResiduals(fittedModel = fit7_tmb)
plot(res7)

#fit7 has worse quantile deviations; let's stick with fit5 and fit6.

dc$fit_vals5 <- fitted(fit5)

xyplot(MMSE ~ Age | factor(Subject.ID), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories per Subject",
       panel = function(x, y, subscripts, ...) {
         
         panel.xyplot(x, y, col = "black", pch = 16, ...)
         
         fit_vals <- dc$fit_vals5[subscripts]
         
         ord <- order(x)
         
         panel.lines(x[ord], fit_vals[ord], col = "blue", lwd = 2)
       })

dc$fit_vals6 <- fitted(fit6)

xyplot(MMSE ~ Age | factor(Subject.ID), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories per Subject",
       panel = function(x, y, subscripts, ...) {
         
         panel.xyplot(x, y, col = "black", pch = 16, ...)
         
         fit_vals <- dc$fit_vals6[subscripts]
         
         ord <- order(x)
         
         panel.lines(x[ord], fit_vals[ord], col = "blue", lwd = 2)
       })

#Both plots are fairly similar

#Centering:
dc$nWBV_c <- scale(dc$nWBV, center = TRUE, scale = FALSE)
dc$Age_c <- scale(dc$Age, center = TRUE, scale = FALSE)
dc$SES_c <- scale(dc$SES, center = TRUE, scale = FALSE)

fit5_centered <- lme(
  MMSE ~ nWBV_c + Age_c + SES_c + nWBV_c:SES_c + Age_c:SES_c,
  random = ~ 1 + nWBV_c | Subject.ID,
  correlation = corAR1(form = ~ 1 | Subject.ID),
  data = dc,
  control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
)
summary(fit5_centered)

test_ml <- update(fit5_centered, method ="ML")
fit5_ml <- update(fit5, method = "ML")
anova(test_ml, fit5_ml)

cent_tmb <- glmmTMB(
  MMSE ~ nWBV_c + Age_c + SES_c + nWBV_c:SES_c + Age_c:SES_c + (1 + nWBV_c | Subject.ID),
  data = dc,
  REML = TRUE
)

#Centering for fit5 offers no improvement in residuals
cent_res <- simulateResiduals(fittedModel = cent_tmb)
plot(cent_res)

plot(res5)

#GROUPING BY CDR, VISUALIZATIONS: PART 2
#-------------------------------------------------------------------------------

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {
         
         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$fit_vals5[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

dc_slope <- dc %>%
  select(Subject.ID, Age, fit_vals5, CDR) %>%
  group_by(Subject.ID, CDR) %>%
  arrange(Age, .by_group = TRUE) %>%
  summarise(
    slope = if (n() >= 2) coef(lm(fit_vals5 ~ Age))[2] else NA_real_,
    .groups = "drop"
  )

avg_slopes_by_CDR_5 <- dc_slope %>%
  group_by(CDR) %>%
  summarise(
    avg_slope = mean(slope, na.rm = TRUE),
    n = n()
  )

print(avg_slopes_by_CDR_5)

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {
         
         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$fit_vals6[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

dc_slope <- dc %>%
  select(Subject.ID, Age, fit_vals6, CDR) %>%
  group_by(Subject.ID, CDR) %>%
  arrange(Age, .by_group = TRUE) %>%
  summarise(
    slope = if (n() >= 2) coef(lm(fit_vals6 ~ Age))[2] else NA_real_,
    .groups = "drop"
  )

avg_slopes_by_CDR_6 <- dc_slope %>%
  group_by(CDR) %>%
  summarise(
    avg_slope = mean(slope, na.rm = TRUE),
    n = n()
  )

print(avg_slopes_by_CDR_6)

#conclusion: fit6 is better since fit5 has positive slopes for 0 group

#SPLINES
#-------------------------------------------------------------------------------

dc$ns_nWBV <- ns(dc$nWBV, df = 4)
ns_basis <- ns(dc$nWBV, df = 4)
ns_df <- as.data.frame(ns_basis)
colnames(ns_df) <- paste0("ns_nWBV_", 1:ncol(ns_df))

dc <- bind_cols(dc, ns_df)

fit_spline <- lme(MMSE ~ ns_nWBV_1 + ns_nWBV_2 + ns_nWBV_3 + Age + SES + nWBV * SES + Age * SES,
  random = ~ 1 + nWBV | Subject.ID,
  data = dc,
  method = "REML"
)

# Check correlations between ALL predictors
cor_matrix <- cor(dc[c("nWBV", "Age", "SES")], use = "complete.obs")
print(cor_matrix)

# Look for correlations of 1.0 or -1.0
which(abs(cor_matrix) > 0.99 & cor_matrix != 1, arr.ind = TRUE)

# Check if any variables are identical
identical(dc$nWBV, dc$Age)
identical(dc$nWBV, dc$SES)
identical(dc$Age, dc$SES)

# Check variance - zero variance = constant variable
apply(dc[c("nWBV", "Age", "SES")], 2, var, na.rm = TRUE)

test <- lme(MMSE ~ ns_nWBV_1 + ns_nWBV_3 + Age + SES + nWBV * SES + Age * SES,
                  random = ~ 1 + nWBV | Subject.ID,
                  data = dc,
                  method = "REML"
)

summary(fit_spline)
summary(test)

ml_fit_spline <- update(fit_spline, method = "ML")
ml_test <- update(test, method = "ML")
anova(ml_fit_spline, ml_test)

fit_spline <- lme(MMSE ~ ns_nWBV_1 + ns_nWBV_3 + Age + SES + nWBV * SES + Age * SES,
                  random = ~ 1 + nWBV | Subject.ID,
                  data = dc,
                  method = "REML")

ml1 <- update(fit_spline, method = "ML")
ml2 <- update(fit6, method = "ML")
anova(ml1, ml2)

#low p-value; fit_spline is better

fit_spline_tmb <- glmmTMB(
  MMSE ~ ns_nWBV_1 + ns_nWBV_2 + ns_nWBV_3 + Age + SES + nWBV * SES + Age * SES
  + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

res_spline <- simulateResiduals(fit_spline_tmb)
plot(res_spline)
plot(res6)

#no improvement in residuals

dc$fit_spline <- fitted(fit_spline)

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {
         
         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$fit_spline[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

dc_slope <- dc %>%
  select(Subject.ID, Age, fit_spline, CDR) %>%
  group_by(Subject.ID, CDR) %>%
  arrange(Age, .by_group = TRUE) %>%
  summarise(
    slope = if (n() >= 2) coef(lm(fit_spline ~ Age))[2] else NA_real_,
    .groups = "drop"
  )

avg_slopes_by_CDR <- dc_slope %>%
  group_by(CDR) %>%
  summarise(
    avg_slope = mean(slope, na.rm = TRUE),
    n = n()
  )

print(avg_slopes_by_CDR)

print(avg_slopes_by_CDR_old)

print(avg_slopes_by_CDR_6)

#For fit6: Better prediction for CDR 1, worse prediction for CDR 0, worse
#prediction for CDR 0.5. Could be caused by limitation in data for CDR 1

summary(fit_spline)

test <- lme(
  MMSE ~ ns_nWBV_1 + ns_nWBV_3 + Age + SES + nWBV * SES + Age * SES,
  random = ~ 1 + nWBV | Subject.ID,
  data = dc,
  method = "REML"
)
summary(test)

ml1 <- update(fit_spline, method = "ML")
ml2 <- update(test, method = "ML")
anova(ml2, ml1)
#low p-value: suggests we should drop ns_nWBV_2

test_tmb <- glmmTMB(
  MMSE ~ ns_nWBV_1 + ns_nWBV_3 + Age + SES + nWBV * SES + Age * SES
  + (1 + nWBV | Subject.ID),
  data = dc,
  REML = TRUE
)

#No noteworthy difference in residuals
res_test <- simulateResiduals(test_tmb)
plot(res_test)

summary(test)

#Let's test slopes before replacing with this new model

dc$test <- fitted(test)

xyplot(MMSE ~ Age | factor(CDR), data = dc,
       type = "p",
       xlab = "Age",
       ylab = "MMSE",
       main = "Observed and Fitted MMSE Trajectories Grouped by CDR",
       panel = function(x, y, subscripts, ...) {
         
         current_ids <- dc$Subject.ID[subscripts]
         unique_ids <- unique(current_ids)
         
         for (id in unique_ids) {
           idx <- which(current_ids == id)
           
           ord <- order(x[idx])
           x_ord <- x[idx][ord]
           y_ord <- y[idx][ord]
           fit_ord <- dc$test[subscripts][idx][ord]
           
           panel.xyplot(x_ord, y_ord, col = "black", pch = 16)
           
           panel.lines(x_ord, fit_ord, col = "blue", lwd = 1.5)
         }
       })

dc_slope <- dc %>%
  select(Subject.ID, Age, test, CDR) %>%
  group_by(Subject.ID, CDR) %>%
  arrange(Age, .by_group = TRUE) %>%
  summarise(
    slope = if (n() >= 2) coef(lm(test ~ Age))[2] else NA_real_,
    .groups = "drop"
  )

avg_slopes_by_CDR_t <- dc_slope %>%
  group_by(CDR) %>%
  summarise(
    avg_slope = mean(slope, na.rm = TRUE),
    n = n()
  )

print(avg_slopes_by_CDR_old)
print(avg_slopes_by_CDR)
print(avg_slopes_by_CDR_t)

#Predictions are worse for CDR 0 and CDR 0.5; only very slightly better for CDR 1
#Final models: fit6, fit_spline

r2(fit2)
r2(fit5)
r2(fit6)
r2(fit_spline)

?glmmTMB

#ZERO-INFLATION
#-------------------------------------------------------------------------------

dc_c <- dc
dc_c

dc_c$MMSE <- 30 - dc_c$MMSE
dc_c

dc_c <- dc_c %>%
  mutate(
    nWBV_scaled = as.numeric(scale(nWBV)),
    Age_scaled = as.numeric(scale(Age)),
    SES_scaled = as.numeric(scale(SES))
  )

spline_basis <- ns(dc_c$nWBV_scaled, df = 3)
dc_c$ns_nWBV_1 <- spline_basis[,1]
dc_c$ns_nWBV_2 <- spline_basis[,2] 
dc_c$ns_nWBV_3 <- spline_basis[,3]

zi_spline_tmb <- glmmTMB(
  MMSE ~ ns_nWBV_1 + ns_nWBV_2 + ns_nWBV_3 + Age_scaled + SES_scaled + Age_scaled * SES_scaled +
    (1 | Subject.ID),
  ziformula = ~ Age_scaled,
  family = nbinom2,
  data = dc_c
)

AIC(fit_spline_tmb, zi_spline_tmb)

res_szi <- simulateResiduals(fittedModel = zi_spline_tmb)
plot(res_szi)

summary(zi_spline_tmb)

test <- glmmTMB(
  MMSE ~ ns_nWBV_1 + ns_nWBV_2 + ns_nWBV_3 + Age_scaled + SES_scaled + (1 | Subject.ID),
  ziformula = ~ Age_scaled,
  family = nbinom2,
  data = dc_c
)

AIC(zi_spline_tmb, test)

summary(test)

res_test <- simulateResiduals(fittedModel = test)
plot(res_test)