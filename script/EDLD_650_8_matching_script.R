######################################################
## Script Name: EDLD_650_8_matching_script.R
## Project Title: EDLD 650 Winter 2022
## Author: David Liebowitz
## Created: 2/18/22
## Last update: 2/18/222
## Purpose: This script imports the Catholic school data and 
## Inputs: ch12_catholic.dta 
######################################################

# Define graphing colors
red_pink <- "#e64173"
turquoise <- "#20B2AA"
orange <- "#FFA500"
red <- "#fb6107"
blue <- "#3b3b9a"
green <- "#8bb174"
grey_light <- "grey70"
grey_mid <- "grey50"
grey_dark <- "grey20"
purple <- "#6A5ACD"
slate <- "#314f4f"

library(pacman)

# These are the packages you will need for the analyses 

p_load(here, tidyverse, DT, ggplot2, xaringan, knitr, kableExtra, 
       modelsummary, stargazer, xaringanthemer, gganimate, ggthemes, fixest, haven, arsenal, MatchIt, gtools)

# You will want to have created a folder for the course and an R project within that folder
# Then, create a folder within your course folder entitled "code"
# Finally, save this R script within the code folder

# This command tells R where your script is and allows you to point it to other folders within the directory
i_am("code/EDLD_650_8_matching_script.R")

# Read data in
catholic <- read_dta(here("./data/ch12_catholic.dta"))
DT::datatable(catholic[c(1,17,51,143,1101),c(1, 3, 6, 8, 17)], fillContainer = FALSE, options = 
                list(pageLength = 5))

# Construct a descriptive table of outcomes across groups
catholic %>% group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(math12), SD_math = sd(math12))

#### Plot mean differences

# First, summarize values
raw_diff <- catholic %>% group_by(catholic) %>% summarise(n_students = n(), mean_math=mean(math12), se_math = sd(math12)/sqrt(n_students))

## Then plot
ggplot(raw_diff, aes(x=as.factor(catholic), y=mean_math, ymin=mean_math-se_math, ymax=mean_math+se_math)) + geom_col(fill=red_pink, alpha=0.4) + 
  geom_linerange() + theme_pander(base_size = 16) +
  xlab("Catholic HS attendance") + scale_y_continuous("Grade 12 math score")

### Fit bivariate regression
ols1 <- lm(math12 ~ catholic, data=catholic)
summary(ols1)

## Table document differences across Catholic school attendees and non
table <- tableby(catholic ~ faminc8 + math8 + white + female, numeric.stats=
                   c("meansd"), cat.stats=c("N", "countpct"), digits=2, data=catholic)
mylabels <- list(faminc8 = "Family income level in 8th grade", 
                 math8 = "8th grade math score")
summary(table, labelTranslations = mylabels)

##########################################################
##
## Propensity-Score Matching (PSM)
##
############################################################

###############################
##        Phase I.            #
###############################

# 1. Fit logistic selection model estimating probability of attendance, given covariates

pscores <- feglm(catholic ~ inc8 + math8 + mathfam, family=c("logit"), data=catholic)
summary(pscores)

# 2. Estimate fitted probability of selection into treatment for each individual
pscore_df <- data.frame(p_score = predict(pscores, type = "response"),
                        catholic = catholic$catholic)
head(pscore_df)

# 3. Examine common support

ggplot(pscore_df, aes(p_score,fill = as.factor(catholic), color = as.factor(catholic))) + 
  geom_density(alpha=0.1) + 
  theme_pander(base_size = 12) + 
  xlab("Probability of Catholic HS attendance")

###############################
##        Phase II.            #
###############################

# 1. Generate matches

matched <- matchit(catholic ~ math8 + inc8, method="nearest", replace=T, 
                   discard="both", data=catholic)

# Create dataframe from matched data
df_match <- match.data(matched)

# Dimensions of dataframe
dim(df_match)

## 2. Examine common support (post-match)

ggplot(df_match, aes(distance,fill = as.factor(catholic), color = as.factor(catholic))) + 
        geom_density(alpha=0.1) + 
        theme_pander(base_size = 16) + 
        xlab("Probability of Catholic HS attendance")

## 3. Examine quality of matches, descriptively
summary(matched)

# Note: Could get even closer with fuller model:
matched2 <- matchit(catholic ~ math8 + inc8 + inc8sq + mathfam, 
                    method="nearest", replace=T, 
                    discard="both", data=catholic)

df_match2 <- match.data(matched2)

ggplot(df_match2, aes(distance, fill = as.factor(catholic), color = as.factor(catholic))) + 
                  geom_density(alpha=0.1) + 
                  theme_pander(base_size = 12) + 
                  xlab("Probability of Catholic HS attendance")

## 4. Once happy with matches, estimate treatment effect in matched sample

psmatch2 <- lm(math12 ~ catholic + math8 + inc8 + inc8sq + mathfam, data=df_match, weights=weights)
#Notice how we have matched on just math8 and inc8 but are now adjusting for more
#in our estimation. This is fine!
summary(psmatch2)

# Estimate confidence intervals
confint(psmatch2)

# Can estimate this without additional controls or with full model match
psmatch1 <- lm(math12 ~ catholic + math8 + inc8, data=df_match, weights=weights)
psmatch3 <- lm(math12 ~ catholic + math8 + inc8 + inc8sq + mathfam, data=df_match2, weights = weights)



##########################################################
##
## Coarsened Exact Matching (CEM)
##
############################################################

####################
##    Phase I     ##
####################

#### Step 1. Define coarsened bins of covariates within which to match

# Create coarsened family income variable
catholic <- mutate(catholic, coarse_inc=ifelse(faminc8 < 5, 1, faminc8))
  catholic$coarse_inc <- as.ordered(catholic$coarse_inc)
  levels(catholic$coarse_inc)

# Same for 8th grade math score
summary(catholic$math8)
mathcuts <- c(43.45, 51.49, 58.55)

#### Step 2. Define matching bins (could do this by hand)
cem <- matchit(catholic ~ coarse_inc + math8, 
               cutpoints=list(math8=mathcuts), method="cem", data = catholic)
df_cem <- match.data(cem)

# Have we dropped any? No
table(df_cem$catholic)

#### Step 3. Examine common support

# Look at distribution across sub-groups as a proportion
df_cem1 <- df_cem %>% group_by(catholic, subclass) %>% 
  summarise(count= n())  
df_cem1 <- df_cem1 %>%  mutate(attend = count / sum(count))

# Plot common support
ggplot(df_cem1, aes(subclass, attend, color=as.factor(catholic))) + 
          geom_col(alpha=0.1, position = position_dodge()) + 
          theme_pander(base_size = 10) + xlab("Subclassification groups by income and 8th grade score")

# Examine quality of matches for each variable grouping
summary(cem)

### Could have different quantiles

# For example, quintiles
math8_quints <- quantcut(catholic$math8, 5)
table(math8_quints)

# You might also have a substantive reason for the cuts:
mathcuts2 <- c(40, 45, 50, 55, 60, 65, 70)


# Let's use the substantive match
cem2 <- matchit(catholic ~ coarse_inc + math8, 
                cutpoints=list(math8=mathcuts2),
                method="cem", 
                data=catholic)

df_cem2 <- match.data(cem2)
summary(cem2)

# Plot common support
df_cem3 <- df_cem2 %>% group_by(catholic, subclass) %>% 
              summarise(count= n())  
df_cem3 <- df_cem3 %>%  mutate(attend = count / sum(count))

# Examine quality of matches for each variable grouping
ggplot(df_cem3, aes(subclass, attend, color = as.factor(catholic))) + 
          geom_col(alpha=0.1, position = position_dodge()) + 
          theme_pander(base_size = 10) + 
          xlab("Subclassification groups by income and 8th grade score")

# Estimate
att1 <- lm(math12 ~ catholic + coarse_inc + math8, data=df_cem, weights = weights)
att2 <- lm(math12 ~ catholic + coarse_inc + math8, data=df_cem2, weights = weights)

summary(att1)
summary(att2)

stargazer(ols1, psmatch1, psmatch2, psmatch3, att1, att2, 
          type='html', omit.stat = c("ser", "adj.rsq", "f"), 
          single.row=T, dep.var.caption="", 
          dep.var.labels.include=F, omit=c("Constant", "coarse_inc", "math8", "inc8", "inc8sq", "mathfam"), 
          covariate.labels=c("Attend catholic school"), notes.append=F,
          star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", 
          notes=c("*p<0.05; **p<0.01; ***p<0.001. Models 2-3 and 5-6 match on income and math score. Model 3 adjusts for higher-order terms and interactions post matching; Model 4 includes them in matching algorithm. Model 6 uses narrower bins to match than Model 5. All CEM and PSM estimates are doubly-robust."), 
          model.names=F, column.labels = c("OLS", "PSM", "CEM"), column.separate = c(1, 3, 2))


#########################################################################################################################
#########################################################################################################################

## Extra code to do PS stratification as an additional approach in DARE #4


# Phase 2: PS Stratifying
# Execute stratification routine w/ 4 strata
stratify <- matchit(catholic ~ math8 + inc8 + mathfam,
                    method="subclass", data=catholic, subclass=4)
df_stratify <- match.data(stratify)

# Check common support ** FOR EACH OF THE STRATA **
ggplot(filter(df_stratify, subclass == "1"), aes(distance, fill = as.factor(catholic), color = as.factor(catholic))) + 
  geom_density(alpha=0.1) + 
  theme_pander(base_size = 12) + 
  xlab("Probability of Catholic HS attendance")

ggplot(filter(df_stratify, subclass == "2"), aes(distance, fill = as.factor(catholic), color = as.factor(catholic))) + 
  geom_density(alpha=0.1) + 
  theme_pander(base_size = 12) + 
  xlab("Probability of Catholic HS attendance")

ggplot(filter(df_stratify, subclass == "3"), aes(distance, fill = as.factor(catholic), color = as.factor(catholic))) + 
  geom_density(alpha=0.1) + 
  theme_pander(base_size = 12) + 
  xlab("Probability of Catholic HS attendance")

ggplot(filter(df_stratify, subclass == "4"), aes(distance, fill = as.factor(catholic), color = as.factor(catholic))) + 
  geom_density(alpha=0.1) + 
  theme_pander(base_size = 12) + 
  xlab("Probability of Catholic HS attendance")

# Phase 2: Assessing balance

summary(stratify)

# Phase 2: Execute stratification routine w 6 strata

stratify2 <- matchit(catholic ~ math8 + inc8 + inc8sq + mathfam,
                     method="subclass", data=catholic, subclass=6)
df_stratify2 <- match.data(stratify2)
summary(stratify2)

# Phase 2: Conduct analysis by stratum
# Can do this using the following routine

# Estimate ATT by stratum (omitting stratum 1)
fit <- lm(math12 ~ subclass + subclass:catholic - 1, data = df_stratify2)

# Estimate marginal effect of Catholic attendance
summary(margins::margins(fit, variables = "catholic", 
                         data = df_stratify[df_stratify$catholic == 1,],
                         vcov = sandwich::vcovHC(fit)))




```
