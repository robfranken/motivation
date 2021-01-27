################################################

# PhD article on motivations LPTA
# Preparation
# Last edited by RF: 20-10-2020

################################################

# Clear working environment
rm (list = ls( ))

# Set working directory
# ... is Github directory

# Load (public) ABS data
load("data_abs_public_v2.RData")
data_abs <- data_abs_public

################################################

# 1. Data

library(sjlabelled)
library(dplyr)
library(tidyverse)
library(careless)
library(psych)

#unlabel
data_abs_public <- unlabel(data_abs_public, verbose=F)

data_abs_public$id <- 1:nrow(data_abs_public) # add identifier for subsequent LPTA

# subset sms data in each wave
sms_w1 <- data_abs_public %>% select(W1_M1_1, W1_M1_2, W1_M1_3, W1_M1_4, W1_M1_5, W1_M1_6, W1_M1_7, W1_M1_8, W1_M2_1, W1_M2_2, W1_M2_3, W1_M2_4, W1_M2_5, W1_M2_6, W1_M2_7, W1_M2_8, W1_M3_1, W1_M3_2, W1_M3_3, W1_M3_4, W1_M3_5, W1_M3_6, W1_M3_7, W1_M3_8, id)

sms_w2 <- data_abs_public %>% select(W2_M1_1, W2_M1_2, W2_M1_3, W2_M1_4, W2_M1_5, W2_M1_6, W2_M1_7, W2_M1_8, W2_M2_1, W2_M2_2, W2_M2_3, W2_M2_4, W2_M2_5, W2_M2_6, W2_M2_7, W2_M2_8, W2_M3_1, W2_M3_2, W2_M3_3, W2_M3_4, W2_M3_5, W2_M3_6, W2_M3_7, W2_M3_8, id) 

sms_w3 <- data_abs_public %>% select(W3_M1_1, W3_M1_2, W3_M1_3, W3_M1_4, W3_M1_5, W3_M1_6, W3_M1_7, W3_M1_8, W3_M2_1, W3_M2_2, W3_M2_3, W3_M2_4, W3_M2_5, W3_M2_6, W3_M2_7, W3_M2_8, W3_M3_1, W3_M3_2, W3_M3_3, W3_M3_4, W3_M3_5, W3_M3_6, W3_M3_7, W3_M3_8, id)

# make a 'string' variable 
sms_w1 <- sms_w1 %>%
  mutate(string_w1 = longstring(.)) %>%
  mutate(md_w1 = outlier(., plot = FALSE))

sms_w2 <- sms_w2 %>%
  mutate(string_w2 = longstring(.)) %>%
  mutate(md_w2 = outlier(., plot = FALSE)) 

sms_w3 <- sms_w3 %>%
  mutate(string_w3 = longstring(.)) %>%
  mutate(md_w3 = outlier(., plot = FALSE)) 

# cap string responding and use MD
cutoff_w1 <- (qchisq(p = 1 - .001, df = (ncol(sms_w1) - 1)))
sms_w1 <- sms_w1 %>%
  filter(string_w1 <= 10,
         md_w1 < cutoff_w1) %>%
  select(-string_w1, -md_w1)

cutoff_w2 <- (qchisq(p = 1 - .001, df = (ncol(sms_w2) - 1)))
sms_w2 <- sms_w2 %>%
  filter(string_w2 <= 10,
         md_w2 < cutoff_w2) %>%
  select(-string_w2, -md_w2)

cutoff_w3 <- (qchisq(p = 1 - .001, df = (ncol(sms_w3) - 1)))
sms_w3 <- sms_w3 %>%
  filter(string_w3 <= 10,
         md_w3 < cutoff_w3) %>%
  select(-string_w3, -md_w3)

sms_wide <- merge(sms_w1, sms_w2, by = "id") 
sms_wide <- merge(sms_wide, sms_w3, by = "id")



list(names(sms_wide)[c(2, 26, 50)])

# we are going to make a long file of the SMS data
# first, subset the sms data in wide format
sms_wide <- data_abs_public %>% 
  select(W1_M1_1, W1_M1_2, W1_M1_3, W1_M1_4, W1_M1_5, W1_M1_6, W1_M1_7, W1_M1_8, W1_M2_1, W1_M2_2, W1_M2_3, W1_M2_4, W1_M2_5, W1_M2_6, W1_M2_7, W1_M2_8, W1_M3_1, W1_M3_2, W1_M3_3, W1_M3_4, W1_M3_5, W1_M3_6, W1_M3_7, W1_M3_8,
         W2_M1_1, W2_M1_2, W2_M1_3, W2_M1_4, W2_M1_5, W2_M1_6, W2_M1_7, W2_M1_8, W2_M2_1, W2_M2_2, W2_M2_3, W2_M2_4, W2_M2_5, W2_M2_6, W2_M2_7, W2_M2_8, W2_M3_1, W2_M3_2, W2_M3_3, W2_M3_4, W2_M3_5, W2_M3_6, W2_M3_7, W2_M3_8,
         W3_M1_1, W3_M1_2, W3_M1_3, W3_M1_4, W3_M1_5, W3_M1_6, W3_M1_7, W3_M1_8, W3_M2_1, W3_M2_2, W3_M2_3, W3_M2_4, W3_M2_5, W3_M2_6, W3_M2_7, W3_M2_8, W3_M3_1, W3_M3_2, W3_M3_3, W3_M3_4, W3_M3_5, W3_M3_6, W3_M3_7, W3_M3_8, id)

list(names(sms_wide)[c(1, 25, 49)])

# then transform
library(reshape2)
sms_long <- reshape(sms_wide,
                    direction = "long",
                    varying = c(list(names(sms_wide)[c(1, 25, 49)]),
                                     list(names(sms_wide)[c(2, 26, 50)]),
                                     list(names(sms_wide)[c(3, 27, 51)]),
                                     list(names(sms_wide)[c(4, 28, 52)]),
                                     list(names(sms_wide)[c(5, 29, 53)]),
                                     list(names(sms_wide)[c(6, 30, 54)]),
                                     list(names(sms_wide)[c(7, 31, 55)]),
                                     list(names(sms_wide)[c(8, 32, 56)]),
                                     list(names(sms_wide)[c(9, 33, 57)]),
                                     list(names(sms_wide)[c(10, 34, 58)]),
                                     list(names(sms_wide)[c(11, 35, 59)]),
                                     list(names(sms_wide)[c(12, 36, 60)]),
                                     list(names(sms_wide)[c(13, 37, 61)]),
                                     list(names(sms_wide)[c(14, 38, 62)]),
                                     list(names(sms_wide)[c(15, 39, 63)]),
                                     list(names(sms_wide)[c(16, 40, 64)]),
                                     list(names(sms_wide)[c(17, 41, 65)]),
                                     list(names(sms_wide)[c(18, 42, 66)]),
                                     list(names(sms_wide)[c(19, 43, 67)]),
                                     list(names(sms_wide)[c(20, 44, 68)]),
                                     list(names(sms_wide)[c(21, 45, 69)]),
                                     list(names(sms_wide)[c(22, 46, 70)]),
                                     list(names(sms_wide)[c(23, 47, 71)]),
                                     list(names(sms_wide)[c(24, 48, 72)])),
                    v.names = c("M1_1", "M1_2", "M1_3","M1_4", "M1_5", "M1_6", "M1_7", "M1_8",
                                "M2_1", "M2_2", "M2_3","M2_4", "M2_5", "M2_6", "M2_7", "M2_8",
                                "M3_1", "M3_2", "M3_3","M3_4", "M3_5", "M3_6", "M3_7", "M3_8"),
                    idvar = "id",
                    timevar = "Timepoint",
                    times = 1:3)
# Reorder
sms_long  <- sms_long [(order(sms_long$id)), ]
# fix(sms_long) to check data structure

sms_long_nm <- sms_long %>%
  na.omit()

psych::describe(sms_long_nm)

#######################

# describe


data(iris)
str(iris)
str(sms_long)
sms_long$Timepoint <- as.factor(sms_long$Timepoint)





input <- weighted %>% 
  gather("Variable", "value") %>% 
  group_by(Variable) %>%
  summarise(Mean=mean(value, na.rm=TRUE), 
            SD=sd(value, na.rm=TRUE), 
            min=min(value, na.rm=TRUE), 
            max=max(value, na.rm=TRUE))

knitr::kable(input, digits=2, "html", caption="Descriptives of SMS W1: weighted sum scores") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) 

##########################

# CFA
# 1. Overall model

library(lavaan)

motivation_model <- "
amotivation =~ M1_5 + M2_4 + M3_1 + M3_6
external    =~ M1_4 + M2_3 + M3_3 + M3_8
introjected =~ M1_7 + M2_2 + M2_8 + M3_7
identified  =~ M1_3 + M1_8 + M2_7 + M3_4
integrated  =~ M1_2 + M2_1 + M2_5 + M3_5
intrinsic   =~ M1_1 + M1_6 + M2_6 + M3_2
"

# examine overall fit
overall.fit <- cfa(model = motivation_model, data = sms_long,
           meanstructure = TRUE,  # gives us the means
           std.lv = TRUE) 

summary(overall.fit, standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)
# gives standardized factor loadings, item average (intercepts), error variances, and r squares

# make table of fit indices
table_fit <- matrix(NA, nrow = 10, ncol = 6)


colnames(table_fit) <- c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
print(table_fit)
kable(table_fit)

# create a picture of standardized solution
# quick and dirty..
library(semPlot)
semPaths(overall.fit, whatLabels = "std", layout = "tree")
# triangles are (standardized) intercepts, arrows are variance (loops) and (standardized) factor loadings

# multi group: equivalence testing for longitudinal measurement
library(equaltestMI)

# apparently, only two groups can be compared...
# so I will run the same procedure for each pair (i.e., w1 vs w2, w2 vs w3, w1 vs w3).
# first subset dataframes that only include two groups (i.e. waves), for subsequent equivalence tests
W1_2 <- ifelse(sms_long_nm$Timepoint == 1, 0, ifelse(sms_long_nm$Timepoint == 2, 1, NA))
sms12 <- sms_long_nm[!is.na(W1_2), ]

W2_3 <- ifelse(sms_long_nm$Timepoint == 2, 0, ifelse(sms_long_nm$Timepoint == 3, 1, NA))
sms23 <- sms_long_nm[!is.na(W2_3), ]

W1_3 <- ifelse(sms_long_nm$Timepoint == 1, 0, ifelse(sms_long_nm$Timepoint == 3, 1, NA))
sms13 <- sms_long_nm[!is.na(W1_3), ]

# estimate the MG model
MG.model <- eqMI.main(model = motivation_model, 
                      data = sms12, 
                      group = "Timepoint", #comparing wave 1 and wave 2
                      meanstructure = TRUE, 
                      output = "both", #mean, covariance, both
                      equivalence.test = FALSE, #change this in a moment
                      adjRMSEA = TRUE,
                      projection = TRUE, #projection method used to test equality of latent factor means (even when equality of intercepts does not hold)
                      bootstrap = FALSE)

# Summary:
# Wave 1
summary(MG.model$convention.sem$LavaanOut$fit.configural.g1, #each model saved here
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)
# Wave 2
summary(MG.model$convention.sem$LavaanOut$fit.configural.g2, #each model saved here
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)


# add to the fit table
table_fit[2, ] <- c("W1 Model", round(fitmeasures(MG.model$convention.sem$LavaanOut$fit.configural.g1,
                                                c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit[3, ] <- c("W2 Model", round(fitmeasures(MG.model$convention.sem$LavaanOut$fit.configural.g2,
                                                c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# models do not differ considerably in fit to the data

# Test for configural invariance
# is the 'picture' of the model the same for each?
# we stack groups together
summary(MG.model$convention.sem$LavaanOut$fit.combine.groups, #each model saved here
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)

# the configural model could also have been defined in the cfa function
# cfa(model = motivation_model,
#     data = sms12,
#     meanstructure = TRUE,
#     group = "Timepoint")


table_fit[4, ] <- c("Configural Model", round(fitmeasures(MG.model$convention.sem$LavaanOut$fit.combine.groups,
                                                c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# this will be the model against which we will test the next model; so a sequential testing analysis

# Metric invariance: constrain the factor loadings
summary(MG.model$convention.sem$LavaanOut$fit.metric, #each model saved here
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)
# we added equality constraints on fs: before both groups got to be estimated on their loadings; now, only 1 loading gets estimated for both groups. 
# let's see what happened to the model

table_fit[5, ] <- c("Metric Model", round(fitmeasures(MG.model$convention.sem$LavaanOut$fit.metric,
                                                          c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# is it invariant in comparison to the configural model?
# does is still fit well?
# CFI difference is smaller than .01, thus invariant/equal...
# same holds for RMSEA/SRMR
# metric models holds: loadings are equal (or close enough) for both groups
# t test would not identify significant differences.

# oh and btw: the metric model could also have been estimated with the cfa call:
# cfa(model = motivation_model,
#     data = sms12,
#     meanstructure = TRUE,
#     group = "Timepoint",
#     group.equal = c("loadings"))


# Scalar invariance:
# we got 'pictures', loadings, now test if the intercepts are the same!
summary(MG.model$convention.sem$LavaanOut$fit.scalar, #each model saved here
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)

table_fit[6, ] <- c("Scalar Model", round(fitmeasures(MG.model$convention.sem$LavaanOut$fit.scalar,
                                                      c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# the intercepts are equal on the scale in w1 and w2.
# this could have been done with the cfa function, by adding:
# group.equal = c("loadings", "intercepts"), thus: loading and intercepts together.

# this line can be used to check if your code corresponds to the cfa function.
MG.model$convention.sem$LavaanOut$fit.scalar@call # proves the model as expected.

# Strict (error) variance
# check if the item residuals the same?
# so, loadings, intercepts, residuals: increasingly constrained
# it will inform whether the variance around items is the same across time.
summary(MG.model$convention.sem$LavaanOut$fit.strict.residuals, #each model saved here
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)

table_fit[7, ] <- c("Strict Model", round(fitmeasures(MG.model$convention.sem$LavaanOut$fit.strict.residuals,
                                                      c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# hmmm.. CFI dropped more than 0.01

# we could estimate partial invariance
# to investigate where the problem is and update the model to fix it
# we are going to free the variances of item residuals, one at a time, to figure out where the problem occurs.
# we will use a loop for this


# write out partial codes
partial_syntax <- paste(colnames(sms12)[3:26], #all sms columns
                        "~~", #residuals
                        colnames(sms12)[3:26]) #all columns again

CFI_list <- 1:length(partial_syntax)
names(CFI_list) <- partial_syntax 

for (i in 1:length(partial_syntax)){
  
  temp <- cfa(model = motivation_model,
              data = sms12,
              meanstructure = TRUE,
              group = "Timepoint",
              group.equal = c("loadings", "intercepts", "residuals"),
              group.partial = partial_syntax[i])
  
  CFI_list[i] <- fitmeasures(temp, "cfi")
}

CFI_list

# now figure out which parameters to "free"
options(scipen = 999)
sort(CFI_list - fitmeasures(MG.model$convention.sem$LavaanOut$fit.strict.residuals, "cfi", decreasing = T))

# and free up those parameters
# let's rerun the whole thing
MG.model_2 <- eqMI.main(model = motivation_model, 
                      data = sms12, 
                      group = "Timepoint", #comparing wave 1 and wave 2
                      meanstructure = TRUE,
                      group.partial = c("M1_6~~M1_6"), #free this parameter
                      output = "both", #mean, covariance, both
                      equivalence.test = FALSE, 
                      adjRMSEA = TRUE,
                      projection = TRUE, 
                      bootstrap = FALSE,
                      quiet = TRUE)

# check summary to see if the equality constraint is gone.
summary(MG.model_2$convention.sem$LavaanOut$fit.strict.residuals,
        standardized = TRUE, rsquare = TRUE, fit.measure = TRUE)


table_fit[8, ] <- c("Strict Model M1_6", round(fitmeasures(MG.model_2$convention.sem$LavaanOut$fit.strict.residuals,
                                                      c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# CFI improved, but the difference with the baseline scalar model is still greater than .01;
# thus we still need to free, at least, 1 more, to achieve "partial" invariance.

MG.model_3 <- eqMI.main(model = motivation_model, 
                        data = sms12, 
                        group = "Timepoint", #comparing wave 1 and wave 2
                        meanstructure = TRUE,
                        group.partial = c("M1_6~~M1_6", "M1_7~~M1_7"), #free these parameters
                        output = "both", #mean, covariance, both
                        equivalence.test = FALSE, 
                        adjRMSEA = TRUE,
                        projection = TRUE, 
                        bootstrap = FALSE,
                        quiet = TRUE)

table_fit[9, ] <- c("Strict Model M1_6 + M1_7", round(fitmeasures(MG.model_3$convention.sem$LavaanOut$fit.strict.residuals,
                                                           c("chisq", "df", "cfi", "rmsea", "srmr")),3))
table_fit
# Conclusion: 
# the SMS is mostly invariant between W1 and W2 - the structure,  factor loadings,  intercepts, and most of the error variances are the same in W1 and W2 !

fit <- cfa(model = motivation_model, data = sms_long_nm,
           meanstructure = TRUE,  
           std.lv = FALSE)
loading <- parameterEstimates(fit)$est[1:24]


sms_long_nm$amotivation <- rowMeans(cbind((sms_long_nm$M1_5*loading[1]), (sms_long_nm$M2_4*loading[2]), (sms_long_nm$M3_1*loading[3]), (sms_long_nm$M3_6*loading[4])))

sms_long_nm$external <- rowMeans(cbind((sms_long_nm$M1_4*loading[5]), (sms_long_nm$M2_3*loading[6]), (sms_long_nm$M3_3*loading[7]),   (sms_long_nm$M3_8*loading[8])))

sms_long_nm$introjected <- rowMeans(cbind((sms_long_nm$M1_7*loading[9]), (sms_long_nm$M2_2*loading[10]), (sms_long_nm$M2_8*loading[11]), (sms_long_nm$M3_7*loading[12])))

sms_long_nm$identified <- rowMeans(cbind((sms_long_nm$M1_3*loading[13]), (sms_long_nm$M1_8*loading[14]), (sms_long_nm$M2_7*loading[15]), (sms_long_nm$M3_4*loading[16])))

sms_long_nm$integrated <- rowMeans(cbind((sms_long_nm$M1_2*loading[17]), (sms_long_nm$M2_1*loading[18]), (sms_long_nm$M2_5*loading[19]), (sms_long_nm$M3_5*loading[20])))

sms_long_nm$intrinsic <- rowMeans(cbind((sms_long_nm$M1_1*loading[21]), (sms_long_nm$M1_6*loading[22]), (sms_long_nm$M2_6*loading[23]), (sms_long_nm$M3_2*loading[24])))

library(tidyverse)
library(crosstable)
input <- sms_long_nm %>% #get factor scores and timepoint as grouping variable
  select(27:32, 2)


crosstable(input, by=Timepoint,
           funs=c("Mean" = mean, "Std. dev." = sd, "Min" = min, "Max" = max), funs_arg=list(digits=3)) %>%
  as_flextable(by_header = "Wave")







?as_flextable

generic_labels = list(id = ".id", variable = "variable", value = "value", total =
                        "Total", label = "label", test = "test", effect = "effect"),

sms_w1 <- sms_long_nm[sms_long_nm$Timepoint==1, ] %>%
  select(-Timepoint, -id) %>% # subset W1
  select(25:30) %>% # subset weighted factor scores
  mutate_all(list(scale)) # standardize indicators

library(mclust)
BIC <- mclustBIC(sms_w1) 
plot(BIC)
summary(BIC)
#####################


# unlabel
data_abs_public <- unlabel(data_abs_public, verbose=F)

# subset sms data in each wave
sms_w1 <- data_abs_public %>% select(W1_M1_1, W1_M1_2, W1_M1_3, W1_M1_4, W1_M1_5, W1_M1_6, W1_M1_7, W1_M1_8, W1_M2_1, W1_M2_2, W1_M2_3, W1_M2_4, W1_M2_5, W1_M2_6, W1_M2_7, W1_M2_8, W1_M3_1, W1_M3_2, W1_M3_3, W1_M3_4, W1_M3_5, W1_M3_6, W1_M3_7, W1_M3_8)
sms_w2 <- data_abs_public %>% select(W2_M1_1, W2_M1_2, W2_M1_3, W2_M1_4, W2_M1_5, W2_M1_6, W2_M1_7, W2_M1_8, W2_M2_1, W2_M2_2, W2_M2_3, W2_M2_4, W2_M2_5, W2_M2_6, W2_M2_7, W2_M2_8, W2_M3_1, W2_M3_2, W2_M3_3, W2_M3_4, W2_M3_5, W2_M3_6, W2_M3_7, W2_M3_8) 
sms_w3 <- data_abs_public %>% select(W3_M1_1, W3_M1_2, W3_M1_3, W3_M1_4, W3_M1_5, W3_M1_6, W3_M1_7, W3_M1_8, W3_M2_1, W3_M2_2, W3_M2_3, W3_M2_4, W3_M2_5, W3_M2_6, W3_M2_7, W3_M2_8, W3_M3_1, W3_M3_2, W3_M3_3, W3_M3_4, W3_M3_5, W3_M3_6, W3_M3_7, W3_M3_8)

# The following script can be used to deal with string responding
#{
#  # clean on string responding and use Mahalanobis distance
#  # make a 'string' variable for each wave
#  sms_w1 <- sms_w1 %>%
#    mutate(string_w1 = longstring(.)) %>%
#    mutate(md_w1 = outlier(., plot = FALSE))
#  
#  sms_w2 <- sms_w2 %>%
#    mutate(string_w2 = longstring(.)) %>%
#    mutate(md_w2 = outlier(., plot = FALSE)) 
#  
#  sms_w3 <- sms_w3 %>%
#    mutate(string_w3 = longstring(.)) %>%
#    mutate(md_w3 = outlier(., plot = FALSE)) 
#  
# # cap string responding and use MD
#  cutoff_w1 <- (qchisq(p = 1 - .001, df = ncol(sms_w1)))
#  sms_w1 <- sms_w1 %>%
#    filter(string_w1 <= 10,
#           md_w1 < cutoff_w1) %>%
#    select(-string_w1, -md_w1)
# 
#  cutoff_w2 <- (qchisq(p = 1 - .001, df = ncol(sms_w2)))
#  sms_w2 <- sms_w2 %>%
#   filter(string_w2 <= 10,
#           md_w2 < cutoff_w2) %>%
#    select(-string_w2, -md_w2)
# 
#  cutoff_w3 <- (qchisq(p = 1 - .001, df = ncol(sms_w3)))
#  sms_w3 <- sms_w3 %>%
#    filter(string_w3 <= 10,
#           md_w3 < cutoff_w3) %>%
#    select(-string_w3, -md_w3)
#}

# bind together (hence assuming independent observations)
names(sms_w1) <- names(sms_w2) <- names(sms_w3) <- c("M1_1", "M1_2", "M1_3", "M1_4", "M1_5", "M1_6", "M1_7", "M1_8",
                                                     "M2_1", "M2_2", "M2_3", "M2_4", "M2_5", "M2_6", "M2_7", "M2_8",
                                                     "M3_1", "M3_2", "M3_3", "M3_4", "M3_5", "M3_6", "M3_7", "M3_8")
sms <- rbind(sms_w1, sms_w2, sms_w3)
sms
# Let's take complete cases for now
sms.nm <- sms %>%
  na.omit() # listwise deletion 

nrow(sms.nm)

################################################


# 2. CFA

library(lavaan)

motivation_model <- "
amotivation =~ M1_5 + M2_4 + M3_1 + M3_6
external    =~ M1_4 + M2_3 + M3_3 + M3_8
introjected =~ M1_7 + M2_2 + M2_8 + M3_7
identified  =~ M1_3 + M1_8 + M2_7 + M3_4
integrated  =~ M1_2 + M2_1 + M2_5 + M3_5
intrinsic   =~ M1_1 + M1_6 + M2_6 + M3_2"

# get fit
fit <- cfa(motivation_model, data=sms.nm,
           std.lv=FALSE) # this may be left out as well
print(fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"),
                  output = "text"), add.h0 = TRUE)

# factor scores (loading first variable latent variable fixed to 1)
loading <- parameterEstimates(fit)
loading <- loading$est[1:24]

# weighted sum (factor) scores
amotivation <- rowMeans(cbind((sms.nm$M1_5*loading[1]), (sms.nm$M2_4*loading[2]), (sms.nm$M3_1*loading[3]), (sms.nm$M3_6*loading[4])))
external <- rowMeans(cbind((sms.nm$M1_4*loading[5]), (sms.nm$M2_3*loading[6]), (sms.nm$M3_3*loading[7]),   (sms.nm$M3_8*loading[8])))
introjected <- rowMeans(cbind((sms.nm$M1_7*loading[9]), (sms.nm$M2_2*loading[10]), (sms.nm$M2_8*loading[11]), (sms.nm$M3_7*loading[12])))
identified <- rowMeans(cbind((sms.nm$M1_3*loading[13]), (sms.nm$M1_8*loading[14]), (sms.nm$M2_7*loading[15]), (sms.nm$M3_4*loading[16])))
integrated <- rowMeans(cbind((sms.nm$M1_2*loading[17]), (sms.nm$M2_1*loading[18]), (sms.nm$M2_5*loading[19]), (sms.nm$M3_5*loading[20])))
intrinsic <- rowMeans(cbind((sms.nm$M1_1*loading[21]), (sms.nm$M1_6*loading[22]), (sms.nm$M2_6*loading[23]), (sms.nm$M3_2*loading[24])))

# make df
weighted.nm <- as.data.frame(cbind(amotivation, external, introjected, identified, integrated, intrinsic))

################################################

# 3. Descriptives

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

input <- weighted.nm %>% 
  gather("Variable", "value") %>% 
  group_by(Variable) %>%
  summarise(Mean=mean(value, na.rm=TRUE), 
            SD=sd(value, na.rm=TRUE), 
            min=min(value, na.rm=TRUE), 
            max=max(value, na.rm=TRUE))

knitr::kable(input, digits=2, "html", caption="Descriptives of SMS (aggregated): weighted factor scores") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) 

# pretty high overall!

################################################

install.packages("factoextra")


