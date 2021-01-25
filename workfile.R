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

clus <- weighted.nm %>%
  na.omit() %>% # listwise deletion
  mutate_all(list(scale)) # standardize indicators

################################################


