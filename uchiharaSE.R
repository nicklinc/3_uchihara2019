setwd("")
library(tidyverse)

rm(list = ls())

df <- read.csv("uchihara_noSE.csv")

# There are five steps to calculating the SE for a correlation coefficient using the sample size, n
# https://www.statskingdom.com/correlation-confidence-interval-calculator.html

# 1. Transform the correlation coefficients to Fisher's z

df$z <- 0.5*log((1+df$r)/(1-df$r))

# 2. Calculate the SD of the transformed correlation

df$sd <- 1/sqrt(df$n-3)

# 3. Calculate the CI for the z statistic

df$z_ll <- df$z-(1.96*df$sd)
df$z_ul <- df$z+(1.96*df$sd)

# 4. Convert back from z to r

df$r_ll <- (exp(2*df$z_ll)-1) / (exp(2*df$z_ll)+1)
df$r_ul <- (exp(2*df$z_ul)-1) / (exp(2*df$z_ul)+1)

# 5. Calculate SE from CIs

df$se <- (df$r_ul-df$r_ll)/3.92

# Create a new spreadsheet that includes the SE

write.csv(df, "uchihara.csv")

