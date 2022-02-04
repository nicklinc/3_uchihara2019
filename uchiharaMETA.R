setwd("")
#install.packages(c("tidyverse", "meta", "metaviz"))
library(tidyverse)
library(meta)
library(metaviz)

rm(list = ls())

df <- read.csv("uchihara.csv")

# It's a good habit to check your data.
# When we look, we can see that two continuous variables have been imported as factors

str(df)

df$basic_vocab <- as.numeric(df$basic_vocab)
df$range <- as.numeric(df$range)

### 'meta' package
# https://search.r-project.org/CRAN/refmans/meta/html/metacor.html

# We need to use metacor() because the effect sizes are correlation coefficients

ma <- metacor(r, # the correlation coefficients
        n, # the sample size for each correlation coefficient
        method.tau = "ML", # estimation method [maximum likelihood]
        sm = "ZCOR", # converts to Fisher's z for calculations
        data = df) # data
summary(ma)


### Plots: metaviz 
# https://rdrr.io/cran/metaviz/f/vignettes/metaviz.Rmd

viz_forest(x = df[, c("r", "se")], 
           study_labels = df[, "author"],
           xlab = "Pearson's r",
           summary_label = "SUMMARY EFFECT",
           method = "ML",
           #annotate_CI = TRUE
           )

viz_funnel(df[, c("r", "se")],
           method = "DL",
           sig_contours = FALSE)

viz_sunset(df[, c("r", "se")],
           method = "DL")

### Moderator Analysis ###

### Age (categorical)
ma_age <- metacor(r, 
              n, 
              subgroup = age, # Meta-analysis with "age" as the moderator
              method.tau = "ML", 
              sm = "ZCOR", 
              data = df) 
summary(ma_age)

# There is an additional level "x", that must have been coded "university" by Uchihara et al.

df$age <- fct_collapse(df$age, 
             University = c("University", "x"))
str(df$age)

ma_age <- metacor(r, 
              n, 
              subgroup = age, 
              method.tau = "ML", 
              sm = "ZCOR", 
              data = df) 
summary(ma_age)


# Vocabulary (continuous)
ma_voc <- metareg(ma, ~ basic_vocab) 
summary(ma_voc)
