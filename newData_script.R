#============================================================

# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle timestamp: 2018-02-01 00:01:42 i386-w64-mingw32 

# Rattle version 5.1.0 user 'admin'

# This log captures Rattle interactions as an R script. 

# For repeatability export this log of all activity to a 
# file using the Export button or the Tools menu. This 
# script can serve as a starting point for developing your 
# own scripts. Exporting to a file called 'model.R' will 
# allow you to type into a new R Console the command 
#"source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access weather dataset and utilities.
library(magrittr) # For the %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2018-02-01 00:02:05 i386-w64-mingw32 

# Load the dataset from file.

fname <- "file:///C:/Users/admin/Documents/Vjera/FAKS/diplomski/3. semestar/USLUGE ZASNOVANE NA LOKACIJI/PROJEKT/code/newData.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2018-02-01 00:02:06 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("speed", "dist", "sqrt", "log", "exp", "pow")

crs$numeric   <- c("speed", "dist", "sqrt", "log", "exp", "pow")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:02:18 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("speed", "sqrt", "log", "exp", "pow")

crs$numeric   <- c("speed", "sqrt", "log", "exp", "pow")

crs$categoric <- NULL

crs$target    <- "dist"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:02:23 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("speed", "sqrt", "log", "exp", "pow")

crs$numeric   <- c("speed", "sqrt", "log", "exp", "pow")

crs$categoric <- NULL

crs$target    <- "dist"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:02:34 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.14 secs

#============================================================
# Rattle timestamp: 2018-02-01 00:03:14 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- "speed"

crs$numeric   <- "speed"

crs$categoric <- NULL

crs$target    <- "dist"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("sqrt", "log", "exp", "pow")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:03:20 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.07 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-02-01 00:04:23 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("speed", "sqrt", "log", "exp", "pow")

crs$numeric   <- c("speed", "sqrt", "log", "exp", "pow")

crs$categoric <- NULL

crs$target    <- "dist"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:04:29 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2018-02-01 00:06:11 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2018-02-01 00:06:16 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2018-02-01 00:06:21 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("speed", "sqrt", "log", "exp", "pow")

crs$numeric   <- c("speed", "sqrt", "log", "exp", "pow")

crs$categoric <- NULL

crs$target    <- "dist"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:06:26 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2018-02-01 00:07:19 i386-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=50 train=35 validate=7 test=8

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.14*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- "speed"

crs$numeric   <- "speed"

crs$categoric <- NULL

crs$target    <- "dist"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("sqrt", "log", "exp", "pow")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-01 00:07:24 i386-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(dist ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs
