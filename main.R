#Main for thesis
setwd("C:/Users/ltswe/Dropbox/France 2019-2020/Thesis")

#call up tidyverse package
library(tidyverse)
library(readxl)
library(stargazer)
library(data.table)
library(plm)
library(Ecdat)
library(mgcv)
library(BBmisc)
library(lmtest)
library(multiwayvcov)
library(pglm)
library(rsq)
library(pwr)
library("xlsx")
library(numbers)
library(openxlsx)


#import and clean data
source("script/import_clean_data.R")

#summary statistics and graphs
source("script/summary")

#regressions
source("script/regressions")

#analyze total number of points
source("script/points")
