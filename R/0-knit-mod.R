library(scales)
library(rmarkdown)
library(kableExtra)
library(matrixcalc)
library(lme4)

proj  <- "longitudinal_equating"
h_dir <- "~/Dropbox/Research"
p_dir <- paste(h_dir, proj, sep = "/")
r_dir <- paste(p_dir, "R", sep = "/")
d_dir <- paste(p_dir, "data", sep = "/")
f_dir <- paste(r_dir, "functions", sep = "/")

source("R/EM_y.R")
source("R/emMeanCov.R")

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)

rmarkdown::render(input = 'R/orf-explore.Rmd')
