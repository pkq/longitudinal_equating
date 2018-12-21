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

options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))
source(paste(f_dir, "linear_equate_x_to_y1.R", sep = "/"))
source(paste(f_dir, "linear_equate_x_to_y2.R", sep = "/"))
source(paste(f_dir, "mv_linear_equate_x_to_y.R", sep = "/"))
source(paste(f_dir, "EM_y.R", sep = "/"))


knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)

rmarkdown::render(input = paste(r_dir, 'orf-explore.Rmd', sep = "/"))


