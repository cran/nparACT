## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = TRUE, results = 'hide', warning = FALSE, message = FALSE----
## First, we save your current workspace
save.image(file=paste(tempdir(), "currsession.RData", sep = "/"))
## make sure you start with a clean session.
rm(list = ls(all = TRUE))
install.packages("nparACT", repos = "http://cran.us.r-project.org")

## ----echo=TRUE, warning=FALSE, results='asis'---------------------------------
library(nparACT)
data(sleepstudy)

## save current working directory so we can reset this later.
olddir <- getwd()

## create a new directory in the temporary directory (don't worry, it will automatically be deleted  
## when you restart your computer)
newdir <- file.path(tempdir(),"nparACT_exmpl")
dir.create(newdir, showWarnings = FALSE)

## write the sleepstudy file to this new directory
write.table(sleepstudy, file = paste(newdir, "sleepstudy.txt", sep = "/"),
row.names=FALSE, col.names = FALSE, quote = FALSE, sep = ",")

## ----echo=TRUE, warning=FALSE, results='asis', eval = FALSE-------------------
# nparACT::nparACT_base("sleepstudy", SR = 4/60)
# 
# ## We again load the workspace image from before the code above was executed
# save.image(file=paste(tempdir(), "currsession.RData", sep = "/"))
# 
# ## we set the directory back to the one we were using before as we were just working in the
# ## temp directory.
# setwd(olddir)

