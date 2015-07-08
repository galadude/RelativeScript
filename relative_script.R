# Michael Glen Galanakis, 2015
# mgalanakis@sund.ku.dk
# 
# Progeny relatives scripts version 1.0.0
# 
# This script provides the function "relative" which allows the user to 
# indicate wether an individual has a n-degree relative with a curtain condition.
# The script requires the package kinship2. 


library(kinship2)
library(plyr)

source("RelativeScript/fix.R")



# The function ped_relatives takes two input parameters:
# x, a data set where all rows belong to the same pedigree;
# and n, an integer deciding the degree of relation.
#
# In should be 1 for first degree relatives, 2 for second degree
# relatives, etc. We assume that x has a columns: 'temp', which shows
# whether an individual is 'positive'; and 'id', which references
# the row number in the original data frame.
#
# ped_relatives returns a data frame with two columns:
# 'id', the row number in the original data set; and 'out', a boolean
# indicating if a row is a n-degree relative to a positive row.

ped_relatives <- function(x, n) {
  if (! "temp" %in% names(x)) stop("needs temp column")
  if (! "id" %in% names(x)) stop("needs id column")
  # This will end up being the out column of the output
  indicator_column <- rep(F,nrow(x))
  
  phantom <- pedLevel(x, MakeParent) # adds missing parents
  b <- with(phantom, kinship(UPN, Father.ID, Mother.ID, Gender))
  
  positive <- phantom[phantom$temp == "TRUE", ] # this breaks my heart
  
  for (upn in positive$UPN) {
    relatives <- b[as.character(upn), ]
    # UPN of n-degree relatives
    n.degree.relatives <- names(relatives[relatives == 1/n * 1/4])
    indicator_column[x$UPN %in% n.degree.relatives] <- T
  }
  return(data.frame(id=x$id,out=indicator_column))
}

# The function relatives takes the parameters: x, a data set with columns
# in the style of Progeny output; c, a vector which shows which rows are
# positive; and n, the degree of relation.
#
# The function returns a boolean vector, which shows if a row is
# a n-degree relative to an affected row.

relatives <- function(x, k, n) {
  if (!identical(names(x)[1:6], c("Global.ID", "Gender", "UPN", "Mother.ID",
                         "Father.ID", "Pedigree.name"))) stop("column names!")
      
  # This function returns the 'phantom parent' which is used
  # to replace missing parents. It takes the input parameters:
  # UPN, the id; sex, whether a individual is male or female; and
  # ped, the pedigree the parent belongs to.
  MakeParent <- function (UPN, sex, ped) {
    nas <- rep(NA, dim(x)[2])
    return (c('phantom1', sex, UPN, 0, 0, ped, nas))
  }
  
  # exceptions
  if (length(k) != nrow(x)) stop("k and data must have same size.")
  if (typeof(k) != "logical") stop("k must be of type logical.")
  if ("temp" %in% names(x)) stop("The column name \"temp\" is reserved,
                                 don't use it.")
  if ("id" %in% names(x)) stop("The column name \"id\" is reserved,
                               don't use it.")
  
  rownames(x) <- NULL
  # now we calculate
  x$id <- rownames(x)
  x$temp <- k
  
  r <- ddply(x, "Pedigree.name", function(y) {
    ped_relatives(y, n)
  })
  
  r[order(as.numeric(as.character(r$id))),]$out
  # ensures rows are in the same order
}
