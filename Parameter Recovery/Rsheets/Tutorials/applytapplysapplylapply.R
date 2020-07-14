# Tutorial: https://www.guru99.com/r-apply-sapply-tapply.html

# Libraries ---------------------------------------------------------------

library(data.table)
library(tidyverse)

# apply() -----------------------------------------------------------------

#apply() takes Data frame, array or matrix as an input and gives output in vector, list or array.

mat1 <- matrix(c(3,2,4,2,56,7,4,3,2,23,1,1),
               nrow = 4,
               ncol = 3, 
               byrow = TRUE)

#calculate the median of the rows (since Margin = 1)
apply(mat1, 1, median)

# apply(X, MARGIN, FUN)
# Here:
#   -x: an array or matrix
# -MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
#   -MARGIN=1`: the manipulation is performed on rows
# -MARGIN=2`: the manipulation is performed on columns
# -MARGIN=c(1,2)` the manipulation is performed on rows and columns
# -FUN: tells which function to apply. Built functions like mean, median, sum, min, max and even user-defined functions can be applied


# lapply() ----------------------------------------------------------------

# lapply() takes list, vector or data frame as input and gives output in list. It does not take a matrix.

lis1 <- list(c(2,4,23,21,4,5,3,2,4,5),
             c(3,5,3,4,5,35,3,2,4,53))

#calculate mean of the two vector in the list. Output stored in list
output <- lapply(lis1, mean)

# save output (class = list) to vector
unlist(output)

# lapply(X, FUN)
# Arguments:
#   -X: A vector or an object
# -FUN: Function applied to each element of x


# sapply() ----------------------------------------------------------------

# function takes list, vector or data frame as input and gives output in vector or matrix

#instead of unlisting lapply(), we can use sapply() that does the same but returns vector
sapply(lis1, mean)

# sapply(X, FUN)
# Arguments:
#   -X: A vector or an object
# -FUN: Function applied to each element of x

# Functions to pass into lapply and sapply can also be written by hand:
cusfun <- function(x){
  x + 4
}

output.cusfun <- lapply(lis1, cusfun)

# Every object of row 2 includes the value of row 1 + 4 (as defined in our function)
rbind(unlist(lis1), unlist(output.cusfun))

#using the function directly in sapply()
output.funinfun <- sapply(lis1, function(lis1) lis1 * 2)

cbind(unlist(lis1[1]), output.funinfun, unlist(lis1[2]))

# tapply() ----------------------------------------------------------------

#tapply is used to summarise
dat1 <- data.frame( Var1 = c(3,5,23,5,3,5,3,5,32,5,32,5,3,5,5,3),
                    Var2 = c(1,1,1,1,1,3,2,2,2,2,2,3,3,3,3,3))

tapply(X = dat1$Var1, INDEX = dat1$Var2, FUN = mean)
#Take X and caluclate FUN by INDEX

# tapply(X, INDEX, FUN = NULL)
# Arguments:
#   -X: An object, usually a vector
# -INDEX: A list containing factor
# -FUN: Function applied to each element of x
