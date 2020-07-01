
# Libraries ---------------------------------------------------------------
library(tidyverse)

# basics ------------------------------------------------------------------

input <- c("Kilian Sennrich", "Daniel Sennrich", "Pascal Sennrich", "Kilian", "Daniel", "Pascal")

str_length(input) # get number of characters of each element of the vector (equivalent to nchar())

str_sub(input, 3, -3) #get all characters from the third to the third last character of every element of the vector

str_sub(input, 3, 3) <- "X" #str_sub can also be used to modify character strings: Here an X is written instead of the third letter
print(input)

str_dup(input, 3) #dublicate a characterstring 3 times. Instead of 3 one can also write c(3, 5) then every other string gets dublicated 3 resp. 5 times

str_to_upper(input) # all characters are written in big Letters

str_to_lower(input) # all characters are written in small Letters

str_to_title(input) #the first letter of every word is in big letters

str_sort(input) #sort all elements of the vector by alphabet

str_order(input) #sort all elements of the vector by alphabet, but return a number

# How to handle whitespace ------------------------------------------------

str_pad(input, 10, "both") #adds whitespace to the left or to the right or to both sides of the element. It only works for single elements

str_trunc(input, 10) #all of the elements have the same length or it can be used to decrease the maximum width of a string

input2 <- str_pad(input, 20, "both")
str_trim(input2) #remove all whitespace from the edge of the vector


# Pattern matching  -------------------------------------------------------

input3 <- c("dear",
            "y062 723 63 58", 
            "076-283-92-67",
            "q079.239.28.28"
            )

pattern <- "([0][1-9][1-9])[- .]([1-9][1-9][1-9])[- .]([1-9][1-9])[- .]([1-9][1-9])"

str_detect(input3, pattern) #checks if the elements of input3 match the pattern

str_subset(input3, pattern) #returns all the elements that match the pattern

str_locate(input3, pattern) #locates the start and the end of the item

str_extract(input3, pattern) #extracts exactly the pattern, that was searched

str_match(input3,pattern) #returns the full element but also separate element parts, that are formed with bracets in the pattern

str_split(input3, pattern) #returns all the elements, that are not included in str_extract

str_replace(input3, pattern, replacement = "XX-XX") #replace the given pattern by a new pattern
