# Description
#
# When passed a list where the elements of the list are names in the format "First Last" this function
# returns the order the rows should be in if alphabetized  by last name or the list alphabetized by last name

# Arguments
#
# FirstLast -   A list of names where the format is "FirstName LastName"
# order     -   A boolean determining if order of alphabetized list or the whole alphabetized 
#               list is returned. Defaults to order = TRUE which returns order

# Example 
#
# list.of.names[ byLastName(list.of.names) ]    # to sort by 


byLastName <- function(FirstLast,order=TRUE)
{   ls <- NULL
ifelse(test = order,
       ls <- order(
           paste0(
               sub(".* ", "",FirstLast),
               ", ",
               sub(" .*", "",FirstLast)
           )),
       ls <- FirstLast[order(paste0(sub(".* ", "",FirstLast),", ",sub(" .*", "",FirstLast)))]
)
ls
}