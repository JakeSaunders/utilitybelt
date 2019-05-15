library(plyr)
test <- ldply (
    lbls,
    function(x) { chemosen[chemosen$pID == x, ]}
)

#https://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe 
library(data.table)
rbindlist(mylist, fill=TRUE) 
