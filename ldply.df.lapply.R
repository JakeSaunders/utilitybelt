library(plyr)
test <- ldply (
    lbls,
    function(x) { chemosen[chemosen$pID == x, ]}
)
