# requires bioconductor package Biostrings
#source("https://bioconductor.org/biocLite.R")
#biocLite("Biostrings")
library(Biostrings)

# read fasta file into R
trinity.fasta  <- readDNAStringSet(filepath = "eh.cd-hit-est.output")
# exact fasta headers and simplify so they will match
trinity.names  <- sub(pattern = "\\s.*.*",replacement = "", x = names(trinity.fasta))
# read in list of trinity ids that are subset retained in new fasta
hit.ids  <- as.character(read.table(file = "xls.split.transcript_id.txt")[,1])
# make boolean that indicates which fasta entries to keep 
matches  <- trinity.names %in% hit.ids

# check work
table(matches)

# subset fasta
trinotate.hit.fasta  <- trinity.fasta[matches]

# write fasta file
writeXStringSet(x = trinotate.hit.fasta,filepath = "trinotate.hits.fasta",format = "fasta")
