#source("https://bioconductor.org/biocLite.R")
#biocLite("Biostrings")
library(Biostrings)

trinity.fasta  <- readDNAStringSet(filepath = "eh.cd-hit-est.output")
trinity.names  <- sub(pattern = "\\s.*.*",replacement = "", x = names(trinity.fasta))
hit.ids  <- as.character(read.table(file = "xls.split.transcript_id.txt")[,1])
matches  <- trinity.names %in% hit.ids

# check work
table(matches)

# subset fasta
trinotate.hit.fasta  <- trinity.fasta[matches]

# write fasta file
writeXStringSet(x = trinotate.hit.fasta,filepath = "trinotate.hits.fasta",format = "fasta")
