# Format Trinotate Output for R

formatTrinotate <- function(df, separator = "|\n|", verbose = TRUE){
    ### start function here 
    # rename column headers
    if(verbose == TRUE) {cat("####################################\n")}
    if(verbose == TRUE) {cat("###       Renaming Columns       ###\n")}
    colnames(df) <- c("gene_id", "transcript_id", "BLASTX", "RNAMMER", "prot_id", 
                      "prot_coords", "BLASTP", "Pfam", "SignalP", "TmHMM", "eggnog", 
                      "Kegg", "GO_blast", "GO_pfam", 
                      "transcript_seq", "peptide_seq")

    if(verbose == TRUE) {cat("###   Splitting BLAST columns    ###\n")}
    df <- df %>% 
        # split BLASTX into columns
        separate(col = BLASTX,sep = "`",into = c("BLASTX","BLASTX_hit2")) %>% 
        separate(col = BLASTX,sep = "\\^", into = c("BLASTX.symbol","BLASTX.species",
                                                    "BLASTX.align","BLASTX.percentID","BLASTX.eval","BLASTX.name","BLASTX.taxonomy" )) %>% 
        # split BLASTP into columns
        separate(col = BLASTP,sep = "`",into = c("BLASTP","BLASTP_hit2")) %>% 
        separate(col = BLASTP,sep = "\\^", into = c("BLASTP.symbol","BLASTP.species",
                                                    "BLASTP.align","BLASTP.percentID","BLASTP.eval","BLASTP.name","BLASTP.taxonomy" ))

    if(verbose == TRUE) {cat("###   Formatting BLAST Outputs   ###\n")}
    # Clean up blast outputs
    df$BLASTX.symbol <- sub(pattern = "_.*$",replacement = "",x = df$BLASTX.symbol)
    df$BLASTP.symbol <- sub(pattern = "_.*$",replacement = "",x = df$BLASTP.symbol)
    df$BLASTX.species <- sub(pattern = ".*_",replacement = "",x = df$BLASTX.species)
    df$BLASTP.species <- sub(pattern = ".*_",replacement = "",x = df$BLASTP.species)
    df$BLASTX.percentID <- as.numeric(sub(pattern = "%ID",replacement = "",df$BLASTX.percentID))
    df$BLASTP.percentID<- as.numeric(sub(pattern = "%ID",replacement = "",df$BLASTP.percentID))
    df$BLASTX.eval <- as.numeric(sub(pattern = "E:",replacement = "", df$BLASTX.eval))
    df$BLASTP.eval <- as.numeric(sub(pattern = "E:",replacement = "", df$BLASTP.eval))
    df$BLASTX.name <- sub(pattern = "RecName: Full=", replacement = "", df$BLASTX.name)
    df$BLASTP.name <- sub(pattern = "RecName: Full=", replacement = "", df$BLASTP.name)
    df$BLASTX.name <- sub(pattern = ";", replacement = "", df$BLASTX.name)
    df$BLASTP.name <- sub(pattern = ";", replacement = "", df$BLASTP.name)
    
    if(verbose == TRUE) {cat("###         Adding NA's          ###\n")}
    # change .'s to NA
    df[df == "."] <- NA

    if(verbose == TRUE) {cat("###        Shortening IDs        ###\n")}
    # remove transcript ID from protein ID
    df$prot_id <- mapply(sub, pattern=df$transcript_id, x=df$prot_id, replacement="")
    # remove gene ID from transcript ID
    df$transcript_id <- mapply(sub, pattern=df$gene_id, x=df$transcript_id, replacement="")

    if(verbose == TRUE) {cat("###      Adding Separators       ###\n")}
    # replace entries with custom separator, default "|\n|"
    df$RNAMMER <- gsub(pattern = "`",replacement = separator,df$RNAMMER)
    df$Pfam <- gsub(pattern = "`",replacement = separator,df$Pfam)
    df$eggnog <- gsub(pattern = "`",replacement = separator,df$eggnog)
    df$Kegg <- gsub(pattern = "`",replacement = separator,df$Kegg)
    df$GO_blast <- gsub(pattern = "`",replacement = separator,df$GO_blast)
    df$GO_pfam <- gsub(pattern = "`",replacement = separator,df$GO_pfam)

    if(verbose == TRUE) {cat("###      Reordering Columns      ###\n")}
    df <- df %>% relocate(
        "gene_id", "transcript_id", "prot_id",
        "BLASTX.symbol", "BLASTX.species", "BLASTX.align", "BLASTX.percentID",
        "BLASTX.eval", "BLASTX.name", "BLASTX.taxonomy", "BLASTX_hit2", "RNAMMER",
        "BLASTP.symbol", "BLASTP.species", "BLASTP.align", "BLASTP.percentID",
        "BLASTP.eval", "BLASTP.name", "BLASTP.taxonomy", "BLASTP_hit2", 
        "GO_blast", "Pfam", "GO_pfam", "prot_coords",
        "SignalP", "TmHMM", "eggnog", "Kegg",
        "transcript_seq", "peptide_seq"
    )
    if(verbose == TRUE) {cat("####################################\n")}
    df
} 


trinotateColumnNames <- function(){
    data.frame(
        column.name = c(
            "gene_id","transcript_id","prot_id",
            "BLASTX.symbol","BLASTX.species","BLASTX.align","BLASTX.percentID",
            "BLASTX.eval","BLASTX.name","BLASTX.taxonomy","BLASTX_hit2",
            "RNAMMER",
            "BLASTP.symbol","BLASTP.species","BLASTP.align","BLASTP.percentID",
            "BLASTP.eval","BLASTP.name","BLASTP.taxonomy","BLASTP_hit2",
            "GO_blast","Pfam","GO_pfam","prot_coords","SignalP","TmHMM",
            "eggnog","Kegg","transcript_seq","peptide_seq" 
        ),
        discription =  c(
            "unique trinity gene number",  
            "Concatenate with gene_id for unique trinity transcript designation",  
            "Concatenate with gene_id and transcript_id for unique trinity protein designation" , 
            "gene symbol for top BLASTx top hit",  
            "Uniprot Species Code of top hit, https://www.uniprot.org/docs/speclist",  
            "Alignments between Query (Q) and Hit (H)",  
            "% of characters in identical between the Query and Hit", 
            "E (expected) value. The number of similar hits of similar quality expected by chance", 
            "Official Full Name",  
            "Full taxonomic info",  
            "Second reported BLASTx hit if reported" ,  
            "RNAmmer predicts ribosomal RNA genes; www.cbs.dtu.dk/cgi-bin/nph-runsafe?man=rnammer", 
            "gene symbol for top BLASTp top hit",
            "Uniprot Species Code of hit, https://www.uniprot.org/docs/speclist",   
            "Alignments between Query (Q) and Hit (H)", 
            "% of characters in identical between the Query and Hit", 
            "E (expected) value. The number of similar hits of similar quality expected by chance",  
            "Official Full Name",  "Full taxonomic info", "Second reported BLASTp hit",  
            "Gene Ontology entries of BLAST hit; geneontology.org",  
            "http://hmmer.org/",  
            "Gene Ontology entries of Pfam hit; geneontology.org",  
            "Protein Coordinates", 
            "signalP v4, predicts  the presence and location of signal peptide cleavagesignalp",  
            "Prediction of transmembrane helices;  www.cbs.dtu.dk/services/TMHMM/TMHMM2.0b.guide.php", 
            "Clusters of orthologous groups; eggnog5.embl.de/#/app/home",  
            "KEGG pathways; www.genome.jp/kegg", 
            "Transcript sequence",  
            "Protein sequence"
        )
    )
}  
