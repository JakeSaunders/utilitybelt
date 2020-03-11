trinotateNames <- function(df){
    colnames(df) <- c("gene_id", "transcript_id", "sprot_Top_BLASTX_hit", "RNAMMER",  
                      "prot_id", "prot_coords", "sprot_Top_BLASTP_hit", "Pfam", "SignalP", 
                      "TmHMM", "eggnog", "Kegg", "gene_ontology_blast", "gene_ontology_pfam", 
                      "transcript", "peptide")
    df
}