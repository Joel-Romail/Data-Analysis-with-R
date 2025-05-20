if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("Biostrings")
BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))



library(readr)
library(Biostrings)

SEQ1<-read.fasta(file = "C:/Users/Joial Danyal/Downloads/sequence.fasta")
dna_seq <- SEQ1[[1]]
dna_seq
seq_length <- length(dna_seq)
seq_length

# Counting Letters 

nucleotides <- alphabetFrequency(dna, baseOnly = TRUE)
a_count <- nucleotides[["A"]]
c_count <- nucleotides[["C"]]
g_count <- nucleotides[["G"]]
t_count <- nucleotides[["T"]]
gc_freq <- (g_count + c_count) / seq_length * 100

cat("Length:", seq_length, "\n",
    "A:", a_count, "C:", c_count, "G:", g_count, "T:", t_count, "\n",
    "GC%:", gc_freq)




# Fetch the sequence
accession <- "NC_001477"
url <- paste0(
  "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=",
  accession, "&rettype=fasta&retmode=text"
)
dna_seq <- readDNAStringSet(url)
# Save and reload the sequence
writeXStringSet(dna_seq, "sequence.fasta")
dna <- readDNAStringSet("sequence.fasta")[[1]]
dna
# Calculate statistics
seq_length <- length(dna)
seq_length
# Counting the A, C, G, T, 
nuc_counts <- alphabetFrequency(dna, baseOnly = TRUE) 
a <- nuc_counts[["A"]]
c <- nuc_counts[["C"]]
g <- nuc_counts[["G"]]
t <- nuc_counts[["T"]]

# GC Frequency 
gc_percent <- (g + c) / seq_length * 100

# Printing Results in one Frame: 
cat("Sequence Length:", seq_length, "\n",
    "A:", a, "C:", c, "G:", g, "T:", t, "\n",
    "GC%:",gc_percent, "%")

