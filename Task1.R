if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("Biostrings")
BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))

library(Biostrings)

dna <- DNAString("TCATAATACGTTTTGTATTCGCCAGCGCTTCGGTGT")
dna

rna <- RNAString(dna)
rna

protein <- Biostrings::translate(rna)
protein

p <- Biostrings::translate(dna)
p




##########################33

dna_shifted <- subseq(dna, start = 2)
dna_shifted


rna_shifted <- RNAString(dna_shifted)
rna_shifted

protein_shifted <- Biostrings::translate(rna_shifted)

protein_shifted

# Why mutations are often Deleterious 
# -----------------------------------
# By removing one nucleotide, we changed every codon after that point.
# This is frameshift mutation, and it is extremely disruptive.
# The reading frame shifts.
# All Amino-Acids after the mutation are wrong.
# The protein may lose its function or even become toxic.



# EXAMPLE = 2

library(readr)
library(Biostrings)

SEQ1<-read.fasta(file = "C:/Users/Joial Danyal/Downloads/sequence.fasta")
dir()

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
