if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.20")
BiocManager::install("Biostrings")
BiocManager::install("pwalign")
library("Biostrings")
library("pwalign")

sigma = nucleotideSubstitutionMatrix(match=1, mismatch=-1, baseOnly=TRUE)
sigma
s1 <- DNAString("AAAC")
s2 <- DNAString("AGC")
globalAlign1 <- pairwiseAlignment(s1, s2, substitutionMatrix = sigma, 
                                  gapOpening = -1, gapExtension = -1)
globalAlign1

# -------- DNA Global Alignment ----------- #  

s10=  DNAString("ACTTCACCAGCTCCCTGGCGGTAAGTTGATCAAAGGAAACGCAAAGTTTTCAAG")
s20=  DNAString("GTTTCACTACTTCCTTTCGGGTAAGTAAATATATAAATATATAAAAATATAATTTTCATC")
mat <- nucleotideSubstitutionMatrix(match = 1, mismatch = -1, baseOnly = TRUE)
globalAlign <- pairwiseAlignment(s10, s20, substitutionMatrix = sigma, 
                                 gapOpening = -1, gapExtension = -1)
globalAlign

# -------- Protein Global Alignment ----------- #  

s3 = AAString("MNALQM")
s4 = AAString("NALMSQA")
# mat <- nucleotideSubstitutionMatrix(match = 1, mismatch = -1, baseOnly = TRUE)
globalAlignment <- pairwiseAlignment(s3, s4, substitutionMatrix = "BLOSUM62", 
                                 gapOpening = -6, gapExtension = -2)
globalAlignment

# ------------- TASK 2 -------------- # 

library(Biostrings)

s11 <- DNAString("ATCGTCATTGCC")
s22 <- DNAString("ATTGCCATCGTC")


sub_matrix <- nucleotideSubstitutionMatrix(match = 1, mismatch = -1, baseOnly = TRUE)

# Global alignment (Needleman-Wunsch) with linear gap penalty (-2)
alignment <- pairwiseAlignment(s11, s22,
                               substitutionMatrix = sub_matrix,
                               gapOpening = -2,
                               gapExtension = -2)
alignment # Alignmnet 
score(alignment) # Alignment Score for these Strings 



# ------------- TASK 3 -------------- # 


library(Biostrings)

seq1 <- AAString("FYGNYK")
seq2 <- AAString("DGSFNW")

# BLOSUM62 substitution matrix
data(BLOSUM62)

# Global alignment (Needleman-Wunsch) with linear gap penalty
alignment <- pairwiseAlignment(seq1, seq2,
                               substitutionMatrix = BLOSUM62,
                               gapOpening = -10,
                               gapExtension = -10)


alignment # Alignment 
score(alignment) # Alignment Score of these Strings 
 


