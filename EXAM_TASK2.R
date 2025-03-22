head(question_data_1_)
head(Hotel)


# Rename columns for better handling
colnames(df) <- c("answer", "Hotel 1", "Hotel 2", "Hotel 3")
                  
# Create the contingency table for "answer" and "hotel"
table_1 <- table(Hotel$answer, Hotel$hotel)
print("Contingency Table:")
print(table_1)

# Calculate row-wise proportions
prob_table1 <- prop.table(table_1, margin = 1)
print("Row-wise Proportions:")
print(prob_table1)

# Calculate overall proportions
overall_proportions <- prop.table(table_1)
print("Overall Proportions:")
print(overall_proportions)

# Bar plot of counts
barplot(table_1,
        main = "Hotel Choice by Answer", xlab = "Hotel", ylab = "Number of Responses",
        col = c("darkblue", "red"),
        legend = rownames(table_1)
)

# Bar plot with "beside = TRUE"
barplot(table_1,
        main = "Hotel Choice by Answer", xlab = "Hotel", ylab = "Number of Responses",
        col = c("darkblue", "red"),
        legend = rownames(table_1), beside = TRUE
)











table_1<-table(question_data_1_$sex, question_data_1_$salary)
table_1

prob_table1<-prop.table(table_1)
prob_table1

counts <- table(question_data_1_$sex, question_data_1_$salary)
barplot(counts,
        main="Salary by sex", xlab="Salary",ylab="Number",
        col = c("darkblue", "red"),
        legend = rownames(counts)
        )

barplot(counts,
        main = "Salary by Sex", xlab = "Salary EUR", ylab = "Number",
        col = c("darkblue", "red"),
        legend = rownames(counts), beside = TRUE
        )

