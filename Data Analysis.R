# KANG JIA QIAN, TP073724
# MICHELLE WONG WEI HUI, TP077765
# MUHAMMAD IBRAHIM MIRZA, TP071659 
# NANG MYA HMUE KHIN, TP076459 

# ------------------------ Data Import ------------------------
# Install required packages
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the dataset
Credit_Risk_Data <- read.csv("D:\\YEAR 2\\SEM 1\\PDFA\\Assignment\\credit_risk_classification.csv", 
                             header = TRUE, sep = ",")

# View the dataset
print(head(Credit_Risk_Data))


# ------------------- Cleaning / Pre-processing -------------------
# Data Inspection
cat("\n--- Data Inspection ---\n")
DataType <- str(Credit_Risk_Data)
print(DataType)

# Check data types
cat("\nColumn Data Types:\n")
print(sapply(Credit_Risk_Data, class))

# Check for missing values
cat("\nMissing Values Check:\n")
print(colSums(is.na(Credit_Risk_Data)))

# Check for duplicates
cat("\nDuplicate Check:\n")
duplicates <- sum(duplicated(Credit_Risk_Data))
cat("Number of duplicate entries:", duplicates, "\n")
if (duplicates > 0) {
  Credit_Risk_Data <- Credit_Risk_Data[!duplicated(Credit_Risk_Data), ]
  cat("Duplicates removed. Dataset now contains:", nrow(Credit_Risk_Data), "rows.\n")
}


# ------------------- Data Validation -------------------
# Validate numerical ranges
cat("\n--- Validating Numerical Ranges ---\n")

# Validate 'age'
if (any(Credit_Risk_Data$age < 18 | Credit_Risk_Data$age > 100)) {
  cat("Out-of-range values detected in 'age'. Filtering...\n")
  Credit_Risk_Data <- Credit_Risk_Data[Credit_Risk_Data$age >= 18 & Credit_Risk_Data$age <= 100, ]
}
cat("Age validation completed.\n")

# Validate 'credit_amount'
summary(Credit_Risk_Data$credit_amount)
# Handle negative or outlier values (if needed)
if (any(Credit_Risk_Data$credit_amount < 0)) {
  cat("Negative credit amounts detected. Filtering...\n")
  Credit_Risk_Data <- Credit_Risk_Data[Credit_Risk_Data$credit_amount >= 0, ]
}

# Validate categorical consistency
cat("\n--- Validating Categorical Data ---\n")
cat("Unique values in 'housing':\n")
print(unique(Credit_Risk_Data$housing))
# Add a check for valid values
valid_housing <- c("own", "rent", "for free")
Credit_Risk_Data <- Credit_Risk_Data[Credit_Risk_Data$housing %in% valid_housing, ]
cat("Housing validation completed.\n")

# Summary of the updated dataset
cat("\n--- Dataset Summary Post-Validation ---\n")
print(summary(Credit_Risk_Data))


# ------------------- Cleaning / Pre-processing (Continued) -------------------
# Visualizing missing data (if needed)
cat("\n--- Visualizing Missing Data ---\n")
missing_counts <- colSums(is.na(Credit_Risk_Data))
barplot(missing_counts, main = "Missing Data Counts", col = "steelblue", las = 2)

cat("\nPreprocessing and validation completed successfully!\n")

# Categorizing employment and job fields
unique(Credit_Risk_Data$employment)
unique(Credit_Risk_Data$job)

Credit_Risk_Data <- Credit_Risk_Data %>%
  mutate(employment_category = case_when(
    employment == "unemployed" ~ "unemployed",
    employment == ">=7" ~ "longEmploy",
    employment == "4<=X<7" ~ "midEmploy",
    employment %in% c("1<=X<4", "<1") ~ "shortEmploy",
    TRUE ~ "other"  # Optional: to handle unexpected values
  ))

Credit_Risk_Data <- Credit_Risk_Data %>%
  mutate(job_category = case_when(
    job == "skilled" ~ "Skilled",
    job == "unskilled resident" ~ "Unskilled resident",
    job == "unemp/unskilled non res" ~ "Unskilled unemployed resident",
    job == "high qualif/self emp/mgmt" ~ "HighQualified self employed",
    TRUE ~ "Other"  # Optional: to handle unexpected values
  ))

# Reclassify checking status
Credit_Risk_Data <- Credit_Risk_Data %>%
  mutate(checking_status = case_when(
    checking_status == "< 0" ~ "No money / Overdrawn",
    checking_status == "0 <= X < 200" ~ "Small balance",
    checking_status == ">= 200" ~ "Healthy balance",
    checking_status == "No Account" ~ "No account",
    TRUE ~ "Unknown"  # Catch any unexpected categories
  ))

# Inspect the new categories
unique(Credit_Risk_Data$employment_category)
unique(Credit_Risk_Data$job_category)

# View the updated dataset
View(Credit_Risk_Data)




# ================================= ANALYSIS ===================================

#----------------------------------Objective 1----------------------------------

# Preprocessing Objective 1: Dividing into age categories
cat("\n--- Adding Age Categories ---\n")
Credit_Risk_Data$age_group <- cut(
  Credit_Risk_Data$age, 
  breaks = c(0, 18, 30, 50, 100), 
  labels = c("Children and Teens", "Young Adult", "Middle-aged", "Elderly")
)
print(unique(Credit_Risk_Data$age_group))

# 1-1 Scatter plot to visualize the relationship between age and credit class
ggplot(Credit_Risk_Data, aes(x = age, fill = class)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "dodge") +
  labs(title = "Distribution of Age by Credit Class", x = "Age", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "gold", "bad" = "darkred"))

# 1-2 Bar plot to show age groups and their association with credit class
ggplot(Credit_Risk_Data, aes(x = age_group, fill = class)) +
  geom_bar(position = "fill", alpha = 0.7) +
  labs(title = "Credit Risk Distribution by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "lightblue", "bad" = "pink"))

# 1-3 Box plot to visualize credit amount distribution by age and credit class
ggplot(Credit_Risk_Data, aes(x = class, y = age, fill = class)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Age as a Predictor for Credit Risk", x = "Credit Class", y = "Age") +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "slateblue", "bad" = "turquoise"))

# 1-4 Faceted bar plot by housing type and credit class
ggplot(Credit_Risk_Data, aes(x = age_group, fill = class)) +
  geom_bar(position = "fill", alpha = 0.7) +
  facet_wrap(~ housing) +
  labs(
    title = "Credit Risk by Age Group and Housing Type",
    x = "Age Group",
    y = "Proportion"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "darkgreen", "bad" = "yellow")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),  # Adds space around the plot
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotates x-axis labels
    strip.text = element_text(size = 10)  # Adjusts facet title size
  )

# 1-5 Faceted bar plot by purpose and credit class
ggplot(Credit_Risk_Data, aes(x = age_group, fill = class)) +
  geom_bar(position = "fill", alpha = 2) +
  facet_wrap(~ purpose, scales = "free") +
  labs(title = "Credit Risk by Age Group and Loan Purpose", x = "Age Group", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "limegreen", "bad" = "khaki")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),  # Adjusts outer margins
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilts x-axis labels
  )

# --------------------------------EXTRA FEATURES--------------------------------
# 1-1 Interaction Between Age Group and Employment
ggplot(Credit_Risk_Data, aes(x = age_group, fill = class)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  facet_wrap(~ employment) +
  labs(
    title = "Credit Risk Distribution by Age Group and Employment",
    x = "Age Group",
    y = "Count"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "blue", "bad" = "orange")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )

# 1-2 Density Plot for Age and Credit Class
ggplot(Credit_Risk_Data, aes(x = age, fill = class)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Age Distribution by Credit Class",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "purple", "bad" = "red"))


# 1-3 Correlation Heatmap
install.packages("reshape2")
library(reshape2)
library(ggplot2)

# Compute correlation matrix
correlation_matrix <- cor(Credit_Risk_Data[sapply(Credit_Risk_Data, is.numeric)], use = "complete.obs")

# Melt correlation matrix for ggplot
correlation_melted <- melt(correlation_matrix)

# Plot heatmap
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(
    title = "Correlation Heatmap",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 1-4 Stacked Bar Plot with Percentage Labels
library(scales)

ggplot(Credit_Risk_Data, aes(x = age_group, fill = class)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = percent) +  # Convert y-axis to percentage
  labs(
    title = "Proportion of Credit Class by Age Group",
    x = "Age Group",
    y = "Proportion"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "green", "bad" = "red")) +
  geom_text(
    stat = "count",
    aes(label = scales::percent((..count..) / tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
    position = position_fill(vjust = 0.5),
    size = 3
  )

# Interactive Plots
install.packages("plotly")
library(plotly)

# Interactive scatter plot
plot <- ggplot(Credit_Risk_Data, aes(x = age, y = credit_amount, color = class)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Interactive Age vs Credit Amount Scatterplot",
    x = "Age",
    y = "Credit Amount"
  ) +
  theme_minimal()

ggplotly(plot)



#----------------------------------Objective 2----------------------------------
# Analyze whether employment stability influences credit risk classification (employment, class)  Bar Graph
library(ggplot2)

ggplot(Credit_Risk_Data, aes(x = employment_category, fill = class)) +
  geom_bar(position = "dodge") + # "dodge" places bars side by side
  labs(
    title = "Impact of Employment Category on Credit Risk Classification",
    x = "Employment Category",
    y = "Count",
    fill = "Credit Class"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analyze the relationship between job category and credit risk classification       (job, class) Bar Graph
ggplot(Credit_Risk_Data, aes(x = job_category, fill = class)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Credit Risk Classification by Job Category",
    x = "Job Category",
    y = "Count",
    fill = "Credit Class"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analyze Correlation between Credit Amount and Job category (credit amount, job category) Scatterplot 
# Load necessary library
ggplot(Credit_Risk_Data, aes(x = job_category, y = credit_amount, color = job_category)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  labs(
    title = "Scatter Plot of Credit Amount by Job Category",
    x = "Job Category",
    y = "Credit Amount"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set1")


# Analyse the distribution of credit amount across employment category and Credit Risk Classification (credit amount, employment category and credit risk classification) Boxplot
ggplot(Credit_Risk_Data, aes(x = employment_category, y = credit_amount, fill = employment_category)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1, notch = TRUE) +
  labs(
    title = "Relationship Between Employment Category and Credit Amount",
    x = "Employment Category",
    y = "Credit Amount",
    fill = "Employment Category"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


# Analyze the connection of employment_category ,job_category , and credit amount (employment, job category, and credit amount) heatmap
# Summarize the data for heatmap
library(dplyr)
heatmap_data <- Credit_Risk_Data %>%
  group_by(employment_category, job_category) %>%
  summarise(mean_credit_amount = mean(credit_amount, na.rm = TRUE)) %>%
  ungroup()


# Heatmap visualization
ggplot(heatmap_data, aes(x = job_category, y = employment_category, fill = mean_credit_amount)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Avg Credit Amount") +
  labs(
    title = "Heatmap of Credit Amount by Employment and Job Categories",
    x = "Job Category",
    y = "Employment Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analyze how credit amount influences the credit risk classification histogram
# Histogram of credit amount by credit classification with facets
ggplot(Credit_Risk_Data, aes(x = credit_amount, fill = class)) +
  geom_histogram(binwidth = 500, alpha = 0.7, position = "identity") +
  facet_wrap(~ class) +
  labs(
    title = "Distribution of Credit Amount by Credit Risk Classification",
    x = "Credit Amount",
    y = "Frequency"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "green", "bad" = "red"))

# --------------------------------EXTRA FEATURES--------------------------------
# Create category of credit amount range and employment category ggplot(Credit_Risk_Data, aes(x = job_category, y = credit_amount, color = credit_amount_range)) +
geom_jitter(width = 0.2, height = 0) +
  labs(title = "Dot Plot of Credit Amount Across Job Categories",
       x = "Job Category", y = "Credit Amount") +
  scale_color_manual(values = c("Low" = "lightblue", "Medium" = "orange", "High" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Distribution of Credit Amount Range across Job Categories
ggplot(Credit_Risk_Data, aes(x = job_category, y = credit_amount, color = credit_amount_range)) +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Dot Plot of Credit Amount Across Job Categories",
       x = "Job Category", y = "Credit Amount") +
  scale_color_manual(values = c("Low" = "lightblue", "Medium" = "orange", "High" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----------------------------------Objective 3----------------------------------
# Objective: Finding out whether customer that are not single have a higher credit risk due to 
# longer loan installment durations and more varied purposes for loans 

# Preprocessing Objective 3: Separating gender and marital status
cat("\n--- Splitting Personal Status ---\n")
Credit_Risk_Data <- Credit_Risk_Data %>%
  separate(personal_status, into = c("gender", "marital_status"), sep = " ", remove = TRUE)

# Recategorizing marital status
Credit_Risk_Data <- Credit_Risk_Data %>%
  mutate(marital_status = case_when(
    marital_status == "single" ~ "Single",
    marital_status %in% c("mar/wid", "div/dep/mar") ~ "Married or Widowed",
    marital_status == "div/sep" ~ "Divorced or Separated",
    TRUE ~ "Other"  # Catch unexpected categories
  ))

cat("\nUnique Values in Updated Gender and Marital Status:\n")
print(unique(Credit_Risk_Data$gender))
print(unique(Credit_Risk_Data$marital_status))


#===============================================================================
# Analysis 1: Identifying the relationship between loan duration and customer marital status

summary(Credit_Risk_Data$duration)

library(ggplot2)

# bar chart
ggplot(Credit_Risk_Data, aes(x = marital_status, y = duration, fill = marital_status)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Loan Duration by Marital Status", 
       x = "Marital Status", 
       y = "Average Loan Duration (Months)", 
       fill = "Marital Status") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))  # Adjust the scale and ticks

# Boxplot
ggplot(Credit_Risk_Data, aes(x = marital_status, y = duration)) +
  geom_boxplot(aes(fill = marital_status)) +
  labs(title = "Distribution of Loan Duration by Marital Status", 
       x = "Marital Status", 
       y = "Loan Duration") +
  theme_minimal()


# Analysis 2 : What are the usual purpose of loan for different marital status

ggplot(Credit_Risk_Data, aes(x = purpose, fill = marital_status)) +
  geom_bar(position = "dodge") +
  labs(title = "Loan Purposes by Marital Status", 
       x = "Loan Purpose", 
       y = "Count", 
       fill = "Marital Status") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-label


heatmap_data <- Credit_Risk_Data %>%
  group_by(purpose, marital_status) %>%
  summarise(count = n(), .groups = "drop") 

# Create heatmap
ggplot(heatmap_data, aes(x = purpose, y = marital_status, fill = count)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(
    title = "Heatmap of Loan Purposes by Marital Status",
    x = "Loan Purpose",
    y = "Marital Status",
    fill = "Loan Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


# Analysis 3: What are the relationship between credit amount and credit class of different marital status


ggplot(Credit_Risk_Data, aes(x = credit_amount, fill = class)) +
  geom_histogram(binwidth = 500, alpha = 0.7, position = "dodge") +
  facet_wrap(~ marital_status) +
  labs(title = "Credit Amount Distribution by Marital Status and Credit Class",
       x = "Credit Amount", 
       y = "Count") +
  theme_minimal()


# Analysis 4 : What are the common credit history of customer depending on their marital status


ggplot(Credit_Risk_Data, aes(x = marital_status, fill = credit_history)) +
  geom_bar(position = "fill") +  
  labs(title = "Proportion of Credit History by Marital Status", 
       x = "Marital Status", 
       y = "Proportion", 
       fill = "Credit History") +
  theme_minimal()


# Analysis 5 : Checking status is one of the factor determine credit class

library(ggplot2)

ggplot(Credit_Risk_Data, aes(x = checking_status, fill = marital_status)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ class) +  # Separate by credit class
  labs(
    title = "Checking Status Distribution by Credit Class and Marital Status",
    x = "Checking Account Status",
    y = "Count",
    fill = "Marital Status"
  ) +
  scale_fill_brewer() +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



#----------------------------------Objective 4----------------------------------#
# Analyze the relationship between property magnitude and credit class
# Load necessary libraries
library(dplyr)
library(ggplot2)
#Bivariate analysis
# Select relevant columns
data_subset <- Credit_Risk_Data %>% 
  select(property_magnitude, class)

# Ensure there are no missing values
data_subset <- na.omit(data_subset)

# Convert columns to factors for analysis
data_subset$property_magnitude <- as.factor(data_subset$property_magnitude)
data_subset$class <- as.factor(data_subset$class)

# Create a contingency table
cat("\n--- Contingency Table: Property Magnitude vs. Credit Class ---\n")
table_property_class <- table(data_subset$property_magnitude, data_subset$class)
print(table_property_class)

# Visualize the relationship
ggplot(data_subset, aes(x = property_magnitude, fill = class)) +
  geom_bar(position = "fill", alpha = 0.8) +
  labs(
    title = "Proportion of Credit Class by Property Magnitude",
    x = "Property Magnitude",
    y = "Proportion",
    fill = "Credit Class"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("good" = "skyblue", "bad" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Univariate analysis - How many people own each type of property
# Ensure the property_magnitude column is treated as a factor
Credit_Risk_Data$property_magnitude <- as.factor(Credit_Risk_Data$property_magnitude)

# Summarize the property ownership counts
property_summary <- table(Credit_Risk_Data$property_magnitude)

# Print the summary
cat("\n--- Property Magnitude Distribution ---\n")
print(property_summary)

# Convert the table into a data frame for visualization
property_df <- as.data.frame(property_summary)
colnames(property_df) <- c("Property_Type", "Count")

# Bar plot for univariate analysis
ggplot(property_df, aes(x = Property_Type, y = Count, fill = Property_Type)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(
    title = "Distribution of Property Ownership Types",
    x = "Property Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  

#HeatMap for Property Magnitude by Credit Class
# Create a contingency table for property_magnitude and credit_risk
heatmap_data <- as.data.frame(table(Credit_Risk_Data$property_magnitude, Credit_Risk_Data$class))
colnames(heatmap_data) <- c("Property_Magnitude", "Class", "Count")

# Generate a heatmap
ggplot(heatmap_data, aes(x = Property_Magnitude, y = Class, fill = Count)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "lightyellow", high = "darkred") +  # Use a yellow-to-red palette
  labs(
    title = "Heatmap of Property Magnitude by Credit Class",
    x = "Property Magnitude",
    y = "Credit Class",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )
# Bivariate analysis: Property Magnitude and Credit Class
ggplot(Credit_Risk_Data, aes(x = property_magnitude, fill = class)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(
    title = "Distribution of Property Magnitude by Credit Class",
    x = "Property Magnitude",
    y = "Count",
    fill = "Credit Class"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )
# Create a contingency table
contingency_table <- table(Credit_Risk_Data$property_magnitude, Credit_Risk_Data$class)

# Perform the Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Display the results
chi_squared_test
if (chi_squared_test$p.value < 0.05) {
  print("There is a high relationship between property type and credit risk classification.")
} else {
  print("No relationship between property type and credit risk classification.")
}