library(tidyverse)
install.packages("caret")
library(caret)
library(MASS)
install.packages("glmnet")
library(glmnet)

# Load the dataset
df <- read.csv("C:\\csv files\\heart.csv")

# Check for missing values
print(colSums(is.na(df)))

# Drop rows with missing values
df <- na.omit(df)

# Summary of the dataset
print(str(df))
print(summary(df))

# Install and load required packages
install.packages("DataExplorer")
library(DataExplorer)

# Generate exploratory data analysis report
create_report(df)

# Visualize distribution of the target variable
print(table(df$Target))

# Plot target variable
plot_target <- function() {
  ggplot(df, aes(x = Target)) +
    geom_bar(fill = "skyblue") +
    geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), vjust = -0.5) +
    labs(x = "Target", y = "Count") +
    theme_minimal()
}

plot_target()

# Plot age distribution and disease probability
plot_age <- function() {
  ggplot(df, aes(x = Age, fill = Target)) +
    geom_density(alpha = 0.5) +
    labs(x = "Age", y = "Density") +
    facet_wrap(~Target, ncol = 1) +
    theme_minimal()
}

plot_age()

# Count of categorical variables
print(table(df$ChestPain))
print(table(df$Thal))

# Plot categorical and continuous variables
plot_grid <- function(is_categorical) {
  if (is_categorical) {
    plot_histogram <- function(attribute) {
      ggplot(df, aes_string(x = attribute)) +
        geom_bar(fill = "skyblue") +
        labs(x = attribute, y = "Count") +
        theme_minimal()
    }
    lapply(c("ChestPain", "Thal"), function(attribute) plot_histogram(attribute))
  } else {
    plot_histogram <- function(attribute, xlabel) {
      ggplot(df, aes_string(x = attribute)) +
        geom_histogram(fill = "skyblue", bins = 20) +
        labs(x = xlabel, y = "Density") +
        theme_minimal()
    }
    lapply(c("Age", "Sex", "RestBP", "Fbs", "Chol", "MaxHR", "ExAng", "Oldpeak", "Ca"), 
           function(attribute) plot_histogram(attribute, xlabel = attribute))
  }
}

plot_grid(is_categorical = TRUE)
plot_grid(is_categorical = FALSE)

# Create dummy variables for categorical variables
df$ChestPain_Typical <- as.integer(df$ChestPain == "typical")
df$ChestPain_Nonanginal <- as.integer(df$ChestPain == "nonanginal")
df$ChestPain_Nontypical <- as.integer(df$ChestPain == "nontypical")
df$ChestPain_Asymptomatic <- as.integer(df$ChestPain == "asymptomatic")

df$Thal_Fixed <- as.integer(df$Thal == "fixed")
df$Thal_Normal <- as.integer(df$Thal == "normal")
df$Thal_Reversible <- as.integer(df$Thal == "reversable")

