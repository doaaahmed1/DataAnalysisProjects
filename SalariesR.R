# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load the dataset
df <- read.csv("salaries.csv")

# Data exploration
head(df)
str(df)  # Display the structure of the dataset
colSums(is.na(df))  # Check for missing values
duplicate_rows <- sum(duplicated(df))
print(paste("Number of duplicate rows:", duplicate_rows))

unique(df$experience_level)  # Unique values in experience_level
unique(df$job_title)  # Unique job titles
unique(df$company_size)  # Unique company sizes

# Summary statistics of salary
summary(df$salary_in_usd)

# Glimpse of the dataset
glimpse(df)

df$experience_level <- factor(df$experience_level, 
                              levels = c("EX", "SE", "MI", "EN"), 
                              labels = c("Expert", "Senior", "Middle", "Entry Level"))


# Rename company_size values for clarity
df$company_size <- ifelse(df$company_size == "S", "Small",
                          ifelse(df$company_size == "M", "Medium",
                                 ifelse(df$company_size == "L", "Large", df$company_size)))

unique(df$employment_type)
df$employment_type <- ifelse(df$employment_type == "FT", "Full Time",
                         ifelse(df$employment_type == "CT", "Contract",
                                ifelse(df$employment_type == "PT", "Part Time", 
                                       ifelse(df$employment_type == "FL", "Freelance" ,df$employment_type))))

# Check unique values after renaming
unique(df$company_size)

# Check the structure and levels of experience_level
levels(df$experience_level)
class(df$experience_level)
str(df)

# Data Visualization

# Top 5 job titles by average salary
top_5_jobs <- df %>%
  group_by(job_title) %>%
  summarize(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_salary)) %>%
  head(5)  # Select top 5 job titles

# Plot top 5 job titles by average salary
ggplot(top_5_jobs, aes(x = reorder(job_title, -avg_salary), y = avg_salary, fill = job_title)) +
  geom_bar(stat = "identity") +  # Create the bar chart
  coord_flip() +  # Flip the axes to make the bars horizontal
  labs(title = "Top 5 Job Titles by Average Salary",
       x = "Job Title",
       y = "Average Salary in USD") +
  scale_fill_manual(values = c("steelblue", "darkorange", "forestgreen", "firebrick", "purple")) +  # Custom colors for bars
  theme_minimal()

# Boxplot of salary vs experience level
ggplot(df, aes(x = experience_level, y = salary_in_usd)) +
  geom_boxplot() +
  labs(title = "Salary vs Experience Level", x = "Experience Level", y = "Salary in USD")

# Bar plot of average salary by company size
ggplot(df, aes(x = company_size, y = salary_in_usd)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Salary by Company Size", x = "Company Size", y = "Average Salary in USD")

# Scatter plot of remote ratio vs salary
ggplot(df, aes(x = remote_ratio, y = salary_in_usd)) +
  geom_point() +
  labs(title = "Remote Ratio vs Salary", x = "Remote Ratio (%)", y = "Salary in USD")

# Bar plot of average salary by employee residence
ggplot(df, aes(x = employee_residence, y = salary_in_usd)) +
  geom_bar(stat = "summary", fun = "mean") +
  coord_flip() +
  labs(title = "Average Salary by Employee Residence", x = "Employee Residence", y = "Average Salary in USD")
