# Load Dataset
data <- read.csv("position_salary.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
data
dim(data)

# Inspect Dataset
head(data)
summary(data)

# Assign Variables
YearsExperience <- data$Experience..Years.
Salary <- data$Salary

# Descriptive Statistics

# Mean
print("Mean of YearsExperience")
mean(YearsExperience, na.rm = TRUE)
print("Mean of Salary")
mean(Salary, na.rm = TRUE)

# Median
print("Median of YearsExperience")
median(YearsExperience, na.rm = TRUE)
print("Median of Salary")
median(Salary, na.rm = TRUE)

# Mode Function
modes <- function(x) {
  freq_table <- table(x)
  mode_values <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  return(mode_values)
}

print("Mode of YearsExperience")
modes(YearsExperience)
print("Mode of Salary")
modes(Salary)

# Standard Deviation
print("Standard Deviation of YearsExperience")
sd(YearsExperience, na.rm = TRUE)
print("Standard Deviation of Salary")
sd(Salary, na.rm = TRUE)

# Graphical Representation

# Histograms
hist(YearsExperience, 
     main = "Histogram of Years of Experience", 
     xlab = "Years of Experience", 
     breaks = 10, 
     col = rainbow(8))
hist(Salary, 
     main = "Histogram of Salaries", 
     xlab = "Salary", 
     breaks = 10, 
     col = rainbow(8))

# Categorize Salary into Ranges
max(Salary)
min(Salary)
Salary_class <- cut(Salary, 
                    breaks = c(500000, 1000000, 1500000, 2000000, 2500000), 
                    labels = c("Low [5L-10L)", "Medium [10L-15L)", "High [15L-20L)", "Very High [20L-25L)"), 
                    right = FALSE)

# Frequency of Salary Categories
Salary_freq <- table(Salary_class)

# Print Frequency Table
print(Salary_freq)

# Pie Chart
pie(Salary_freq, 
    labels = paste(names(Salary_freq), "(", Salary_freq, ")", sep = ""), 
    col = rainbow(4), 
    main = "Pie Chart of Salary Ranges")

# Scatter Plot
plot(YearsExperience, Salary, 
     main = "Scatter Plot of YearsExperience vs Salary", 
     xlab = "Years of Experience", 
     ylab = "Salary", 
     col = "blue", 
     pch = 16)

# Add Regression Line
model <- lm(Salary ~ YearsExperience)
abline(model, col = "red", lwd = 2)

# Box Plots
boxplot(YearsExperience, 
        main = "Box Plot of Years of Experience", 
        ylab = "Years of Experience", 
        col = "lightgreen")
boxplot(Salary, 
        main = "Box Plot of Salaries", 
        ylab = "Salary", 
        col = "lightblue")

# Line Graph
sorted_YearsExperience <- sort(YearsExperience)
sorted_Salary <- sort(Salary)

plot(sorted_YearsExperience, sorted_Salary, 
     main = "Years of Experience vs Salary (Sorted)", 
     type = "o", 
     col = "blue", 
     pch = 16, 
     xlab = "Years of Experience (Sorted)", 
     ylab = "Salary (Sorted)")

# Add Legend
legend("topright", 
       legend = c("YearsExperience vs Salary"), 
       col = c("blue"), 
       lty = c(1), 
       pch = c(16))

# Load Required Library
library(BSDA)

# Group Salaries Based on YearsExperience
median_experience <- median(YearsExperience, na.rm = TRUE)

group1 <- Salary[YearsExperience <= median_experience]  # Group 1
group2 <- Salary[YearsExperience > median_experience]   # Group 2

# Calculate Standard Deviations for Both Groups
sigma_x <- sd(group1, na.rm = TRUE)
sigma_y <- sd(group2, na.rm = TRUE)

# Perform Two-Sample Z Test
z_test_result <- z.test(x = group1, y = group2,
                        alternative = "two.sided",  # Two-tailed test
                        sigma.x = sigma_x, 
                        sigma.y = sigma_y)
print("Two-Sample Z Test Results:")
print(z_test_result)

# Categorize YearsExperience
YearsExperience_class <- cut(YearsExperience, 
                             breaks = c(-Inf, 3, 7, Inf), 
                             labels = c("Low", "Medium", "High"))

# Perform ANOVA on Salary by Experience Category
anova_result_experience <- aov(Salary ~ YearsExperience_class, data = data)
print("ANOVA Results (Salary by Experience Category):")
summary(anova_result_experience)
  