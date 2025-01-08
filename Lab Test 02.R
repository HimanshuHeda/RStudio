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

# Question number 2

# Perform ANOVA on Salary by Experience Category
anova_result_experience <- aov(Salary ~ YearsExperience_class, data = data)
print("ANOVA Results (Salary by Experience Category):")
summary(anova_result_experience)

# que number 3

# Heights of individuals
heights <- c(63, 63, 66, 67, 68, 69, 70, 70, 13, 23)

# Perform t-test
t_test <- t.test(heights, mu=66)

# Output
print(t_test)



# question number 4 

# Gain in weight
diet_A <- c(25, 32, 30, 34, 24, 14, 32, 24, 30, 31, 35, 13)
diet_B <- c(44, 34, 22, 10, 47, 31, 40, 30, 32, 35, 18, 21, 35, 29, 15)

# Perform t-test
t_test_diets <- t.test(diet_A, diet_B)

# Output
print(t_test_diets)

