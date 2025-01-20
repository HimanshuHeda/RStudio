# Que 1 : --
age <- c(56,42,72,56,63,47,55,49,38,42,68,60)
bp <- c(14,125,168,118,149,128,158,145,115,140,013,016)

model <- lm(bp ~ age)

b0 <- coef(model)[1] # Intercept
b1 <- coef(model)[2] # Slope

# Regression equation
cat("The regression line is: bp =", b0, "+", b1, "* age\n")

# Que 2 : --
# a time study engineer developed a new sequence of operation elements that he hopes will
# reduce the mean cycle that of a certain production process. the results of a time (in minutes)
# study od 20 cycles are given below:
  
  
# If the present mean cycle time is 12.5 minutes should he adopt the new sequence()  
  
  




# Data
cycle <- c(13.25, 12.97, 13.15, 13.08, 13.31, 13.28, 12.89, 13.16, 
           13.04, 13.09, 13.15, 13.14, 13.47, 12.98, 13.04, 13.11, 
           13.25, 13.15, 13.00, 13.18)

# Perform one-sample t-test
t_test <- t.test(cycle, mu = 12.5, alternative = "greater")

# Display results
t_test





# Que 3 

