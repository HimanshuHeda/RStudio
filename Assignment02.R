# Load Dataset
data <- read.csv("HUBC_stock_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Assign Variables
Open <- data$open
High <- data$high
Low <- data$low
Close <- data$close

# Descriptive Statistics

# Mean
print("Mean of Open")
mean(Open, na.rm = TRUE)
print("Mean of High")
mean(High, na.rm = TRUE)
print("Mean of Close")
mean(Close, na.rm = TRUE)

# Median
print("Median of Open")
median(Open, na.rm = TRUE)
print("Median of High")
median(High, na.rm = TRUE)
print("Median of Close")
median(Close, na.rm = TRUE)

# Mode Function
modes <- function(x) {
  freq_table <- table(x)
  mode_values <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  return(mode_values)
}

print("Mode of Open")
modes(Open)
print("Mode of High")
modes(High)
print("Mode of Close")
modes(Close)

# Standard Deviation
print("Standard Deviation of Open")
sd(Open, na.rm = TRUE)
print("Standard Deviation of High")
sd(High, na.rm = TRUE)
print("Standard Deviation of Close")
sd(Close, na.rm = TRUE)

# Graphical Representation

# Histograms
hist(Open, 
     main = "Histogram of Open Prices", 
     xlab = "Open Prices", 
     breaks = 10, 
     col = rainbow(8))
hist(High, 
     main = "Histogram of High Prices", 
     xlab = "High Prices", 
     breaks = 10, 
     col = rainbow(8))
hist(Close, 
     main = "Histogram of Close Prices", 
     xlab = "Close Prices", 
     breaks = 10, 
     col = rainbow(8))

# Pie Chart for Open Price Categories
max(Open, na.rm = TRUE)
min(Open, na.rm = TRUE)
Open_class <- cut(Open, c(0, 50, 100, 150), right = FALSE)
Open_class
Open_freq <- table(Open_class)
Open_freq
pie(Open_freq, 
    labels = c("Low [0-50)", "Medium [50-100)", "High [100-150)"), 
    col = rainbow(10))

# Box Plot
boxplot(Open, 
        main = "Box Plot of Open Prices", 
        ylab = "Open Prices", 
        col = "lightgreen")
boxplot(High, 
        main = "Box Plot of High Prices", 
        ylab = "High Prices", 
        col = "lightblue")
boxplot(Close, 
        main = "Box Plot of Close Prices", 
        ylab = "Close Prices", 
        col = "pink")

# Line Graph
sorted_Open <- sort(data$open)
sorted_High <- sort(data$high)
sorted_Close <- sort(data$close)

plot(sorted_Open, sorted_High, 
     main = "Open vs High Prices", 
     type = "o", 
     lty = 2, 
     pch = 16, 
     col = "blue", 
     xlab = "Open Prices", 
     ylab = "High Prices")

lines(sorted_Open, sorted_Close, 
      type = "o", 
      lty = 1, 
      pch = 17, 
      col = "red"
      xlab = "Open Prices", 
      ylab = "Close Prices")

# Add Legend
legend("topright", 
       legend = c("Open vs High", "Open vs Close"), 
       col = c("blue", "red"), 
       lty = c(2, 1), 
       pch = c(16, 17))

