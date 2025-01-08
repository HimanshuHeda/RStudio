data <- read.csv("diabetes.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
data
dim(data)
glucose<-data$Glucose
bp<-data$BloodPressure
st<-data$SkinThickness
insulin<-data$Insulin
bmi<-data$BMI
DPF<-data$DiabetesPedigreeFunction
age<-data$Age
outcome<-data$Outcome
hist(bp,main = "Histogram of Blood Pressure of People",xlab="Blood Pressure",ylab="No. of people",breaks = 12,col=rainbow(8))
hist(st,main = "Histogram of Skin Thickness of people",xlab="Skin Thickness",ylab="No. of people",breaks = 12,col=rainbow(8))
hist(insulin,main = "Histogram of Insulin of people",ylab="No. of people",xlab="Insulin",breaks = 12,col=rainbow(8))
hist(age,main = "Histogram of Age of people",xlab="Age",ylab="No. of people",breaks = 12,col=rainbow(8))
#pie
max(bp);min(bp);
bp_class<-cut(bp,c(0,70,80,90,100,125),right=FALSE)
bp_class
bp_freq<-table(bp_class)
bp_freq
pie(bp_freq,main ="Pie Chart on Blood Pressure", labels=c("Low[0-70)","Low-Medium[70-80)","Medium[80-90)","High-Medium[90-100)","High[7-8)"),col=rainbow(10))

max(st);min(st);
st_class<-cut(st,c(0,30,40,100),right=FALSE)
st_class
st_freq<-table(st_class)
st_freq
pie(st_freq,labels=c("Thin[0-30)","Medium[30-60)","Thick[40-100)"),col=rainbow(10))

max(bmi);min(bmi);
bmi_class<-cut(bmi,c(0,24,30,36,42,70),right=FALSE)
bmi_class
bmi_freq<-table(bmi_class)
bmi_freq
pie(bmi_freq,labels=c("Underweight[0-24)","Normal[24-30)","Normal-High[30-36)","Obese[36-42)","High-Obese[42-60"),col=rainbow(10))


#Box Plot

boxplot(bp,main = "Box Plot of Blood Pressure",ylab = "Blood Pressure",col = "red")
boxplot(st,main = "Box Plot of Skin Thickness",ylab = "Skin Thickness",col = "green")
boxplot(bmi,main = "Box Plot of BMI",ylab = "BMI",col = "pink")

#scatter Plot

plot(bp, bmi,
     main = "Scatter Plot ",
     xlab = "Blood Pressure",
     ylab = "BMI",          
     pch = 19,
     col = "blue")


#Despcriptive statistics

#Mean

print("Mean of Blood Pressure")
mean(bp)
print("Mean of Skin Thickness")
mean(st)
print("Mean of BMI")
mean(bmi)
print("Mean of Insulin")
mean(insulin)


#median
print("Median of Blood Pressure")
median(bp)

print("Median of Skin Thickness")
median(st)

print("Median of BMI")
median(bmi)

print("Median of Insulin")
median(insulin)

#Mode

modes<-function(x){
  freq_table<-table(x)
  mode_values <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  return(mode_values)
}
print("Mode of Blood Pressure")
modes(bp)
print("Mode of Skin Thickness")
modes(st)
print("Mode of BMI")
modes(bmi)
print("Mode of Insulin")
modes(insulin)

#Standard Deviation

print("Standard Deviation of Blood Pressure")
sd(bp)
print("Standard Deviation of Skin Thickness")
sd(st)
print("Standard Deviation of BMI")
sd(bmi)
print("Standard Deviation of Insulin")
sd(insulin)



#Two sample Z Test
library(BSDA)
bp1 <- data$BloodPressure[data$Outcome == 0]  # No Diabetes
bp2 <- data$BloodPressure[data$Outcome == 1]  # Diabetes
sigma_x <- sd(bp1)
sigma_y <- sd(bp2)

z.test(x = bp1, y = bp2, 
       alternative = "two.sided", 
       sigma.x = sigma_x, 
       sigma.y = sigma_y)


st1 <- st[data$Outcome == 0]  # No Diabetes
st2 <- st[data$Outcome == 1]  # Diabetes
sigma_x <- sd(st1)
sigma_y <- sd(st2)

z.test(x = st1, y = st2, 
       alternative = "two.sided", 
       sigma.x = sigma_x, 
       sigma.y = sigma_y)

insulin1 <- insulin[data$Outcome == 0]  # No Diabetes
insulin2 <- insulin[data$Outcome == 1]  # Diabetes
sigma_x <- sd(insulin1)
sigma_y <- sd(insulin2)

z.test(x = insulin1, y = insulin2, 
       alternative = "two.sided", 
       sigma.x = sigma_x, 
       sigma.y = sigma_y)

glucose1 <- glucose[data$Outcome == 0]  # No Diabetes
glucose2 <- glucose[data$Outcome == 1]  # Diabetes
sigma_x <- sd(glucose1)
sigma_y <- sd(glucose2)

z.test(x = glucose1, y = glucose2, 
       alternative = "two.sided", 
       sigma.x = sigma_x, 
       sigma.y = sigma_y)

#Anova

anova_result <- aov(age ~ bmi, data = data)
summary(anova_result)

anova_result <- aov(bp ~ st, data = data)
summary(anova_result)


anova_result <- aov(bp ~ insulin, data = data)
summary(anova_result)

anova_result <- aov(st ~ insulin, data = data)
summary(anova_result)

