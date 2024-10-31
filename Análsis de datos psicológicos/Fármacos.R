#install libraries
install.packages("psych")
install.packages("car")
install.packages("lsr")
install.packages("lmtest")
install.packages("heplots")
library(heplots)
library(psych)
library(lsr)
library(car)
library(lmtest)

# Load Data
presión_arterial_df<-read.csv("C:/Users/santi/Downloads/Ejercicio 3 - Hoja 1.csv", header = TRUE)

# Clean the data
presión_arterial_df$PRESIÓN<-as.numeric(presión_arterial_df$PRESIÓN)
presión_arterial_df$FÁRMACO<-as.character(presión_arterial_df$FÁRMACO)
head(presión_arterial_df)
str(presión_arterial_df)

# Statistics

# ANOVA TEST supuestos
## Assumptions
### Make a vector from each group
# Create vector for group one
group_one <- presión_arterial_df[presión_arterial_df$FÁRMACO == "1", "PRESIÓN"]

# Create vector for group two
group_two <- presión_arterial_df[presión_arterial_df$FÁRMACO == "2", "PRESIÓN"]

# Create vector for group three
group_three <- presión_arterial_df[presión_arterial_df$FÁRMACO == "3", "PRESIÓN"]

### Normal distribution test: Shapiro-Wilk normality test
shapiro.test(group_one)
shapiro.test(group_two)
shapiro.test(group_three)

### Independent groups: Durbin Watson
lm_dw<-lm(PRESIÓN ~ FÁRMACO, data = presión_arterial_df)
dwtest(lm_dw)

## Homogeneity of variances: Levene test
leveneTest(PRESIÓN ~ FÁRMACO, data = presión_arterial_df)

# ANOVA TEST
ANOVA<-aov(PRESIÓN ~ FÁRMACO, data = presión_arterial_df)
summary(ANOVA)
# Orthogonal model
MODELO <- lm(PRESIÓN~C1 + C2, data = presión_arterial_df)
summary(MODELO)







