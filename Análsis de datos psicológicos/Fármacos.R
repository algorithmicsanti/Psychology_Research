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
presi�n_arterial_df<-read.csv("C:/Users/santi/Downloads/Ejercicio 3 - Hoja 1.csv", header = TRUE)

# Clean the data
presi�n_arterial_df$PRESI�N<-as.numeric(presi�n_arterial_df$PRESI�N)
presi�n_arterial_df$F�RMACO<-as.character(presi�n_arterial_df$F�RMACO)
head(presi�n_arterial_df)
str(presi�n_arterial_df)

# Statistics

# ANOVA TEST supuestos
## Assumptions
### Make a vector from each group
# Create vector for group one
group_one <- presi�n_arterial_df[presi�n_arterial_df$F�RMACO == "1", "PRESI�N"]

# Create vector for group two
group_two <- presi�n_arterial_df[presi�n_arterial_df$F�RMACO == "2", "PRESI�N"]

# Create vector for group three
group_three <- presi�n_arterial_df[presi�n_arterial_df$F�RMACO == "3", "PRESI�N"]

### Normal distribution test: Shapiro-Wilk normality test
shapiro.test(group_one)
shapiro.test(group_two)
shapiro.test(group_three)

### Independent groups: Durbin Watson
lm_dw<-lm(PRESI�N ~ F�RMACO, data = presi�n_arterial_df)
dwtest(lm_dw)

## Homogeneity of variances: Levene test
leveneTest(PRESI�N ~ F�RMACO, data = presi�n_arterial_df)

# ANOVA TEST
ANOVA<-aov(PRESI�N ~ F�RMACO, data = presi�n_arterial_df)
summary(ANOVA)
# Orthogonal model
MODELO <- lm(PRESI�N~C1 + C2, data = presi�n_arterial_df)
summary(MODELO)







