#load dataset 
df <- read.csv("Exploratory.csv", header = T)

#reduce the dimensionality of dataset

#check for missing values in dataset 
df <- na.omit(df)

#create dataframe with likert-scaled items only(Q1-Q28)
df_pca <- df[,6:33]

#Do Keiser-Meyer-Olkin (KMxO) to check if dimensional reduction should be done
library(psych)
KMO(df_pca)
#The overall MSA = 0.99 which is greater than 0.8 ,therefore, the dataset can be reduced dimensionally

  #Run a Principal Component Analysis (PCA)
#Plot a Scree plot for Correlation, Fit a full model to check scree plot 
Components <- prcomp(df_pca)
screeplot(Components, type = 'lines')
# Based on the Screeplot there are 10 points of correlation but point 1 and 2 are the most significant point of correlation

#Run parallel analysis to confirm my findings in the scree plot 
library(paran)
paran(df_pca)
# After running the Paran test we confirmed that the 2 components identified in the previous PCA test are indeed significant

#force 2 components 
library(psych)
df2 <- pca(df_pca, nfactors = 2)

#check loading 
df2
#A PCA test was ran and 10 components were Identified based on the diagram we see that components 1 and 2 most correlate with the questions
#The result of the PCA analysis Components 2 has a higher correlation matrix to
#Q1 - Q12 which focuses on course satisfaction, and Components 1 has a higher correlation to Q13
#-Q28 which focuses on instructors satisfaction

#Get the scores for the 2 components and add them to the data set. The reduction of dimensionality is the reason why we have 2 components which makes it easier and more streamlined to analyse 
df$RC1 <- df2$score[,1]

df$RC2 <- df2$score[,2]

View(df)

df$instr = as.factor(df$instr)
df$difficulty = as.factor(df$difficulty)
df$nb.repeat = as.factor(df$nb.repeat)
#I will run a Manova test. A manova test requires only categorical variables,
# therefore, I have converted instr,difficulty into categorical variables. 

#Check whether the RC1 and RC2 are normally distributed 
qqnorm(df$RC1)
qqnorm(df$RC2)
qqline(df$RC1)
qqline(df$RC2)
# based on the QQplot there are farily normal. 

#check homogeneity of variance
library(car)
leveneTest(df$RC1 ~ df$instr)
leveneTest(df$RC1 ~ df$difficulty)
leveneTest(df$RC1 ~ df$nb.repeat)
leveneTest(df$RC2 ~ df$instr)
leveneTest(df$RC2 ~ df$difficulty)
leveneTest(df$RC2 ~ df$nb.repeat)
#Levene test shows for homogeneity of variance RC1 ~ instr is significant @ P<0.05 
boxplot(df$RC1 ~ df$instr)
boxplot(df$RC1 ~ df$difficulty)
boxplot(df$RC1 ~ df$nb.repeat)
boxplot(df$RC2 ~ df$instr)
boxplot(df$RC2 ~ df$difficulty)
boxplot(df$RC2 ~ df$nb.repeat)

#MANOVA test
fit <- manova(cbind(df$RC1, df$RC2) ~ df$instr + df$difficulty + df$nb.repeat)
summary(fit)

#ANOVA results for each DV
summary.aov(fit)
summary.lm(fit)
summary.lm(aov)

library(lsr)
anova1 <- aov(RC1 ~ instr + difficulty + nb.repeat, data = df)
anova2 <- aov(RC2 ~ instr + difficulty + nb.repeat, data = df)
etaSquared(anova1)
etaSquared(anova2)

#confidence intervals
library(sjPlot)
plot_model(anova1, type = "pred")
plot_model(anova2, type = "pred")

#Post hoc test
TukeyHSD(anova1)
TukeyHSD(anova2)



