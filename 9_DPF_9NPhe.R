library(tidyverse)
library(DescTools)
library(dplyr)

Data <- RSTUDIO_9NPhe_Static_Jeru_2024_Trial_Statistics_Cardiology
Data$DPF <- as.factor(Data$DPF)
Data$Concentration_μM <- as.factor(Data$Concentration_μM)
Data$BPM <- Data$BPS*60
Data.9 <- subset(Data, DPF == 9)

#Visualize Data
ggplot(Data.9, aes(x = Concentration_μM,y = BPM)) + 
  geom_boxplot(aes(group = Concentration_μM))

#Visualize Normal Distribution 
ggplot(Data.9, aes(x = BPS)) +
  geom_histogram() + 
  xlim(1, 2) + 
  facet_wrap(~Concentration_μM, ncol = 1)

#Shapiro Wilk Test for Normal Distrubution 
ShW.0 <- subset(Data.9, Concentration_μM == 0)
shapiro.test(ShW.0$BPM)

ShW.1 <- subset(Data.9, Concentration_μM == 1)
shapiro.test(ShW.1$BPM)

ShW.5 <- subset(Data.9, Concentration_μM == 5)
shapiro.test(ShW.5$BPM)

ShW.20 <- subset(Data.9, Concentration_μM == 20)
shapiro.test(ShW.20$BPM)

ShW.50 <- subset(Data.9, Concentration_μM == 50)
shapiro.test(ShW.50$BPM)

#Test for Equal Variance 
Data.9 %>%
  group_by(Concentration_μM) %>%
  summarize(var(BPS))

bartlett.test(BPM ~ Concentration_μM, data = Data.9)

#Kruskal-Wallis Test 
KW.test <- kruskal.test(Data.9$BPM, Data.9$Concentration_μM)
print(KW.test)

#Post hoc = Dunnetts Test 
DunnettTest(x = Data.9$BPM, g = Data.9$Concentration_μM)
DunnettTest(x = Data.9$BPM, g = Data.9$Concentration_μM) %>% plot()
