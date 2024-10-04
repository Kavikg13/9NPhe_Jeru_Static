# Null Hypotheses: 
# 
# H1: The means of BPS grouped by Concentration are the same 
# H2: The means of BPS grouped by Life Stage (DPF) are the same 
# H3: There is n interaction between Concentraiton and Life Stage
#
#
install.packages("lmom")
install.packages("DescTools")
library(tidyverse)
library(DescTools)
library(dplyr)

Data <- RSTUDIO_9NPhe_Static_Jeru_2024_Trial_Statistics_Cardiology
Data$DPF <- as.factor(Data$DPF)
Data$Concentration_μM <- as.factor(Data$Concentration_μM)
Data$BPM <- Data$BPS*60

#2 Histograms to visualize data 
plot(BPS ~ Concentration_μM + DPF, data = Data)
plot(BPS ~ Concentration_μM + DPF, data = Data)
#Visualize distribution
ggplot(Data, aes(x = BPS)) +
  geom_histogram() + 
  xlim(1, 2) + 
  facet_wrap(~Concentration_μM, ncol = 1)

ggplot(Data, aes(x = BPS)) +
  geom_histogram() + 
  xlim(1, 2) + 
  facet_wrap(~DPF, ncol = 1)

#Test for Variance 
Data %>%
  group_by(Concentration_μM) %>%
  summarize(var(BPS))

Data %>%
  group_by(DPF) %>%
  summarize(var(BPS))

#Two Way Anova 

twoANOVA <- aov(BPS ~ Concentration_μM * DPF, data = Data)
summary(twoANOVA)

#Post Hoc
DunnettTest(x = Data$BPS, g = Data$Concentration_μM)
DunnettTest(x = Data$BPS, g = Data$Concentration_μM) %>% plot(xlim = c(-0.4,0.1))

#One Way ANOVA for DPF
DPF.aov <- aov(BPS~DPF, data = Data)
summary(DPF.aov)
#Post Hoc
TukeyHSD(DPF.aov)
TukeyHSD(DPF.aov) %>% plot()

