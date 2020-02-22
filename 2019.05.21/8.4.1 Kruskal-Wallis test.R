#Vdr-/- mouse data
library(dplyr)
data<-mutate(df_CH_G,Group=factor(df_CH_G$Group4,levels=unique(df_CH_G$Group4)))




#Obtain Descriptive Statistics
library(FSA)
Summarize(value~Group4,data=df_CH_G)

#Generate Histogram by Group
#Individual plots in panel of 2 columns and 2 rows
library(lattice)
histogram(~value|Group4, data=df_CH_G,layout=c(2,2))

#Kruskal wallis test of Chao 1 richness
kruskal.test(value~Group4,data = df_CH_G)
qchisq(0.95,3)

install.packages("DescTools")
library("DescTools")
#Tukey method for adjusting p-values
Test_N <- NemenyiTest(x=df_CH_G$value,g=df_CH_G$Group4,dist="tukey")
Test_N

#Dunn Test for Multiple Comparisons
Test_N<-dunnTest(df_CH_G$value~df_CH_G$Group4,data=df_CH_G,method="bh")
Test_N
