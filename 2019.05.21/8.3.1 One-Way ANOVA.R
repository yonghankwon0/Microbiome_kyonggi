# Choa1을 measure로 하는 데이터 생성
library(vegan)
abund_table<-read.csv('C:/Users/yongh/Documents/R/R-lab/data/VdrGenusCounts.csv',row.names = 1, check.names = F)
abund_table<-t(abund_table)
CH<-estimateR(abund_table)[2,]
df_CH<-data.frame(sample=names(CH),value=CH,measure=rep("Chaol",length(CH)))
df_CH_G<-cbind(df_CH,grouping)
rownames(df_CH_G)<-NULL
df_CH_G

# group4(Location + Group)에 대한 새로운 변수 생성
df_CH_G$Group4<-with(df_CH_G,interaction(Location,Group))
df_CH_G

# Group4를 기준으로 boxplot 그리기.
boxplot(value~Group4,data=df_CH_G,col=rainbow(4))

# 등분산성 검정을 하기 위한 데이터 전처리
library(dplyr)
df_CH_G4<-select(df_CH_G, Group4, value)
df_CH_G4

#등분산성 검정
#bartlett.test(df_CH_G4,Group4) #error
qchisq(0.95,1) #bartlett 테스트의 K통계량 < 카이제곱 통계량 크면 귀무가설을 받아들임.
fligner.test(df_CH_G4,Group4)

#anova test 실행. Group간의 차이가 없음.
fit<-lm(value~Group4,data=df_CH_G)
anova(fit)
summary(aov(value~Group4,data = df_CH_G))
aov_fit<-aov(value~Group4,data=df_CH_G)
summary(aov_fit,intercept = T)

#broom package를 활용해 깔끔하고 더 정보가 좋은 테이블 얻을 수 있다.
library(broom)
tidy(aov_fit)
augment(aov_fit)
glance(aov_fit)