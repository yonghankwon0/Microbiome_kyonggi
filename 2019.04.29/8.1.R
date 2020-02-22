abund_table<-read.csv('C:/Users/yongh/Documents/R/R-lab/2019.04.29/VdrGenusCounts.csv',
                      row.names = 1, check.names = F)
abund_table<-t(abund_table)
abund_table

grouping<-data.frame(row.names = rownames(abund_table),t(as.data.frame(strsplit(rownames(abund_table),"_"))))                     

grouping$Location<-with(grouping, ifelse(X3 %in% "drySt-28F","Fecal","Cecal"))
grouping$Group<-with(grouping,ifelse(as.factor(X2)%in%c(11,12,13,14,15),c("Vdr-/-"),c("WT")))
grouping<-grouping[,c(4,5)]
grouping

install.packages('vegan')
library('vegan')
H<-diversity(abund_table,"shannon")

df_H<-data.frame(sample=names(H),value=H,measure=rep("Shannon",length(H)))
df_H

df_G<-cbind(df_H,grouping)
df_G

Fecal_G<-subset(df_G,Location=="Fecal")
Fecal_G

library(ggplot2)
p<-ggplot(Fecal_G,aes(x=value))+
  geom_histogram(color='black',fill='black')+
  facet_grid(Group ~ .)
p

library(plyr)
mu<-ddply(Fecal_G,"Group",summarise,grp.mean=mean(value))
mu

p+geom_vline(data=mu,aes(xintercept=grp.mean,color='red'),
             linetype='dashed')

fit_t<-t.test(value~Group,data=Fecal_G)
fit_t

fit_w<-wilcox.test(value~Group,data=Fecal_G)
fit_w