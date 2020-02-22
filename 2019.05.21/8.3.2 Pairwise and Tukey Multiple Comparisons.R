#Pairwis tests of mean differences
pairwise.t.test(df_CH_G$value,df_CH_G$Group4,p.adjust='none',pool.sd = T)

#conservative Bonferroni adjustment
pairwise.t.test(df_CH_G$value,df_CH_G$Group4,p.adjust="bonferroni",pool.sd = T)

#Holm method
pairwise.t.test(df_CH_G$value,df_CH_G$Group4,p.adjust="holm",pool.sd=T)

#Benjamini & Hochberg(BH)
pairwise.t.test(df_CH_G$value,df_CH_G$Group4,p.adjust="BH",pool.sd=T)

#Benjamini & Yekutieli
pairwise.t.test(df_CH_G$value,df_CH_G$Group4,p.adjust="BY",pool.sd=T)

#Tukey multiple comparisons of means
TukeyHSD(aov_fit,conf.level = 0.95)
plot(TukeyHSD(aov(df_CH_G$value~df_CH_G$Group4),conf.level = 0.95))