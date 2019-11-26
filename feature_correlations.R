# Do a two-tailed t-test with unequal variance to test difference in mean value of features with a moving window

colnames(Data)
featureColumns <- c(7,6,20,11,19,12,13,8,9,10)
windowSpan = 20
spans = seq(0,260,windowSpan)
nSpans = length(spans)-1
table_ttest <- matrix( nrow = nSpans, ncol = length(featureColumns))
table_pvalue <- matrix( nrow = nSpans, ncol = length(featureColumns))

spanLabels <-c()
for (i in 1:nSpans){
  spanLabels[i]<-paste(as.character(spans[i]/2),"-",as.character(spans[i+1]/2),sep="")
}

for (f in 1:length(featureColumns)){
feature = featureColumns[f]
  for (i in 1:nSpans){
    dataTemp<-Data[which(Data$timePoint >= spans[i] & Data$timePoint < spans[i+1]),]
    ttest<-t.test(dataTemp[,feature]~treatmentType,data = dataTemp, var.equal=FALSE, paired=FALSE, alternative = "two.sided")
    table_ttest[i,f]<-ttest$statistic
    table_pvalue[i,f]<-ttest$p.value
    # boxplot(dataTemp[,feature]~dataTemp$treatmentType)
  }
}
rm(f,feature,i,dataTemp,ttest)

# Rows are windows, and columns are t-test statistics
df_ttest<-as.data.frame(table_ttest)
colnames(df_ttest)<-colnames(Data)[featureColumns]
rownames(df_ttest)<-spanLabels
rm(table_ttest)
# Rows are windows, and columns are p-value of t-test statistics
df_pvalue<-as.data.frame(table_pvalue)
colnames(df_pvalue)<-colnames(Data)[featureColumns]
rownames(df_pvalue)<-spanLabels
rm(table_pvalue)

library(gplots)
library(RColorBrewer)
# install.packages('latex2exp')
library(latex2exp)

my_palette <- colorRampPalette(c("darkred", "white", "blue"))(n = 50)
rowLabels<-c(TeX("$A$"),TeX("$P$"),TeX("$a_f$"),TeX("$a_h$"),TeX("$J_{zz}$"),TeX("$f_{vex}$"),TeX("$f_{cav}$")
                    ,TeX("$N_{90}$"),TeX("$\\bar{A}_{90}$"),TeX("$\\sigma_{90}$"))



heatmap.2(t(as.matrix(df_ttest)),dendrogram = "row",Colv="NA",col = my_palette,trace="none",margins =c(8,8),
          xlab= TeX("Time span (hours)"),key= TRUE,symkey = TRUE,keysize = 1.1,density.info="none",key.title = NA,key.xlab = NA,
          labRow = rowLabels,cexRow=1.5,cexCol = 1.5)

text("blahhh", side = 1,
      line = margins[1] - 1.25 + cex.lab / 5,
      cex = cex.lab)
library(xtable)
xtable(df_pvalue,digits=-1)




