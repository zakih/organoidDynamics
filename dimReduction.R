###### Load Extracted features #######

#Use to study data in Data\BoundaryCoordinates_basal &  Data\BoundaryCoordinates_fgf2
# columns:
# 1.  organoid/image name
# 2.  treatment type
# 3.  folder
# 4.  file name
# 5.  time point
# 6.  perimeter
# 7.  area
# 8.  numModes
# 9.  ampMean90
# 10. ampStd90
# 11. aaHullRatio
# 12. convex
# 13. concave
# 14. collinear
# 15. fractionAllCurvatureChanges
# 16. fractionConvConcChanges
# 17. Ixx
# 18. Iyy
# 19. Izz
# 20. FormFactor

# Time code execution
ptm <- proc.time()






library(openxlsx)
# For old method:
Data<- read.csv(file="allOrganoidFeatures.csv", header=T, sep=",")
colnames(Data)<-c("organoidName","treatmentType","folder","filename","timePoint","perimeter","area","numModes","ampMean90","ampStd90","aaHullRatio","perConvex","perConcave","perCollinear","fAllCurvatureChanges","fConvConcChanges","Ixx","Iyy","Izz","formFactor")
sapply(Data,class)

# Remove data points beyond T=260
Data<-Data[-which(Data$timePoint>260),]
summary(Data$timePoint)

# Fix negative-area calcs and formFactor
Data$area<-abs(Data$area)
Data$formFactor <- (4*pi*Data$area)/(Data$perimeter^2)

# Check for NA's:
summary(Data)
which(is.na(Data))



# Check number of files is correct
numFilesPerOrganoid <-as.data.frame(table(Data$organoidName))
colnames(numFilesPerOrganoid)<-c("organoidName","numFiles")
rm(numFilesPerOrganoid)



# Color code by tissue type
colFGF2<-rgb(235,68,182,maxColorValue = 255)
colBasal<-rgb(68,235,121,maxColorValue = 255)
Data$color <- c(rep(colBasal,dim(Data)[1]))
for ( i in 1:dim(Data)[1]){
  if (Data$treatmentType[i]=="fgf2") {
    Data$color[i]<- colFGF2
  }
}
rm(i)
table(Data$treatmentType)
table(Data$color)

colBasalDark<- rgb(56, 193, 99,maxColorValue = 255) 
colFGF2Dark<- rgb(175, 51, 136,maxColorValue = 255) 


# Possible erroneous outliers
Data[which.max(Data$perCollinear),]
Data[which.max(Data$perConvex),]
Data[which.max(Data$perConcave),]
which(Data$perConcave == 1)
# Remove organoids with outlier %concavity 
# Data<-Data[-which(Data$perConcave == 1),]
# rownames(Data)<-NULL



# Alpha colors
library(scales)
a<-0.8
colBasalAlpha<-alpha(colBasal,a)
colFGF2Alpha<-alpha(colFGF2,a)

Data$colorAlpha[which(Data$treatmentType == "fgf2")]<-alpha(Data$color[which(Data$treatmentType == "fgf2")],0.05)
Data$colorAlpha[which(Data$treatmentType == "basal")]<-alpha(Data$color[which(Data$treatmentType == "basal")],0.4)

plot(Data$perimeter,Data$area,col = Data$colorAlpha)

rm(a)





####### PCA   ######

# Select original features to reduce
colNames<-as.data.frame(colnames(Data))
colnames(colNames)<-"names"
# col2clusterAll<-c(6,7,8,9,10,11,12,14,19,22) # all
col2cluster<-c(6,7,8,9,11,12,13,19,20) # all but mode stdev, fvex and fcol
colNames$names[col2cluster]
# original dimensions:
length(col2cluster)

# Perform PCA
doLogData = 1 # 0: don't log transform features, 1: log transform area,perimeter, Izz, ampmean90, ampstd90
if (doLogData==0){
  myPCA <- prcomp(Data[,col2cluster], scale. = T, center = T)  # Raw features
}else{
  DataLog<-Data
  col2log<-c(6,7,9,10,19)
  colNames$names[col2log]
  for (i in 1:length(col2log)){
    feat2Log<-col2log[i]
    DataLog[,feat2Log]<-log(DataLog[,feat2Log])
  }
  rm(col2log,feat2Log,i)
  myPCA <- prcomp(DataLog[,col2cluster], scale. = T, center = T)  # Log-transformed features
}
rm(DataLog)

#myPCA$rotation # loadings: coefficient of original features in each PC
#myPCA$x # scores: original data projected along PC's
# biplot(myPCA, scale = 0)

#compute standard deviation of each principal component
std_dev <- myPCA$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex
cumsum(prop_varex)
#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")




# PCA reduced data set
DataReduced<-as.data.frame(myPCA$x)
DataReduced$color <-Data$color
DataReduced$colorAlpha <-Data$colorAlpha
DataReduced$treatmentType <- Data$treatmentType
DataReduced$timePoint<-Data$timePoint
# DataReduced<-cbind(DataT1T2[,c(1:(1+nStatsAdded))],DataReduced)


rm(colNames,col2cluster)
dev.off()


####### PCA-Viz  ######
library('scales')
library('ggplot2')
# install.packages('grid')
library(grid)
library(plyr)



# png(filename = "PCA_grid.png",width = 7, height = 6.5, units = "in", pointsize = 12,bg = "white",  res = 300)
# tiff(filename = "PCA_grid.tif",width = 7, height = 6.5, units = "in", res = 400)
# pdf(file ="PCA_grid.pdf", width=6, height=6,bg = "transparent")
postscript(file ="PCA_grid2.eps", width=6.1, height=6.1,bg = "transparent")



par(mfrow=c(2,2), mai = c(0, 0, 0, 0), omi = c(0.01,0,0,0),mar=c(0.8, 0.8, 0.2, 0.2)*4,mgp = c(2,1,0))

# Plot A: Variance
x<- as.data.frame(myPCA$rotation)
x$var<-rownames(x)
varTitles<-rownames(x)
# varTitles <- mapvalues(varTitles, 
                      #  from=c("perimeter","area","Izz","perConvex","perConcave","perCollinear","ampMean90","aaHullRatio","formFactor","ampStd90","numModes"), 
                      # to=c("P", "A", "J_zz","f_vex","f_cav","f_col","A_90","a_h","a_f","sdev_90","N_90"))
# varTitles <- mapvalues(varTitles, 
                       # from=c("perimeter","area","Izz","perConvex","perCollinear","ampMean90","aaHullRatio","formFactor","numModes"), 
                       # to=c("P", "A", "J_zz","f_vex","f_col","A_90","a_h","a_f","N_90"))

varTitles <- mapvalues(varTitles, 
                       from=c("perimeter","area","Izz","perConvex","perConcave","ampMean90","aaHullRatio","formFactor","numModes"), 
                       to=c("P", "A", "J_zz","f_vex","f_cav","A_90","a_h","a_f","N_90"))



x$var<-varTitles
x<-x[order(abs(x$PC1)),]
positions <-x$var
xPC1<-x[,c("PC1","var")]
xPC1$PC<-c(rep(1,dim(xPC1)[1]))
xPC2<-x[,c("PC2","var")]
xPC2$PC<-c(rep(2,dim(xPC1)[1]))
colnames(xPC1)[1]<-"weights"
colnames(xPC2)[1]<-"weights"
x<-rbind(xPC1,xPC2)
rownames(x)<-NULL
x$PC<-as.factor(x$PC)


# par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0.01,0,0,0),mar=c(0.8, 0.8, 0.2, 0.2)*4,mgp = c(2,1,0))
plot(cumsum(prop_varex), type="o",pch=16, col = "black", xlab = "# principal components",
     ylab = "Cumulative variance explained",main = "",ylim = c(0,1),
     panel.first = abline(0.9,0,col = "gray2",lty = 2),tck = -.02,
     bty = "n")

if (doLogData==0){
  text(cumsum(prop_varex)[1:4],labels = c(1:4),pos = 3)
  text(c(5:9),cumsum(prop_varex)[5:9],labels = c(5:9),pos = 1)
}else{
  text(cumsum(prop_varex)[1:9],labels = c(1:9),pos = 1)
}
# Inset plot of weights
# Basic barplot
# p<-ggplot(data=x, aes(x=var, y=weights,fill = PC)) + coord_flip() +
#   geom_bar(stat="identity", width=1, position=position_dodge())+
#   theme_minimal()+ scale_fill_manual(values=c('gray10','gray50'))+
#   scale_x_discrete(limits = positions) +
#   theme(text = element_text(size=10),axis.line=element_line(),
#         panel.grid.major.x = element_line(color = "gray90",linetype = "solid"),panel.grid = element_blank(),
#         legend.title = element_text(colour="black", size=8, face="bold"),
#         legend.text = element_text(colour="black", size = 8, face = "bold"),
#         legend.justification=c(0,1), legend.position=c(0,1))+
#   ylab("")+xlab("")
p<-ggplot(data=x, aes(x=var, y=weights,fill = PC)) + coord_flip() +
  geom_bar(stat="identity", width=1, position=position_dodge())+
  theme_minimal()+ scale_fill_manual(values=c('gray10','gray50'))+
  scale_x_discrete(limits = positions) +
  theme(text = element_text(size=10),axis.line=element_line(),
        panel.grid.major.x = element_line(color = "gray90",linetype = "solid"),panel.grid = element_blank(),
        legend.position="none")+ ylab("")+xlab("")

# vp <- viewport(width = 0.7, height = 0.75, x = 0.6, y = 0.47)
vp <- viewport(width = 0.4, height = 0.3, x = 0.3, y = 0.71)
print(p, vp = vp)
if (doLogData == 0){
  # For non-log transform data
  text(3.4,0.65+0.05,"PC-1",col = "gray10",cex = 0.9,pos =4)
  text(3.4,0.58+0.05,"PC-2",col = "gray50",cex = 0.9,pos = 4)
}else{
  # For DataLog:
  text(7.9,0.65+0.07,"PC-1",col = "gray10",cex = 0.9,pos =4)
  text(7.9,0.58+0.07,"PC-2",col = "gray50",cex = 0.9,pos = 4)
}
text(1.3,0.97,labels = "A",font = 2,ps =14,cex = 1.2)



# Plot B: Basal + FGF2
pcX <-1
pcY <-2
if (doLogData == 0){
  # For non-log transform data
  xMin = -5
  xMax = 15
  yMin = -5
  yMax = 6
}else{
  # For DataLog:
  xMin = -10
  xMax = 6
  yMin = -6
  yMax = 6
}
DataReduced$colorAlpha[which(Data$treatmentType == "fgf2")]<-alpha(Data$color[which(Data$treatmentType == "fgf2")],0.15)
DataReduced$colorAlpha[which(Data$treatmentType == "basal")]<-alpha(Data$color[which(Data$treatmentType == "basal")],0.2)
symbol<-16
plot(DataReduced[,pcX],DataReduced[,pcY],col=DataReduced$color,bg=DataReduced$color,pch = symbol,cex = 0.2,
     xlab = "PC-1",ylab = "PC-2",main = "",asp=1,
     xlim = c(xMin,xMax),ylim = c(yMin,yMax))
if (doLogData == 0){
  legend("topright",
         inset = c(0,-0.1),
         cex = 1, 
         bty = "o", 
         legend = c("Basal","FGF2"), 
         title = NA,
         text.col = "black",
         col = c(colBasal,colFGF2), 
         pt.bg = c(colBasal,colFGF2),
         pt.cex = 1,
         pch = c(symbol,symbol),
         y.intersp = 1.6,
         x.intersp = 1,
         horiz = F)
  
    text(-4,9,labels = "B",font = 2,ps =14,cex = 1.2)
}else{
  legend("bottomright",
         inset = c(0,0), 
         cex = 1, 
         bty = "n", 
         legend = c("Basal","FGF2"), 
         title = NA,
         title.col = "black",
         text.col = "black",
         col = c(colBasal,colFGF2), 
         pt.bg = c(colBasal,colFGF2),
         pt.cex = 1.2,
         pch = c(symbol,symbol),
         y.intersp = 1.6,
         x.intersp = 1,
         horiz = F)
  
    text(-9,7,labels = "B",font = 2,ps =14,cex = 1.2)
}

# FGF2- hull
X_fgf2 <- as.matrix(DataReduced[which(DataReduced$treatmentType == "fgf2"),c("PC1","PC2")])
hpts <- chull(X_fgf2)
hpts <- c(hpts, hpts[1])
lines(X_fgf2[hpts, ],col = colFGF2Dark)
# basal- hull 
X_basal <- as.matrix(DataReduced[which(DataReduced$treatmentType == "basal" ),c("PC1","PC2")])
hpts <- chull(X_basal)
hpts <- c(hpts, hpts[1])
lines(X_basal[hpts, ],col = colBasalDark)




# # FGF2- hull t1
# X_fgf2 <- as.matrix(DataReduced[which(DataReduced$treatmentType == "fgf2" & DataReduced$timePoint<87.33),c("PC1","PC2")])
# hpts <- chull(X_fgf2)
# hpts <- c(hpts, hpts[1])
# lines(X_fgf2[hpts, ],col = colFGF2Gradient[1])
# # FGF2- hull t2
# X_fgf2 <- as.matrix(DataReduced[which(DataReduced$treatmentType == "fgf2" & DataReduced$timePoint>=87.33 & DataReduced$timePoint < 174.667),c("PC1","PC2")])
# hpts <- chull(X_fgf2)
# hpts <- c(hpts, hpts[1])
# lines(X_fgf2[hpts, ],col = colFGF2Gradient[2])
# # FGF2- hull t3
# X_fgf2 <- as.matrix(DataReduced[which(DataReduced$treatmentType == "fgf2" & DataReduced$timePoint>174.667),c("PC1","PC2")])
# hpts <- chull(X_fgf2)
# hpts <- c(hpts, hpts[1])
# lines(X_fgf2[hpts, ],col = colFGF2Gradient[3])
# # basal- hull t1
# X_basal <- as.matrix(DataReduced[which(DataReduced$treatmentType == "basal" & DataReduced$timePoint<87.33),c("PC1","PC2")])
# hpts <- chull(X_basal)
# hpts <- c(hpts, hpts[1])
# lines(X_basal[hpts, ],col = colBasalGradient[1])
# # basal- hull t2
# X_basal <- as.matrix(DataReduced[which(DataReduced$treatmentType == "basal" & DataReduced$timePoint>=87.33 & DataReduced$timePoint < 174.667),c("PC1","PC2")])
# hpts <- chull(X_basal)
# hpts <- c(hpts, hpts[1])
# lines(X_basal[hpts, ],col = colBasalGradient[2])
# # basal- hull t3
# X_basal <- as.matrix(DataReduced[which(DataReduced$treatmentType == "basal" & DataReduced$timePoint>174.667),c("PC1","PC2")])
# hpts <- chull(X_basal)
# hpts <- c(hpts, hpts[1])
# lines(X_basal[hpts, ],col = colBasalGradient[3])


# By time:
colFGF2First<-rgb(242, 113, 201,maxColorValue = 255)
colFGF2Last<-rgb(99, 17, 73,maxColorValue = 255)
colBasalFirst<-rgb(110, 239, 151,maxColorValue = 255)
colBasalLast<-rgb(8, 79, 31,maxColorValue = 255)
nTimeGradients = 3
colfuncBasal <- colorRampPalette(c(colBasalFirst, colBasalLast))
colBasalGradient<-colfuncBasal(nTimeGradients)
colfuncFGF2 <- colorRampPalette(c(colFGF2First, colFGF2Last))
colFGF2Gradient<-colfuncFGF2(nTimeGradients)


DataReduced$colorAlphaTime<-DataReduced$colorAlpha
for (i in 1:dim(DataReduced)[1]){
  if (DataReduced$timePoint[i]<87.33){
    if (DataReduced$treatmentType[i] == "basal"){
      DataReduced$colorAlphaTime[i]<-colBasalGradient[1]
    }else{
      DataReduced$colorAlphaTime[i]<-colFGF2Gradient[1]
    }
  }else if (DataReduced$timePoint[i]< 174.667 && DataReduced$timePoint[i]>=87.33){
    if (DataReduced$treatmentType[i] == "basal"){
      DataReduced$colorAlphaTime[i]<-colBasalGradient[2]
    }else{
      DataReduced$colorAlphaTime[i]<-colFGF2Gradient[2]
    }
  }else
    if (DataReduced$treatmentType[i] == "basal"){
      DataReduced$colorAlphaTime[i]<-colBasalGradient[3]
    }else{
      DataReduced$colorAlphaTime[i]<-colFGF2Gradient[3]
    }
}
table(DataReduced$colorAlphaTime)


# Plot C: Basal
plot(DataReduced[which(Data$treatmentType == "basal"),pcX],DataReduced[which(Data$treatmentType == "basal"),pcY],col=DataReduced$colorAlphaTime[which(Data$treatmentType == "basal")],pch = symbol,cex = 0.5,
     xlab = "PC-1",ylab = "PC-2",main = "",asp=1,tck = -.02,bg=DataReduced$colorAlphaTime[which(Data$treatmentType == "basal")],
     xlim = c(xMin,xMax),ylim = c(yMin,yMax))

if (doLogData == 0){
  legend("topright", legend = c(expression('t'[1]),expression('t'[2]),expression('t'[3])),pch = symbol,
         col = c(colBasalGradient[1],colBasalGradient[2],colBasalGradient[3]),
         title = "Basal",horiz = T,bty = "o",text.font = 2,cex = 1)
  text(-4,9,labels = "C",font = 2,ps =14,cex = 1.2)
}else{
  legend(0,-4.5, legend = c(expression('t'[1]),expression('t'[2]),expression('t'[3])),pch = symbol,
         col = c(colBasalGradient[1],colBasalGradient[2],colBasalGradient[3]),
         title = "Basal",horiz = T,bty = "o",text.font = 2,cex = 1)
  text(-9,7,labels = "C",font = 2,ps =14,cex = 1.2)
}


# Plot D: FGF2
plot(DataReduced[which(Data$treatmentType == "fgf2"),pcX],DataReduced[which(Data$treatmentType == "fgf2"),pcY],col=DataReduced$colorAlphaTime[which(Data$treatmentType == "fgf2")],pch = symbol,cex = 0.5,
     xlab = "PC-1",ylab = "PC-2",main = "",asp=1,tck = -.02,bg = DataReduced$colorAlphaTime[which(Data$treatmentType == "fgf2")],
     xlim = c(xMin,xMax),ylim = c(yMin,yMax))

if (doLogData == 0){
  legend("topright", legend = c(expression('t'[1]),expression('t'[2]),expression('t'[3])),pch = symbol, 
         col = c(colFGF2Gradient[1],colFGF2Gradient[2],colFGF2Gradient[3]),
         title = "FGF2",horiz = T,bty = "o",text.font = 2,cex = 1)
  text(-4,9,labels = "D",font = 2,ps =14,cex = 1.2)
}else{
  legend(0,-4.5, legend = c(expression('t'[1]),expression('t'[2]),expression('t'[3])),pch = symbol, 
         col = c(colFGF2Gradient[1],colFGF2Gradient[2],colFGF2Gradient[3]),
         title = "FGF2",horiz = T,bty = "o",text.font = 2,cex = 1)
  text(-9,7,labels = "D",font = 2,ps =14,cex = 1.2)
}


dev.off()

 



















rm(x,varTitles,xPC1,xPC2,positions,p,vp, pcX,pcY, symbol,hpts)
rm(colFGF2First,colFGF2Last,colBasalFirst,colBasalLast,nTimeGradients,colfuncBasal,colBasalGradient,colfuncFGF2, colFGF2Gradient,i)
rm(colNames, col2cluster1,col2cluster2,col2cluster3, col2cluster4, col2cluster5)
rm(myPCA, std_dev,pr_var,prop_varex,i)
rm(xMin,xMax,yMin,yMax)


# Plot time series in PC1-PC2 space
col2plot <- c(1:2)
Data2Plot<-cbind(Data$organoidName,Data$treatmentType,Data$timePoint,DataReduced[,col2plot])
colnames(Data2Plot)[1]<-"organoidName"
colnames(Data2Plot)[2]<-"treatmentType"
colnames(Data2Plot)[3]<-"timePoint"
sapply(Data2Plot,class)
Data2Plot$organoidName<-as.character(Data2Plot$organoidName)
for (i in 1:dim(Data2Plot)[1]){
  name<-Data2Plot$organoidName[i]
  nameEdited <- substr(name, start=24, stop=29) 
  Data2Plot$organoidName[i]<-nameEdited
}

numTypes = 40
organoidNames<-as.data.frame(table(Data2Plot$organoidName))
sapply(organoidNames,class)
organoidNames<-as.character(organoidNames$Var1)

xMin = -10
xMax = 6
yMin = -6
yMax = 6


pdf(file = "pcSpace_timeSeries.pdf",width = 17.7, height = 10)
# tiff(filename = "pcSpace_timeSeries.tif",width = 17.7, height = 10, units = "in",  res = 400)
# png(filename = "pcSpace_timeSeries.png",width = 17.7, height = 10, units = "in", pointsize = 12,bg = "white",  res = 300)
par(mfrow=c(5,8), mai = c(0, 0, 0, 0), omi = c(0.01,0,0,0),mar=c(0.8, 0.8, 0.2, 0.2)*4,mgp = c(2,1,0))
for (i in 1:numTypes){
  # Select i'th organoid's first 2 principal components
  X_i <- Data2Plot[which(Data2Plot$organoidName == organoidNames[i]),c("PC1","PC2")]
  if (i<=10){
    col_i = colBasal
  }else{
    col_i = colFGF2
  }
  plot(1, type="n",  xlim = c(-5,15),ylim = c(-5,6),xlab = "PC-1",ylab = "PC-2",main = "",asp=1,tck = -.02)
  par(new = T)
  plot(X_i$PC1,X_i$PC2,xlab = "",ylab = "",main = "",asp=1,xlim = c(xMin,xMax),ylim = c(yMin,yMax),type = "l",col = col_i,
       xaxt = "n", yaxt = "n", bty = "n")
  par(new = T)
  plot(X_i$PC1[1],X_i$PC2[1],xlab = "",ylab = "",main = "",asp=1,pch = 16, col = "dodgerblue4",xlim = c(xMin,xMax),ylim = c(yMin,yMax),
       xaxt = "n", yaxt = "n", bty = "n",cex = 1.5)
  par(new = T)
  plot(X_i$PC1[261],X_i$PC2[261],xlab = "",ylab = "",main = "",asp=1,pch = 16, col = "firebrick4",xlim = c(xMin,xMax),ylim = c(yMin,yMax),
       xaxt = "n", yaxt = "n", bty = "n",cex = 1.5)
  # text(12,5,as.character(i))
  text(-5,5,as.character(i))
}
dev.off()



# Plot PC vs Time time series
col2plot <- c(1:4)
Data2Plot<-cbind(Data$organoidName,Data$treatmentType,Data$timePoint,DataReduced[,col2plot])
colnames(Data2Plot)[1]<-"organoidName"
colnames(Data2Plot)[2]<-"treatmentType"
colnames(Data2Plot)[3]<-"timePoint"
sapply(Data2Plot,class)
Data2Plot$organoidName<-as.character(Data2Plot$organoidName)
for (i in 1:dim(Data2Plot)[1]){
  name<-Data2Plot$organoidName[i]
  nameEdited <- substr(name, start=24, stop=29) 
  Data2Plot$organoidName[i]<-nameEdited
}


library(scales)
a<-0.4
colBasalAlpha<-alpha(colBasal,a)
colFGF2Alpha<-alpha(colFGF2,a)

pdf(file = "pc_timeSeries.pdf",width = 6.4, height = 11)
# tiff(filename = "pc_timeSeries.tif",width = 6.4, height = 11, units = "in", res = 400)
# png(filename = "pc_timeSeries.png",width = 6.4, height = 11, units = "in", pointsize = 12,bg = "white",  res = 300)
par(mfrow=c(4,1), mai = c(0, 0, 0, 0), omi = c(0.01,0,0,0),mar=c(0.8, 0.8, 0.2, 0.2)*4,mgp = c(2,1,0))

# PC-1
yMin = -6
yMax = 6
for (organoidNum in 1:40){
  dataTemp <- Data2Plot[which(Data2Plot$organoidName == organoidNames[organoidNum]),c("PC1","PC2","PC3","PC4")]
  dataTemp$timePoint<-c(0:260)
  if (organoidNum < 10){
    col = colBasalAlpha
  }else {
    col = colFGF2Alpha
  }
  if (organoidNum == 1){
    plot(dataTemp$timePoint,dataTemp$PC1,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = "Time",ylab = "PC-1")
  }else{
    plot(dataTemp$timePoint,dataTemp$PC1,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = NA, ylab = NA, main = NA, axes = F)
  }
  
  par( new = T)
}
par(new = F)

# PC-2
yMin = -6
yMax = 6
for (organoidNum in 1:40){
  dataTemp <- Data2Plot[which(Data2Plot$organoidName == organoidNames[organoidNum]),c("PC1","PC2","PC3","PC4")]
  dataTemp$timePoint<-c(0:260)
  if (organoidNum < 10){
    col = colBasalAlpha
  }else {
    col = colFGF2Alpha
  }
  if (organoidNum == 1){
    plot(dataTemp$timePoint,dataTemp$PC2,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = "Time",ylab = "PC-2")
  }else{
    plot(dataTemp$timePoint,dataTemp$PC2,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = NA, ylab = NA, main = NA, axes = F)
  }
  par( new = T)
}
par( new = F)

# PC-3
yMin = -4
yMax = 4
for (organoidNum in 1:40){
  dataTemp <- Data2Plot[which(Data2Plot$organoidName == organoidNames[organoidNum]),c("PC1","PC2","PC3","PC4")]
  dataTemp$timePoint<-c(0:260)
  if (organoidNum < 10){
    col = colBasalAlpha
  }else {
    col = colFGF2Alpha
  }
  if (organoidNum == 1){
    plot(dataTemp$timePoint,dataTemp$PC3,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = "Time",ylab = "PC-3")
  }else{
    plot(dataTemp$timePoint,dataTemp$PC3,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = NA, ylab = NA, main = NA, axes = F)
  }
  
  par( new = T)
}
par( new = F)

# PC-4
yMin = -2
yMax = 5
for (organoidNum in 1:40){
  dataTemp <- Data2Plot[which(Data2Plot$organoidName == organoidNames[organoidNum]),c("PC1","PC2","PC3","PC4")]
  dataTemp$timePoint<-c(0:260)
  if (organoidNum < 10){
    col = colBasalAlpha
  }else {
    col = colFGF2Alpha
  }
  if (organoidNum == 1){
    plot(dataTemp$timePoint,dataTemp$PC4,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = "Time",ylab = "PC-4")
  }else{
    plot(dataTemp$timePoint,dataTemp$PC4,type = "l",col = col, ylim = c(yMin,yMax),
         xlab = NA, ylab = NA, main = NA, axes = F)
  }
  
  par( new = T)
}
par( new = F)

dev.off()






rm(Data2Plot, X_i,i,organoidNames, numTypes,col2plot)
rm(yMin,yMax,xMin,xMax,varTitles,hpts,i,nTimeGradients,pcX,pcY)
rm(positions,pr_var,prop_varex,std_dev,symbol)
rm(x,X_basal,X_fgf2,xPC1,xPC2)
rm(p,vp)
rm(dataTemp,a,name,nameEdited,organoidNum,col,col_i)


timeFinal <- proc.time() - ptm
