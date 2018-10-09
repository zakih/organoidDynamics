

####### PCA animation ########
numTimePointsAnimation = 260
nTimeGradients = numTimePointsAnimation
colBasalGradient<-colfuncBasal(nTimeGradients)
colFGF2Gradient<-colfuncFGF2(nTimeGradients)
pcX <- 1
pcY <-2
DataReducedFGF2<-DataReduced[which(Data$treatmentType == "fgf2"),]
DataReducedBasal<-DataReduced[which(Data$treatmentType == "basal"),]


timePoint <- 260
plotX <-DataReducedFGF2[which(DataReducedFGF2$timePoint==timePoint),pcX]
plotY <-DataReducedFGF2[which(DataReducedFGF2$timePoint==timePoint),pcY]
centroid<-c(mean(plotX),mean(plotY))
col <- colFGF2Gradient[timePoint]
library(scales)
# mar = c(bottom, left, top, right)
png(filename = "test.png",width = 4, height = 4, units = "in", pointsize = 12,bg = "white",  res = 100)
par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 1, 0.2, 0.2)*4)
plot(plotX,plotY,col=col,pch = symbol,cex = 1.4,xlab = "",ylab = "",main = "",asp=1,
     xlim = c(-5,15),ylim = c(-5,6))
text(14,6.5,timePoint)
par(new = T)
plot(centroidTimeSeries[1:timePoint,1],centroidTimeSeries[1:timePoint,2],type = "l",lty=1,col = alpha("red",0.4),lwd = 2,
     xlab = NA, ylab = NA, main = NA, axes = F,xlim = c(-5,15),ylim = c(-5,6))
par(new = T)
plot(centroid[1],centroid[2],pch = 10,cex = 1.3,col ="black",xlab = NA, ylab = NA, main = NA, axes = F,xlim = c(-5,15),ylim = c(-5,6))
title(xlab="PC-1", line=2.5, cex.lab=1.2)
title(ylab="PC-2", line=2.5, cex.lab=1.2)
dev.off()

centroidTimeSeries<-matrix(0,nrow = numTimePointsAnimation,ncol =2)
dir.create("animationFGF2")
for (t in 1:numTimePointsAnimation) {
  plotX <-DataReducedFGF2[which(DataReducedFGF2$timePoint==t),pcX]
  plotY <-DataReducedFGF2[which(DataReducedFGF2$timePoint==t),pcY]
  col <- colFGF2Gradient[t]
  centroid<-c(mean(plotX),mean(plotY))
  centroidTimeSeries[t,1]<-centroid[1]
  centroidTimeSeries[t,2]<-centroid[2]
  
  
  fileName=paste("animationFGF2/frame-",sprintf("%03d", t), ".png", sep="")
  png(filename = fileName,width = 4, height = 4, units = "in", pointsize = 12,bg = "white",  res = 100)
  par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 1, 0.2, 0.2)*4)
  plot(plotX,plotY,col=col,pch = symbol,cex = 1.4,xlab = "",ylab = "",main = "",asp=1,
       xlim = c(-5,15),ylim = c(-5,6))
  text(14,6.5,t)
  par(new = T)
  plot(centroidTimeSeries[1:t,1],centroidTimeSeries[1:t,2],type = "l",lty=1,col = alpha("blue",0.4),lwd = 2,
       xlab = NA, ylab = NA, main = NA, axes = F,xlim = c(-5,15),ylim = c(-5,6))
  par(new = T)
  plot(centroid[1],centroid[2],pch = 10,cex = 1.3,col ="black",xlab = NA, ylab = NA, main = NA, axes = F,xlim = c(-5,15),ylim = c(-5,6))
  title(xlab="PC-1", line=2.5, cex.lab=1.2)
  title(ylab="PC-2", line=2.5, cex.lab=1.2)
  dev.off()
}



centroidTimeSeries<-matrix(0,nrow = numTimePointsAnimation,ncol =2)
dir.create("animationBasal")
for (t in 1:numTimePointsAnimation) {
  plotX <-DataReducedBasal[which(DataReducedBasal$timePoint==t),pcX]
  plotY <-DataReducedBasal[which(DataReducedBasal$timePoint==t),pcY]
  col <- colBasalGradient[t]
  centroid<-c(mean(plotX),mean(plotY))
  centroidTimeSeries[t,1]<-centroid[1]
  centroidTimeSeries[t,2]<-centroid[2]
  
  
  fileName=paste("animationBasal/frame-",sprintf("%03d", t), ".png", sep="")
  png(filename = fileName,width = 4, height =4, units = "in", pointsize = 12,bg = "white",  res = 100)
  par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 1, 0.2, 0.2)*4)
  plot(plotX,plotY,col=col,pch = symbol,cex = 1.4,xlab = "",ylab = "",main = "",asp=1,
       xlim = c(-5,15),ylim = c(-5,6))
  text(14,6.5,t)
  par(new = T)
  plot(centroidTimeSeries[1:t,1],centroidTimeSeries[1:t,2],type = "l",lty=1,col = alpha("red",0.4),lwd = 2,
       xlab = NA, ylab = NA, main = NA, axes = F,xlim = c(-5,15),ylim = c(-5,6))
  par(new = T)
  plot(centroid[1],centroid[2],pch = 10,cex = 1.3,col ="black",xlab = NA, ylab = NA, main = NA, axes = F,xlim = c(-5,15),ylim = c(-5,6))
  title(xlab="PC-1", line=2.5, cex.lab=1.2)
  title(ylab="PC-2", line=2.5, cex.lab=1.2)
  
  dev.off()
}


rm(numTimePointsAnimation,nTimeGradients,colBasalGradient,colFGF2Gradient,pcX,pcY)
rm(DataReducedFGF2,DataReducedBasal,timePoint,plotX,plotY,centroid, col,t)



####### PCA 3D plot ####### 
library(plotly)
library(scales)
DataReduced$colorAlpha[which(Data$treatmentType == "fgf2")]<-alpha(Data$color[which(Data$treatmentType == "fgf2")],0.08)
DataReduced$colorAlpha[which(Data$treatmentType == "basal")]<-alpha(Data$color[which(Data$treatmentType == "basal")],0.3)
DataReduced$treatmentType<-Data$treatmentType


p <- plot_ly(DataReduced, x = ~PC1, y = ~PC2, z = ~PC3,color = ~treatmentType, colors = ~colorAlpha) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC-1'),
                      yaxis = list(title = 'PC-2'),
                      zaxis = list(title = 'PC-3')))
p
p <- plot_ly(DataReduced, x = ~PC1, y = ~PC2, z = ~PC3) %>%
  add_markers(color = ~factor(treatmentType),colors = ~colorAlpha) %>%
  layout(scene = list(xaxis = list(title = 'PC-1'),
                      yaxis = list(title = 'PC-2'),
                      zaxis = list(title = 'PC-3')))



# FGF2 types in 3D
DataReduced$timePoint<-Data$timePoint
DataReducedFGF2<-DataReduced[which(DataReduced$treatmentType =="fgf2"),]
DataReducedFGF2$timePointEpoch<-DataReducedFGF2$timePoint
for (i in 1:dim(DataReducedFGF2)[1]){
  if (DataReducedFGF2$timePoint[i]<87.33){
    DataReducedFGF2$timePointEpoch[i]<-1
  }else if (DataReducedFGF2$timePoint[i]< 174.667 && DataReducedFGF2$timePoint[i]>=87.33){
    DataReducedFGF2$timePointEpoch[i]<-2
  }else
    DataReducedFGF2$timePointEpoch[i]<-3
}
table(DataReducedFGF2$timePointEpoch)
colorsFGF2<-c(colFGF2Gradient[1],colFGF2Gradient[2],colFGF2Gradient[3])
p <- plot_ly(DataReducedFGF2, x = ~PC1, y = ~PC2, z = ~PC3,color = ~timePointEpoch,  colors = colorsFGF2) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC-1'),
                      yaxis = list(title = 'PC-2'),
                      zaxis = list(title = 'PC-3')))
p

# BASAL types in 3D
DataReducedBasal<-DataReduced[which(DataReduced$treatmentType =="basal"),]
DataReducedBasal$timePointEpoch<-DataReducedBasal$timePoint
for (i in 1:dim(DataReducedBasal)[1]){
  if (DataReducedBasal$timePoint[i]<87.33){
    DataReducedBasal$timePointEpoch[i]<-1
  }else if (DataReducedBasal$timePoint[i]< 174.667 && DataReducedBasal$timePoint[i]>=87.33){
    DataReducedBasal$timePointEpoch[i]<-2
  }else
    DataReducedBasal$timePointEpoch[i]<-3
}
table(DataReducedBasal$timePointEpoch)
colorsBasal<-c(colBasalGradient[1],colBasalGradient[2],colBasalGradient[3])
p <- plot_ly(DataReducedBasal, x = ~PC1, y = ~PC2, z = ~PC3,color = ~timePointEpoch,  colors = colorsBasal) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC-1'),
                      yaxis = list(title = 'PC-2'),
                      zaxis = list(title = 'PC-3')))
p
rm(p,DataReducedBasal, colorsBasal,i,colorsFGF2,DataReducedFGF2  )


