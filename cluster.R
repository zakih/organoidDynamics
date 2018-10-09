####### 1-5. PCA-Heirarchical clustering ########

# Time code execution
ptm <- proc.time()




# install.packages("gplots")
library('gplots')
library(dendextend)

# 1. Pick feature set (Principal components)
nPCAs = 3
col2cluster<-c(1:nPCAs)  


# 2. prepare data to cluster
Data2Cluster<-cbind(Data$organoidName,Data$treatmentType,Data$timePoint,DataReduced[,col2cluster])
colnames(Data2Cluster)[1]<-"organoidName"
colnames(Data2Cluster)[2]<-"treatmentType"
colnames(Data2Cluster)[3]<-"timePoint"
sapply(Data2Cluster,class)
Data2Cluster$organoidName<-as.character(Data2Cluster$organoidName)
for (i in 1:dim(Data2Cluster)[1]){
  name<-Data2Cluster$organoidName[i]
  nameEdited <- substr(name, start=24, stop=29) 
  Data2Cluster$organoidName[i]<-nameEdited
}


# 3. Select clustering parameters
numFeatures = length(col2cluster);
numTypes = 40;
numTimePoints = 261;
D = matrix(0,nrow = numTypes,ncol = numTypes)
gType = 2 # 1: l1 loss, 2: l2 loss, 3: cosine similarity


# 4. Calculate distance matrix
organoidNames<-as.data.frame(table(Data2Cluster$organoidName))
organoidNames<-as.character(organoidNames$Var1)
for (i in 1:numTypes-1){
  # Select i'th organoid's feature matrix
  colNumLastFeature <-dim(Data2Cluster)[2]
  X_i <- Data2Cluster[which(Data2Cluster$organoidName == organoidNames[i]),c(4:colNumLastFeature)]
  # # Standardize X_i
  # for (kk in 1:dim(X_i)[2]){
  #   temp<-as.data.frame(X_i[,kk])
  #   colMean <- mean(temp[,1])
  #   colStd <- sd(temp[,1])
  #   X_i[,kk]<-(X_i[,kk]-colMean)/colStd
  # }
  
  # Old method for D
  # for (j in (i+1):numTypes){
  #   # Select j'th organoid's feature matrix
  #   X_j <- Data2Cluster[which(Data2Cluster$organoidName == organoidNames[j]),c(4:colNumLastFeature)]
  #   # # Standardize X_i
  #   # for (kk in 1:dim(X_j)[2])
  #   # {
  #   #   temp<-as.data.frame(X_j[,kk])
  #   #   colMean <- mean(temp[,1])
  #   #   colStd <- sd(temp[,1])
  #   #   X_j[,kk]<-(X_j[,kk]-colMean)/colStd
  #   # } 
  #   Dvector = c(rep(0,numFeatures))
  #   for (k in 1:numFeatures){
  #     
  #     if (gType == 3){
  #       x<-X_i[,k]
  #       y<-X_j[,k]
  #       numerator <- 0
  #       x_square <-0
  #       y_square <-0
  #       for (cos_i in 1:numTimePoints){
  #         numerator <- numerator + x[cos_i]*y[cos_i]
  #         x_square <- x_square + x[cos_i]*x[cos_i]
  #         y_square <- y_square + y[cos_i]*y[cos_i]
  #       }
  #       Dvector[k] = numerator/(sqrt(x_square)*sqrt(y_square))
  #     }else {
  #       for (t in 1:numTimePoints){
  #         if(gType ==1){
  #           g_Xitk_X_jtk = abs(X_i[t,k]-X_j[t,k]) # l_1
  #         }else if (gType==2){
  #           g_Xitk_X_jtk = (X_i[t,k]-X_j[t,k])^2 # l_2
  #         }
  #         Dvector[k]<-Dvector[k]+g_Xitk_X_jtk
  #       }
  #     }
  #   }
  #   # Save D(i,j): avg distance over k features between X_i and X_j
  #   if(gType ==1){
  #     D[i,j]<-sum(Dvector)/(numFeatures*numTimePoints) # l_1
  #   }else if (gType==2){
  #     D[i,j]<-sqrt(sum(Dvector))/(numFeatures*numTimePoints) # l_2
  #   }else if (gType==3){
  #     D[i,j]<- 1- sum(Dvector)/numFeatures
  #   }
  #   D[j,i]<-D[i,j]
  # }
  
  for (j in (i+1):numTypes){
    # Select j'th organoid's feature matrix
    X_j <- Data2Cluster[which(Data2Cluster$organoidName == organoidNames[j]),c(4:colNumLastFeature)]
    # # Standardize X_i
    # for (kk in 1:dim(X_j)[2])
    # {
    #   temp<-as.data.frame(X_j[,kk])
    #   colMean <- mean(temp[,1])
    #   colStd <- sd(temp[,1])
    #   X_j[,kk]<-(X_j[,kk]-colMean)/colStd
    # } 
    Dvector = c(rep(0,numFeatures))
    for (k in 1:numFeatures){
      g_Xitk_X_jtk = 0;
      for (t in 1:numTimePoints){
            g_Xitk_X_jtk = g_Xitk_X_jtk + (X_i[t,k]-X_j[t,k])^2 # l_2
      }
      Dvector[k]<-sqrt(g_Xitk_X_jtk)
    }
    # Save D(i,j): avg distance over k features between X_i and X_j
    if (gType==2){
      D[i,j]<-sum(Dvector)/(numFeatures) # l_2
    }
    D[j,i]<-D[i,j]
  }
}

heatmap(D)



# 5. Hierarchichal clustering
distObj<-as.dist(D,diag = TRUE)
# clusters <- hclust(distObj,method = "average")
clusters <- hclust(distObj,method = "complete") # single/complete/average/mcquitty/median/centroid/ward.D/ward.D2
dendroFeatures<-as.dendrogram(clusters)
# plot(dendroFeatures, main = "",ylab = "", horiz = FALSE)

totalDist<-rowSums(D)
dendroFeatures<-reorder(dendroFeatures, wts =totalDist, agglo.FUN = sum)
# plot(dendroFeatures, main = "",ylab = "", horiz = FALSE)
heatMatrix<-as.matrix(D)
# clusterColors<-c("darkseagreen4" ,"darkgoldenrod2", "dodgerblue4","indianred3")
# clusterColors<-c("dodgerblue4","indianred3","darkseagreen4","darkgoldenrod2")
clusterColors<-c("darkseagreen4","darkgoldenrod2","dodgerblue4","indianred3")
dendroFeatures <- color_branches(dendroFeatures, k =4,col= clusterColors)
par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 0, 0, 0))
dendroFeatures %>% set("branches_lwd", 3.5) %>% plot(main = "",ylab = "", horiz = FALSE, yaxt = "n")
plot(dendroFeatures, main = "",ylab = "", horiz = FALSE, yaxt = "n")

heatmap.2(heatMatrix,symm = TRUE,scale = "none" ,trace = "none",
          Rowv = dendroFeatures,dendrogram = "column" , revC = TRUE)
dev.off()

###### 6. Visualize clustering #####

if (gType == 1){
  fileName = paste("cluster_l1Similiarity","_k",numFeatures,".svg",sep = "")
}else if (gType == 2){
  fileName = paste("cluster_l2Similiarity","_k",numFeatures,".svg",sep = "")
}else if (gType ==3){
  fileName = paste("cluster_cosineSimiliarity","_k",numFeatures,".svg",sep = "")
}
# png(filename = fileName,width = 14, height = 4, units = "in", pointsize = 12,bg = "white",  res = 200)
# par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 0, 0, 0))
# plot(dendroFeatures, main = "",ylab = "", horiz = FALSE, yaxt = "n")
# dev.off()


svg(file = fileName,width = 14, height = 4,bg="transparent")
# pdf(file = fileName,width = 14, height = 4)
# tiff(filename = fileName,width = 14, height = 4,units = "in", res = 400)
# png(filename = fileName,width = 14, height = 4, units = "in", pointsize = 12,bg = "white",  res = 400)
par(mfrow=c(1,1), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 0, 0, 0))
dendroFeatures %>% set("branches_lwd", 3.5) %>% plot(main = "", yaxt = "n",ylab = "")
dev.off()

# 6. Cluster images
# install.packages('png')
require(png)

if (gType == 1){
  fileName = paste("cluster_timeSeriesGraphs_l1","_k",numFeatures,".svg",sep = "")
}else if (gType == 2){
  fileName = paste("cluster_timeSeriesGraphs_l2","_k",numFeatures,".svg",sep = "")
}else if (gType ==3){
  fileName = paste("cluster_timeSeriesGraphs_cosine","_k",numFeatures,".svg",sep = "")
}

svg(file = fileName,width = 56, height = 2,bg="transparent")
# pdf(file = fileName,width = 56, height = 2)
# tiff(filename = fileName,width = 56, height = 2,units = "in", res = 400)
# png(filename = fileName,width = 56, height = 2, units = "in", pointsize = 12,bg = "white",  res = 400)
par(mar=0*c(1,1,1,1),oma = 0*c(1,1,1,1))
plot(c(0,400),c(0,12),ty="n",bty= "n",xaxt="n",yaxt="n",xlab="",ylab="",asp =1)
# plot(c(0,400),c(0,12),ty="n",xlab="",ylab="",asp =1)
delta = 10
yBottom = 1
yTop = yBottom + delta
x_i = 1
clusterOrder<-order.dendrogram(dendroFeatures)
for (i in 1:40){
  # if (gType == 1){
  #   imNum = clusterOrder_l1_k6[i]
  # }else if (gType == 2){
  #   imNum = clusterOrder_l2_k6[i]
  # }else if (gType ==3){
  #   imNum = clusterOrder_cosine_k6[i]
  # }
  imNum = clusterOrder[i]
  
  # Full time history
  # img<- readPNG(paste("organoid_timeseries_thmb_",imNum,".png", sep=""))
  # Only last contour 
  img<- readPNG(paste("image_firstLast/organoid_timeseries_last_contour_",imNum,".png", sep=""))
  rasterImage(img,x_i,yBottom,x_i+delta,yTop)
  x_i = x_i + delta
}
dev.off()


###### 7. Cluster feature stats #######

# 7.1 Cluster membership
clusterMemberShip <- as.data.frame(cutree(dendroFeatures, k = 4))
clusterMemberShip$organoidNum<-rownames(clusterMemberShip)
colnames(clusterMemberShip)[1]<-"cluster"
names<-as.data.frame(table(Data$organoidName))
clusterMemberShip<-cbind(names,clusterMemberShip)
clusterMemberShip$Freq<-NULL
colnames(clusterMemberShip)[1]<-"organoidName"


# # Switch cluster numbers to match dendrogram colorbranches output
id1<-which(clusterMemberShip$cluster==1)
id2<-which(clusterMemberShip$cluster==2)
id3<-which(clusterMemberShip$cluster==3)
id4<-which(clusterMemberShip$cluster==4)
# Switch cluster id's in two steps:
clusterMemberShip$cluster[id1]<-11
clusterMemberShip$cluster[id2]<-31
clusterMemberShip$cluster[id3]<-21
clusterMemberShip$cluster[id4]<-41
id1<-which(clusterMemberShip$cluster==11)
id2<-which(clusterMemberShip$cluster==21)
id3<-which(clusterMemberShip$cluster==31)
id4<-which(clusterMemberShip$cluster==41)
clusterMemberShip$cluster[id1]<-1
clusterMemberShip$cluster[id2]<-2
clusterMemberShip$cluster[id3]<-3
clusterMemberShip$cluster[id4]<-4


# 7.2 Cluster colors
# clusterColors
Data$organoidNum<-c(rep(0,dim(Data)[1]))
Data$cluster<-c(rep(0,dim(Data)[1]))
Data$clusterColors<-c(rep("",dim(Data)[1]))
numTypes = 40
for (i in 1:numTypes){
  organoidName <- clusterMemberShip$organoidName[i]
  cluster <- clusterMemberShip$cluster[i]
  organoidNum<-clusterMemberShip$organoidNum[i]
  Data$cluster[which(Data$organoidName == organoidName)]<-cluster
  Data$organoidNum[which(Data$organoidName == organoidName)]<-organoidNum
  Data$clusterColors[which(Data$organoidName == organoidName)]<-clusterColors[cluster]
}
table(clusterMemberShip$cluster)
table(Data$cluster)
table(Data$cluster,Data$clusterColors)
DataClusterStats<-Data
write.csv(as.matrix(clusterMemberShip[,c(2,3)]),"clusterMembership.csv",row.names = FALSE)



# 7.3 Cluster in PC-space
DataClusterStats$PC1<-DataReduced[,"PC1"]
DataClusterStats$PC2<-DataReduced[,"PC2"]
DataClusterStats$PC3<-DataReduced[,"PC3"]
doLogData = 1
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
symbol<-16

svg(file= "cluster_pc_space.svg", width = 5, height = 5,bg="transparent")
# pdf(file= "cluster_pc_space.pdf", width = 5, height = 5)
# tiff(filename = "cluster_pc_space.tif", width = 5, height = 5,units = "in", res = 400)
# png(filename = "cluster_pc_space.png",width = 5, height = 5, units = "in", pointsize = 12,bg = "white",  res = 200)
par(mfrow = c(1,1),  omi = c(0,0,0,0),mar=c(2, 2, 0.5, 1))
plot(DataClusterStats$PC1,DataClusterStats$PC2,pch = symbol, col = DataClusterStats$clusterColors,cex = 0.6,
     xlab = "",ylab = "",main = "",asp=1,
     xlim = c(xMin,xMax),ylim = c(yMin,yMax))
legend("topleft",
       inset = c(0,0), 
       cex = 1, 
       bty = "n", 
       legend = c("cluster-1","cluster-2","cluster-3","cluster-4"), 
       title = NA,
       title.col = "black",
       text.col = "black",
       col = clusterColors, 
       pt.bg = clusterColors,
       pt.cex = 1.2,
       pch = c(rep(symbol,4)),
       y.intersp = 1.6,
       x.intersp = 1,
       horiz = F)
dev.off()


timeTrimester3<-173
svg(file = "cluster_pc_space_lastTrimester.svg", width = 5, height = 5,bg="transparent")
# pdf(file = "cluster_pc_space_lastTrimester.pdf", width = 5, height = 5)
# tiff(filename = "cluster_pc_space_lastTrimester.tif", width = 5, height = 5,units = "in", res = 400)
par(mfrow = c(1,1),  omi = c(0,0,0,0),mar=c(2, 2, 0.5, 1))
plot(DataClusterStats$PC1[which(DataClusterStats$timePoint>timeTrimester3)],DataClusterStats$PC2[which(DataClusterStats$timePoint>timeTrimester3)],pch = symbol, col = DataClusterStats$clusterColors[which(DataClusterStats$timePoint>timeTrimester3)],
     cex = 0.5, xlab = "",ylab = "",main = "",asp=1,
     xlim = c(xMin,xMax),ylim = c(yMin,yMax))
legend("topleft",
       inset = c(0,0), 
       cex = 1, 
       bty = "n", 
       legend = c("cluster-1","cluster-2","cluster-3","cluster-4"), 
       title = NA,
       title.col = "black",
       text.col = "black",
       col = clusterColors, 
       pt.bg = clusterColors,
       pt.cex = 1.2,
       pch = c(rep(symbol,4)),
       y.intersp = 1.6,
       x.intersp = 1,
       horiz = F)
dev.off()




# 7.4  Cluster stat boxplots
colNames<-as.data.frame(colnames(Data))
colnames(colNames)<-"names"
col2cluster<-c(6,7,8,9,11,12,14,19,20) # all but mode stdev
colNames$names[col2cluster]

# PDM 1st mode variance
varExplained_1stMode = read.csv("varExplained_1stMode.csv",header = F)
varExplained_3Modes = read.csv("varExplained_3Modes.csv",header = F)
auc_5Modes = read.csv("auc_5Modes.csv",header = F)
clusterMemberShip$varExplained_1stMode<-varExplained_1stMode$V1
clusterMemberShip$varExplained_3Modes<-varExplained_3Modes$V1
clusterMemberShip$auc_5Modes<-auc_5Modes$V1

clusterMemberShip$clusterColors<-c(rep("",dim(clusterMemberShip)[1]))
clusterMemberShip$clusterColors[which(clusterMemberShip$cluster ==1)]<-clusterColors[1]
clusterMemberShip$clusterColors[which(clusterMemberShip$cluster ==2)]<-clusterColors[2]
clusterMemberShip$clusterColors[which(clusterMemberShip$cluster ==3)]<-clusterColors[3]
clusterMemberShip$clusterColors[which(clusterMemberShip$cluster ==4)]<-clusterColors[4]
table(clusterMemberShip$cluster,clusterMemberShip$clusterColors)
table(clusterMemberShip$cluster)


sapply(clusterMemberShip,class)
# png(filename = "varExplained_1stMode.png",width = 4, height = 3, units = "in", pointsize = 12,bg = "white",  res = 200)
# tiff(filename = "varExplained_1stMode.tif", width = 4, height = 3,units = "in", res = 400)
# par(mfrow = c(1,1),  omi = c(0,0,0,0),mar=c(2.5, 5, 0.5, 1), cex.axis=1.2)
# boxplot(varExplained_1stMode~cluster,data = clusterMemberShip, col = clusterColors)
# dev.off()


clusterMemberShip$clusterLabels<-paste("C",clusterMemberShip$cluster,sep = "")
DataClusterStatsT3<-DataClusterStats[which(DataClusterStats$timePoint>173),] # for last time trimester
# DataClusterStatsT3<-DataClusterStats[which(DataClusterStats$timePoint>230),]  # for last time quarter
DataClusterStatsT3$clusterLabels<-paste("C",DataClusterStatsT3$cluster,sep = "")
# all boxplots
# png(filename = "cluster_boxplots.png",width = 4, height = 12, units = "in", pointsize = 12,bg = "white",  res = 200)
# tiff(filename = "cluster_boxplots.tif", width = 4, height = 12,units = "in", res = 400)
svg(file = "cluster_boxplots.svg", width = 4, height = 12,bg="transparent")
# pdf(file = "cluster_boxplots.pdf", width = 4, height = 12)
# par(mfrow = c(6,1),  omi = c(0,0,0,0),mar=c(2.5, 5, 0.5, 1))
par(omi = c(0,0,0,0),mar=c(2.5, 5, 0.5, 1),mgp = c(1,0.5,0))
layout(matrix(c(1,2,3,4,5,6,7), 7, 1, byrow = TRUE), 
       widths=c(1,1,1,1,1,1,1), heights=c(2,1,1,1,1,1,1))
plot(DataClusterStats$PC1,DataClusterStats$PC2,pch = symbol, col = DataClusterStats$clusterColors,cex = 0.6,
     xlab = "",ylab = "",main = "",asp=1,
     xlim = c(xMin,xMax),ylim = c(yMin,yMax),tck = -0.01)
legend("topleft",
       inset = c(0,0), 
       cex = 1, 
       bty = "n", 
       legend = c("C1","C2","C3","C4"), 
       title = NA,
       title.col = "black",
       text.col = "black",
       col = clusterColors, 
       pt.bg = clusterColors,
       pt.cex = 1.2,
       pch = c(rep(symbol,4)),
       y.intersp = 1.6,
       x.intersp = 1,
       horiz = F)
boxplot(DataClusterStatsT3$perimeter~DataClusterStatsT3$clusterLabels, col = clusterColors,main = "",ylab = "")
boxplot(DataClusterStatsT3$area~DataClusterStatsT3$clusterLabels, col = clusterColors,main = "",ylab = "")
boxplot(DataClusterStatsT3$perConvex~DataClusterStatsT3$clusterLabels, col = clusterColors,main = "",ylab = "")
boxplot(DataClusterStatsT3$formFactor~DataClusterStatsT3$clusterLabels, col = clusterColors,main = "",ylab = "")
boxplot(varExplained_1stMode~clusterLabels,data = clusterMemberShip, col = clusterColors)
boxplot(auc_5Modes~clusterLabels,data = clusterMemberShip, col = clusterColors)
dev.off()





### extra plots ####
# 
# # par(mfrow=c(3,3), mai = c(0, 0, 0, 0), omi = c(0,0,0,0),mar=c(1, 0, 0, 0))
# par(mfrow=c(3,3))
# for (i in 1:length(col2cluster)){
#   i_feat = col2cluster[i]
#   boxplot(DataClusterStats[,i_feat]~DataClusterStats$cluster, col = clusterColors,main = colNames$names[i_feat])
# }
# 
# par(mfrow=c(3,3))
# for (i in 1:length(col2cluster)){
#   i_feat = col2cluster[i]
#   boxplot(DataClusterStatsT3[,i_feat]~DataClusterStatsT3$cluster, col = clusterColors,main = colNames$names[i_feat])
# }
# 
# 
# 
# par(mfrow=c(1,3))
# boxplot(varExplained_1stMode~cluster,data = clusterMemberShip, col = clusterColors)
# boxplot(varExplained_3Modes~cluster,data = clusterMemberShip, col = clusterColors)
# boxplot(auc_5Modes~cluster,data = clusterMemberShip, col = clusterColors)
# plot(clusterMemberShip$varExplained_1stMode,clusterMemberShip$varExplained_3Modes,cex = 2,pch = 16, col = clusterMemberShip$clusterColors)
# plot(clusterMemberShip$varExplained_1stMode,clusterMemberShip$auc_5Modes,cex = 2,pch = 16, col = clusterMemberShip$clusterColors)
# plot(clusterMemberShip$auc_5Modes,clusterMemberShip$varExplained_3Modes,cex = 2,pch = 16, col = clusterMemberShip$clusterColors)
# 
# 
# 
# library(scales)
# clusterMemberShip$clusterColors_alpha<-alpha(clusterMemberShip$clusterColors,0.5)
# plot(clusterMemberShip$varExplained_1stMode,clusterMemberShip$auc_5Modes,cex = 3,pch = 21, 
#      bg = clusterMemberShip$clusterColors_alpha,
#      xlab = "Fraction of variance in 1st mode",
#      ylab = "AUC fraction-variance curve 5 modes")
# abline(mean(clusterMemberShip$auc_5Modes),0)
# abline(v = mean(clusterMemberShip$varExplained_1stMode))
# text(clusterMemberShip$varExplained_1stMode,clusterMemberShip$auc_5Modes,clusterMemberShip$organoidNum,col ="black")
# 
# 
# boxplot(DataClusterStats$ampMean90~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$perimeter~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$Izz~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$area~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$formFactor~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$aaHullRatio~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$numModes~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$perCollinear~DataClusterStats$cluster, col = clusterColors)
# boxplot(DataClusterStats$perConvex~DataClusterStats$cluster, col = clusterColors)
# 
# 
# # feat vs feat
# plot(DataClusterStats$ampMean90,DataClusterStats$perimeter,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$perimeter,DataClusterStats$area,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$formFactor,DataClusterStats$Izz,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$aaHullRatio,DataClusterStats$Izz,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$formFactor,DataClusterStats$numModes,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$perCollinear,DataClusterStats$perConvex,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$aaHullRatio,DataClusterStats$perConvex,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$formFactor,DataClusterStats$perConvex,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$perConvex,DataClusterStats$Izz,pch = 16, col = DataClusterStats$clusterColors)
# # PC vs time
# plot(DataClusterStats$timePoint,DataClusterStats$PC1,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$PC2,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$PC3,pch = 16, col = DataClusterStats$clusterColors)
# # feat vs time
# plot(DataClusterStats$timePoint,DataClusterStats$area,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$perimeter,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$Izz,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$ampMean90,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$perConvex,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$perCollinear,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$formFactor,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$aaHullRatio,pch = 16, col = DataClusterStats$clusterColors)
# plot(DataClusterStats$timePoint,DataClusterStats$numModes,pch = 16, col = DataClusterStats$clusterColors)
# 
# ## 3D visualization first three principal components
# library(plotly)
# DataClusterStats$cluster<-as.factor(DataClusterStats$cluster)
# table(DataClusterStats$clusterColors, DataClusterStats$cluster)
# p <- plot_ly(DataClusterStats, x = ~PC1, y = ~PC2, z = ~PC3,color = ~cluster, colors = clusterColors) %>%
#   add_markers(sizes=c(2,3),size =~2.5) %>%
#   layout(scene = list(xaxis = list(title = 'PC-1'),
#                       yaxis = list(title = 'PC-2'),
#                       zaxis = list(title = 'PC-3')))
# p




rm(p,id1,id2,id3,id4,symbol,xMax,xMin,yMax,yMin,i_feat,DataClusterStatsT3,timeTrimester3)
rm(varExplained_1stMode,auc_5Modes,varExplained_3Modes)
rm(organoidName,cluster,organoidNum,i,names)
rm(Dvector,clusterColors,nPCAs,colNumLastFeature)
rm(X_i,X_j,i,j,k,t,kk,temp,colMean,colStd,g_Xitk_X_jtk)
rm(x,y,x_square,y_square,numerator,cos_i)
rm(colNames,col2cluster,Data2Cluster,i,name,nameEdited )
rm(numFeatures,numTypes,numTimePoints,D, organoidNames)
rm(heatMatrix,totalDist, filename)
rm(gType,distObj,clusters,dendroFeatures, fileName)
rm(delta,yBottom,yTop,x_i,clusterOrder,i,imNum,img)



timeFinal <- proc.time() - ptm