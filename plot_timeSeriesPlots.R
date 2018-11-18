#### Time series plots ####
library(scales)
a<-0.4
colBasalAlpha<-alpha(colBasal,a)
colFGF2Alpha<-alpha(colFGF2,a)
lineWidth = 0.5


i = 7
dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
summary(dataTemp$area)
timeVec<-(dataTemp$timePoint*30)/60
plot(timeVec,dataTemp$area,type = "l")
     
# Check number of files is correct
numFilesPerOrganoid <-as.data.frame(table(Data$organoidName))
colnames(numFilesPerOrganoid)<-c("organoidName","numFiles")

# svg(filename= "timeSeries_all.svg",width = 5.5, height = 8,bg = "transparent")
# pdf(file= "timeSeries_all.pdf",width = 5.5, height = 8)
# tiff(filename = "timeSeries_all.tif",width = 5.5, height = 8, units = "in", res = 400)
png(filename = "timeSeries_all.png",width = 5.5, height = 8, units = "in", pointsize = 12,bg = "white",  res = 300)

# mai : c(bottom, left, top, right)
par(mfrow=c(5,2), oma = c(0,0,0,0),mai = c(0.4,0.55,0.13,0.15),cex.axis=1)
axis_tck = -0.05
ylabels = 0


#######  1. Area ###### 
max(Data$area)
min(Data$area)
xmin = 0
xmax = 135
ymin = 0
ymax = 55000



# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$area,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
}
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'A')
}

# legend("topleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )



# # Mean value
# mean_ah_Basal<-mean(Data$area[which(Data$treatmentType == "basal")])
# mean_ah_FGF2<-mean(Data$area[which(Data$treatmentType == "fgf2")])
# abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# 
# # Linear fit
# lmBasal = lm(area ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal  = lmBasal$coefficients[2]
# bBasal  = lmBasal$coefficients[1]
# lmFGF2  = lm(area ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2   = lmFGF2$coefficients[2]
# bFGF2   = lmFGF2$coefficients[1]
# abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


# Exponential fit
y = Data$area[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(area) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# area = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$area[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(area) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# area = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2


tt<-seq(xmin,xmax,1)
fitBasal<-a_basal*exp(b_basal*tt)
lines(tt,fitBasal,col = colBasalDark,lwd = 2)

fitFGF2<-a_fgf2*exp(b_fgf2*tt)
lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)

c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)

par(new = F)

#######  2. Perimeter  ###### 
max(Data$perimeter)
min(Data$perimeter)
xmin = 0
xmax = 135
ymin = 0
ymax = 1500

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$perimeter,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
}
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'P')
}
# legend("topleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )



# # Mean value
# mean_ah_Basal<-mean(Data$perimeter[which(Data$treatmentType == "basal")])
# mean_ah_FGF2<-mean(Data$perimeter[which(Data$treatmentType == "fgf2")])
# abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# 
# # Linear fit
# lmBasal = lm(perimeter ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal  = lmBasal$coefficients[2]
# bBasal  = lmBasal$coefficients[1]
# lmFGF2  = lm(perimeter ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2   = lmFGF2$coefficients[2]
# bFGF2   = lmFGF2$coefficients[1]
# abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


# Exponential fit
y = Data$perimeter[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(perimeter) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# perimeter = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$perimeter[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(perimeter) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# perimeter = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2


tt<-seq(xmin,xmax,1)
fitBasal<-a_basal*exp(b_basal*tt)
lines(tt,fitBasal,col = colBasalDark,lwd = 2)

fitFGF2<-a_fgf2*exp(b_fgf2*tt)
lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)

c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)

par(new = F)

#######  3. Form Factor  ###### 
max(Data$formFactor)
min(Data$formFactor)
xmin = 0
xmax = 135
ymin = 0
ymax = 1

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$formFactor,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
}
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'f_f')
}
# legend("bottomleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )


# # Mean value
# mean_ah_Basal<-mean(Data$formFactor[which(Data$treatmentType == "basal")])
# mean_ah_FGF2<-mean(Data$formFactor[which(Data$treatmentType == "fgf2")])
# abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# 
# # Linear fit
# lmBasal = lm(formFactor ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal  = lmBasal$coefficients[2]
# bBasal  = lmBasal$coefficients[1]
# lmFGF2  = lm(formFactor ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2   = lmFGF2$coefficients[2]
# bFGF2   = lmFGF2$coefficients[1]
# abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


# Exponential fit
y = Data$formFactor[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(formFactor) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# formFactor = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$formFactor[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(formFactor) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# formFactor = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2


tt<-seq(xmin,xmax,1)
fitBasal<-a_basal*exp(b_basal*tt)
lines(tt,fitBasal,col = colBasalDark,lwd = 2)

fitFGF2<-a_fgf2*exp(b_fgf2*tt)
lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)



c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)


par(new = F)


#######  4. Hull area ratio/Solidity #######  
max(Data$aaHullRatio)
min(Data$aaHullRatio)
xmin = 0
xmax = 135
ymin = 0.4
ymax = 1

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$aaHullRatio,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
  
}
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'a_H')
}
# legend("bottomleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )

# Mean value
# mean_ah_Basal<-mean(Data$aaHullRatio[which(Data$treatmentType == "basal")])
# mean_ah_FGF2<-mean(Data$aaHullRatio[which(Data$treatmentType == "fgf2")])
# abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)

# Linear fit
# lmBasal = lm(aaHullRatio ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal  = lmBasal$coefficients[2]
# bBasal  = lmBasal$coefficients[1]
# lmFGF2  = lm(aaHullRatio ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2   = lmFGF2$coefficients[2]
# bFGF2   = lmFGF2$coefficients[1]
# abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


# Exponential fit
y = Data$aaHullRatio[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(aaHullRatio) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# aaHullRatio = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$aaHullRatio[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(aaHullRatio) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# aaHullRatio = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2


tt<-seq(xmin,xmax,1)
fitBasal<-a_basal*exp(b_basal*tt)
lines(tt,fitBasal,col = colBasalDark,lwd = 2)

fitFGF2<-a_fgf2*exp(b_fgf2*tt)
lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)


c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)


par(new = F)





#######  5. Izz -log scale #######  
xmin = 0
xmax = 135
ymin = 13
ymax = 20

y = Data$Izz[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
summary(exponential.model)
# log(Izz) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# Izz = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$Izz[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
summary(exponential.model)
# log(Izz) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# Izz = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2

c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,log(dataTemp$Izz),type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n", log = "")
  par(new =T)
}
abline(c_1basal,c_2basal,col = colBasalDark,lwd = 2)
abline(c_1fgf2,c_2fgf2,col = colFGF2Dark,lwd = 2)
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'log(J_zz)')
}
# legend("topleft",
#        inset = c(0.01,0.0),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n",
#        horiz = T
# )

par(new = F)

#######  6. percent convex #######  
min(Data$perConvex)
max(Data$perConvex)
xmin = 0
xmax = 135
ymin = 0.22
ymax = 0.42

meanConvexBasal<-mean(Data$perConvex[which(Data$treatmentType == "basal")])
meanConvexFGF2<-mean(Data$perConvex[which(Data$treatmentType == "fgf2")])

lmBasal = lm(perConvex ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
mBasal = lmBasal$coefficients[2]
bBasal = lmBasal$coefficients[1]
lmFGF2 = lm(perConvex ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
mFGF2 = lmFGF2$coefficients[2]
bFGF2 = lmFGF2$coefficients[1]


# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$perConvex,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
}
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'f_convex')
}
# legend("topleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )


# 
# # Mean value
# mean_ah_Basal<-mean(Data$perConvex[which(Data$treatmentType == "basal")])
# mean_ah_FGF2<-mean(Data$perConvex[which(Data$treatmentType == "fgf2")])
# abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# 
# # Linear fit
# lmBasal = lm(perConvex ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal  = lmBasal$coefficients[2]
# bBasal  = lmBasal$coefficients[1]
# lmFGF2  = lm(perConvex ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2   = lmFGF2$coefficients[2]
# bFGF2   = lmFGF2$coefficients[1]
# abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


# Exponential fit
y = Data$perConvex[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(perConvex) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# perConvex = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$perConvex[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(perConvex) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# perConvex = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2


tt<-seq(xmin,xmax,1)
fitBasal<-a_basal*exp(b_basal*tt)
lines(tt,fitBasal,col = colBasalDark,lwd = 2)

fitFGF2<-a_fgf2*exp(b_fgf2*tt)
lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)


c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)


par(new = F)

# #######  7. percent concave #######



min(Data$perConcave)
max(Data$perConcave)
xmin = 0
xmax = 135
ymin = 0.25
ymax = 0.45

meanConcaveBasal<-mean(Data$perConcave[which(Data$treatmentType == "basal")])
meanConcaveFGF2<-mean(Data$perConcave[which(Data$treatmentType == "fgf2")])

lmBasal = lm(perConcave ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
mBasal = lmBasal$coefficients[2]
bBasal = lmBasal$coefficients[1]
lmFGF2 = lm(perConcave ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
mFGF2 = lmFGF2$coefficients[2]
bFGF2 = lmFGF2$coefficients[1]


# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$perConcave,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
}
axis(side=1, labels=TRUE)
axis(side=2, labels=TRUE)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
# mtext(side = 2, line = 2,cex = 0.8, "% Concave")
# legend("bottomleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )

#
#
# # Mean value
# mean_ah_Basal<-mean(Data$perConcave[which(Data$treatmentType == "basal")])
# mean_ah_FGF2<-mean(Data$perConcave[which(Data$treatmentType == "fgf2")])
# abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
#
# # Linear fit
# lmBasal = lm(perConcave ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal  = lmBasal$coefficients[2]
# bBasal  = lmBasal$coefficients[1]
# lmFGF2  = lm(perConcave ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2   = lmFGF2$coefficients[2]
# bFGF2   = lmFGF2$coefficients[1]
# abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


# Exponential fit
y = Data$perConcave[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(perConcave) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# perConcave = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$perConcave[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(perConcave) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# perConcave = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2


tt<-seq(xmin,xmax,1)
fitBasal<-a_basal*exp(b_basal*tt)
lines(tt,fitBasal,col = colBasalDark,lwd = 2)

fitFGF2<-a_fgf2*exp(b_fgf2*tt)
lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)


c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)

par(new = F)

# #######  7a. percent collinear #######  
# min(Data$perCollinear)
# max(Data$perCollinear)
# xmin = 0
# xmax = 135
# ymin = 0.19
# ymax = 0.47
# 
# meanCollinearBasal<-mean(Data$perCollinear[which(Data$treatmentType == "basal")])
# meanCollinearFGF2<-mean(Data$perCollinear[which(Data$treatmentType == "fgf2")])
# 
# lmBasal = lm(perCollinear ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# mBasal = lmBasal$coefficients[2]
# bBasal = lmBasal$coefficients[1]
# lmFGF2 = lm(perCollinear ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# mFGF2 = lmFGF2$coefficients[2]
# bFGF2 = lmFGF2$coefficients[1]
# 
# 
# # par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
# for (i in 1:dim(numFilesPerOrganoid)[1]){
#   dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
#   if(dataTemp$treatmentType[1] %in% "basal"){
#     lineColor = colBasalAlpha
#   }else{
#     lineColor = colFGF2Alpha
#   }
#   timeVec<-(dataTemp$timePoint*30)/60
#   plot(timeVec,dataTemp$perCollinear,type = "l",col = lineColor,
#        xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
#        xaxt='n', yaxt='n',bty="n")
#   par(new =T)
# }
# axis(side=1, labels=TRUE,tck=axis_tck)
# axis(side=2, labels=TRUE,tck=axis_tck)
# mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
# if (ylabels == 0 ){
#   mtext(side = 2, line = 2,cex = 0.8,'')
# }else{
#   mtext(side = 2, line = 2,cex = 0.8, 'f_collinear')
# }
# # legend("bottomleft",
# #        inset = c(0.05,0.05),
# #        cex = 1.2, lwd = 2,
# #        legend = c("Basal","FGF2"),
# #        title = NA,
# #        text.col = "black",
# #        col = c(colBasal,colFGF2),
# #        lty = 1,
# #        bty = "n"
# # )
# 
# 
# #
# # # Mean value
# # mean_ah_Basal<-mean(Data$perCollinear[which(Data$treatmentType == "basal")])
# # mean_ah_FGF2<-mean(Data$perCollinear[which(Data$treatmentType == "fgf2")])
# # abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# # abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# #
# # # Linear fit
# # lmBasal = lm(perCollinear ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# # mBasal  = lmBasal$coefficients[2]
# # bBasal  = lmBasal$coefficients[1]
# # lmFGF2  = lm(perCollinear ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# # mFGF2   = lmFGF2$coefficients[2]
# # bFGF2   = lmFGF2$coefficients[1]
# # abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# # abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)
# #
# 
# # Exponential fit
# y = Data$perCollinear[which(Data$treatmentType %in% 'basal')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
# exponential.model <- lm(log(y)~ x)
# # log(perCollinear) = c_1 + c_2*t
# c_1basal = exponential.model$coefficients[1]
# c_2basal = exponential.model$coefficients[2]
# # perCollinear = ae^bt
# a_basal = exp(c_1basal)
# b_basal = c_2basal
# 
# y = Data$perCollinear[which(Data$treatmentType %in% 'fgf2')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
# exponential.model <- lm(log(y)~ x)
# # log(perCollinear) = c_1 + c_2*t
# c_1fgf2 = exponential.model$coefficients[1]
# c_2fgf2 = exponential.model$coefficients[2]
# # perCollinear = ae^bt
# a_fgf2 = exp(c_1fgf2)
# b_fgf2 = c_2fgf2
# 
# 
# tt<-seq(xmin,xmax,1)
# fitBasal<-a_basal*exp(b_basal*tt)
# lines(tt,fitBasal,col = colBasalDark,lwd = 2)
# 
# fitFGF2<-a_fgf2*exp(b_fgf2*tt)
# lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)
# 
# c(a_basal ,b_basal)
# c(a_fgf2 ,b_fgf2)
# 
# par(new = F)








#######  8. numModes #######  
max(Data$numModes)
min(Data$numModes)
xmin = 0
xmax = 135
ymin = 0
ymax = 150

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,dataTemp$numModes,type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n")
  par(new =T)
}
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'N_90')
}
# legend("topleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )



# # Mean value
mean_ah_Basal<-mean(Data$numModes[which(Data$treatmentType == "basal")])
mean_ah_FGF2<-mean(Data$numModes[which(Data$treatmentType == "fgf2")])
c(mean_ah_Basal,mean_ah_FGF2)
abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
rm(mean_ah_Basal,mean_ah_FGF2)

# Linear fit
lmBasal = lm(numModes ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
mBasal  = lmBasal$coefficients[2]
bBasal  = lmBasal$coefficients[1]
lmFGF2  = lm(numModes ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
mFGF2   = lmFGF2$coefficients[2]
bFGF2   = lmFGF2$coefficients[1]
abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)


c(mBasal ,bBasal)
c(mFGF2 ,bFGF2)


# 
# # Exponential fit
# y = Data$numModes[which(Data$treatmentType %in% 'basal')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
# exponential.model <- lm(log(y)~ x)
# # log(numModes) = c_1 + c_2*t
# c_1basal = exponential.model$coefficients[1]
# c_2basal = exponential.model$coefficients[2]
# # numModes = ae^bt
# a_basal = exp(c_1basal)
# b_basal = c_2basal
# 
# y = Data$numModes[which(Data$treatmentType %in% 'fgf2')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
# exponential.model <- lm(log(y)~ x)
# # log(numModes) = c_1 + c_2*t
# c_1fgf2 = exponential.model$coefficients[1]
# c_2fgf2 = exponential.model$coefficients[2]
# # numModes = ae^bt
# a_fgf2 = exp(c_1fgf2)
# b_fgf2 = c_2fgf2
# 
# 
# tt<-seq(xmin,xmax,1)
# fitBasal<-a_basal*exp(b_basal*tt)
# lines(tt,fitBasal,col = colBasalDark,lwd = 2)
# 
# fitFGF2<-a_fgf2*exp(b_fgf2*tt)
# lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)




par(new = F)


# #######  9. Mode amplitude mean #######  
# max(Data$ampMean90)
# min(Data$ampMean90)
# xmin = 0
# xmax = 135
# ymin = 0
# ymax = 15000
# 
# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
# for (i in 1:dim(numFilesPerOrganoid)[1]){
#   dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
#   if(dataTemp$treatmentType[1] %in% "basal"){
#     lineColor = colBasalAlpha
#   }else{
#     lineColor = colFGF2Alpha
#   }
#   timeVec<-(dataTemp$timePoint*30)/60
#   plot(timeVec,dataTemp$ampMean90,type = "l",col = lineColor,
#        xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
#        xaxt='n', yaxt='n',bty="n")
#   par(new =T)
# }
# axis(side=1, labels=TRUE)
# axis(side=2, labels=TRUE)
# mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
# mtext(side = 2, line = 2,cex = 0.8, "Mean mode amplitude")
# legend("topleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )
# 
# 
# 
# # # Mean value
# # mean_ah_Basal<-mean(Data$ampMean90[which(Data$treatmentType == "basal")])
# # mean_ah_FGF2<-mean(Data$ampMean90[which(Data$treatmentType == "fgf2")])
# # abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# # abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# # 
# # # Linear fit
# # lmBasal = lm(ampMean90 ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# # mBasal  = lmBasal$coefficients[2]
# # bBasal  = lmBasal$coefficients[1]
# # lmFGF2  = lm(ampMean90 ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# # mFGF2   = lmFGF2$coefficients[2]
# # bFGF2   = lmFGF2$coefficients[1]
# # abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# # abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)
# 
# 
# # Exponential fit
# y = Data$ampMean90[which(Data$treatmentType %in% 'basal')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
# exponential.model <- lm(log(y)~ x)
# # log(ampMean90) = c_1 + c_2*t
# c_1basal = exponential.model$coefficients[1]
# c_2basal = exponential.model$coefficients[2]
# # ampMean90 = ae^bt
# a_basal = exp(c_1basal)
# b_basal = c_2basal
# 
# y = Data$ampMean90[which(Data$treatmentType %in% 'fgf2')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
# exponential.model <- lm(log(y)~ x)
# # log(ampMean90) = c_1 + c_2*t
# c_1fgf2 = exponential.model$coefficients[1]
# c_2fgf2 = exponential.model$coefficients[2]
# # ampMean90 = ae^bt
# a_fgf2 = exp(c_1fgf2)
# b_fgf2 = c_2fgf2
# 
# 
# tt<-seq(xmin,xmax,1)
# fitBasal<-a_basal*exp(b_basal*tt)
# lines(tt,fitBasal,col = colBasalDark,lwd = 2)
# 
# fitFGF2<-a_fgf2*exp(b_fgf2*tt)
# lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)
# 


#######  9a. Mode amplitude mean -log scale #######  
xmin = 0
xmax = 135
ymin = 3
ymax = 11

y = Data$ampMean90[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(ampMean90) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# ampMean90 = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$ampMean90[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(ampMean90) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# ampMean90 = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2

c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,log(dataTemp$ampMean90),type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n", log = "")
  par(new =T)
}
abline(c_1basal,c_2basal,col = colBasalDark,lwd = 2)
abline(c_1fgf2,c_2fgf2,col = colFGF2Dark,lwd = 2)
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'log(A_90)')
}
# legend("bottomleft",
#        inset = c(0.01,0.0),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n",
#        horiz = T
# )

par(new = F)



# #######  10.Mode amplitude stdev #######  
# max(Data$ampStd90)
# min(Data$ampStd90)
# xmin = 0
# xmax = 135
# ymin = 0
# ymax = 21500
# 
# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
# for (i in 1:dim(numFilesPerOrganoid)[1]){
#   dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
#   if(dataTemp$treatmentType[1] %in% "basal"){
#     lineColor = colBasalAlpha
#   }else{
#     lineColor = colFGF2Alpha
#   }
#   timeVec<-(dataTemp$timePoint*30)/60
#   plot(timeVec,dataTemp$ampStd90,type = "l",col = lineColor,
#        xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
#        xaxt='n', yaxt='n',bty="n")
#   par(new =T)
# }
# axis(side=1, labels=TRUE)
# axis(side=2, labels=TRUE)
# mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
# mtext(side = 2, line = 2,cex = 0.8, "Mode amplitude stdev")
# legend("topleft",
#        inset = c(0.05,0.05),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n"
# )
# # 
# # 
# # # Mean value
# # mean_ah_Basal<-mean(Data$ampStd90[which(Data$treatmentType == "basal")])
# # mean_ah_FGF2<-mean(Data$ampStd90[which(Data$treatmentType == "fgf2")])
# # abline(mean_ah_Basal,0,col = colBasalDark,lwd = 2)
# # abline(mean_ah_FGF2,0,col = colFGF2Dark,lwd = 2)
# # 
# # # Linear fit
# # lmBasal = lm(ampStd90 ~ timePoint, data = Data[which(Data$treatmentType == "basal"),])
# # mBasal  = lmBasal$coefficients[2]
# # bBasal  = lmBasal$coefficients[1]
# # lmFGF2  = lm(ampStd90 ~ timePoint, data = Data[which(Data$treatmentType == "fgf2"),])
# # mFGF2   = lmFGF2$coefficients[2]
# # bFGF2   = lmFGF2$coefficients[1]
# # abline(bBasal,mBasal,col = colBasalDark,lwd = 2)
# # abline(bFGF2,mFGF2,col = colFGF2Dark,lwd = 2)
# 
# 
# # Exponential fit
# y = Data$ampStd90[which(Data$treatmentType %in% 'basal')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
# exponential.model <- lm(log(y)~ x)
# # log(ampStd90) = c_1 + c_2*t
# c_1basal = exponential.model$coefficients[1]
# c_2basal = exponential.model$coefficients[2]
# # ampStd90 = ae^bt
# a_basal = exp(c_1basal)
# b_basal = c_2basal
# 
# y = Data$ampStd90[which(Data$treatmentType %in% 'fgf2')]
# x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
# exponential.model <- lm(log(y)~ x)
# # log(ampStd90) = c_1 + c_2*t
# c_1fgf2 = exponential.model$coefficients[1]
# c_2fgf2 = exponential.model$coefficients[2]
# # ampStd90 = ae^bt
# a_fgf2 = exp(c_1fgf2)
# b_fgf2 = c_2fgf2
# 
# 
# tt<-seq(xmin,xmax,1)
# fitBasal<-a_basal*exp(b_basal*tt)
# lines(tt,fitBasal,col = colBasalDark,lwd = 2)
# 
# fitFGF2<-a_fgf2*exp(b_fgf2*tt)
# lines(tt,fitFGF2,col = colFGF2Dark,lwd = 2)
# 
# 
# 
# 
# 
# 
# 
# 
#


#######  10a. Mode amplitude stdev log scale #######  
xmin = 0
xmax = 135
ymin = 4
ymax = 11

y = Data$ampStd90[which(Data$treatmentType %in% 'basal')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'basal')]
exponential.model <- lm(log(y)~ x)
# log(ampStd90) = c_1 + c_2*t
c_1basal = exponential.model$coefficients[1]
c_2basal = exponential.model$coefficients[2]
# ampStd90 = ae^bt
a_basal = exp(c_1basal)
b_basal = c_2basal

y = Data$ampStd90[which(Data$treatmentType %in% 'fgf2')]
x = (1/2)*Data$timePoint[which(Data$treatmentType %in% 'fgf2')]
exponential.model <- lm(log(y)~ x)
# log(ampStd90) = c_1 + c_2*t
c_1fgf2 = exponential.model$coefficients[1]
c_2fgf2 = exponential.model$coefficients[2]
# ampStd90 = ae^bt
a_fgf2 = exp(c_1fgf2)
b_fgf2 = c_2fgf2

c(a_basal ,b_basal)
c(a_fgf2 ,b_fgf2)

# par(mfrow=c(1,1), oma = c(0,0,1,0),mai = c(0.7,0.7,0.1,0.13),cex.axis=0.7)
for (i in 1:dim(numFilesPerOrganoid)[1]){
  dataTemp<-Data[which(Data$organoidName %in% numFilesPerOrganoid$organoidName[i]),]
  if(dataTemp$treatmentType[1] %in% "basal"){
    lineColor = colBasalAlpha
  }else{
    lineColor = colFGF2Alpha
  }
  timeVec<-(dataTemp$timePoint*30)/60
  plot(timeVec,log(dataTemp$ampStd90),type = "l",col = lineColor,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "", main = "",lwd =lineWidth,
       xaxt='n', yaxt='n',bty="n", log = "")
  par(new =T)
}
abline(c_1basal,c_2basal,col = colBasalDark,lwd = 2)
abline(c_1fgf2,c_2fgf2,col = colFGF2Dark,lwd = 2)
axis(side=1, labels=TRUE,tck=axis_tck)
axis(side=2, labels=TRUE,tck=axis_tck)
mtext(side = 1, line = 2,cex = 0.8 ,'Time (hours)')
if (ylabels == 0 ){
  mtext(side = 2, line = 2,cex = 0.8,'')
}else{
  mtext(side = 2, line = 2,cex = 0.8, 'log(s_90)')
}

# legend("bottomleft",
#        inset = c(0.01,0.0),
#        cex = 1.2, lwd = 2,
#        legend = c("Basal","FGF2"),
#        title = NA,
#        text.col = "black",
#        col = c(colBasal,colFGF2),
#        lty = 1,
#        bty = "n",
#        horiz = T
# )

par(new = F)


dev.off()
rm(tt,a_basal,a_fgf2,x,y,c_1basal,c_1fgf2,c_2basal,c_2fgf2)
rm(timeVec,dataTemp,i,xmin,xmax,ymin,ymax,lineWidth,lineColor)
rm(exponential.model,lmBasal,lmFGF2,numFilesPerOrganoid,a,axis_tck,b_basal,b_fgf2)
rm(fitBasal,fitFGF2,mBasal,meanCollinearBasal,meanCollinearFGF2,meanConvexBasal,meanConvexFGF2)
rm(mFGF2,ptm,timeFinal,ylabels)
