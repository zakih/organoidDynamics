##### Area vs Perimeter ######
# Get coefficients of A = C_i P^2 for basal and FGF2 fits
#second degree
y = Data$area[which(Data$treatmentType %in% 'basal')]
x = Data$perimeter[which(Data$treatmentType %in% 'basal')]
x2 = x^2
fitBasal <-lm(y~ x2 + 0)
summary(fitBasal)
plot(x,y)
y = Data$area[which(Data$treatmentType %in% 'fgf2')]
x = Data$perimeter[which(Data$treatmentType %in% 'fgf2')]
x2 = x^2
fitFGF2 <-lm(y~ x2 + 0)
summary(fitFGF2)
plot(x,y)


# Basal coefficient of perimeter^2
fitBasal$coefficients
# FGF2 coefficient of perimeter^2
fitFGF2$coefficients


#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(0,10000, length=500)
# yy <- xx*xx*fitTumor$coefficients[2] + fitTumor$coefficients[1]
yyBasal<- xx*xx*fitBasal$coefficients[1]
yyFGF2<- xx*xx*fitFGF2$coefficients[1]
par(new = T)
lines(xx,yyBasal,col=colBasal)
par(new = T)
lines(xx,yyFGF2,col=colFGF2)




library(scales)
Data$colorAlpha<-Data$color
Data$colorAlpha[which(Data$treatmentType == "fgf2")]<-alpha(Data$color[which(Data$treatmentType == "fgf2")],0.1)
Data$colorAlpha[which(Data$treatmentType == "basal")]<-alpha(Data$color[which(Data$treatmentType == "basal")],0.5)
Data$circleArea <- (Data$perimeter^2)/(4*pi)
min(Data$perimeter)
max(Data$perimeter)
min(Data$area)
max(Data$area)
xmin = 0
xmax = 1500
ymin = 0
ymax = 55000

# By time
summary(Data$timePoint)




colFGF2First<-rgb(242, 113, 201,maxColorValue = 255)
colFGF2Last<-rgb(99, 17, 73,maxColorValue = 255)
colBasalFirst<-rgb(110, 239, 151,maxColorValue = 255)
colBasalLast<-rgb(8, 79, 31,maxColorValue = 255)
nTimeGradients = 3
colfuncBasal <- colorRampPalette(c(colBasalFirst, colBasalLast))
colBasalGradient<-colfuncBasal(nTimeGradients)
colfuncFGF2 <- colorRampPalette(c(colFGF2First, colFGF2Last))
colFGF2Gradient<-colfuncFGF2(nTimeGradients)
Data$colorAlphaTime<-Data$colorAlpha
for (i in 1:dim(Data)[1]){
  if (Data$timePoint[i]<87.33){
    if (Data$treatmentType[i] == "basal"){
      Data$colorAlphaTime[i]<-colBasalGradient[1]
    }else{
      Data$colorAlphaTime[i]<-colFGF2Gradient[1]
    }
  }else if (Data$timePoint[i]< 174.667 && Data$timePoint[i]>=87.33){
    if (Data$treatmentType[i] == "basal"){
      Data$colorAlphaTime[i]<-colBasalGradient[2]
    }else{
      Data$colorAlphaTime[i]<-colFGF2Gradient[2]
    }
  }else
    if (Data$treatmentType[i] == "basal"){
      Data$colorAlphaTime[i]<-colBasalGradient[3]
    }else{
      Data$colorAlphaTime[i]<-colFGF2Gradient[3]
    }
}
table(Data$colorAlphaTime)

# svg(filename= "area_perimeter_by_time.svg",width = 6, height = 6,bg = "transparent")
pdf("area_perimeter_by_time.pdf", width=6, height=6)
# tiff(filename = "area_perimeter_by_time.tif", width=6, height=6,units = "in",res = 400)


plot(Data$perimeter[which(Data$treatmentType == "fgf2")],Data$area[which(Data$treatmentType == "fgf2")],col= Data$colorAlphaTime[which(Data$treatmentType == "fgf2")],pch = 20,xlim = c(xmin,xmax),ylim = c(ymin,ymax),
     xlab = "", ylab = "", cex = 0.5,tck = -0.015)
par(new = T)
plot(Data$perimeter[which(Data$treatmentType == "basal")],Data$area[which(Data$treatmentType == "basal")],col= Data$colorAlphaTime[which(Data$treatmentType == "basal")],pch = 20,xlim = c(xmin,xmax),ylim = c(ymin,ymax),
     xlab = "", ylab = "", cex = 0.5)
# legend("topleft",
#        inset = c(0.05,0.05), 
#        cex = 1, 
#        bty = "n", 
#        legend = c("Basal","FGF2"), 
#        title = NA,
#        title.col = "black",
#        text.col = "black",
#        col = "black", 
#        pt.bg = c(colBasal,colFGF2),
#        pt.cex = 1.2,
#        pch = c(21,21,21),
#        y.intersp = 1.6,
#        x.intersp = 1)
par(new = T)
lines(xx,yyBasal,col=colBasalDark,lwd = 2)
par(new = T)
lines(xx,yyFGF2,col=colFGF2Dark,lwd = 2)
par(new = T)
# plot(sort(Data$perimeter),sort(Data$circleArea),col = "black",type = "l",xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "")
perimeterSpan <- seq(xmin,xmax, length=500)
circleArea<- perimeterSpan*perimeterSpan/(4*pi)
lines(perimeterSpan,circleArea,col="black",lwd = 2)
# legend("topleft",
#        inset = c(0.04,0.28),
#        cex = 1,
#        legend = c("A = P^2/4pi"),
#        title = NA,
#        text.col = "black",
#        col = "black",
#        lty = 1,
#        bty = "n"
# )
dev.off()












rm(i,nTimeGradients,perimeterSPan,bBasal,bFGF2,circleArea)
rm(colFGF2First,colFGF2Last,colBasalFirst,colBasalLast,colfuncBasal,colfuncFGF2,colFGF2Gradient)
rm(colBasalGradient,perimeterSpan)
rm(y,x,x2,fitBasal,fitFGF2,xx,yyBasal,yyFGF2)
rm(colfill,d,xmin,xmax,ymin,ymax,xx,yyBasal,yyFGF2,fitBasal,fitFGF2,x,y,x2)


