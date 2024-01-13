library(readxl)
Data <- read_excel("C:/Users/12468/Desktop/Data.xlsx")
View(Data)
attach(Data)
Data$Density <- Data$Population/Data$LA
Data$Density1 <- log(Data$Density)
#数据可视化分析
#基本分布
library(psych)
describe(Data)
summary(AHP)
summary(ALP)
summary(Data$Density1)
summary(GDP)
summary(PGDI)

#带核密度曲线的直方图
hist(AHP, prob = TRUE, breaks = 20,col = "blue",xlab = "平均房价", ylab = "密度", main = "2018年各城市平均房价：添加核密度曲线")
lines(density(AHP), col = "black", lwd = 2)
hist(ALP, prob = TRUE, breaks = 20,col = "blue",xlab = "平均住宅地价", ylab = "密度", main = "2018年各城市平均住宅地价：添加核密度曲线")
lines(density(AHP), col = "black", lwd = 2)
hist(Data$Density1, prob = TRUE, breaks = 20,col = "blue",xlab = "人口密度", ylab = "密度", main = "2018年各城市人口密度：添加核密度曲线")
lines(density(Data$Density1), col = "black", lwd = 2)
hist(GDP, prob = TRUE, breaks = 20,col = "blue",xlab = "GDP", ylab = "密度", main = "2018年各城市GDP：添加核密度曲线")
lines(density(GDP), col = "black", lwd = 2)
hist(PGDI, prob = TRUE, breaks = 20,col = "blue",xlab = "人均可支配收入", ylab = "密度", main = "2018年各城市人均可支配收入：添加核密度曲线")
lines(density(PGDI), col = "black", lwd = 2)

#小提琴图
library(vioplot)
vioplot(AHP,col = "blue", xlab = "平均房价", ylab = "价格")
vioplot(ALP,col = "blue", xlab = "平均住宅地价", ylab = "价格")
vioplot(Data$Density1,col = "blue", xlab = "人口密度", ylab = "密度")
vioplot(GDP,col = "blue", xlab = "GDP", ylab = "亿元")
vioplot(PGDI,col = "blue", xlab = "人均可支配收入", ylab = "元", ylim = c(20000,80000)

#点图
dotchart(AHP,lcolor = "grey95",xlab = "平均房价")
dotchart(ALP,lcolor = "grey95",xlab = "平均住宅地价")
dotchart(GDP,lcolor = "grey95",xlab = "GDP")
dotchart(PGDI,lcolor = "grey95",xlab = "人均可支配收入")
dotchart(Data$Density1,lcolor = "grey95",xlab = "人口密度")

#条形图和饼图
table1 <- table(East)
barplot(table1, xlab = "是否位于东部沿海地区", ylab = "频数", density = 30, col = c("grey50","grey80"),border = "black",main = "是否位于东部沿海地区简单条形图")
name <- names(table1)
percent <- prop.table(table1)*100
percent1 <- substr(percent,1,4)
labs <- paste(name,"所占百分比",":",percent1,"%")
pie(table1,labels = labs, radius = 1, main = "是否位于东部沿海地区饼图")

#数据推断
#正态性检验
shapiro.test(AHP）
ks.test(AHP,"pnorm",mean(AHP),sd(AHP))
qqnorm(AHP,xlab = "理论分位数", ylab = "样本分位数")
qqline(AHP, col = "red", lwd = 2)

Data$AHP2 <- log(AHP)
qqnorm(Data$AHP2,xlab = "理论分位数", ylab = "样本分位数")
qqline(Data$AHP2, col = "red", lwd = 2)
shapiro.test(Data$AHP2)

Datasmall <- subset(Data, Data$AHP2<=10)
qqnorm(Datasmall$AHP2,xlab = "理论分位数", ylab = "样本分位数")
qqline(Datasmall$AHP2, col = "red", lwd = 2)
shapiro.test(Datasmall$AHP2)
ks.test(Datasmall$AHP2,"pnorm",mean(Datasmall$AHP2),sd(Datasmall$AHP2))

#参数估计
DatasamllEast <- subset(Datasmall,Datasmall$East == 1)
DatasmallnoneEast <- subset(Datasmall,Datasmall$East == 0)
#单样本
library(TeachingDemos)
t.test(Datasmall$AHP2,conf.level = 0.95)
sigma.test(Datasmall$AHP2,conf.level = 0.95)
t.test(DatasamllEast$AHP2,conf.level = 0.95)
sigma.test(DatasamllEast$AHP2,conf.level = 0.95)
t.test(DatasmallnoneEast$AHP2,conf.level = 0.95)
sigma.test(DatasmallnoneEast$AHP2,conf.level = 0.95)
#东和非东的差异
t.test(x=DatasamllEast$AHP2,y=DatasmallnoneEast$AHP2,var.equal = FALSE)
var.test(DatasamllEast$AHP2,DatasmallnoneEast$AHP2,alternative = "two.sided")

#假设检验
#平均房价是否在8000左右
t.test(Datasmall$AHP2,mu = log(8000) 
t.test(DatasamllEast$AHP2,mu = log(8000))
t.test(DatasmallnoneEast$AHP2,mu = log(8000))
#东部和非东部之间的差异
t.test(DatasamllEast$AHP2,DatasmallnoneEast$AHP2,var.equal = FALSE)
library(lsr)
cohensD(DatasamllEast$AHP2,DatasmallnoneEast$AHP2)
var.test(DatasamllEast$AHP2,DatasmallnoneEast$AHP2,alternative = "two.sided")

#线性回归
#一元线性回归
#AHP和ALP
library(carData)
library(car)
scatterplot(log(AHP)~log(ALP),data = Data, xlab = "平均地价的对数", ylab = "平均房价的对数")
cor(log(AHP),log(ALP))
cor.test(log(AHP),log(ALP))
model1 <- lm(log(AHP)~log(ALP))
summary(model1)
par(mfrow=c(2,2),cex = 0.8)
plot(model1)
ncvTest(model1)

#AHP和GDP
scatterplot(log(AHP)~log(GDP),data = Data, xlab = "GDP的对数", ylab = "平均房价的对数")
cor(log(AHP),log(GDP))
cor.test(log(AHP),log(GDP))
modelGDP <- lm(log(AHP)~log(GDP),data = Data)
summary(modelGDP)
par(mfrow=c(2,2),cex = 0.8)
plot(modelGDP)
ncvTest(modelGDP)

#AHP和PGDI
scatterplot(log(AHP)~log(PGDI),data = Data, xlab = "人均可支配收入的对数", ylab = "平均房价的对数")
cor(log(AHP),log(PGDI))
cor.test(log(AHP),log(PGDI))
modelPGDI <- lm(log(AHP)~log(PGDI), data = Data)
summary(modelPGDI) 
par(mfrow=c(2,2),cex = 0.8)
plot(modelPGDI)
ncvTest(modelPGDI)

#AHP和Density1
scatterplot(log(AHP)~log(Data$Density1),data = Data, xlab = "人口密度的对数的对数", ylab = "平均房价的对数")
cor(log(AHP),log(Data$Density1))
cor.test(log(AHP),log(Data$Density1))
modelDensity <-lm(log(AHP)~log(Density1), data = Data)
summary(modelDensity)
ncvTest(modelDensity)
par(mfrow=c(2,2),cex = 0.8)
plot(modelDensity)

#多元线性回归
model2 <- lm(AHP ~ ALP + GDP + PGDI + East + Density1, data = Data)
summary(model2)
confint(model2)
anova(model2)
vif(model2)
1/vif(model2)
par(mfrow = c(1,2),mai = c(0.8,0.8,0.4,0.1), cex = 0.8, cex.main =0.7)
plot(model2, which = 1:2)
ncvTest(model2)

model3 <- lm(log(AHP)~log(ALP)+log(GDP)+log(PGDI)+East + log(Density1), data = Data)
summary(model3)
ncvTest(model3)

model4 <- step(model3)
summary(model4)
AIC(model3,model4)
par(mfrow = c(1,2),mai = c(0.8,0.8,0.4,0.1), cex = 0.8)
plot(model4,which = 1:2)
durbinWatsonTest(model4)
vif(model4) 
1/vif(model4)

model5 <- lm(log(AHP)~ log(ALP) + I(log(ALP)^2) + log(PGDI) + East, data = Data )
summary(model5)
par(mfrow = c(1,2),mai = c(0.8,0.8,0.4,0.1), cex = 0.8)
plot(model5,which = 1:2)
ncvTest(model5)
durbinWatsonTest(model5)
vif(model5)
1/vif(model5)
library(lm.beta)
model5.beta <- lm(model5)summary(model5.beta)
