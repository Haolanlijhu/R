install.packages("vars")
library(vars)
install.packages("urca")
library(urca)
library(lmtest)
library(tseries)

ts <- ts(Data = df)
attach(ts)

#Pearson’s product-moment correlation
cor.test(LCHFP,LDHSP, method="spearman")
cor.test(LCHFP,LDHSP, method="kendall")
cor.test(RUHFP,RCHFP, method="spearman")
cor.test(RUHFP,RCHFP, method="kendall")
cor.test(RUHFP,RDHSP, method="spearman")
cor.test(RUHFP,RDHSP, method="kendall")

#ADF test
adf.test(LCHFP)
adf.test(LDHSP)
adf.test(RUHFP)
adf.test(RCHFP)
adf.test(RDHSP)

DCHFP = diff(LCHFP)
DDHSP = diff(LDHSP)
adf.test(DCHFP)
adf.test(DDHSP)

#Selection of lag order
data <- data.frame(DCHFP, DDHSP)
tsdata <- ts(data)
lag_order <- VARselect(tsdata, lag.max = 20)
data2  <- data.frame(RUHFP, RCHFP)
tsdata2 <- ts(data2)
lag_order2 <- VARselect(tsdata2, lag.max = 20)
data3  <- data.frame(RUHFP, RDHSP)
tsdata3 <- ts(data3)
lag_order3 <- VARselect(tsdata3, lag.max = 20)

#Johansen cointegration test
johansen_test <- ca.jo(tsdata, type = "eigen", ecdet = "const", K = lag_order$selection, spec = "longrun")
johansen_test2 <- ca.jo(tsdata, type = "trace", ecdet = "const", K = lag_order$selection, spec = "longrun")
summary(johansen_test)
summary(johansen_test2)

#Granger causality test
result <- causality(VAR(tsdata, p = lag_order$selection), cause = "DCHFP", effect = "DDHSP")
result2 <- causality(VAR(tsdata, p = lag_order$selection), cause = "DDHSP", effect = "DCHFP")
result3 <- causality(VAR(tsdata2, p = lag_order2$selection), cause = "RUHFP", effect = "RCHFP")
result4 <- causality(VAR(tsdata2, p = lag_order2$selection), cause = "RCHFP", effect = "RUHFP")
result5 <- causality(VAR(tsdata3, p = lag_order3$selection), cause = "RUHFP", effect = "RDHSP")
result6 <- causality(VAR(tsdata3, p = lag_order3$selection), cause = "RDHSP", effect = "RUHFP")

#SVAR Impluse Response
par(mfrow=c(4,4))
for(i in 1:4)
{
p=SVAR(tsdata2, estmethod = 'scoring',  Amat =NULL, Bmat =bmat,start = NULL, max.iter = 100)
svec.irf <- irf(p,response=name[i], n.ahead = 48, boot = TRUE)

for(j in 1:2)
{
p=as.vector(svec.irf$irf[[j]])
q=as.vector(svec.irf$Upper[[j]])
k=as.vector(svec.irf$Lower[[j]])
low=min(k)-0.1
high=max(q)+0.1
plot(p,type='l',main=paste(paste(svec.irf$response,'from'),svec.irf$impulse[j]),ylim=c(low,high))
lines(q,type='l',lty=2,col='red')
lines(k,type='l',lty=2,col='red')
abline(h=0,col='red')
}
}

par(mfrow=c(4,4))
for(i in 1:4)
{
p=SVAR(tsdata3, estmethod = 'scoring',  Amat =NULL, Bmat =bmat,start = NULL, max.iter = 100)
svec.irf <- irf(p,response=name[i], n.ahead = 48, boot = TRUE)

for(j in 1:2)
{
p=as.vector(svec.irf$irf[[j]])
q=as.vector(svec.irf$Upper[[j]])
k=as.vector(svec.irf$Lower[[j]])
low=min(k)-0.1
high=max(q)+0.1
plot(p,type='l',main=paste(paste(svec.irf$response,'from'),svec.irf$impulse[j]),ylim=c(low,high))
lines(q,type='l',lty=2,col='red')
lines(k,type='l',lty=2,col='red')
abline(h=0,col='red')
}
}


