#pracas: 1005(D.J.Gaspar), 1006 (Liberdade),1023(República),27 (Calçadão Grajau),1039(CCSP),Raul Seixas(1044)
# intervalos mapeados de hora em hora
#período da amostra: variável para cada praça
#qtde de usuários conectados por hora

library(tseries)
library(forecast)
library(graphics)
library(TSPred)

#Importando dados
df=read.csv('~/R/eventos/praca27_17p.csv',header=F)
data= c(df$V1,df$V2,df$V3,df$V4,df$V5,df$V6,df$V7,df$V8,df$V9,df$V10,df$V11,df$V12,df$V13,df$V14,df$V15,df$V16,df$V17,df$V18,df$V19,df$V20,df$V21,df$V22,df$V23,df$V24,df$V25,df$V26,df$V27)

pracah<-ts(data,frequency=24,start=c(1,1))
ts.plot(pracah,type="l",main="Praça - Período do Evento")
#pracah
max_y=max(pracah)
###############################################
#Treinamento e Teste do BD
#x é a posição do dia na tabela de entrada, não necessariamente o dia
x=8
pracah.train = window(pracah,start=c(x,1),end=c(x+9,24))
pracah.test = window(pracah,start=c(x+10,1),end=c(x+10,24))
plot(pracah.test)

###############################################
#Desenvolvimento e análise do Modelo ARIMA/SARIMA
arima1=auto.arima(pracah.train,trace=TRUE,test="adf")
#arima1=arima(pracah.train,c(2,0,3),seasonal = list(order = c(1,0,0),period=24))
#summary(arima1)
##############################

#Predição
arima1.forecast=forecast(arima1,h=24)
arima1.forecast
plot(arima1.forecast,xlab="Dias",ylab="Usuários Conectados",axes=FALSE)
box()
axis(1, at=1:19,lab=c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
axis(2, las=1, at=20*0:max_y)
plotarimapred(pracah.test,arima1,xlim=c(x+10,x+11),ylab="Usuários Previstos Conectados", xlab="Horas",range.percent = 0.1)
#accuracy(arima1.forecast,pracah.test)
#summary(arima1.forecast)

##################################
#Alisamento exponencial
pracah_final= window(pracah,start=c(x,1),end=c(x+10,24))
holt<-HoltWinters(pracah.train,seasonal="additive",beta=FALSE)
p<-forecast(holt,h=24)

plot(pracah_final,ylab="Usuários conectados",type="o", xlab="Dias",axes=FALSE)
box()
#primeira posição da legenda do eixo x
axis(1, at=1:19,lab=c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
#ultima posição da legenda do eixo x
#axis(1, at=1:28,lab=c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,01))
axis(2, las=1, at=20*0:max_y)
lines(fitted(arima1.forecast), col="red", lty=2)
lines(fitted(p), col="green", lty=2)
lines(arima1.forecast$mean, type="o", col="red")
lines(p$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("Dados","ARIMA","(SE)Holt-Winters"))
#accuracy(p)

####################################################
#Métricas
accuracy(arima1.forecast)
accuracy(p)
