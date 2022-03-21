
##### SETTINGS #####
#Seminar s0569723
#Thi Thanh Tu Phan

install.packages('glmnet')
library(glmnet)
library(datasets)
install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
graphics.off()
rm(list = ls())

#Datensatz einlesen:

data = read.table("~/Documents/PM10 - Kopie.txt")
head(data)

##### Datenanlyse #####

#Namen für einzelne Spalte eingeben
colnames (data) = c("PM10","AnzahlAutos","Temperatur","Windgeschwindigkeit","Temperaturdifferenz", "Windrichtung", "Tagesstunde","Tagesnummer")
PM10               = (data[1])
AnzahlAutos        = (data[2])  
Temperatur         = (data[3])
Windgeschwindigkeit= (data[4])
Temperaturdifferenz= (data[5])
Windrichtung       = (data[6])
Tagesstunde        = (data[7])
Tagesnummer        = (data[8])

max(Tagesnummer)

# Missings
any(is.na(data))

# Entfernung der Tagesnummer Spalte 

dataset = data%>%select(-Tagesnummer)

# visualization

pdf("my_plot.pdf")

gg1 = ggplot(dataset, aes(x=PM10))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") + xlim(0,6)+
        stat_function(fun = dnorm, args = list(mean=mean(dataset$PM10), sd=sd(dataset$PM10)))


gg2 = ggplot(data, aes(x=AnzahlAutos))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") + xlim(3,9)+
        stat_function(fun = dnorm, args = list(mean=mean(dataset$AnzahlAutos), sd=sd(dataset$AnzahlAutos)))

gg3 = ggplot(dataset, aes(x=Temperatur))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") +
        stat_function(fun = dnorm, args = list(mean=mean(dataset$Temperatur), sd=sd(dataset$Temperatur)))


gg4 = ggplot(dataset, aes(x=Windgeschwindigkeit))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") + xlim(0,11)+
        stat_function(fun = dnorm, args = list(mean=mean(dataset$Windgeschwindigkeit), sd=sd(dataset$Windgeschwindigkeit)))



gg5 = ggplot(dataset, aes(x=Temperaturdifferenz))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") +
        stat_function(fun = dnorm, args = list(mean=mean(dataset$Temperaturdifferenz), sd=sd(dataset$Temperaturdifferenz)))

gg6 = ggplot(dataset, aes(x=Windrichtung))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") + xlim(0,360) +
        stat_function(fun = dnorm, args = list(mean=mean(dataset$Windrichtung), sd=sd(dataset$Windrichtung)))



gg7 = ggplot(dataset, aes(x=Tagesstunde))+ 
        geom_histogram(aes(y=..density..), fill="steelblue", color="grey") + xlim(0,24) +
        stat_function(fun = dnorm, args = list(mean=mean(dataset$Tagesstunde), sd=sd(dataset$Tagesstunde)))



grid.arrange(gg1, gg2, gg3, gg4,gg5, gg6, gg7, nrow = 4, ncol =2)
dev.off()

pdf("my_scatterplot.pdf")

 plot1 <- ggscatter(dataset, x="AnzahlAutos", y="PM10", add="reg.line", conf.int = TRUE, add.params = list(color="blue", fill="lightgray")) + stat_cor(method = "pearson")

 plot2 <- ggscatter(dataset, x="Temperatur", y="PM10", add="reg.line", conf.int = TRUE, add.params = list(color="blue", fill="lightgray")) + stat_cor(method = "pearson")

 plot3 <- ggscatter(dataset, x="Windgeschwindigkeit", y="PM10", add="reg.line", conf.int = TRUE, add.params = list(color="blue", fill="lightgray")) + stat_cor(method ="pearson")

 plot4 <- ggscatter(dataset, x="Temperaturdifferenz", y="PM10", add="reg.line", conf.int = TRUE, add.params = list(color="blue", fill="lightgray")) + stat_cor(method = "pearson")

 plot5 <- ggscatter(dataset, x="Windrichtung", y="PM10", add="reg.line", conf.int = TRUE, add.params = list(color="blue", fill="lightgray")) + stat_cor(method = "pearson")

 plot6 <-ggscatter(dataset, x="Tagesstunde", y="PM10", add="reg.line", conf.int = TRUE, add.params = list(color="blue", fill="lightgray")) + stat_cor(method = "pearson")


 grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6, nrow = 3, ncol =2)
dev.off()



####### Multiple Lineare Regression #####

# Modell für multiple lineare Regression
mlm      = lm(PM10 ~ AnzahlAutos+ Temperatur + Windgeschwindigkeit + Temperaturdifferenz + Windrichtung + Tagesstunde,  data = dataset)
summary(mlm) 



##### Lasso Regression #####
# Spliting  dataset into two parts based on outcome: 70% and 30%
set.seed(89)
train   = sample(1:nrow(dataset), nrow(dataset)*0.70)
test    = (-train)
trainset = dataset[train,]
testset  = dataset[test,]
x        = model.matrix(PM10~.,dataset)[,-1]
y        = dataset$PM10
y.test   =  y[test]

# fit der Lasso Regression
grid      = 10^seq(2, -2, by = -.1)
lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out    = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam   = cv.out$lambda.min
bestlam

#verwende best lambda für Variablenselektion

out        = glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type="coefficients",s=bestlam)[1:7,]
lasso.coef 
lasso.coef[lasso.coef!=0] 




##### #Vorhersage der PM10-Konzentration#####


PM10.lasso =  function(x,y) {
       PM  =  0.29360700*x -0.08814452 *y + 1.49488319
               return(PM)
               }
    
PM10.lasso (7.74414, 4.2)
PM10.lasso (8.03398, 4.8)
PM10.lasso (4.70048, 4.3)
PM10.lasso (7.52510, 3.0)
PM10.lasso (7.76260, 5.6)




