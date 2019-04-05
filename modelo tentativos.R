
data <- read.table(file = file.choose(),header = T,sep = ";")
head(data)
data <- data[,c(1:53)]
head(data)
data <- data[,-c(1,2,3,5,6,7,8,11,12,13,14,15,16,17,18,21,51,52,53)]
head(data)

data$Edad <- as.factor(data$Edad)
data$Estrato<- as.factor(data$Estrato)
data$sexo<- as.factor(data$sexo)
data$Peri..Pantorrilla<-as.numeric(data$Peri..Pantorrilla)
moda <- lm( formula = Peso~ ., data = data)
summary(moda)
require(olsrr)
require(gamlss)
data1 <- na.omit(data)
n<-nrow(data1)
modag <- gamlss(formula = Peso~.,data = data1)
summary(modag)
stepAIC(modag)

#modelo final
modb <- lm(formula = Peso ~ Estrato + Edad + sexo +  
             Pasist + talla.sentado..cms. + Pli.Biceps + Pli.Subesca +  
             Pli.Cresta.iliaca + pli..Supraespinal + Pli..Pantorrilla +  
             Peri..Brazo.relax + Peri..Brazo.flex + Peri..Cintura +  
             Peri..Cadera + Peri..Pantorrilla + Diam..Femoral +  
             wells1 + Dina.der1 + Dina.Izq1 + Dina.der2 + Dina.Izq2 +  
             X20.mtetros1,data = data)
summary(modb)
head(data)
a<-predict(object = modb,newdata = data1[,-6])
cor(a,data1$Peso)

#graficas del peso separadas por generos y en cada uno de los niveles de edad 

boxplot(Peso~Edad,data = subset(data1,sexo =="femenino"),
        col = topo.colors(n = 16,alpha = 0.5),main="Peso promedio por edades para las mujeres",
        xlab="Edades",ylab="Peso en kg")
boxplot(Peso~Edad,data = subset(data1,sexo =="masculino"),
        col = topo.colors(n = 16,alpha = 0.5),main="Peso promedio por edades para los hombres",
        xlab="Edades",ylab="Peso en kg")


boxplot(Peso~Estrato,data = data1)
boxplot(Peso~Edad,data = subset(data1,Estrato=="2"))
boxplot(Peso~Edad,data = subset(data1,Estrato=="3"))
boxplot(Peso~Edad,data = subset(data1,Estrato=="4"))
boxplot(Peso~Edad,data = subset(data1,Estrato=="5"))
boxplot(Peso~Edad,data = subset(data1,Estrato=="6"))


#modelo 1
modb <- lm(formula = Peso ~ Estrato + Edad + sexo +  Pasist + talla.sentado..cms. + Pli.Biceps + Pli.Subesca +  
             Pli.Cresta.iliaca + pli..Supraespinal + Pli..Pantorrilla +  Peri..Brazo.relax + Peri..Brazo.flex + Peri..Cintura +  
             Peri..Cadera + Peri..Pantorrilla + Diam..Femoral +  
             wells1 + Dina.der1 + Dina.Izq1 + Dina.der2 + Dina.Izq2 +  X20.mtetros1,data = data)
summary(modb)

#modelo 2
require(gamlss)
data1 <- na.omit(data)
modb2 <- gamlss(formula = Peso ~ Estrato + Edad + sexo +   Pasist + Pli.Biceps + Pli.Subesca +  
                  Pli.Cresta.iliaca + pli..Supraespinal + Pli..Pantorrilla +  
                  Peri..Brazo.relax +Peri..Cintura +  Peri..Cadera + Peri..Pantorrilla + Diam..Femoral +  
                  wells1 + Dina.der1 +X20.mtetros1,data = data1)
summary(modb2)

#modelo 3 quite estrato  peri pantorrilla no significativa 
modb3 <- lm(formula = Peso ~Edad + sexo +  Pasist + Pli.Biceps + Pli.Subesca +  
              Pli.Cresta.iliaca + pli..Supraespinal + Pli..Pantorrilla +  
              Peri..Brazo.relax +Peri..Cintura + Peri..Cadera + Diam..Femoral +  
              wells1 + Dina.der1 +X20.mtetros1,data = data)
summary(modb3)

#modelo 4 quite wells1 x20 metros  no significativa 
modb4 <- lm(formula = Peso ~Edad + sexo +  Pasist + Pli.Biceps + Pli.Subesca +  
              Pli.Cresta.iliaca + pli..Supraespinal + Pli..Pantorrilla +  Peri..Brazo.relax +Peri..Cintura +  
              Peri..Cadera + Diam..Femoral + Dina.der1,data = data)
summary(modb4)

#modelo 5 modelo gamlls porque no cumple normalidad buscar familia 
require(gamlss)
peso1<- fitDist(Peso,type= "realplus", data=data1, k=log(n))
peso1$fits
modb5 <- gamlss(formula = Peso ~Edad + sexo +  Pasist + Pli.Biceps + Pli.Subesca +  
                  Pli.Cresta.iliaca + pli..Supraespinal + Pli..Pantorrilla +  Peri..Brazo.relax +Peri..Cintura +  
                  Peri..Cadera + Diam..Femoral + Dina.der1,family = BCPEo  ,data = data1)
summary(modb5)

plot(datah$Peso)
plot(datam$Peso)
shapiro.test(data$Peso)
plot(density(data1$Peso))

datam = subset(data1,sexo =="femenino")[,c(1,2,4:34)]
datah = subset(data1,sexo =="masculino")[,c(1,2,4:34)]

peso1<- fitDist(Peso,type= "realplus", data=data1, k=log(n))
peso1$fits
