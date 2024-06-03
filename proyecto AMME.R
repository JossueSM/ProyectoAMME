library(readr)
weather <- read_csv("https://raw.githubusercontent.com/JossueSM/ProyectoAMME/main/weather_prediction_dataset.csv")


#Dusseldorf

weather_dusseldorf=weather[,41:51]#Variables correspondientes a la Ciudad de Dusseldorf - Alemania
set.seed(123) # Establecer una semilla para reproducibilidad

train_indices = sample(seq_len(nrow(weather_dusseldorf)), size = 0.7 * nrow(weather_dusseldorf))# Crear un vector de índices aleatorios que representan el 70% del dataset

weather_train = weather_dusseldorf[train_indices, ]# Crear el conjunto de entrenamiento con el 70% de los datos

weather_test = weather_dusseldorf[-train_indices, ]# Crear el conjunto de prueba con el 30% restante de los datos

temp_max = weather_train[,11, drop = TRUE]#para hacerlo vector / Variable a predecir
x1 = weather_train[,1, drop = TRUE]#cloud_cover
x2 = weather_train[,2, drop = TRUE]#wind_speed 
x3 = weather_train[,3, drop = TRUE]#wind_gust
x4 = weather_train[,4, drop = TRUE]#humidity
x5 = weather_train[,5, drop = TRUE]#pressure
x6= weather_train[,6, drop = TRUE]#global_radiation
x7 = weather_train[,7, drop = TRUE]#precipitation
x8 = weather_train[,8, drop = TRUE]#sunshine



reg=lm(temp_max~x1+x2+x3+x4+x5+x6+x7+x8)#verificar cuales variables son significantes
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  57.21465   12.20988   4.686 2.93e-06 ***
#x1           -0.07825    0.08953  -0.874   0.3822    
#x2            0.15300    0.11041   1.386   0.1659    
#x3           -0.08676    0.04738  -1.831   0.0672 .  
#x4           -2.81494    1.39846  -2.013   0.0442 *  
#x5          -46.52177   11.75888  -3.956 7.82e-05 ***
#x6            9.99315    0.26661  37.483  < 2e-16 ***
#x7            1.28688    0.23617   5.449 5.55e-08 ***
#x8           -0.82235    0.07283 -11.291  < 2e-16 ***

#en base al pvalue teniendo en cuenta un alpha de 0.01 vemos que #pressure #global_radiation #precipitation #sunshine
#tienen más significancia en el modelo por lo cual vamos a tomar esas variables para nuestra regresión
c=cbind(temp_max,x1,x2,x3,x4,x5,x6,x7,x8)
cor=cor(c)
library("corrplot")
corrplot(cor)

curation=function(data,reg,tamanio,variables_ind){
  tvalor=qt(0.01/2, length(tamanio)-variables_ind-1-1, lower.tail = FALSE)
  filasaEliminar = which((rstandard(reg)>2|rstandard(reg)< (2*-1)) | rstudent(reg) > tvalor|rstudent(reg)<(tvalor*-1))
  return(data[-filasaEliminar,])
}

c1=cbind(temp_max,x5,x6,x7,x8)
cor1=cor(c1)
corrplot(cor1)

reg1=lm(temp_max~x5+x6+x7+x8)
res1=summary(reg1)
r2_1=res1$adj.r.squared #R2 ajustado = 0.6483266

plot(reg1)



#Primera limpieza
d1=curation(weather_train,reg1,temp_max,4)
reg2=lm(d1$DUSSELDORF_temp_max ~ d1$DUSSELDORF_pressure + d1$DUSSELDORF_global_radiation+d1$DUSSELDORF_precipitation+d1$DUSSELDORF_sunshine)
res2=summary(reg2)
r2_2=res2$adj.r.squared
ie_1=dim(weather_train)[1]-dim(d1)[1]#numero de instancias eliminadas con datos outliers

#Segunda limpieza
d2=curation(d1,reg2,d1$DUSSELDORF_temp_max,4)
reg3=lm(d2$DUSSELDORF_temp_max ~ d2$DUSSELDORF_pressure + d2$DUSSELDORF_global_radiation+d2$DUSSELDORF_precipitation+d2$DUSSELDORF_sunshine)
res3=summary(reg3)
r2_3=res3$adj.r.squared
ie_2=dim(d1)[1]-dim(d2)[1]#numero de instancias eliminadas con datos outliers

#tercera limpieza
d3=curation(d2,reg3,d2$DUSSELDORF_temp_max,4)
reg4=lm(d3$DUSSELDORF_temp_max ~ d3$DUSSELDORF_pressure + d3$DUSSELDORF_global_radiation+d3$DUSSELDORF_precipitation+d3$DUSSELDORF_sunshine)
res4=summary(reg4)
r2_4=res4$adj.r.squared
ie_3=dim(d2)[1]-dim(d3)[1]#numero de instancias eliminadas con datos outliers

#cuarta limpieza
d4=curation(d3,reg4,d3$DUSSELDORF_temp_max,4)
reg5=lm(d4$DUSSELDORF_temp_max ~ d4$DUSSELDORF_pressure + d4$DUSSELDORF_global_radiation+d4$DUSSELDORF_precipitation+d4$DUSSELDORF_sunshine)
res5=summary(reg5)
r2_5=res5$adj.r.squared
ie_4=dim(d3)[1]-dim(d4)[1]#numero de instancias eliminadas con datos outliers

#Quinta limpieza
d5=curation(d4,reg5,d4$DUSSELDORF_temp_max,4)
reg6=lm(d5$DUSSELDORF_temp_max ~ d5$DUSSELDORF_pressure + d5$DUSSELDORF_global_radiation+d5$DUSSELDORF_precipitation+d5$DUSSELDORF_sunshine)
res6=summary(reg6)
r2_6=res6$adj.r.squared
ie_5=dim(d4)[1]-dim(d5)[1]#numero de instancias eliminadas con datos outliers

#Sexta limpieza
d6=curation(d5,reg6,d5$DUSSELDORF_temp_max,4)
reg7=lm(d6$DUSSELDORF_temp_max ~ d6$DUSSELDORF_pressure + d6$DUSSELDORF_global_radiation+d6$DUSSELDORF_precipitation+d6$DUSSELDORF_sunshine)
res7=summary(reg7)
r2_7=res7$adj.r.squared
ie_6=dim(d5)[1]-dim(d6)[1]#numero de instancias eliminadas con datos outliers

#Septima Limpieza
d7 = curation(d6, reg7,d6$DUSSELDORF_temp_max, 4)
reg8 = lm(DUSSELDORF_temp_max ~ DUSSELDORF_pressure + DUSSELDORF_global_radiation + DUSSELDORF_precipitation + DUSSELDORF_sunshine, data = d7)
res8 = summary(reg8)
r2_8 = res8$adj.r.squared
ie_7 = dim(d6)[1] - dim(d7)[1]  # número de instancias eliminadas con datos outliers

#Datos outliers con rstandar
#-------------------------------------------------------------------------------
rs8=rstandard(reg8)
posc=1
for (i in rs8){
  if(i>=2 | i<=-2 ){
    print(paste("Dato Outlier N° Instancia :",posc," Valor: ",i))
  }
  posc=posc+1
}
#-------------------------------------------------------------------------------
#Datos outliers con Rstudent
rst8=rstudent(reg8)
tresht=qt(0.01/2,length(d7$DUSSELDORF_temp_max)-1-4-1)
posc=1
for (i in rst8){
  if(i>=tresht*-1 | i<=tresht ){
    print(paste("Dato Outlier N° Instancia :",posc," Valor: ",i))
  }
  posc=posc+1
}
#-------------------------------------------------------------------------------
#Datos Influyentes con Influence
tresh=3*(4+1)/length(d7$DUSSELDORF_temp_max)
hi=influence(reg8)$hat
posc=1
for (i in hi){
  if(i>tresh){
    print(paste("Dato Influyente N° Instancia :",posc," Valor: ",i))
  }
  posc=posc+1
}


#Datos Influyentes con la distancia de cooks
cook=cooks.distance(reg8)
posc1=1
for (i in hi){
  if(i>1){
    print(paste("Dato Influyente N° Instancia :",posc," Valor: ",i))
  }
  posc1=posc1+1
}


c8=cbind(d7$DUSSELDORF_temp_max,d7$DUSSELDORF_pressure,d7$DUSSELDORF_global_radiation,d7$DUSSELDORF_precipitation,d7$DUSSELDORF_sunshine)
cor8 = cor(c8)
corrplot(cor8)# Grafico de las correlaciones entre variables
plot(reg8)#Si hay homocedasticidad y Normalidad

#Coeficientes de regresión para la regresión n8
b0=res8$coefficients[1]
b1=res8$coefficients[2]
b2=res8$coefficients[3]
b3=res8$coefficients[4]
b4=res8$coefficients[5]



#y_est=74.88956 - 66.65907x1 + 9.98650x2 + 0.90186x3 - 0.76881x4
y_est=b0+b1*(weather_test$DUSSELDORF_pressure)+b2*(weather_test$DUSSELDORF_global_radiation)+b3*(weather_test$DUSSELDORF_precipitation)+b4*(weather_test$DUSSELDORF_sunshine) #ecuacion de regresión estimada



