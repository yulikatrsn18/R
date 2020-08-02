setwd("D:\\SMT 6\\Analisis Deret Waktu\\Tugas2")
rm(list = ls(all=TRUE))

#Input Data
water<-read.table("water.txt", header=T)
head(water)
attach(water)

#Mengubah data menjadi data time series
water.ts<-ts(WaterConsumption, start = c(2010,1),freq=12)

#Plot Data
library(ggplot2)
library(forecast)
autoplot(water.ts, color=c("#00868B"), size=1)+
  labs(x="Tahun", 
       title="Water Consumption in Saudi Arabia", 
       subtitle = "Period: Jan 2010 - Jul 2017")

#Plot Dekomposisi Data
water.stl<-stl(water.ts, s.window = "periodic")
library(ggplot2)
library(forecast)
autoplot(water.stl)+
  geom_line(col=c("#00868B"), size=1)+
  labs(x="Tahun", 
       title="Data Decompotition")

#Plot 4 tahun 2010-2014
water.ts.4th<-ts(water.ts, start = c(2010,1), end=c(2013,12), freq=12)
library(ggplot2)
library(forecast)
autoplot(water.ts.4th, color=c("#00868B"), size=1)+
  geom_point(pch=1)+
  labs(x="Tahun", 
       title="Water Consumption in Saudi Arabia", 
       subtitle = "Period: 2010 - 2014")

#Plot ACF dan PACF
library(ggplot2)
library(forecast)
library(cowplot)
plot_grid(
  autoplot(acf(as.vector(water.ts), lag.max = 50),color=c("#00868B"), size=1)+
    labs(title="Plot ACF"),
  autoplot(pacf(as.vector(water.ts), lag.max = 50),color=c("#00868B"), size=1)+
    labs(title="Plot PACF"),
  ncol = 1, nrow = 2)

#Uji Formalitas Kestatoneran
#Uji ADF
library(tseries)
adf.test(water.ts)
#Uji KPSS
kpss.test(water.ts)

#Spesifikasi Model
#Penstationeran Data
#Differencing terhadap tren
diff.tren.water.ts<-diff(water.ts, lag=1)
library(ggplot2)
library(forecast)
autoplot(diff.tren.water.ts, color=c("#00868B"), size=1)+
  labs(x="Tahun", 
       title="Plot Differencing terhadap Tren")

#Plot ACF dan PACF setelah Differencing terhadap Tren
library(ggplot2)
library(forecast)
library(cowplot)
plot_grid(
  autoplot(acf(as.vector(diff.tren.water.ts), lag.max = 50), color=c("#00868B"), size=1)+
    labs(title="Plot ACF setelah Differencing terhadap Tren"),
  autoplot(pacf(as.vector(diff.tren.water.ts), lag.max = 50), color=c("#00868B"), size=1)+
    labs(title="Plot PACF setelah Differencing terhadap Tren"),
  ncol = 1, nrow = 2)

#Uji Formalitas Kestatoneran setelah data didifferencing terhadap Tren
#Uji ADF setelah data didifferencing terhadap Tren
adf.test(diff.tren.water.ts)
#Uji KPSS setelah data didifferencing terhadap Tren
kpss.test(diff.tren.water.ts)

#Differencing lagi terhadap musiman
diff.musim.water.ts<-diff(diff.tren.water.ts, lag=12)
library(ggplot2)
library(forecast)
autoplot(diff.musim.water.ts, color=c("#00868B"), size=1)+
  labs(x="Tahun", 
       title="Plot Differencing terhadap Musiman")

#Plot ACF dan PACF setelah Differencing terhadap Musiman
library(ggplot2)
library(forecast)
library(cowplot)
plot_grid(
  autoplot(acf(as.vector(diff.musim.water.ts), lag.max = 50), color=c("#00868B"), size=1)+
    labs(title="Plot ACF setelah Differencing terhadap Musiman"),
  autoplot(pacf(as.vector(diff.musim.water.ts), lag.max = 50), color=c("#00868B"), size=1)+
    labs(title="Plot PACF setelah Differencing terhadap Musiman"),
  ncol = 1, nrow = 2)

#Plot EACAF Data setelah Differencing terhadap Tren
library(TSA)
eacf(diff.tren.water.ts)

#Pemodelan
library(astsa)
sarima.water.ts.011.011<-arima(water.ts, order=c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
sarima.water.ts.011.012<-arima(water.ts, order=c(0,1,1), seasonal = list(order=c(0,1,2), period=12))
sarima.water.ts.011.013<-arima(water.ts, order=c(0,1,1), seasonal = list(order=c(0,1,3), period=12))
sarima.water.ts.011.014<-arima(water.ts, order=c(0,1,1), seasonal = list(order=c(0,1,4), period=12))
sarima.water.ts.012.011<-arima(water.ts, order=c(0,1,2), seasonal = list(order=c(0,1,1), period=12))
sarima.water.ts.012.012<-arima(water.ts, order=c(0,1,2), seasonal = list(order=c(0,1,2), period=12))
sarima.water.ts.012.013<-arima(water.ts, order=c(0,1,2), seasonal = list(order=c(0,1,3), period=12))
sarima.water.ts.012.014<-arima(water.ts, order=c(0,1,2), seasonal = list(order=c(0,1,4), period=12))

sarima.water.ts.111.011<-arima(water.ts, order=c(1,1,1), seasonal = list(order=c(0,1,1), period=12))
sarima.water.ts.111.012<-arima(water.ts, order=c(1,1,1), seasonal = list(order=c(0,1,2), period=12))
sarima.water.ts.111.013<-arima(water.ts, order=c(1,1,1), seasonal = list(order=c(0,1,3), period=12))
sarima.water.ts.111.014<-arima(water.ts, order=c(1,1,1), seasonal = list(order=c(0,1,4), period=12))
sarima.water.ts.211.011<-arima(water.ts, order=c(2,1,1), seasonal = list(order=c(0,1,1), period=12))
sarima.water.ts.211.012<-arima(water.ts, order=c(2,1,1), seasonal = list(order=c(0,1,2), period=12))
sarima.water.ts.211.013<-arima(water.ts, order=c(2,1,1), seasonal = list(order=c(0,1,3), period=12))
sarima.water.ts.211.014<-arima(water.ts, order=c(2,1,1), seasonal = list(order=c(0,1,4), period=12))


#Seleksi Model
sarima.water.ts.011.011$aic
sarima.water.ts.011.012$aic
sarima.water.ts.011.013$aic
sarima.water.ts.011.014$aic
sarima.water.ts.012.011$aic
sarima.water.ts.012.012$aic
sarima.water.ts.012.013$aic
sarima.water.ts.012.014$aic

sarima.water.ts.111.011$aic
sarima.water.ts.111.012$aic
sarima.water.ts.111.013$aic
sarima.water.ts.111.014$aic
sarima.water.ts.211.011$aic
sarima.water.ts.211.012$aic
sarima.water.ts.211.013$aic
sarima.water.ts.211.014$aic

#Diagnostik Model sarima.water.ts.111.011
#Uji Kenormalan Residual
#Plot Residual
res.sarima.water.ts.111.011<-(sarima.water.ts.111.011$residuals)
plot.ts(res.sarima.water.ts.111.011, 
        col=c("#00868B"), 
        ylab="Residual", main="Plot Residual")
abline(h=0)
#Plot Residual Terstandarkan
res.std.sarima.water.ts.111.011<-rstandard(sarima.water.ts.011.011)
library(TSA)
plot.ts(res.std.sarima.water.ts.111.011, 
        col=c("#00868B"), 
        ylab="Residual Terstandarkan", main="Plot Residual Terstandarkan")
abline(h=0)
#Plot QQ
qqnorm(res.sarima.water.ts.111.011, col=c("#00868B"))
qqline(res.sarima.water.ts.111.011, col="red")
#Uji Formal Kenormalan Residual
shapiro.test(res.sarima.water.ts.111.011)


#Uji AUtokorelasi Residual
#Plot ACF Residual
autoplot(acf(res.sarima.water.ts.111.011))
#Uji Ljung-Box
library(astsa)
sarima.ljung.box<-sarima(water.ts,1,1,1,0,1,1,12,no.constant = TRUE)


#Peramalan 12 Bulan Kedepan
forecast.water.ts<-predict(sarima.water.ts.111.011, 12)
forecast.water.ts

#Plot Peramalan
L<-forecast.water.ts$pred-2*forecast.water.ts$se
U<-forecast.water.ts$pred+2*forecast.water.ts$se

min.y<-min(water.ts, L) #batas minimum sb y
max.y<-max(water.ts, U) #batas maximum sb y

ts.plot(water.ts, forecast.water.ts$pred,
        col=c("#00868B","red"), 
        ylim=c(min.y, max.y),
        xlab="Tahun", ylab="Water Consumption",
        main="Prediction of Water Consumption in Saudi Arabia
        Period: Aug 2017 - Jul 2018")
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")

