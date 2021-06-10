source('CheckupRes.R')
library(tseries)
library(forecast)
library(TSA)
library(corrplot)

setwd('/Users/elvinagovendasamy/DataChallenge/')
X_pirineos = read.csv('X_pirineos_extraction_2020-12-02.csv', sep=',')
#y_pirineos = read.csv('y_pirineos_extraction_2020-12-02.csv', sep=',')

#X_pirineos_sortie = X_pirineos[,-c(1,42,40,38,36,34,32,28,27)]
X_pirineos = X_pirineos[,-c(1,27,28)] # pour ne garder que des variables quantitatives
X_pirineos$LNOTIFICATION_Sortie = log(X_pirineos$NOTIFICATION_Sortie + 1)
attach(X_pirineos)

Data = X_pirineos[,-c(29,30)]
n = dim(X_pirineos)[1]

##Régression linéaire modèle complet
Reg = lm(LNOTIFICATION_Sortie ~ ., data = Data)
summary(Reg) #R² adj = 0.4298
step(Reg, direction = "both") #donne le modèle qui minimise l'AIC

Reg2 = lm(LNOTIFICATION_Sortie ~ spread_france_espagne + settlement_price_fr_s3 + 
                  average_settlement_price_m1 + average_settlement_price_y1 + 
                  average_settlement_price_y2 + average_settlement_price_s2 + 
                  average_settlement_price_s3 + NOTIFICATION_Entree_J.1 + NOTIFICATION_Sortie_J.1 + 
                  NOTIFICATION_Entree_J.5 + NOTIFICATION_Sortie_J.5, data = Data)
summary(Reg2) #R² adj = 0.4391
AIC(Reg2) #AIC : 2207.124

Reg3 = lm(LNOTIFICATION_Sortie ~ spread_france_espagne + 
                  average_settlement_price_m1 + average_settlement_price_y1 + 
                  average_settlement_price_y2 + average_settlement_price_s2 + 
                  NOTIFICATION_Entree_J.1 + NOTIFICATION_Sortie_J.1 + 
                  NOTIFICATION_Sortie_J.5, data = Data)
summary(Reg3) #R² adj = 0.4383
AIC(Reg3) #AIC : 2205.659

Reg4 = lm(LNOTIFICATION_Sortie ~ spread_france_espagne + 
                  average_settlement_price_m1 + average_settlement_price_y1 + 
                  average_settlement_price_y2 + average_settlement_price_s2 + NOTIFICATION_Sortie_J.1, data = Data)

summary(Reg4) #R² adj = 0.4356
AIC(Reg4) #AIC :  2208.998

Reg5 = lm(LNOTIFICATION_Sortie ~ spread_france_espagne + 
                  average_settlement_price_y1 + 
                  average_settlement_price_y2 + average_settlement_price_s2 + NOTIFICATION_Sortie_J.1, data = Data)
summary(Reg5) #R² adj = 0.4327
AIC(Reg5) #AIC : 2213.569


##Toutes les variables de la régression linéaire 5 sont significatives, 
##peu corrélées donc on garde ce modèle pour la suite


plot(LNOTIFICATION_Sortie, type='l')
lines(Reg5$fitted.values, col='red')

plot(Reg5$residuals, type='l')
acf(Reg5$residuals) #périodicité de 7
pacf(Reg5$residuals) #idem

ResReg = ts(Reg5$residuals, frequency = 7)

##On part sur un modèle SARIMAX (régression linéaire sur la série + bruit SARIMA)
##On passe au log pour réduire les échelles (hum moyen avec les pb de 0)

bruit_sortie_diff = diff(ResReg) #d=1, D=0
bruit_sortie_diff7 = diff(ResReg,7) #d=0, D=1
bruit_sortie_diff7d = diff(diff(ResReg,7)) #d=1, D=1

plot(bruit_sortie_diff, type = 'l')
acf(as.numeric(bruit_sortie_diff), lag.max = 50)  #MA(1) ?
pacf(as.numeric(bruit_sortie_diff), lag.max = 50) #partie AR importante
adf.test(bruit_sortie_diff)
kpss.test(bruit_sortie_diff)
#Cette série semble stationnaire d'après les tests 

plot(bruit_sortie_diff7, type = 'l')
acf(as.numeric(bruit_sortie_diff7), lag.max = 50)  # gros pic à 7, --> P=1 ?
pacf(as.numeric(bruit_sortie_diff7), lag.max = 50) #pics à 7, 14, 21, etc ... --> ecore périodique
adf.test(bruit_sortie_diff7)
kpss.test(bruit_sortie_diff7)
#Cette série semble également stationnaire d'après les tests mais il semble rester de la périodicité
#pas ouf

plot(bruit_sortie_diff7d, type = 'l')
acf(as.numeric(bruit_sortie_diff7d), lag.max = 50)  # P=1 ?
pacf(as.numeric(bruit_sortie_diff7d), lag.max = 50) #encore de la périodicité ? + partie AR importante
adf.test(bruit_sortie_diff7d)
kpss.test(bruit_sortie_diff7d)
#Cette série semble également stationnaire mais pacf pas folle

## Un mosèle avec d=1, D=0 semble plutôt bon 

auto.arima(ResReg, allowdrift = T, allowmean = T, ic='bic')
#SARIMA (0,0,0)x(0,0,1)[7] with zero mean 
Mod1 = Arima(LNOTIFICATION_Sortie, order = c(0,0,0), seasonal = list(order=c(0,0,1), period=7), xreg = cbind(spread_france_espagne, average_settlement_price_y1, average_settlement_price_y2, average_settlement_price_s2, NOTIFICATION_Sortie_J.1))

auto.arima(ResReg, d=1, allowdrift = T, allowmean = T, ic='bic')
#SARIMA (2,1,0)x(0,0,1)[7] mais résidus pas blancs

Mod2 = Arima(LNOTIFICATION_Sortie, order = c(2,1,1), seasonal = list(order=c(0,0,1), period=7), xreg = cbind(spread_france_espagne, average_settlement_price_y1, average_settlement_price_y2, average_settlement_price_s2, NOTIFICATION_Sortie_J.1))

##Première impression visuelle de la qualité des modèles

#Modèle 1
plot(LNOTIFICATION_Sortie, type='l')
lines(Mod1$fitted, col = 'red')
checkupRes(Mod1$residuals)
shapiro.test(Mod1$residuals) #on rejette le caractère gaussien des résidus
Box.test(Mod1$residuals, type = "Ljung-Box", lag = 7) # bruit blanc

#Modèle 2
dev.off()
plot(LNOTIFICATION_Sortie, type='l')
lines(Mod2$fitted, col = 'red') # à l'air de prévoir les pics beaucoup mieux
checkupRes(Mod2$residuals)
shapiro.test(Mod2$residuals) #on rejette le caractère gaussien des résidus
Box.test(Mod2$residuals, type = "Ljung-Box", lag = 7) #bruit blanc


### Comparaison sur un critère prédictif
Tps = time(LNOTIFICATION_Sortie)
NTps = Tps[(n-6):n]

#série tronquée
LNST = LNOTIFICATION_Sortie[1:(n-7)]

#Variables du xreg tronquée
sfeT = spread_france_espagne[1:(n-7)]
aspy1T = average_settlement_price_y1[1:(n-7)]
aspy2T = average_settlement_price_y2[1:(n-7)]
asps2T = average_settlement_price_s2[1:(n-7)]
NSJ_1T = NOTIFICATION_Sortie_J.1[1:(n-7)]

Nsfe = spread_france_espagne[(n-6):n]
Naspy1 = average_settlement_price_y1[(n-6):n]
Naspy2 = average_settlement_price_y2[(n-6):n]
Nasps2 = average_settlement_price_s2[(n-6):n]
NNSJ_1 = NOTIFICATION_Sortie_J.1[(n-6):n]

Mod1T = Arima(LNST, order = c(0,0,0), seasonal = list(order=c(0,0,1), period=7), xreg = cbind(sfeT, aspy1T, aspy2T, asps2T, NSJ_1T))
Mod2T = Arima(LNST, order = c(2,1,1), seasonal = list(order=c(0,0,1), period=7), xreg = cbind(sfeT, aspy1T, aspy2T, asps2T, NSJ_1T))


pred1T = forecast(Mod1T, h=7, xreg = cbind(Nsfe, Naspy1, Nasps2, Nasps2, NNSJ_1))$mean
pred2T = forecast(Mod2T, h=7, xreg = cbind(Nsfe, Naspy1, Nasps2, Nasps2, NNSJ_1))$mean


LNS_DP = LNOTIFICATION_Sortie[(n-6):n]
plot(LNS_DP, type = 'l', ylim = c(15,20))
lines(as.numeric(pred1T), type = 'l', col = 'red')
lines(as.numeric(pred2T), type = 'l', col = 'blue')

##Comparaison des MSE
MSE1 = sum((LNS_DP - pred1T)^2)/7 ; MSE1
MSE2 = sum((LNS_DP - pred2T)^2)/7 ; MSE2 #meilleur 

##Prédiction

corr = mean(exp(Mod2T$residuals))
pred2 = exp(pred2T)*corr - 1#comment gérer le +1 dans le log ?

NS_DP = NOTIFICATION_Sortie[(n-6):n]
plot(NS_DP, ylim = c(0,2e8), type='l')
lines(as.numeric(pred2), col='red')

ecart = (NS_DP - pred2)/10e6; ecart # 5 prédictions sur 7 en dessous de 5GWh d'écart

##Reste à tester la robustesse du modèle sur des fenêtres glissantes 
