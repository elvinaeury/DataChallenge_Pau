library(readr)
library(forecast)
library(tseries)
library(dplyr)
library(Amelia)
library(ggplot2)


setwd('/Users/elvinagovendasamy/DataChallenge/')
X_pirineos <- read.csv("X_pirineos_extraction_2020-12-02.csv", sep=",")
attach(X_pirineos)


X_pirineos_entree = X_pirineos[,-c(1,28,27)]

# ----------------------------------------------- REGRESSION LINEAIRE ---------------------------------------

plot(NOTIFICATION_Entree, type='l')

LNOTIFICATION_Entree =log(NOTIFICATION_Entree +1)
plot (LNOTIFICATION_Entree, type='l')
Data = X_pirineos[,-c(1,27,28)]
Data = Data[,-c(29)]
n = dim(X_pirineos)[1]

decompose(LNOTIFICATION_Entree)

Reg = lm(LNOTIFICATION_Entree ~ ., data = Data)
summary(Reg)

Reg2 = lm(LNOTIFICATION_Entree ~ spread_france_espagne + settlement_price_fr_m1 + settlement_price_fr_m3 + 
            settlement_price_fr_y2 + settlement_price_fr_q2 + settlement_price_fr_s2 + settlement_price_fr_s3 +
            average_settlement_price_m2 + average_settlement_price_m3 + average_settlement_price_q2 + average_settlement_price_s2 +
            interruptible_booked_sortie + NOTIFICATION_Entree_J.5 )

summary(Reg2)

x= as.Date(X_pirineos[,1])
dev.off()

plot(LNOTIFICATION_Entree, type='l')
lines(Reg2$fitted.values, col='red')

plot(Reg2$residuals, type='l')
acf(Reg2$residuals) #périodicité de 7
pacf(Reg2$residuals) #idem


ResReg = ts(Reg2$residuals, frequency = 7)
plot(decompose(ResReg))

##On part sur un modèle SARIMAX (régression linéaire sur la série + bruit SARIMA)
##On passe au log pour réduire les échelles (hum moyen avec les pb de 0)

bruit_entree_diff = diff(ResReg) #d=1, D=0
bruit_entree_diff7 = diff(ResReg,7) #d=0, D=1
bruit_entree_diff7d = diff(diff(ResReg,7)) #d=1, D=1

plot(bruit_entree_diff, type = 'l')
acf(as.numeric(bruit_entree_diff), lag.max = 50)  # propre
pacf(as.numeric(bruit_entree_diff), lag.max = 50) #partie AR importante
adf.test(bruit_entree_diff) # stationnairité OK
kpss.test(bruit_entree_diff) # stationnairité OK


plot(bruit_entree_diff7, type = 'l')
acf(as.numeric(bruit_entree_diff7), lag.max = 50)  # gros pic à 7, --> P=1 ?
pacf(as.numeric(bruit_entree_diff7), lag.max = 50) #pics à 7, 14, 21, etc ... --> encore périodique
adf.test(bruit_entree_diff7) # stationnairité OK
kpss.test(bruit_entree_diff7) # stationnairité OK


plot(bruit_entree_diff7d, type = 'l')
acf(as.numeric(bruit_entree_diff7d), lag.max = 50)  # gros pic à 7, P=1 ?
pacf(as.numeric(bruit_entree_diff7d), lag.max = 50) #encore de la périodicité ? + partie AR importante
adf.test(bruit_entree_diff7d) # stationnairité OK
kpss.test(bruit_entree_diff7d) # stationnairité OK
#Cette série semble également stationnaire mais pacf pas folle

## Un mosèle avec d=1, D=0 semble plutôt bon 

auto.arima(ResReg, allowdrift = T, allowmean = T, ic='bic')
#SARIMA (2,0,1)x(1,0,0)[7] with zero mean 
Mod1 = Arima(LNOTIFICATION_Entree, order = c(2,0,1), seasonal = list(order=c(1,0,0), period=7),include.mean = FALSE,
             xreg = cbind(spread_france_espagne + settlement_price_fr_m1 + settlement_price_fr_m3 + settlement_price_fr_y2 + settlement_price_fr_q2 + settlement_price_fr_s2 + settlement_price_fr_s3 +
                            average_settlement_price_m2 + average_settlement_price_m3 + average_settlement_price_q2 + average_settlement_price_s2 +interruptible_booked_sortie + NOTIFICATION_Entree + NOTIFICATION_Entree_J.5 ))

summary(Mod1)


auto.arima(ResReg, d=1, allowdrift = T, allowmean = T, ic='bic')
#SARIMA (2,1,2)(1,0,0)[7]  with zero mean 
Mod2 = Arima(LNOTIFICATION_Entree, order = c(2,1,2), seasonal = list(order=c(1,0,0), period=7),include.mean = TRUE,
             xreg = cbind(spread_france_espagne + settlement_price_fr_m1 + settlement_price_fr_m3 + settlement_price_fr_y2 + settlement_price_fr_q2 + settlement_price_fr_s2 + settlement_price_fr_s3 +
                            average_settlement_price_m2 + average_settlement_price_m3 + average_settlement_price_q2 + average_settlement_price_s2 +interruptible_booked_sortie + NOTIFICATION_Entree + NOTIFICATION_Entree_J.5 ))

summary(Mod2)

##Première impression visuelle de la qualité des modèles

#Modèle 1
plot(LNOTIFICATION_Entree, type='l')
lines(Mod1$fitted, col = 'red')
checkupRes(Mod1$residuals)
shapiro.test(Mod1$residuals) #on rejette le caractère gaussien des résidus
Box.test(Mod1$residuals, type = "Ljung-Box", lag = 7) # bruit blanc

#Modèle 2
dev.off()
plot(LNOTIFICATION_Entree, type='l')
lines(Mod2$fitted, col = 'red') # à l'air de prévoir les pics beaucoup mieux
checkupRes(Mod2$residuals)
shapiro.test(Mod2$residuals) #on rejette le caractère gaussien des résidus
Box.test(Mod2$residuals, type = "Ljung-Box", lag = 7) #bruit blanc

### Comparaison sur un critère prédictif
Tps = time(LNOTIFICATION_Entree)
NTps = Tps[(n-6):n]

#série tronquée
LNST = LNOTIFICATION_Entree[1:(n-7)]

#Variables du xreg tronquée



sfeT = spread_france_espagne[1:(n-7)]
spfm1T = settlement_price_fr_m1[1:(n-7)]
spfm3T = settlement_price_fr_m3[1:(n-7)]
spfy2T = settlement_price_fr_y2[1:(n-7)]
spfq2T = settlement_price_fr_q2[1:(n-7)]
spfs2T = settlement_price_fr_s2[1:(n-7)]
spfs3T = settlement_price_fr_s3[1:(n-7)]
aspm2T = average_settlement_price_m2[1:(n-7)]
aspm3T = average_settlement_price_m3[1:(n-7)]
aspq2T = average_settlement_price_q2[1:(n-7)]
asps2T = average_settlement_price_s2[1:(n-7)]
ibsT = interruptible_booked_sortie[1:(n-7)]
NET = NOTIFICATION_Entree[1:(n-7)]
NEJ_5T = NOTIFICATION_Entree_J.5[1:(n-7)]

Nsfe = spread_france_espagne[(n-6):n]
Nspfm1 = settlement_price_fr_m1[(n-6):n]
Nspfm3 = settlement_price_fr_m3[(n-6):n]
Nspfy2= settlement_price_fr_y2[(n-6):n]
Nspfq2 = settlement_price_fr_q2[(n-6):n]
Nspfs2 = settlement_price_fr_s2[(n-6):n]
Nspfs3 = settlement_price_fr_s3[(n-6):n]
Naspm2 = average_settlement_price_m2[(n-6):n]
Naspm3 = average_settlement_price_m3[(n-6):n]
Naspq2 = average_settlement_price_q2[(n-6):n]
Nasps2 = average_settlement_price_s2[(n-6):n]
Nibs = interruptible_booked_sortie[(n-6):n]
NNE = NOTIFICATION_Entree[(n-6):n]
NNEJ_5 = NOTIFICATION_Entree_J.5[(n-6):n]




Mod1T = Arima(LNST, order = c(2,0,1), seasonal = list(order=c(1,1,0), period=7), xreg = cbind(sfeT, spfm1T,spfm3T,spfy2T,spfq2T, spfs2T, spfs3T,aspm2T,aspm3T,aspq2T, asps2T, ibsT, NET ,NEJ_5T))
Mod2T = Arima(LNST, order = c(2,1,2), seasonal = list(order=c(1,0,0), period=7), xreg = cbind(sfeT, spfm1T,spfm3T,spfy2T,spfq2T, spfs2T, spfs3T,aspm2T,aspm3T,aspq2T, asps2T, ibsT, NET ,NEJ_5T))

pred1T = forecast(Mod1T, h=7, xreg = cbind(Nsfe, Nspfm1, Nspfm3, Nspfy2, Nspfq2, Nspfs2, Nspfs3, Naspm2, Naspm3, Naspq2, Nasps2, Nibs, NNE, NNEJ_5))$mean
pred2T = forecast(Mod2T, h=7, xreg = cbind(Nsfe, Nspfm1, Nspfm3, Nspfy2, Nspfq2, Nspfs2, Nspfs3, Naspm2, Naspm3, Naspq2, Nasps2, Nibs, NNE, NNEJ_5))$mean



LNE_DP = LNOTIFICATION_Entree[(n-6):n]
plot(LNE_DP, type = 'l', ylim = c(10,20))
lines(as.numeric(pred1T), type = 'l', col = 'red')
lines(as.numeric(pred2T), type = 'l', col = 'blue')

##Comparaison des MSE
MSE1 = sum((LNE_DP - pred1T)^2)/7 ; MSE1 #meilleur
MSE2 = sum((LNE_DP - pred2T)^2)/7 ; MSE2  

##Prédiction


pred1 = exp(pred1T)

NE_DP = NOTIFICATION_Entree[(n-6):n]
plot(NS_DP, type='l')
lines(as.numeric(pred1), col='red')

ecart = (NE_DP - pred1)/1e6; ecart # 6 prédictions sur 7 en dessous de 5GWh d'écart


##Reste à tester la robustesse du modèle sur des fenêtres glissantes 
## correction 

split_data = function(serie, xreg, fen, periode){
  n=length(serie)
  
  if(nrow(xreg)!=n){
    print("Longueur des regresseurs inférieure à longueur de la série")
  }
  
  tailles = n - periode*(fen:1 - 1)
  
  liste_series = c()
  liste_xreg = c()
  
  for (i in 1:fen){
    assign(paste0("xreg",i), xreg[1:(tailles[i]),])
    assign(paste0("serie",i), serie[1:(tailles[i])])
    
    liste_series = c(liste_series,paste0("serie",i))
    liste_xreg = c(liste_xreg,  paste0("xreg",i))
  }
  
  return(list(listes_series = mget(liste_series), liste_xreg = mget(liste_xreg)))
}

fen = 5

X_reg = cbind(spread_france_espagne, settlement_price_fr_m1, settlement_price_fr_m3, settlement_price_fr_y2, settlement_price_fr_q2, settlement_price_fr_s2, settlement_price_fr_s3,
                average_settlement_price_m2, average_settlement_price_m3, average_settlement_price_q2, average_settlement_price_s2,interruptible_booked_sortie,
              NOTIFICATION_Entree, NOTIFICATION_Entree_J.5 )

resultat = split_data(LNOTIFICATION_Entree, X_reg, fen, 7)

for (i in 1:fen){
  LPirineos_entree = resultat$listes_series[[i]]
  
  n = length(LPirineos_entree)
  h = 7 # horizon de prédiction
  
  
  LPirineos_entreeT = LPirineos_entree[1:(n-h)]
  LPirineos_entree_test = LPirineos_entree[(n-h+1):n]
  
  X_reg = resultat$liste_xreg[[i]]
  
  X_reg_train2 = X_reg[1:(n-h),]
  X_reg_test2 = X_reg[(n-h+1):n,]
  
  modeleLogT = Arima(
    y = LPirineos_entreeT,
    order = c(2,0,1),
    seasonal = list(order=c(1,1,0), period = 7),
    xreg = X_reg_train2
  )
  
  corr = mean(exp(modeleLogT$residuals))
  PredT_log = forecast(modeleLogT, h=h, xreg = X_reg_test2)
  exp_PredT_log = exp(PredT_log$mean) - 1
  Ecart_absolus_mod_log_exp = abs(exp_PredT_log - exp(LPirineos_entree_test))
  test = Ecart_absolus_mod_log_exp/10^6
  
  cat(paste0("Période ", i, " : "), test, "\n")
}
