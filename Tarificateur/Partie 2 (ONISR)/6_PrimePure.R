###################Calcul de la prime pure ONISR################################################
#Calcul de la prime pure
coefficients=c(coefficients(modelefreqONISR),coefficients(modelesevONISR))
coef.freq=coefficients(modelefreqONISR)
coef.sev=coefficients(modelesevONISR)

# on cree les matrices design  
mfreq=model.matrix(~ vh_sale_end + vh_value
                   + pol_coverage + vh_fuel + drv_drv2
                   + pol_sit_duration + pol_duration + vh_age
                   + vh_weight + drv_age_lic1 + drv_age1
                   + pol_bonus+ClusterFreq,Tableaudep)

msev=model.matrix(~pol_coverage+ vh_sale_end + vh_type + vh_value + ClusterSev ,Tableaudep)

#calcul de l'esperence de la frequence
esp.freq=exp(mfreq %*% coef.freq)
#calcul de l'esperence de la severite
esp.sev=exp(msev %*% coef.sev)
#la prime pure
prime=esp.freq*esp.sev
primepureONISR=data.frame(Tableaudep$id_client,prime)
names(primepureONISR)=c('id_client','prime_pure')
hist(primepureONISR$prime_pure,main='Prime pure',xlab='montant de la prime ONISR')
summary(primepureONISR$prime_pure)
