######################prime pure############################################
#Calcul de la prime pure
coefficients=c(coefficients(modelefreq),coefficients(modelesev))
coef.freq=coefficients(modelefreq)
coef.sev=coefficients(modelesev)

# on cree les matrices design  
mfreq=model.matrix(~ vh_sale_end + vh_value
                   + pol_coverage + vh_fuel + drv_drv2
                   + pol_sit_duration + pol_duration + vh_age
                   + vh_weight + drv_age_lic1 + drv_age1
                   + pol_bonus,TabNA)

msev=model.matrix(~pol_coverage+ vh_sale_end + vh_type + vh_value ,TabNA)

#calcul de l'esperence de la frequence
esp.freq=exp(mfreq %*% coef.freq)
#calcul de l'esperence de la severite
esp.sev=exp(msev %*% coef.sev)
#la prime pure
prime=esp.freq*esp.sev
primepure=data.frame(TabNA$id_client,prime)
names(primepure)=c('id_client','prime_pure')
hist(primepure$prime_pure,main='Prime pure',xlab='montant de la prime')
summary(primepure$prime_pure)
