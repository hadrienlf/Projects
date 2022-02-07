#####################severite:##############################################
#Pour les glm pour la severite:

tab_sev1$vh_fuel = as.numeric(tab_sev1$vh_fuel)
tab_sev1$pol_bonus = as.numeric(tab_sev1$pol_bonus)
tab_sev1$pol_duration = as.numeric(tab_sev1$pol_duration)
tab_sev1$pol_sit_duration = as.numeric(tab_sev1$pol_sit_duration)
tab_sev1$drv_age1 = as.numeric(tab_sev1$drv_age1)
tab_sev1$drv_age_lic1 = as.numeric(tab_sev1$drv_age_lic1)
#en plottant les claim_amount, on voit que certaines valeurs sont tres elevees donc on va les retirer et les traiter a part
#on retire les donnees ou la severite est negative:
tab_sev_normale=subset(tab_sev1,claim_amount<60000)
tab_sev_extremes=subset(tab_sev1,claim_amount>=60000)
#en plottant les claim_amount, on voit que certaines valeurs sont tres elevees donc on va les retirer et les traiter a part
#on retire les donnees ou la severite est negative:
#tab_sev_normale=subset(tab_sev,claim_amount<60000)
#tab_sev_extremes=subset(tab_sev,claim_amount>=60000)

sev_extremes_tot= sum(tab_sev_extremes$claim_amount)
#si on veut les repartir sur toutes les severites normales:
cst=sev_extremes_tot/nrow(tab_sev_normale)
sev_totale=tab_sev_normale
sev_totale$claim_amount=tab_sev_normale$claim_amount + cst 
#on retire les dommages extremes:
#q=quantile(Tableau0$severite,seq(0,1,0.01))
#sev_normale=which(Tableau0$severite<q[100])
#sev_extreme=Tableau0[-sev_normale,]
#Tableau_sev=Tableau0[sev_normale,]
orga=sort(sev_totale$claim_amount)
plot(1:length(orga),orga,xlab="numero du sinistre",ylab="montant du sinistre")

smp_size=floor(0.80*nrow(sev_totale))
index=sample(seq_len(nrow(sev_totale)),size=smp_size)
Trainsev=sev_totale[index,]
Testsev=sev_totale[-index,]

model0g = glm(claim_amount~1,
              data=Trainsev,
              family=Gamma(link=log)
)

modelfullg = glm(claim_amount~pol_coverage+pol_pay_freq+pol_bonus+pol_duration+pol_sit_duration
                 +drv_drv2+drv_sex1+drv_age_lic1+drv_age_lic2
                 +vh_fuel+vh_type+vh_din+vh_sale_end+vh_value+vh_age,
                 data=Trainsev,
                 family=Gamma(link=log)
)
# mod = step(model0g,
#            scope = list(upper=modelfullg),
#            direction="both",
#            test="Chisq",
#            data=Trainsev)

modelfinalg = glm(claim_amount ~ pol_coverage + vh_fuel + vh_sale_end + pol_bonus + 
                    drv_age_lic1 + vh_value + vh_type + drv_drv2 + drv_age_lic2,
                  data=sev_totale,
                  family=Gamma(link=log)
)

modelegamma = glm(formula = claim_amount ~ pol_coverage + vh_sale_end + vh_fuel + 
                    vh_type + vh_value + pol_bonus + drv_age_lic1, family = Gamma(link = log), 
                  data = Trainsev)
anova(modelegamma,test='Chisq')

#on retire vh_fuel et pol_bonus:
modelegamma = glm(formula = claim_amount ~ pol_coverage + vh_sale_end  + 
                    vh_type + vh_value + drv_age_lic1, family = Gamma(link = log), 
                  data = Trainsev)
anova(modelegamma,test='Chisq')
#on retire drv_age_lic1:
modelegamma = glm(formula = claim_amount ~ pol_coverage + vh_sale_end  + 
                    vh_type + vh_value, family = Gamma(link = log), 
                  data = Trainsev)
anova(modelegamma,test='Chisq')
#toutes les variiables restantes sont explicatives donc on a le modele avec toutes les donnees:

modelesev = glm(formula = claim_amount ~ pol_coverage + vh_sale_end  + 
                  vh_type + vh_value, family = Gamma(link = log), 
                data = sev_totale)
anova(modelesev,test='Chisq')


simu  <-  simulateResiduals(modelfinalg,  n=250)
plotSimulatedResiduals(simu)

predg=predict(modelfinalg,newdata=Trainsev,level=0.95,type='response')
validg=predict(modelfinalg,newdata=Testsev,level=0.95,type='response')
#plot(validg,Testsev$claim_amount)
#plot(validg)
abs(sum(Testsev$claim_amount-validg))/sum(Testsev$claim_amount)*100
#les differences relatives pour chaque prediction:
#zzz=abs((Testsev$claim_amount-validg))/(Testsev$claim_amount)
#absolues:
#zzza=abs((Testsev$claim_amount-validg))

rmmodel0ig = glm(claim_amount~1,
                 data=Trainsev,
                 family=inverse.gaussian(link = "log")
)

modelfullig = glm(claim_amount~pol_coverage+pol_pay_freq+pol_bonus+pol_duration+pol_sit_duration
                  +drv_drv2+drv_sex1+drv_age_lic1+drv_age_lic2
                  +vh_fuel+vh_type+vh_din+vh_sale_end+vh_value+vh_age,
                  data=Trainsev,
                  family=inverse.gaussian(link = "log")
)
# mod = step(model0g,
#            scope = list(upper=modelfullg),
#            direction="both",
#            test="Chisq",
#            data=Trainsev)
modelfinalig = glm(claim_amount ~ pol_coverage + vh_fuel + vh_sale_end + pol_bonus + 
                     drv_age_lic1 + vh_value + vh_type + drv_drv2 + drv_age_lic2,
                   data=tab_sev_normale,
                   family=inverse.gaussian(link = "log")
)

qqnorm(log(modelesev$residuals));qqline(log(modelesev$residuals))
dwt(modelesev,method='resample', alternative='two.sided')
plot(sev_totale$claim_amount,modelesev$fitted.values)