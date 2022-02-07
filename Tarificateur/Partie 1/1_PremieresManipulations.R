rm(list=ls())
######################librairies############################################
library(zoo)
library(xts)
library(sp)
library(CASdatasets)
library(FactoMineR)
library(dplyr)
library(ade4)
library(tidyverse)
library(corrplot)
library(factoextra)
library(vcd)
library(questionr)
library(MASS)
library(rmarkdown)
library(AER)
library(MLmetrics)
library(caret)
library(InformationValue)
library(readr)
library(stats)
library(rcompanion)
library(car)
library(DHARMa)
library(rms)
#####################donnees#############################################

data(pg17trainpol)
data(pg17trainclaim)
summary(pg17trainpol)
summary(pg17trainclaim)
pol=pg17trainpol
claim=pg17trainclaim

####################creation des donnees##############################################
#creation du bloc de donnees:
donnees=merge(pol,claim,by='id_client',all.x=TRUE)
donnees$claim_amount[is.na(donnees$claim_amount)]=0
donnees$claim_nb[is.na(donnees$claim_nb)]=0
combi=aggregate(claim$claim_nb,by=list(claim$id_client),sum)
names(combi)<-c('id','claim_nb')
freq_totale=aggregate(donnees$claim_nb, by=list(donnees$id_client),sum)
severite_totale=aggregate(donnees$claim_amount, by=list(donnees$id_client),sum) #utiliser merge, allx true
tab_sev=merge(claim,pol,by='id_client',all.x=TRUE)
tab_sev=subset(tab_sev,claim_amount>0)
names(severite_totale)=c('id_client','severite')
names(freq_totale)=c('id_client','freq')
resume=merge(freq_totale,severite_totale,by='id_client',all.x=TRUE)
####################Tableau##############################################
summary(resume$severite)
summary(resume$freq)
Tableau=merge(resume,donnees,by='id_client',all.x=TRUE)
Tableau$claim_nb[is.na(Tableau$claim_nb)]=0
#####################mettre les tableaux en numerique#############################################
#on met toutes les donnees sous forme numeriques:
Tableau$pol_pay_freq<-as.numeric(Tableau$pol_pay_freq)
Tableau$drv_sex1<-as.numeric(Tableau$drv_sex1)
Tableau$drv_drv2<-as.numeric(Tableau$drv_drv2)
Tableau$pol_coverage<-as.numeric(Tableau$pol_coverage)
Tableau$pol_payd<-as.numeric(Tableau$pol_payd)
Tableau$pol_usage<-as.numeric(Tableau$pol_usage)
Tableau$vh_type<-as.numeric(Tableau$vh_type)
Tableau$id_claim<-as.numeric(Tableau$id_claim)
base=Tableau

tab_sev1=tab_sev
tab_sev1$pol_pay_freq<-as.numeric(tab_sev1$pol_pay_freq)
tab_sev1$drv_sex1<-as.numeric(tab_sev1$drv_sex1)
tab_sev1$drv_drv2<-as.numeric(tab_sev1$drv_drv2)
tab_sev1$pol_coverage<-as.numeric(tab_sev1$pol_coverage)
tab_sev1$pol_payd<-as.numeric(tab_sev1$pol_payd)
tab_sev1$pol_usage<-as.numeric(tab_sev1$pol_usage)
tab_sev1$vh_type<-as.numeric(tab_sev1$vh_type)
tab_sev1$id_claim<-as.numeric(tab_sev1$id_claim)


#certaines lignes du tableau ne sont pas utilisables car il y a des NA dans certaines colonnes auquelles on va s'interesser
Tableau=Tableau[-which(is.na(Tableau$vh_age)),]
#Tableau=Tableau[-161,]
tab_sev=tab_sev[-which(is.na(tab_sev$vh_age)),]
#tab_sev=tab_sev[-161,]
tab_sev1=tab_sev1[-which(is.na(tab_sev1$vh_age)),]
#tab_sev1=tab_sev1[-161,]
#############################on regroupe des modalites#####################################
#pour les variables ayant trop de modalite, on peut creer des groupes plus grossiers (pour eviter de trop galerer en temps de calculs):
a=Tableau$drv_age1
Tableau$drv_age1=cut(a,breaks=c(18,25,40,60,80,103),include.lowest = TRUE)
b=Tableau$drv_age_lic1
#table(b)
#on va utiliser les quantiles pour avoir une repartition interressante; en essayant par pas de 0.1 et par pas de 0.2 les 2 semblent interressants:
Tableau$drv_age_lic1=cut(b,breaks=quantile(b,seq(0,1,0.2)),include.lowest = TRUE)
c=Tableau$pol_bonus
#table(c)
#on voit les nombres de valeurs pour les differents bonus, on va donc selectionner nous meme les valeurs clefs avec table(Tableau$pol_bonus):
Tableau$pol_bonus=cut(c,breaks=c(0.5,0,53,0.60,1,1.5,2.16),include.lowest = TRUE)
#Pour pol_duration aussi on peut reduire le nombre de facteurs:
#table(Tableau$pol_duration)
#on va utiliser les quantiles:
d=Tableau$pol_duration
Tableau$pol_duration=cut(d,breaks=quantile(d,seq(0,1,0.1)),include.lowest = TRUE)
#table(Tableau$pol_sit_duration)
#on voit avec cette fonction qu'on peut regrouper:
e=Tableau$pol_sit_duration
Tableau$pol_sit_duration=cut(e,breaks=quantile(d,seq(0,1,0.1)),include.lowest = TRUE)

#################TabNA pour la frequence#################################################

#freq:
TabNA = na.omit(dplyr::select(Tableau,-c(id_vehicle.y,id_year.y,id_claim,vh_make,vh_model,drv_sex2,drv_age2,drv_age_lic2)))

TabNA$vh_fuel = as.numeric(TabNA$vh_fuel)
TabNA$pol_bonus = as.numeric(TabNA$pol_bonus)
TabNA$pol_duration = as.numeric(TabNA$pol_duration)
TabNA$pol_sit_duration = as.numeric(TabNA$pol_sit_duration)
TabNA$drv_age1 = as.numeric(TabNA$drv_age1)
TabNA$drv_age_lic1 = as.numeric(TabNA$drv_age_lic1)

#################train/pol###################################
set.seed(1234)
smp_size=floor(0.80*nrow(TabNA))
index=sample(seq_len(nrow(TabNA)),size=smp_size)
Train=TabNA[index,]
Test=TabNA[-index,]