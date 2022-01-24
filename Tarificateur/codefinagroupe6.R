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
##################################################################
#################TabNA pour la frequence#################################################

#avec rcompanion

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
##################frequence##################################
hist(TabNA$freq,prob=TRUE)
lines(dpois(x=0:nrow(TabNA),lambda=mean(TabNA$freq)),type="l", col = "red")
#tester si les donnees sont sous la forme d'une distribution Poisson
dispersion_test <- function(x) 
{
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  
  cat(
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")
  
  invisible(res)
}
dispersion_test(TabNA$freq)
#It's not Poisson datas

#for negative binomial:
model0nb = glm.nb(freq~1,
              data=Train
)
modelfullnb = glm.nb(freq~pol_bonus+pol_coverage+pol_pay_freq+
                   vh_sale_end+vh_value+vh_age+vh_cyl+vh_din+vh_fuel+vh_sale_begin+vh_speed+vh_value+vh_weight+
                   drv_sex1+drv_drv2
                 +drv_age1+drv_age_lic1+pol_sit_duration+pol_duration,
                 data=Train
)
# modnb = step(model0nb,
#            scope = list(upper=modelfullnb),
#            direction="both",
#            test="Chisq",
#            data=Train)

#for poisson:
model0p = glm(freq~1,
              data=Train,
              family=poisson()
)
modelfullp = glm(freq~pol_bonus+pol_coverage+pol_pay_freq+
                   vh_sale_end+vh_value+vh_age+vh_cyl+vh_din+vh_fuel+vh_sale_begin+vh_speed+vh_value+vh_weight+
                   drv_sex1+drv_drv2
                 +drv_age1+drv_age_lic1+pol_sit_duration+pol_duration,
                 data=Train,
                 family=poisson()
)
# mod = step(modelfullp,
#            scope = list(upper=model0p),
#            direction="both",
#            test="Chisq",
#            data=Train)

modelfinal = glm(freq~ 
                   pol_bonus + pol_coverage,
                 data=TabNA,
                 family=poisson()
)
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                     pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                     drv_age1 + pol_bonus + vh_speed + vh_din + vh_sale_begin,
                 data=Train
)
anova(modelfinalnb,test='Chisq')
#on retire la variable vh_sale_begin:
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus + vh_din + vh_speed,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#on peut retirer vh_din:
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus + vh_sale_begin,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#on peut retirer vh_sale_begin:
modelfinalnb = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                        pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                        drv_age1 + pol_bonus,
                      data=Train
)
anova(modelfinalnb,test='Chisq')
#Toutes les variables semblent expliquatives

autre1=glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                drv_age1 + pol_bonus,data=TabNA)
(anova(autre1,test='Chisq'))

#le glm avec toutes les donnees:
modelefreq = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                    pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                    drv_age1 + pol_bonus,
                  data=TabNA
)
summary(modelefreq)



qqnorm(log(modelefreq$residuals));qqline(log(modelefreq$residuals))
dwt(modelefreq,method='resample', alternative='two.sided')
#plot(TabNA$freq,modelefreq$fitted.values)

#on compare a un modele qui a l'air de bien marcher:)
(anova(modelfinal,test='Chisq'))
compareGLM(modelfinal,autre1)
#,modelfinalnb
#drv_age_lic1 + vh_age + pol_duration + drv_drv2 + vh_type + 
#vh_sale_end + vh_value + pol_sit_duration + drv_age_lic2 + 
#pol_bonus + pol_coverage

prednb=predict(modelfinal,newdata=Train,level=0.95,type='response')
valid=predict(modelfinal,newdata=Test,level=0.95,type='response')
abs(sum(Test$freq)-sum(valid)) #erreur absolue
abs(sum(Test$freq-valid))/sum(Test$freq)*100 #erreur relative

erreurp=abs(Test$freq-valid)
mean(erreurp)
summary(erreurp)

arrp=round(valid)
erreurarrp=abs(Test$freq-arrp)
mean(erreurarrp)
summary(erreurarrp)

erreurposp=erreurp[which(erreurarrp>0)]
mean(erreurposp)
summary(erreurposp)

prednbnb=predict(modelfinalnb,newdata=Train,level=0.95,type='response')
validnb=predict(modelfinalnb,newdata=Test,level=0.95,type='response')
abs(sum(Test$freq)-sum(validnb)) #erreur absolue
abs(sum(Test$freq-validnb))/sum(Test$freq)*100 #erreur relative

erreur=abs(Test$freq-validnb)
mean(erreur)
summary(erreur)

arr=round(validnb)
erreurarr=abs(Test$freq-arr)
mean(erreurarr)
summary(erreurarr)

erreurpos=erreur[which(erreurarr>0)]
mean(erreurpos)
summary(erreurpos)


# autre2=glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
#                 pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
#                 drv_age1 + pol_bonus,data=Train)
# pred1$pred=predict(autre2,level=0.95,type='response')
# valid1$pred=predict(autre2,newdata=Test,level=0.95,type='response')
# 
# rmse_tr=RMSE(pred1$pred,freq)
# rmse_te=RMSE(valid1$pred,freq)


# plot.confidence <- function(df, feature) {
#   library(ggplot2)
#   p <- ggplot(df, aes_string(x = feature, 
#                              y = "fit")) + 
#     geom_line(colour = "blue") + 
#     geom_point() + 
#     geom_ribbon(aes(ymin = lwr, ymax = upr), 
#                 alpha = 0.5) 
#   return(p)
# }
# plot.confidence.features <- function(data, features) {
#   plots <- list()
#   for (feature in features) {
#     p <- plot.confidence(data, feature)
#     plots[[feature]] <- p
#   }
#   library(gridExtra)
#   #grid.arrange(plots[[1]], plots[[2]], plots[[3]])
#   do.call(grid.arrange, plots)
# }
# 
# data <- cbind(Train, prednb)
# plot.confidence.features(data, colnames(Train))
# plot(data)


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

##################################################################
############################ONISR######################################
library(maptools)
library(RColorBrewer)
library(classInt)
library(raster)
library(rgdal)

#on a la carte de france decoupee en departements avec:
#carte = readOGR(dsn = "C:/Users/hadri/Documents/R/departements-20180101.shp", layer = "SHAPEFILE")
#spdf_departement <- readOGR(
  #"https://www.data.gouv.fr/fr/datasets/r/eb36371a-761d-44a8-93ec-3d728bec17ce",
  #"data/departements-20140306-100m-shp/", 
 # layer= "departements-20140306-100m"
#)
#on a la carte de france decoupee en departements avec:
#FranceFormes = getData(name="GADM", country="FRA", level=2)
#plot(FranceFormes, main="Carte de la France, départements")

#ONISR: on a des donnees separees en 4 categories: Caracteristiques, lieux, vehicules, usagers
caract=read.csv("C:/Users/hadri/Desktop/donnees/caracteristiques-2017.csv")
lieux=read.csv("C:/Users/hadri/Desktop/donnees/lieux-2017.csv")
usagers=read.csv("C:/Users/hadri/Desktop/donnees/usagers-2017.csv")
vehicules=read.csv("C:/Users/hadri/Desktop/donnees/vehicules-2017.csv")

#dans la base caract, les departements sont donnes par 3 chiffres mais on va regarder que les 2 premiers
class(caract$dep)='numeric'
caract$dep=caract$dep/10

#dans la base vehicules, il y a des categories qui ne nous interessent pas car pas utilisees depuis 2006 ou des categories de vehicules 
#que nous n'avons pas regarde dans la creation de notre modele; on va garder les categories: 7,10,13,14,15 (verifier si dans pgtrain on avait des types de vehicules speciaux)
class(vehicules$catv)='numeric'
a=vehicules$Num_Acc[which(vehicules$catv==7|vehicules$catv==10|vehicules$catv==13|vehicules$catv==15|vehicules$catv==14)]

#donc on retire les donnees que l'on ne va pas utiliser:
vehicules=subset(vehicules,vehicules$Num_Acc %in% a)
caract=subset(caract,caract$Num_Acc %in% a)
lieux=subset(lieux,lieux$Num_Acc %in% a)
m1=merge(caract,lieux)
m2=merge(m1,vehicules)
m=merge(m2,usagers)

m$Num_Acc = as.character(m$Num_Acc)

hist(floor(m$hrmn)/100)
hist(m$grav,breaks=c(0.5,1.5,2.5,3.5,4.5))


library(rgdal)
library('classInt') 
library('plotrix')  
library(geojsonio)
library(stringr)
library(readxl)

dep=geojson_read("C:/Users/hadri/Documents/R/contour-des-departements.geojson",what="sp")
names(dep)=c('dep','nom')
dep_num=as.numeric(dep$dep)
gravitepardep=c(m$dep,m$grav)

#on retire les departement d'outre mer:
m$dep[which(m$dep>96)]=NA
m=m[complete.cases(m[,17]),]

#on modifie pour la Corse:
m$dep[which(m$dep==20.1)]='2A'
m$dep[which(m$dep==20.2)]='2B'

DEP=merge(m,dep)

hist(m$catu,breaks=5,main='usagers',xlab="type d'usager")

###########################carte gravite moyenne#######################################
gravite=aggregate(m$grav, by=list(m$dep),mean)
#on veut recupere un 0 devant les chiffres:
gravite$Group.1=str_pad(gravite$Group.1, 2, pad = "0")

col=findColours(classIntervals(var = gravite$x, n = 3 , style="quantile"),
                smoothColors("green","orange","red"))

leg = findColours(
  classIntervals(var = gravite$x, n = 3 , style="quantile"),
  smoothColors("green","orange","red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la gravite moyenne:
plot(dep)
j=0
for(i in gravite$Group.1){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "gravite moyenne :")
########################## cartefrequence par departement########################################
m$newcol=rep(1,length(m$Num_Acc))
freqq=aggregate(m$newcol, by=list(m$dep),sum)
#on veut recupere un 0 devant les chiffres:
freqq$Group.1=str_pad(freqq$Group.1, 2, pad = "0")

col=findColours(classIntervals(var = freqq$x, n = 10 , style="quantile"),
                smoothColors("green",8,"red"))

leg = findColours(
  classIntervals(var = freqq$x, n = 10 , style="quantile"),
  smoothColors("green",8,"red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la frequence:
plot(dep)
j=0
for(i in freqq$Group.1){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "nombre d'accidents :")
#############################carte /pop departement#####################################
#on essaye en regardant proportionnelement a la population:
populations = read_excel("C:/Users/hadri/Desktop/donnees/populations.xlsx")
freqq=aggregate(m$newcol, by=list(m$dep),sum)
names(freqq)=c('Dep','x')
reu=merge(freqq,populations,by='Dep')
#on veut recupere un 0 devant les chiffres:
reu$Dep=str_pad(reu$Dep, 2, pad = "0")
#on se facilite le plot en remettant la corse en departement 20 et 20.5:
reu$Dep[which(reu$Dep=='2A')]='20'
reu$Dep[which(reu$Dep=='2B')]='20.5'
#pour 100.000 habitants:
reu$moy=reu$x/(reu$`Population 2019`/100000)


col=findColours(classIntervals(var = reu$moy, n = 4 , style="quantile"),
                smoothColors("green",'yellow','orange',"red"))

leg = findColours(
  classIntervals(var = reu$moy, n = 4 , style="quantile"),
  smoothColors("green",'yellow','orange',"red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la frequence:
plot(dep)
freqq$Dep=str_pad(freqq$Dep, 2, pad = "0")
j=0
for(i in freqq$Dep){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "nombre d'accidents :")
#####################carte annee de naissance de la victime#############################################
mage=m[complete.cases(m$an_nais),]
mage$age=2021-mage$an_nais
age=aggregate(mage$age, by=list(mage$dep),mean)
#on veut recupere un 0 devant les chiffres:
age$Group.1=str_pad(age$Group.1, 2, pad = "0")

col=findColours(classIntervals(var = age$x, n = 3 , style="quantile"),
                smoothColors("green","orange","red"))

leg = findColours(
  classIntervals(var = age$x, n = 3 , style="quantile"),
  smoothColors("green","orange","red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la frequence:
plot(dep)
j=0
for(i in age$Group.1){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "age moyen de la victime :")

names(freqq)=c('Dep','x')
populations$Dep=str_pad(populations$Dep, 2, pad = "0")
pop=merge(freqq,populations,by='Dep')
#on se facilite le plot en remettant la corse en departement 20 et 20.5:
reu$Dep[which(reu$Dep=='2A')]='20'
reu$Dep[which(reu$Dep=='2B')]='20.5'
plot(pop$`Population 2019`/1000,log(pop$x))
#relation claire entre le nombre d'accidents et le nombre d'habitants. d'ou l'harmonisation en divisant les resultats par le nombre d'habitants par departement
plot(pop$Dep,pop$x,type='h',xlab='Departements',ylab='Nombre d\'accidents')
plot(pop$Dep,pop$x/pop$`Population 2019`,type='h',xlab='Departements',ylab='Nombre d\'accidents lissé')
#on voit bien que le lissage par population reste interessant.

#si on veut remettre en 2A et 2B:
#pop$Dep[which(reu$Dep=='20')]='2A'
#pop$Dep[which(reu$Dep=='20.5')]='2B'

m$agg
hist(m$agg)
hist(m$choc)
hist(m$obsm)
hist(m$surf)
hist(m$atm)
hist(m$env1)
hist(m$int)

#voir si c'est les vacances:
mois=aggregate(m$newcol,by=list(m$mois),FUN=sum)
plot(1:12,mois$x,type='h')
#ca n'a pas l'air d'etre le cas


#voir si la majorite des accidents est en agglomeration ou non:
aglo=aggregate(m$agg,by=list(m$dep),FUN=median)
#on veut recupere un 0 devant les chiffres:
aglo$Group.1=str_pad(gravite$Group.1, 2, pad = "0")

col=findColours(classIntervals(var = aglo$x, n = 2,style="quantile" ),
            smoothColors("green","red"))
leg = findColours(
  classIntervals(var = aglo$x, n = 2 , style="quantile"),
              smoothColors("green","red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la gravite moyenne:
plot(dep)
j=0
for(i in aglo$Group.1){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "gravite moyenne :")


####################entrainement clusters##############################################
library(factoextra)
library(cluster)

#pour se familiariser avec les clusters:

mcluster=subset(m,select=c(catu,sexe,an_nais,trajet,secu))
mclusterna=na.omit(mcluster)
mclusterna1=scale(mclusterna)
mtest=mclusterna1[1:10000,]
#pour la frequence
df=freqq
#df$Dep[which(df$Dep=='2A')]='20'
#df$Dep[which(df$Dep=='2B')]='20.5'
#df$Dep=as.numeric(df$Dep)
#pour la gravite:
gravite1=subset(m,select=c(dep,grav))
gravite1$grav[gravite1$grav==2] = 5
gravite1$grav[gravite1$grav==4] = 2
gravite1$grav[gravite1$grav==5] = 4
gravite1=aggregate(gravite1$grav,by=list(gravite1$dep),FUN=mean)
names(gravite1)=c('Dep','x')
#gravite1$Dep[which(gravite1$Dep=='2A')]='20'
#gravite1$Dep[which(gravite1$Dep=='2B')]='20.5'
#gravite1$Dep=as.numeric(gravite1$Dep)
gravite1$Dep=str_pad(gravite1$Dep, 2, pad = "0")
df1=merge(df,gravite1,'Dep')

df1$Dep[which(df1$Dep=='2A')]='20'
df1$Dep[which(df1$Dep=='2B')]='20.5'
df$Dep=as.numeric(df1$Dep)

row.names(df1)=df1$Dep
df2=df1[,-1]

fviz_nbclust(df2, kmeans, method = "wss")

km <- kmeans(df2, centers = 4, nstart = 25)
km
fviz_cluster(km, data = df2)

aggregate(df2, by=list(cluster=km$cluster), mean)

#######################Severite et frequence par departement###########################################
#variables a mettre par departement: ensuite on creera des clusters comme precedement
#Pour la frequence: nbaccidents/hab, surface (surf)des routes (en retirant les valeurs inutiles), agglo,prof(si routes plates ou pas), intersections (int)
#pour la severite: type de collision (col), obstacle mobile touche, choc, agglo, grav

#frequence:
#1: accidents pour 100k habitants
popp=pop
row.names(popp)=popp$Dep
popp=subset(popp,select=c(Dep,x,`Population 2019`))
popp$acc_par_hab=popp$x/(popp$`Population 2019`/100000)

popp$score_pop=popp$acc_par_hab/max(popp$acc_par_hab) #pour avoir un score entre 0 et 1
#le score par departement pour le nombre d'accident pour 100k habitants est:
scorepop=subset(popp,select=c(Dep,score_pop))

#2:surface par departement:
surface = aggregate(m$int,by=list(m$dep),FUN=median)
names(surface)=c('Dep','surf')

neige<-table(m$dep,m$surf)
neige<-neige[,-c(1,10)]
neige<-neige[,c(2,5,7)]/apply(neige,1,sum)  
neige<-neige[,1]+neige[,2]+neige[,3]     
score_surf<-neige/max(neige)

neige1=as.data.frame(score_surf)
neige1$Dep=row.names(neige1)
names(neige1)=c('score','Dep')
neige=merge(surface,neige1,by='Dep')

#le score d'intemperies exceptionnelles est donne par:
scoreneige=subset(neige,select=c(Dep,score))
scoreneige$Dep=str_pad(scoreneige$Dep, 2, pad = "0")

# #On peut le regarder sur une carte:
# col=findColours(classIntervals(var = surfa$neige, n = 2 , style="quantile"),
#                 smoothColors("green","red"))
# 
# leg = findColours(
#   classIntervals(var = surfa$neige, n = 2 , style="quantile"),
#   smoothColors("green","red"),
#   under="moins de", over="plus de", between="-", cutlabels=FALSE)
# 
# 
# #on plot la map de France avec les donnees par departement sur la neige/verglas:
# plot(dep)
# j=0
# for(i in surfa$Dep){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
# legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
#        legend=names(attr(leg,"table")),
#        title = "neige/verglas par departement :")

#3: agglomeration
aglo=aggregate(m$agg,by=list(m$dep),FUN=median)
names(aglo)=c('Dep','agglo')

agglo=table(m$dep,m$agg)
agglo=agglo[,1]/apply(agglo,1,sum)  
score_agg<-agglo/max(agglo) 

agg1=as.data.frame(score_agg)
agg1$Dep=row.names(agg1)
names(agg1)=c('score','Dep')
agg=merge(aglo,agg1,by='Dep')


#le score d'intemperies exceptionnelles est donne par:
scoreagg=subset(agg,select=c(Dep,score))
scoreagg$Dep=str_pad(scoreagg$Dep, 2, pad = "0")


#le score d'agglomeration est donne par:
scoreagg=subset(aglo,select=c(Dep,score))
scoreagg$Dep=str_pad(scoreagg$Dep, 2, pad = "0")
#plus le score est proche de 1 plus le nombre d'accidents en agglomeration represente une grosse part dans le total des accidents

#4: intersections:
inter=aggregate(m$int,by=list(m$dep),FUN=median)
names(inter)=c('Dep','int')

intersections1=table(m$dep,m$int) 
intersections1=intersections1[,-1]
intersections1=intersections1[,seq(2,8)]/apply(intersections1,1,sum)
intersections1=apply(intersections1,1,sum)  
scoreintersections=intersections1/max(intersections1)

inter1=as.data.frame(scoreintersections)
inter1$Dep=row.names(inter1)
names(inter1)=c('score','Dep')
inter=merge(inter,inter1,by='Dep')

#le score d'intersections est donne par:
scoreint=subset(inter,select=c(Dep,score))
scoreint$Dep=str_pad(scoreint$Dep, 2, pad = "0")



scorefreq=merge(scorepop,scoreneige,by='Dep')
scorefreq=merge(scorefreq,scoreagg,by='Dep')
scorefreq=merge(scorefreq,scoreint,by='Dep')
names(scorefreq) = c('Dep','pop','neige','agglo','intersections')

#on somme tous les scores puis on les normalisera:
scorefreq$total=scorefreq$pop+scorefreq$neige+scorefreq$agglo+scorefreq$intersections
scorefreq$total=scorefreq$total/max(scorefreq$total)

#severite: type de collision (col), obstacle mobile touche, choc, agglo, grav
#on normalisera tout par le nombre dhabitant par departement pour avoir des resultats plus precis, avec la base de donnees 'populations'

#1: type de collision
#on nettoie les na
mcol=m[-which(is.na(m$col)),]
hist(mcol$col)
coll1=aggregate(mcol$col,by=list(mcol$dep),FUN=median)
names(coll1) = c('Dep','collisions')
#on ne garde que les variables a
col1 = which(mcol$col==1)
col2 = which(mcol$col==2)
col3 = which(mcol$col==3)
collisions=c(col1,col2,col3)

plot(table(mcol$dep[collisions])/(populations$`Population 2019`/100000),xlab="departement",ylab="nombre d'accidents entre 2 vehicules par personne par departement")
score_col = (table(mcol$dep[collisions])/(populations$`Population 2019`/100000))
score_col = score_col/max(score_col)

coll=as.data.frame(score_col)
names(coll)=c('Dep','score')
coll=merge(coll,coll1,by='Dep')

#le score de collisions est donne par:
scorecoll=subset(coll,select=c(Dep,score))
scorecoll$Dep=str_pad(scorecoll$Dep, 2, pad = "0")
scorecoll$score=scorecoll$score/max(scorecoll$score)


#2:obstacle mobile touche
#on ne va prendre en compte que les collisions avec des vehicules et des pietons car les autres seraient moins 'graves'
#on nettoie les na
mobsm=m[-which(is.na(m$obsm)),]
hist(mobsm$obsm)
obsm1=aggregate(mobsm$obsm,by=list(mobsm$dep),FUN=median)
names(obsm1) = c('Dep','obsm')
#on ne garde que les variables a
ob1 = which(mobsm$obsm==1)
ob2 = which(mobsm$obsm==2)
obs = c(ob1,ob2)

score_obsm = (table(mobsm$dep[obs])/(populations$`Population 2019`/100000))
score_obsm = score_obsm/max(score_obsm)

obsm=as.data.frame(score_obsm)
names(obsm)=c('Dep','score')
obsm=merge(obsm,obsm1,by='Dep')

#le score de collisions est donne par:
scoreobsm=subset(obsm,select=c(Dep,score))
scoreobsm$Dep=str_pad(scoreobsm$Dep, 2, pad = "0")
scoreobsm$score=scoreobsm$score/max(scoreobsm$score)

#3: choc, on ne va regarder que les tonneaux car represente la plus grosse gravite:
#on nettoie les na
mchoc=m[-which(is.na(m$choc)),]
hist(mchoc$choc)
choc1=aggregate(mchoc$choc,by=list(mchoc$dep),FUN=median)
names(choc1) = c('Dep','chocs')
#on ne garde que les variables a
cho = which(mchoc$choc==9)

score_choc = (table(mchoc$dep[cho])/(populations$`Population 2019`/100000))
score_choc = score_choc/max(score_choc)

choc=as.data.frame(score_choc)
names(choc)=c('Dep','score')
choc=merge(choc,choc1,by='Dep')

#le score de choc est donne par:
scorechoc=subset(choc,select=c(Dep,score))
scorechoc$Dep=str_pad(scorechoc$Dep, 2, pad = "0")
scorechoc$score=scorechoc$score/max(scorechoc$score)

#4: agglomeration, on reprend le score trouve pour la frequence:
#scoreagg

#5: grav: on ne va prendre que les personnes tuees et hospitalisees car les blesses legers ne coutent pas trop d'argent a l'assurance
#on nettoie les na
grav1=aggregate(m$grav,by=list(m$dep),FUN=median)
names(grav1) = c('Dep','gravite')
#on ne garde que les variables a
grav2 = which(m$grav==2) #les tues
grav3 = which(m$grav==3) #les hospitalises
gra = c(grav2,grav3)

score_grav = (table(m$dep[gra])/(populations$`Population 2019`/100000))
score_grav = score_grav/max(score_grav)

grav=as.data.frame(score_grav)
names(grav)=c('Dep','score')
grav=merge(grav,grav1,by='Dep')

#le score de gravite est donne par:
scoregrav=subset(grav,select=c(Dep,score))
scoregrav$Dep=str_pad(scoregrav$Dep, 2, pad = "0")
scoregrav$score=scoregrav$score/max(scoregrav$score)

scoresev=merge(scorecoll,scoreobsm,by='Dep')
names(scoresev)=c('Dep','collisions','obstacle mobile')
scoresev=merge(scoresev,scorechoc,by='Dep')
scoresev=merge(scoresev,scoreagg,by='Dep')
names(scoresev)=c('Dep','collisions','obstacle mobile','choc','agglomeration')
scoresev=merge(scoresev,scoregrav,by='Dep')
names(scoresev)=c('Dep','collisions','obstacle mobile','choc','agglomeration','gravite')

#on somme tous les scores puis on les normalisera:
scoresev$total=scoresev$collisions+scoresev$`obstacle mobile`+scoresev$choc+scoresev$agglomeration+scoresev$gravite
scoresev$total=scoresev$total/max(scoresev$total)

test=merge(scoresev,scorefreq,by='Dep')

#les scores par departement:
scoredep = merge(subset(scorefreq,select=c('Dep','total')),subset(scoresev,select=c('Dep','total')),by = 'Dep')
names(scoredep) = c('Dep','freq','sev')
###################Clusters###############################################
#nombre de clusters:
row.names(scoredep)=scoredep$Dep
row.names(scorefreq)=scorefreq$Dep
row.names(scoresev)=scoresev$Dep
#df3=test[,-c(1,7,12)]
df2=scoredep[,-1]
dffreq=scorefreq[,-c(1,6)]
dfsev=scoresev[,-c(1,7)]

#fviz_nbclust(df3, kmeans, method = "wss")
fviz_nbclust(df2, kmeans, method = "wss")
fviz_nbclust(dffreq, kmeans, method = "wss")    #3 ou 4 groupes
fviz_nbclust(dfsev, kmeans, method = "wss")     #2 ou 3 groupes


km <- kmeans(df2, centers = 3, nstart = 69)
km
fviz_cluster(km, data = df2)

#au total:
km <- kmeans(df2, centers = 5, nstart = 69)      #3 ou 5 clusters semblent biens
km
fviz_cluster(km, data = df2)

#pour la frequence:
clusterfreq = kmeans(dffreq, centers = 3, nstart = 2)   #3 clusters semblent mieux
clusterfreq
fviz_cluster(clusterfreq, data = dffreq)

#pour la severite:
clustersev = kmeans(dfsev, centers = 3, nstart = 69)   #3 clusters semblent mieux
clustersev
fviz_cluster(clustersev, data = dfsev)

#aggregate(df2, by=list(cluster=km$cluster), mean)


aggregate(dffreq, by=list(cluster=clusterfreq$cluster), mean)
aggregate(dfsev, by=list(cluster=clustersev$cluster), mean)
######################Ajout des donnees ONISR############################################
#modifier le pol insee code des bases de donnees precedentes en numeros de departements
Tableaudep=TabNA
Tableaudep$Dep=str_sub(Tableaudep$pol_insee_code,start=1,end=2)

clusterfr=as.data.frame(clusterfreq$cluster)
clusterseverite=as.data.frame(clustersev$cluster)

clusterfr$Dep=row.names(clusterfr)
clusterseverite$Dep=row.names(clusterfr)

clustersss=merge(clusterfr,clusterseverite,by='Dep')

Tableaudep=merge(Tableaudep,clusterfr,by='Dep')
Tableaudep=merge(Tableaudep,clusterseverite,by='Dep')
#Maintenant on a les variables qui nous donne le groupe de departement dans lequel se situe l'assure. On les renomme pour plus de clarete:
names(Tableaudep)[32]='ClusterFreq'
names(Tableaudep)[33]='ClusterSev'

#Pour sev_totale:
sevtotdep=sev_totale
sevtotdep$Dep=str_sub(sevtotdep$pol_insee_code,start=1,end=2)

clusterfr=as.data.frame(clusterfreq$cluster)
clusterseverite=as.data.frame(clustersev$cluster)

clusterfr$Dep=row.names(clusterfr)
clusterseverite$Dep=row.names(clusterfr)

sevtotdep=merge(sevtotdep,clusterfr,by='Dep')
sevtotdep=merge(sevtotdep,clusterseverite,by='Dep')
#Maintenant on a les variables qui nous donne le groupe de departement dans lequel se situe l'assure. On les renomme pour plus de clarete:
names(sevtotdep)[38]='ClusterFreq'
names(sevtotdep)[39]='ClusterSev'

#############################Ajout des variables departements#####################################
#On va ajouter les variables clusters aux glm deja etudies precedement et voir leurs impacts

modelefreqONISR = glm.nb(freq~vh_sale_end + vh_value + pol_coverage + vh_fuel + drv_drv2 + 
                              pol_sit_duration + pol_duration + vh_age + vh_weight + drv_age_lic1 + 
                              drv_age1 + pol_bonus + ClusterFreq,
                            data=Tableaudep
)
(anova(modelefreqONISR,test='Chisq'))
#on voit bien que ClusterFreq est une variable explicative du modele
qqnorm(log(modelefreqONISR$residuals));qqline(log(modelefreqONISR$residuals))


modelesevONISR = glm(formula = claim_amount ~ pol_coverage + vh_sale_end  + 
                             vh_type + vh_value + ClusterSev, family = Gamma(link = log), 
                           data = sevtotdep)
(anova(modelesevONISR,test='Chisq'))
qqnorm(log(modelesevONISR$residuals));qqline(log(modelesevONISR$residuals))
#La variables ClusterSev est explicative du modele
simuONISR  =  simulateResiduals(modelesevONISR,  n=250)
plotSimulatedResiduals(simuONISR)

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

###################Comparaison des primes pures:################################################
#Comparaison des primes pures:
summary(primepure$prime_pure)
summary(primepureONISR$prime_pure)

sum(primepure$prime_pure)
sum(primepure$prime_pure-primepureONISR$prime_pure)
mean(primepure$prime_pure-primepureONISR$prime_pure)

boxplot(primepure$prime_pure)
boxplot(primepureONISR$prime_pure)
###################classes les plus importantes dans la prime pure
#pour ONISR:
completONISR = merge(Tableaudep,primepureONISR,by='id_client')

tapply(completONISR$prime_pure, completONISR$ClusterFreq, mean) 
tapply(completONISR$prime_pure, completONISR$ClusterSev, mean) 
 
plot(tapply(completONISR$prime_pure, completONISR$pol_bonus, mean) ,ylab="prime pure")
plot(tapply(completONISR$prime_pure, completONISR$pol_coverage, mean),ylab="prime pure")          #bcp de diff
plot(tapply(completONISR$prime_pure, completONISR$pol_coverage, mean),ylab="prime pure")     #pol_coverage
tapply(completONISR$prime_pure, completONISR$pol_duration, mean)        #interessant
plot(tapply(completONISR$prime_pure, completONISR$pol_duration, mean),ylab="prime pure")      #pol duration
tapply(completONISR$prime_pure, completONISR$pol_sit_duration, mean)    #bcp de diff
plot(tapply(completONISR$prime_pure, completONISR$pol_sit_duration, mean),ylab="prime pure")         #polsit duration
tapply(completONISR$prime_pure, completONISR$pol_payd, mean)
plot(tapply(completONISR$prime_pure, completONISR$pol_usage, mean),ylab="prime pure")

tapply(completONISR$prime_pure, completONISR$drv_drv2, mean)#bcp de diff
plot(tapply(completONISR$prime_pure, completONISR$drv_sex1, mean),ylab="prime pure")    #2e conducteur
plot(tapply(completONISR$prime_pure, completONISR$drv_drv2, mean),ylab="prime pure")     #sexe
plot(tapply(completONISR$prime_pure, completONISR$drv_age1, mean),ylab="prime pure")    #age
plot(tapply(completONISR$prime_pure, completONISR$drv_age_lic1, mean),ylab="prime pure")#age license

tapply(completONISR$prime_pure, completONISR$vh_age, mean)      
plot(tapply(completONISR$prime_pure, completONISR$vh_age, mean) ,ylab="prime pure")       #vh age
plot(tapply(completONISR$prime_pure, completONISR$vh_cyl, mean) ,ylab="prime pure")       #cylindre   pas ouf
plot(tapply(completONISR$prime_pure, completONISR$vh_fuel, mean),ylab="prime pure")                               #fuel
plot(tapply(completONISR$prime_pure, completONISR$vh_sale_end, mean) ,ylab="prime pure")  #vh sale end
plot(tapply(completONISR$prime_pure, completONISR$vh_speed, mean) ,ylab="prime pure")     #interessant
plot(tapply(completONISR$prime_pure, completONISR$vh_type, mean),ylab="prime pure")                           #interessant
plot(tapply(completONISR$prime_pure, completONISR$vh_value, mean) ,ylab="prime pure")     #value
plot(tapply(completONISR$prime_pure, completONISR$vh_weight, mean) ,ylab="prime pure") 

plot(tapply(completONISR$prime_pure, completONISR$Dep, mean) ,ylab="prime pure") 

deppp=tapply(completONISR$prime_pure, completONISR$Dep, mean)
deppp=as.data.frame(deppp)
deppp$Dep=row.names(deppp)

col=findColours(classIntervals(var = deppp$deppp, n = 4,style="quantile" ),
                smoothColors("green","yellow","orange","red"))
leg = findColours(
  classIntervals(var = deppp$deppp, n = 4 , style="quantile"),
  smoothColors("green","yellow","orange","red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la gravite moyenne:
plot(dep)
j=0
for(i in deppp$Dep){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "Prime Pure moyenne :")



#avec 3 couleurs:

col=findColours(classIntervals(var = deppp$deppp, n = 5,style="quantile" ),
                smoothColors("green","yellow","orange","yellow","red"))
leg = findColours(
  classIntervals(var = deppp$deppp, n = 5 , style="quantile"),
  smoothColors("green","yellow","yellow","orange","red"),
  under="moins de", over="plus de", between="-", cutlabels=FALSE)

#on plot la map de France avec les donnees par departement sur la gravite moyenne:
plot(dep)
j=0
for(i in deppp$Dep){j=j+1; plot(dep[dep$dep==i,],lwd=2,col=col[j],add=T)}
legend("bottomleft",fill=attr(leg, "palette"),cex=0.75,
       legend=names(attr(leg,"table")),
       title = "Prime Pure moyenne :")


var(completONISR$prime_pure)
var(primepure$prime_pure)


sum(completONISR$prime_pure)
sum(primepure$prime_pure)


plot(tapply(completONISR$prime_pure, completONISR$ClusterFreq, mean) ,ylab="prime pure")
plot(tapply(completONISR$prime_pure, completONISR$ClusterSev, mean) ,ylab="prime pure")
