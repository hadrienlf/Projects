## MEMOIRE ACTUARIAT SUJET A8 ##
## TARIFICATION EN ASSURANCE IARD AVEC LES GLM EN INTEGRANT LES DONNEES ISSUES DE LA SECURITE ROUTIERE ##

## ELLIOT MULLER, NICOLAS WAGNER, HUGO SALLEZ ##


rm(list = ls())
library(CASdatasets)
library(MASS)

#### Commencent et creation des donnees ####
#Ne pas oublier d'activer les packages sp, zoo, xts, CASdatasets, MASS

#On observe les données de toutes les polices d'assurance et tous les sinistres
data(pg17trainpol)   
data(pg17trainclaim)
tp<-pg17trainpol
tc<-pg17trainclaim
data(pg17testyear1)
td<-pg17testyear1

#On résume les données des 100 000 polices d'assurance
summary(tp)

#L'objectif, dès le départ est de représenter dans tp, le nombre d'accidents par couple client-véhicule
#Une fois cela fait, on aura pour chaque véhicule, le nombre d'accidents
#Avec les infos relatives aux véhicules et au conducteur, nous pourrons dresser un GLM permettant de prévoir le nombre d'accidents 
#susceptibles de se produire en fonction des infos du véhicules et du conducteur 
#Par la suite, il faudra faire de même pour déterminer la sévérité de chaque sinistre i.e. ce que coutera un accident individuellement

#On commence par regrouper, dans les deux tableaux, les id_client et les id_vehicle
tc$id_policy <- paste(tc$id_client, tc$id_vehicle, sep="-")
tp$id_policy<-paste(tp$id_client, tp$id_vehicle, sep="-")

#ON crée ensuite tc2, une matrice de deux colonnes qui pour chaque couple client-véhicule renvoie le nombre de sinistres
tc2 <- aggregate(tc$claim_nb, by=list(tc$id_policy), sum)
names(tc2)<-c('id_policy','claim_nb')    #on renomme les colonnes pour fusionner sans problème

#On crée ensuite TAB, qui contient les 100 000 polices d'assurance, avec le couple client-véhicule, et le nombre de sinistres
TAB<-merge(tp,tc2,by='id_policy',all.x=TRUE)
TAB$claim_nb[is.na(TAB$claim_nb)]<-0

#On met en plus aux données de polices la survenue d'un accident ou non 
accident<-(tp$id_client %in% tc$id_client)
TAB<-cbind(TAB,accident)

#On observe les caractéristiques pour tous ceux qui ont eu un sinistre 
#Ce tableau permettra de déterminer la sévérité, il est donc à conserver
TabSin<-merge(tp,tc)
SumSin<-sum(abs(TabSin$claim_amount))     #On somme tous les sinistres, pour comparer par la suite (bcp plus tard) a la somme des primes pures
TabSin<- subset(TabSin, claim_amount > 0)

#On supprime la 840eme police d'assurance sinistree de TAB ET de TabSin car son vh_value est NA
TAB<-TAB[-840,]
t<-which(TabSin$id_client=="A00000765")
TabSin<-TabSin[-t,]

#On cree une fonction qui va convertir les codes postaux en departement
#Cela nous servira pour la suite avec l'ONISR pour faire des comparaisons par departement
departement<-function(X){
  ind_corse_sud<-grep("2A", as.character(X$pol_insee_code))
  ind_corse_nord<-grep("2B", as.character(X$pol_insee_code))
  X$pol_insee_code<-as.numeric(as.character(X$pol_insee_code))
  X$pol_insee_code<-trunc(X$pol_insee_code/1000)
  X$pol_insee_code[ind_corse_sud]<-20
  X$pol_insee_code[ind_corse_nord]<-20.5
  return(X)
}
TAB<-departement(TAB)  #On l'applique a TAB
TabSin<-departement(TabSin) #Et a TabSin


##### Numerisation et discretisation des variables (Frequence) ####

#Variables a 2 modalites, faciles a numeriser 
TAB$drv_sex1<-factor(TAB$drv_sex1,levels=c("M","F"),labels=c(0,1))
TAB$pol_payd<-factor(TAB$pol_payd,levels=c("No","Yes"),labels=c(0,1))
TAB$drv_drv2<-factor(TAB$drv_drv2,levels=c("No","Yes"),labels=c(0,1))

#On s'interesse maintenant a toutes les autres variables 
#Selon les donnees, on divisera les classes soit par deciles, soit par valeurs "trouvees a la main" 
# C'est le cas quand il n'y a pas assez de valeurs differentes par exemple

age.d<-TAB$drv_age1
age.d = cut(age.d, breaks=c(0,25,45,60,max(age.d)), include.lowest = TRUE)
summary(age.d) 

bonus.d<-TAB$pol_bonus
bonus.d = cut(bonus.d, breaks=c(0.5,0.505,0.55,1,1.5,2,max(bonus.d)), include.lowest = TRUE)
summary(bonus.d) 

duration.d<-TAB$pol_duration
breaksDuration<-quantile(duration.d,seq(0,1,0.1))
duration.d = cut(duration.d, breaks=breaksDuration, include.lowest = TRUE)
summary(duration.d) 

agelic.d<-TAB$drv_age_lic1
breaksAgeLic<-quantile(agelic.d,seq(0,1,0.1))
agelic.d = cut(agelic.d, breaks=breaksAgeLic, include.lowest = TRUE)
summary(agelic.d) 

SitDu.d<-TAB$pol_sit_duration
SitDu.d = cut(SitDu.d, breaks=c(0,1,2,3,5,max(SitDu.d)), include.lowest = TRUE)
summary(SitDu.d) 


#On fait ensuite une serie de plot pour trouver des tendances et regrouper en classes plus larges
#Cela permet soit d'eliminer des variables inutiles
#Soit de faire des classes plus larges, et donc de diminuer la dimension

par(mfrow=c(2,2))
plot(tapply(TAB$claim_nb, TAB$drv_drv2, mean),ylab="Frequence d'accidents",main="Presence ou non d'un deuxieme conducteur")
plot(tapply(TAB$claim_nb, age.d, mean),ylab="Frequence d'accidents",main="Age") #l'age a une influence
plot(tapply(TAB$claim_nb, agelic.d, mean),ylab="Frequence d'accidents",main="Age Licence") #l'age du permis a clairement une influence
plot(tapply(TAB$claim_nb, TAB$pol_coverage, mean),ylab="Frequence d'accidents",main="Pol Coverage") #On pourra s'en servir, la modalite peu representee sera dans la moyenne 
plot(tapply(TAB$claim_nb, bonus.d,mean), ylab="Frequence d'accidents",main="Bonus") #bonus ne semble pas avoir d'incidence
barplot(tapply(TAB$claim_nb, TAB$pol_pay_freq, mean),ylab="Frequence d'accidents",main="Pol Pay Freq") #On ne l'utilisera probablement pas #on peut en faisant deux modalites
barplot(tapply(TAB$claim_nb, TAB$vh_fuel, mean)[1:2],ylab="Frequence d'accidents",main="Vh Fuel") #Les vehicules diesel sont plus exposes, roulent plus car carburant moins cher
plot(tapply(TAB$claim_nb, TAB$pol_usage, mean),ylab="Frequence d'accidents",main="Pol Usage")   #On utilisera certainement pas pol_usage car les donnees sont mal reparties


#Ces resultats peuvent etre valides par un test du khi deux : 
#Par exemple, pour l'age du permis, on va considerer les catégories 25-29 ans, 29-33 ans, 45-50 ans 
#Et on effectue un test du Khi deux pour voir si elles sont ou non independantes

#Test du Khi-deux pour voir la relation entre 25-29 ans et 45-50
t3<-((TAB$drv_age_lic1<29 & TAB$drv_age_lic1>=25) | (TAB$drv_age_lic1<50 & TAB$drv_age_lic1>=45))
Nb_Sinistres<-TAB$drv_age_lic1[t3]
t1<-which(Nb_Sinistres>=45 & Nb_Sinistres<50)
t2<-which(Nb_Sinistres<29 & Nb_Sinistres>=25)
Nb_Sinistres[t1]<-"age de 45 a 50 ans"
Nb_Sinistres[t2]<-"age de 25 a 29 ans"
tab<-table(Nb_Sinistres,TAB$claim_nb[t3])
tab
chisq.test(Nb_Sinistres,TAB$claim_nb[t3])
#Le test donne une valeur de 12.623 (pour 4 degres de liberte), avec une p-value de 0.01327
#On rejette l'hypothese d'independance : avoir un permis bien plus vieux a une influence sur la sinistralite

#Test du Khi-deux pour voir la relation entre 25-29 ans et 29-33 ans
t3<-(TAB$drv_age_lic1<33 & TAB$drv_age_lic1>=25)
Nb_Sinistres<-TAB$drv_age_lic1[t3]
t1<-which(Nb_Sinistres>=29 & Nb_Sinistres<33)
t2<-which(Nb_Sinistres<29 & Nb_Sinistres>=25)
Nb_Sinistres[t1]<-"age de 29 a 33 ans"
Nb_Sinistres[t2]<-"age de 25 a 29 ans"
tab<-table(Nb_Sinistres,TAB$claim_nb[t3])
tab
chisq.test(Nb_Sinistres,TAB$claim_nb[t3])
#Le test donne une valeur de 3.2856 (pour 4 degres de liberte), avec une p-value de 0.05112
#On ne rejette pas l'hypothese d'independance : a ce stade il ne semble pas y avoir de difference, selon que l'on ait 25-29 ans ou 29-33 ans


## Apparte sur les covariables liees au vehicule dans la frequence ##
#Comme explique dans le rapport, la valeur du vehicule degage une tres forte tendance, on regarde la correlation avec les autres covariables liees au vehicule

#On plot le nb de sinistres en fonction de la valeur du vehicule pour voir la forte tendance
par(mfrow=c(1,1))
Value.d<-TAB$vh_value
breaksValue<-quantile(Value.d,seq(0,1,0.1))
Value.d = cut(Value.d, breaks=breaksValue,include.lowest = TRUE) 
summary(Value.d)

plot(tapply(TAB$claim_nb, Value.d, mean),ylab="Frequence d'accidents",main="Valeur du vehicule") 

#Test de V-Cramer, pour observer de façon numerique la relation entre les variables 

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  return(as.numeric(CV))
}

#Si la valeur du test est superieure a 0.3,la relation entre les covariables est consideree comme forte

vcramer<-c(cv.test(TAB$vh_value,TAB$vh_weight),cv.test(TAB$vh_value,TAB$vh_speed),cv.test(TAB$vh_value,TAB$vh_din),
           cv.test(TAB$vh_value,TAB$vh_cyl),cv.test(TAB$vh_value,TAB$vh_sale_begin),cv.test(TAB$vh_value,TAB$vh_sale_end),
           cv.test(TAB$vh_value,TAB$vh_type),cv.test(TAB$vh_value,TAB$vh_age))
plot(vcramer,ylim=c(0,1),xlim=c(0.5,8))
point.names <-c("Weight","Speed","Din","Cyl","Sale_Begin","Sale_end","Type","Age")
text(vcramer+0.07,point.names)
abline(0.3,0,type="l",col='red')
abline(0.2,0,type="l",col='green')

#Ici on obtient un vecteur de valeurs superieures a 0,7, la correlation est tres forte, on ne gardera donc que vh-Value pour la frequence
# Cela nous evite d'avoir a observer toutes les autres variables en plus


#### Garder les covariables importantes et les discretiser ####

#On ne garde finalement que les covariables qui semblent a priori avoir une importance dans l'explication de la frequence
# (Voir le rapport pour des explication plus precises)
# On garde egalement les variables liees au vehicule car elles serviront dans le calcul de la prime pure (qui depend de la frequence ET de la severite)
garder<-c("id_policy","pol_coverage","pol_pay_freq","drv_drv2","drv_sex1","drv_age_lic1","vh_age","vh_din","vh_fuel",
          "vh_sale_end","vh_type","vh_value","claim_nb","accident","pol_insee_code")
TAB<-subset(TAB,select=garder)


#Maintenant, on cree les classes donnees par les graphiques avec les variables qu'il nous reste
#Pour chaque classe, on cree une colonne : l'individu aura 0 s'il n'appartient pas a la classe, 1 sinon

#Pol Coverage
pol_coverage_Median<-(TAB$pol_coverage=="Median1" | TAB$pol_coverage=="Median2")  #Median1 et Median2 sont regroupes car se comportent pareil
pol_coverage_Mini<-(TAB$pol_coverage=="Mini")
TAB<-cbind(TAB,pol_coverage_Median,pol_coverage_Mini)
TAB$pol_coverage_Median<-factor(TAB$pol_coverage_Median,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$pol_coverage_Mini<-factor(TAB$pol_coverage_Mini,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$pol_coverage<-NULL   #On supprime la colonne initiale, devenue inutile

#Pol Pay Freq
TAB$pol_pay_freq<-(TAB$pol_pay_freq=="Biannual" | TAB$pol_pay_freq=="Yearly")   # 2 groupes : Biannual et Yearly vs Monthly et Quarterly
TAB$pol_pay_freq<-factor(TAB$pol_pay_freq,levels=c(FALSE,TRUE),labels=c(0,1))

#Drv age lic
age_lic_1_20<-(TAB$drv_age_lic1>=1 & TAB$drv_age_lic1<=20)   #3 groupes : 1 a 20, 20 a 36 ans, 36 ans et plus
age_lic_36etplus<-(TAB$drv_age_lic1>36)     #La variable la plus representee est (20,36], elle sert de reference donc vaudra 0
TAB<-cbind(TAB,age_lic_1_20,age_lic_36etplus)  #On ne se donne pas la peine de faire une nouvelle colonne
TAB$age_lic_1_20<-factor(TAB$age_lic_1_20,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$age_lic_36etplus<-factor(TAB$age_lic_36etplus,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$drv_age_lic1<-NULL  #Suppression de la colonne

#Valeur du vehicule
value9600<-(TAB$vh_value<9600)
value9600_16200<-(TAB$vh_value>=9600 & TAB$vh_value<16200)
TAB<-cbind(TAB,value9600,value9600_16200)
TAB$value9600<-factor(TAB$value9600,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$value9600_16200<-factor(TAB$value9600_16200,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$vh_value<-NULL

#Le carburant (le fait qu'on le garde alors qu'elle est liee au vehicule est expliquee dans le rapport)
TAB$vh_fuel<-(TAB$vh_fuel=="Gasoline" | TAB$vh_fuel=="Hybrid")   # 2 groupes : Diesel ou non Diesel
TAB$vh_fuel<-factor(TAB$vh_fuel,levels=c(FALSE,TRUE),labels=c(0,1))


#### Variables a numeriser et discretiser (Severite) ####

#On supprime de TabSin les variables inutiles a l'etude
TabSin<-TabSin[!colnames(TabSin)%in%c("drv_age1","drv_sex1","drv_sex1","drv_age_lic1","drv_age2","drv_sex2",
                                      "drv_age_lic2","drv_drv2","pol_bonus","pol_coverage","pol_duration",
                                      "pol_sit_duration","pol_pay_freq","pol_payd","pol_usage",
                                      "claim_nb","id_claim","id_client","id_vehicle","id_year")]

#D'apres les infos donnees par CasDataset, din, cylinder, speed et value sont hautement correles
#Ces resultats sont confirmes par l'ACP (voir rapport)

din.d<-TabSin$vh_din
breaksDin<-quantile(din.d,seq(0,1,0.1))
din.d = cut(din.d, breaks=breaksDin, include.lowest = TRUE)
summary(din.d)

cyl.d<-TabSin$vh_cyl
breaksCyl<-quantile(cyl.d,seq(0,1,0.1))
cyl.d = cut(cyl.d, breaks=breaksCyl, include.lowest = TRUE)
summary(cyl.d)

speed.d<-TabSin$vh_speed
breaksSpeed<-quantile(speed.d,seq(0,1,0.1))
speed.d = cut(speed.d, breaks=breaksSpeed, include.lowest = TRUE)
summary(speed.d)

value.d<-TabSin$vh_value
breaksValue<-quantile(value.d,seq(0,1,0.1))
value.d = cut(value.d, breaks=breaksValue, include.lowest = TRUE)
summary(value.d)

#On fait des plots de meme qu'avec la frequence pour observer des tendances
par(mfrow= c(2,2))
plot(tapply(TabSin$claim_amount, din.d, mean) , ylab="Montant moyen" , main="Montant moyen selon Puissance Moteur")
plot(tapply(TabSin$claim_amount, cyl.d, mean) , ylab="Montant moyen" , main="Montant moyen selon Cylindre")
plot(tapply(TabSin$claim_amount, speed.d, mean) , ylab="Montant moyen" , main="Montant moyen selon Vitesse")
plot(tapply(TabSin$claim_amount, value.d, mean) , ylab="Montant moyen",main="Montant moyen selon Valeur")


weight.d<-TabSin$vh_weight
breaksWeight<-quantile(weight.d,seq(0,1,0.1))
weight.d = cut(weight.d, breaks=breaksWeight, include.lowest = TRUE)
summary(weight.d)

vhage.d<-TabSin$vh_age
breaksVhage<-quantile(vhage.d,seq(0,1,0.1))
vhage.d = cut(vhage.d, breaks=breaksVhage, include.lowest = TRUE)
summary(vhage.d)

sb.d<-TabSin$vh_sale_begin
breaksSaleBegin<-quantile(sb.d,seq(0,1,0.1))
sb.d = cut(sb.d, breaks=breaksSaleBegin, include.lowest = TRUE)
summary(sb.d)

se.d<-TabSin$vh_sale_end
breaksSaleEnd<-quantile(se.d,seq(0.1,1,0.1))    #Commence a 0.1 car 1 est le 1er ET le 2eme decile
se.d = cut(se.d, breaks=breaksSaleEnd, include.lowest = TRUE)
summary(se.d)


par(mfrow= c(2,2))
plot(tapply(TabSin$claim_amount, weight.d, mean) , ylab="Montant moyen",main="Montant moyen selon Masse")
plot(tapply(TabSin$claim_amount, vhage.d, mean) , ylab="Montant moyen",main="Montant moyen selon Age du Vehicule")
plot(tapply(TabSin$claim_amount, sb.d, mean) , ylab="Montant moyen",main="Montant moyen selon Sale Begin")
plot(tapply(TabSin$claim_amount, se.d, mean) , ylab="Montant moyen",main="Montant moyen selon Sale End")

plot(tapply(TabSin$claim_amount, TabSin$vh_fuel, mean) , ylab="Montant moyen",main="Montant moyen selon Carburant")  #trop faibles differences
plot(tapply(TabSin$claim_amount, TabSin$vh_type, mean) , ylab="Montant moyen",main="Montant moyen selon Type")  #Clairement une difference

#Observations graphiques pour la severite : 

#Comme la Puissance Moteur, la cylindree, la valeur et la vitesse sont correlees d'apres les infos du CASDataset,
#On ne gardera que la puissance du moteur --> 2 modalites : <=120 ou >120

TabSin$vh_value<-NULL
TabSin$vh_speed<-NULL
TabSin$vh_cyl<-NULL

#Difficile de decrire une tendance avec la masse du vehicule 
TabSin$vh_weight<-NULL
#Clairement une tendance avec vh_sale_begin et vh_sale_end mais elles sont probablement correlees 
#On peut distinguer une tendance avec l'age du véhicule egalement, un vehicule plus vieux occasionnera des accidents moins couteux

#Enfin, les differences de montant avec le fuel sont faibles, contrairement au type de vehicule
#Cependant, comme le modele n'est pas tres fourni, on decide de garder cette variable

TabSin$vh_model<-NULL
TabSin$vh_make<-NULL

cv.test(TabSin$vh_sale_begin,TabSin$vh_sale_end)  # =0.57
#Sale begin est tres correlee avec sale end, cf test de V Cramer + ACP
TabSin$vh_sale_begin<-NULL

#Type du vehicule
TabSin$vh_type<-(TabSin$vh_type=="Commercial")   
TabSin$vh_type<-factor(TabSin$vh_type,levels=c(FALSE,TRUE),labels=c(0,1))

#Puissance du vehicule
TabSin$vh_din<-(TabSin$vh_din>120)   
TabSin$vh_din<-factor(TabSin$vh_din,levels=c(FALSE,TRUE),labels=c(0,1))

#Age du vehicule
TabSin$vh_age<-(TabSin$vh_age>11)   
TabSin$vh_age<-factor(TabSin$vh_age,levels=c(FALSE,TRUE),labels=c(0,1))

#Carburant du vehicule
TabSin$vh_fuel<-(TabSin$vh_fuel=="Gasoline" | TabSin$vh_fuel=="Hybrid")
TabSin$vh_fuel<-factor(TabSin$vh_fuel,levels=c(FALSE,TRUE),labels=c(0,1))

#Fin de la campagne marketing
sale_end_6_11<-(TabSin$vh_sale_end>6 & TabSin$vh_sale_end<=11)   
sale_end_11_plus<-(TabSin$vh_sale_end>11)    
TabSin<-cbind(TabSin,sale_end_6_11,sale_end_11_plus)
TabSin$sale_end_6_11<-factor(TabSin$sale_end_6_11,levels=c(FALSE,TRUE),labels=c(0,1))
TabSin$sale_end_11_plus<-factor(TabSin$sale_end_11_plus,levels=c(FALSE,TRUE),labels=c(0,1))
TabSin$vh_sale_end<-NULL   


#### ETAPE GLM-FREQUENCE : On commence par tout numeriser et creer notre base d'apprentissage ####

#Numerisation de toutes nos variables explicatives

TAB$pol_pay_freq<-as.numeric(TAB$pol_pay_freq)-1  #Les modalites 1 et 2 doivent devenir 0 et 1
TAB$drv_sex1<-as.numeric(TAB$drv_sex1)-1          #On fait donc -1 a chaque fois
TAB$pol_coverage_Median<-as.numeric(TAB$pol_coverage_Median)-1
TAB$pol_coverage_Mini<-as.numeric(TAB$pol_coverage_Mini)-1
TAB$age_lic_1_20<-as.numeric(TAB$age_lic_1_20)-1
TAB$age_lic_36etplus<-as.numeric(TAB$age_lic_36etplus)-1
TAB$drv_drv2<-as.numeric(TAB$drv_drv2)-1
TAB$value9600<-as.numeric(TAB$value9600)-1
TAB$value9600_16200<-as.numeric(TAB$value9600_16200)-1
TAB$vh_fuel<-as.numeric(TAB$vh_fuel)-1

#Pour les GLM, on doit expliquer le nombre d'accidents 
#Deux choix : Poisson ou Binomiale negative

#Tentative Poisson Forward Backward
#FB
modelefreq_poiss<-glm(formula=claim_nb~1,family=poisson(link=log),data=TAB)

#select.variables.stepwise=step(modelefreq_poiss, scope=~vh_fuel + pol_pay_freq 
                               #+ drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 
                               #+ age_lic_36etplus + value9600 + value9600_16200 + drv_drv2, direction="both",data=TAB)

#FB garde dans cet ordre : pol_coverage_Mini + pol_coverage_Median + vh_fuel + value9600 + value9600_16200 
#+ pol_pay_freq + age_lic_36etplus + age_lic_1_20 + drv_sex1 + drv_drv2

#On crée maintenant notre base d'apprentissage
d_freq<-sort(sample(nrow(TAB),nrow(TAB)*0.8,replace=FALSE)) #On prend 80% des donnees 
appren<-TAB[d_freq,]
test<-TAB[-d_freq,]
summary(appren)
summary(test)

#On recree modelefreq_poiss avec les covariables gardees par le forward backward sur l'apprentissage
modelefreq_poiss<-glm(formula=claim_nb~vh_fuel + pol_pay_freq 
                      + drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                        age_lic_36etplus + value9600 + value9600_16200 + drv_drv2, family=poisson(link=log),data=appren)
summary(modelefreq_poiss)
#La p-value pour la variable drv_drv2 est superieure a 0.05 
#On suppose que cette variable n'influe pas le modele, et on la retire
modelefreq_poiss<-glm(formula=claim_nb~vh_fuel + pol_pay_freq 
                      + drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                        age_lic_36etplus + value9600 + value9600_16200,family=poisson(link=log),data=appren)
summary(modelefreq_poiss)

#Avec l'anova on teste le modele sans variables
modelefreq_poiss.0<-glm(claim_nb ~1,family=poisson(link=log),data=appren)
anova(modelefreq_poiss.0,modelefreq_poiss, test="Chisq")
#On rejette le modele de nullite des parametres


#La fonction predict nous donne alors les estimations de frequences de sinistres sur la base d'apprentissage
FREQ<-predict(modelefreq_poiss,newdata=appren,level=0.95,type='response')

#On teste les resultats sur la base de validation
valid<-predict(modelefreq_poiss,newdata=test,level=0.95,type='response')


#Moyen de verification : on compare le nb de sinistres prevus par le modele avec le nb de sinistres ayant effectivement eu lieu
# C'est une façon de tester la vraisemblance du modele dans le cas d'une regression de Poisson
abs(sum(test$claim_nb)-sum(valid))   #erreur absolue
abs(sum(test$claim_nb)-sum(valid))/sum(test$claim_nb)*100   #erreur relative (en pourcentage)
#On obtient des valeurs finales relativement proches de celles observees, ce premier modele est concluant
(rmse=mean((test$claim_nb - valid)^2))

#On procede a un test du Khi-deux pour voir neanmoins si les donnees sont distribuees comme une loi de Poisson
tirage<-rpois(19999,valid)   #19 999 car 19 999 observations dans notre base de validation
ti<-test$claim_nb
for (i in 1:length(tirage)){
  if (ti[i]>max(tirage)){
    ti[i]=max(tirage)
  }
}
cpt1<-table(tirage)
cpt2<-table(ti)
chisq.test(cpt2,p=cpt1,rescale.p = TRUE)
#Les donnees ne sont pas distribuees comme une loi de Poisson (p-value < 0.05), c'etait un resultat previsible
#L'autre modele que l'on peut donc envisager est le modele binomial negatif, caracteristique d'une situation de sur-dispersion

#On regarde la dispersion des donnees, pour chaque classe
dispersion<-function(x){
  m1<-mean(TAB$claim_nb[which(x==1)])
  v1<-var(TAB$claim_nb[which(x==1)])
  m2<-mean(TAB$claim_nb[which(x==0)])
  v2<-var(TAB$claim_nb[which(x==0)])
  return(list(moyenne=m1,variance=v1,moyenne2=m2,variance2=v2))
}
par(mfrow=c(1,1))
a<-c(dispersion(TAB$pol_pay_freq),dispersion(TAB$vh_fuel),dispersion(TAB$age_lic_1_20),
     dispersion(TAB$age_lic_36etplus),dispersion(TAB$pol_coverage_Median),dispersion(TAB$pol_coverage_Mini),
     dispersion(TAB$value9600),dispersion(TAB$value9600_16200))

plot(a[seq(1,32,2)],a[seq(2,32,2)],xlab='moyennes',ylab='variances',main="Etude de la dispersion des donnees")
abline(0,1,col='blue')

#On constate effectivement une legere surdispersion, on va donc envisager un modele binomial negatif 

# Tentative de binomiale negative forward backward
# La procedure est la meme  
modelefreq<-glm.nb(formula=claim_nb~1,data=appren)

#Backward Forward sur le tableau TAB entier
#select.variables.stepwise=step(modelefreq, scope=~vh_fuel + pol_pay_freq 
                               #+ drv_sex1 + pol_coverage_Median + pol_coverage_Mini +  age_lic_1_20 
                               #+ age_lic_36etplus + value9600 + value9600_16200 + drv_drv2, direction="both",data=TAB)

#Le FB garde dans cet ordre : pol_coverage_Mini + pol_coverage_Median + vh_fuel + value9600 + value9600_16200 
#+ pol_pay_freq + age_lic_36etplus + age_lic_1_20 + drv_sex1 + drv_drv2
#ie toutes les variables

#On teste donc le GLM avec toutes les variables
modelefreq<-glm.nb(formula=claim_nb~vh_fuel + pol_pay_freq +
                     drv_sex1 +drv_drv2 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                     age_lic_36etplus + value9600 + value9600_16200 + drv_drv2,data=appren)
summary(modelefreq)

# p_value > 0.05 pour drv_drv2, on la retire 
modelefreq<-glm.nb(formula=claim_nb~vh_fuel + pol_pay_freq +
                     drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                     age_lic_36etplus + value9600 + value9600_16200 ,data=appren)
summary(modelefreq)

modelefreq.0<-glm(claim_nb ~1,family=poisson(link=log),data=appren)
anova(modelefreq.0,modelefreq, test="Chisq")
#On rejette le modele de nullite des parametres

#On recalcule nos predictions d'accidents
FREQnb<-predict(modelefreq,newdata=appren,level=0.95,type='response')
validnb<-predict(modelefreq,newdata=test,level=0.95,type='response')
(rmse=mean((test$claim_nb - validnb)^2))

#On constate que les modeles sont sensiblement equivalents, on peut d'ailleurs mesures la distance qui les separe l'un de l'autre
#Le modele Bin Neg a un critere AIC legerement plus faible, mais met plus de temps a s'executer
sum(abs(FREQ-FREQnb))
sum((FREQ-FREQnb)**2)

#Enfin, pour ce modele qu'on privilegiera, on peut observer les residus par classe
#On cree pour cela une fonction residual
residual<-function(x){
  t1<-which(x==1)
  t2<-which(x==0)
  tt1<-(test$claim_nb[t1]-validnb[t1])/sqrt(validnb[t1])
  tt2<-(test$claim_nb[t2]-validnb[t2])/sqrt(validnb[t2])
  return(list(mean(tt1),mean(tt2)))
}
a<-c(residual(test$age_lic_1_20),residual(test$age_lic_36etplus),residual(test$vh_fuel),
     residual(test$pol_pay_freq),residual(test$pol_coverage_Median),residual(test$pol_coverage_Mini),
     residual(test$drv_sex1), residual(test$value9600),residual(test$value9600_16200))
a<-unlist(a)
plot(a,ylim=c(-0.1,0.1),main="Moyenne des résidus par classe")
point.names <-c("Age","Age","Age","Age","Fuel","Fuel","Freq","Freq","Cov",
                "Cov","Cov","Cov","Sex","Sex","Valeur","Valeur","Valeur","Valeur")
text(a+0.06,point.names)
#La caracterisitque avec les plus gros residus est celle du sexe
#Cela s'explique probablement par le fait que c'est la caracteristique la moins influente

#On represente ensuite un nuage de points des residus de Pearson et des residus de déviance
res1<-(test$claim_nb-validnb)/sqrt(validnb) #Résidus de Pearson
ni<-test$claim_nb
res2<-sign(ni-validnb)*sqrt(2*((ni*log(max(1,ni)/validnb))-(ni-validnb))) #Résidus de déviance

#1er plot
par(mfrow=c(1,1))
plot(res1, type = "p", cex = 0.5, main = "Résidus de Pearson", 
     col = "springgreen2", ylim = c(-3, 3))
abline(h = c(-2, 2), col = "red")

#2nd plot
plot(res2, type = "p", cex = 0.5, main = "Résidus de déviance", 
     col = "springgreen2", ylim = c(-3, 3))
abline(h = c(-2, 2), col = "red")
#Certains residus sont superieurs a 2, souligne le fait que l'analyse des residus n'a qu'un interet limite lorsque
#la variable a expliquer ne prend que quelques valeurs 

#Donc on se tourne vers une binomiale negative, on va garder les coefficients de ce modele
coeffFreq<-coefficients(modelefreq) 

# on recree modelefreq avec 100% des donnees, le but est de determiner une prime pure pour l'annee 2018
#Maintenant que notre modele est assure performant (cf les graphes de vraisemblance dans le rapport), plus besoin de base de validation
modelefreq<-glm.nb(formula=claim_nb~vh_fuel + pol_pay_freq +
                     drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                     age_lic_36etplus + value9600 + value9600_16200 ,data=TAB)
summary(modelefreq)
coeffFreq<-coefficients(modelefreq)
FREQnb<-predict(modelefreq,newdata=TAB,level=0.95,type='response')

# On va identifier les classes pour lesquelles la proba d'avoir un accident est la plus faible
# On recree un tableau a part avec nos variables et nos estimations de frequence
class<-cbind(TAB$vh_fuel , TAB$pol_pay_freq , TAB$drv_sex1 ,
             TAB$pol_coverage_Median , TAB$pol_coverage_Mini , TAB$age_lic_1_20 ,
             TAB$age_lic_36etplus , TAB$value9600 , TAB$value9600_16200 , round(FREQnb,5))
colnames(class)<-c("vh_fuel" , "pol_pay_freq" , "drv_sex1" ,
                   "pol_coverage_Median" , "pol_coverage_Mini" , "age_lic_1_20" ,
                   "age_lic_36etplus" , "value9600" , "value9600_16200" , "Frequence")
class<-data.frame(class)
#Nous avons 95 valeurs differentes pour lambda : 95 classes d'usagers
#On regroupe les differentes classes d'usagers
class<-cbind(aggregate(. ~ Frequence, class,mean)[1],round(aggregate(. ~ Frequence, class,mean)[,-1],1))
# La classe la plus a risque : véhicule cher, homme, paiement haute freq, diesel 

#On affiche la proba de non accident pour les classes extremes
theta<-modelefreq$theta
dnbinom(0,size=theta,prob=theta/(theta+class$Frequence[c(1,2,3,length(class[,1])-2,length(class[,1])-1,length(class[,1]))]))


#### Etape GLM - SEVERITE #### 

#Numerisation de toutes nos variables explicatives
TabSin$vh_age<-as.numeric(TabSin$vh_age)-1   #De meme, les modalites ici sont 1 ou 2 
TabSin$vh_din<-as.numeric(TabSin$vh_din)-1   #Comme on veut 0 ou 1, on fait -1
TabSin$vh_fuel<-as.numeric(TabSin$vh_fuel)-1
TabSin$vh_type<-as.numeric(TabSin$vh_type)-1
TabSin$sale_end_6_11<-as.numeric(TabSin$sale_end_6_11)-1
TabSin$sale_end_11_plus<-as.numeric(TabSin$sale_end_11_plus)-1


# On supprime les sinistres anormalement eleves (on les traitera a part)
q<-quantile(TabSin$claim_amount,seq(0,1,0.01))
t1<-which(TabSin$claim_amount<q[100])     
t2<-TabSin[-t1,]    #on garde les "catastrophes" de cote (1% des plus gros sinistres)
TabSin<-TabSin[t1,]
q1<-length(t2[,1])/(length(t2[,1])+length(TabSin[,1]))    #On calcule la probabilite d'avoir une "catastrophe"
EC<-mean(t2$claim_amount)          #On calcule le montant moyen d'une "catastrophe"

#On lance une approche Backward-Forward
modelesev<-glm(claim_amount~1,family=Gamma(link=log),data=TabSin)

#select.variables.stepwise=step(modelesev, scope=~ vh_din + vh_age  
                               #+ vh_type + sale_end_6_11 + sale_end_11_plus + vh_fuel ,direction="both", data=TabSin)

#Le FB garde dans cet ordre : vh_age + vh_type + sale_end_6_11 + sale_end_11_plus + vh_fuel + vh_din
#ie garde toutes les variables

#On crée maintenant notre base d'apprentissage et notre base de test
d_sev<-sort(sample(nrow(TabSin),nrow(TabSin)*0.8,replace=FALSE))
appren<-TabSin[d_sev,]
test<-TabSin[-d_sev,]
summary(appren)
summary(test)


#On recree modelesev avec les covariables gardees par le forward backward
modelesev<-glm(claim_amount~vh_din + vh_age + vh_type + sale_end_6_11 + sale_end_11_plus + vh_fuel ,family=Gamma(link=log),data=appren)
summary(modelesev)
#Toutes les p-values sont <0.05, on garde toutes les variables

modelesev.0<-glm(claim_amount ~1,family=Gamma(link=log),data=appren)
anova(modelesev.0,modelesev, test="Chisq")
#On rejette le modele de nullite des parametres


#On predit nos donnees de la meme facon qu'avec la frequence
SEV<-predict(modelesev,newdata=appren,level=0.95,type='response')
validsev<-predict(modelesev,newdata=test,level=0.95,type='response')
summary(SEV)

#On plot un graphe des residus pour ce modele Gamma 
residualSev<-function(X){
  t1<-which(X==1)
  t2<-which(X==0)
  tt1<-(modelesev$residuals[t1])
  tt2<-(modelesev$residuals[t2])
  return(list(sum(tt1),sum(tt2)))
}
a<-c(residualSev(appren$vh_din),residualSev(appren$vh_age),residualSev(appren$vh_type),residualSev(appren$sale_end_11_plus),residualSev(appren$vh_fuel))
a<-unlist(a)
plot(a,ylim=c(min(a)-0.00015,max(a))+0.00015,main="Somme des résidus par classe")
point.names <-c("Puissant","Peu Puissant","Agé","Peu Agé","Commercial","Tourisme","Ancien","Nouveau","Essence","Diesel")
text(a+0.0001,point.names)  #Les plus gros residus viennent de la date de la fin de campagne marketing

#On montre ici qu'un modele lognormal se differencie par la distribution des residus, mais ne garde pas toutes les variables par ailleurs
par(mfrow=c(1,2))
plotRes<-function(a,b,c,d,e,f,modele){   #Cette function va afficher les residus
  t1<-which(appren$vh_age==a & appren$vh_din==b & appren$vh_fuel==c & appren$vh_type==d & appren$sale_end_6_11==e &appren$sale_end_11_plus==f)
  return(modele$residuals[t1])
}
hist(plotRes(0,0,0,0,1,0,modelesev),main="Résidus des individus d'une même classe, modèle Gamma")
modelesev.lognorm<-glm(log(claim_amount)~vh_din + vh_age + vh_type + sale_end_6_11 + sale_end_11_plus + vh_fuel ,family=gaussian,data=appren)
summary(modelesev.lognorm)   #On teste un modele lognormal
hist(plotRes(0,0,0,0,1,0,modelesev.lognorm),main="Résidus des individus d'une même classe, modèle log normal")
par(mfrow=c(1,1))

#Si notre outil est proche de 0, le modele fait de bonnes predictions
outil<-sum(1-test$claim_amount/validsev)

#On trouve des sinistres de cout moyen de 890 (le moyenne devrait etre de 1000,
# mais n'oublions pas que nous avons retire les catastrophes)
#Le modele est concluant, on en garde donc les coefficients
coeffsev<-coefficients(modelesev)

# On recree modelesev avec 100% des donneees
modelesev<-glm(claim_amount~vh_din + vh_age + vh_type + sale_end_6_11 + sale_end_11_plus + vh_fuel 
               ,family=Gamma(link=log),data=TabSin)
summary(modelesev)
coeffsev<-coefficients(modelesev) #Nos coefficients
SEVval<-predict(modelesev,newdata=TabSin,level=0.95,type='response') #Nos predictions

# On va identifier les classes pour lesquelles les accidents sont les plus couteux
# On recree un tableau a part avec nos variables et nos estimations de severite
class<-cbind(appren$vh_din , appren$vh_age , appren$vh_type ,
             appren$sale_end_6_11 , appren$sale_end_11_plus , appren$vh_fuel , SEV)
colnames(class)<-c("vh_dinl" , "vh_age" , "vh_type" ,"sale_end_6_11" , "sale_end_11_plus" , "vh_fuel", "Montant")
class<-data.frame(class)

class<-cbind(aggregate(. ~ Montant, class,mean)[1],round(aggregate(. ~ Montant, class,mean)[,-1],1))
# La classe la plus a risque : moteur puissant, vh peu age, vh de tourisme, en vente depuis peu, roulant a l'essence


#### Calcul de la prime pure ####


#Le calcul de la prime pure va s'effectuer sur TAB, il faut donc y discretiser les variables liees a la severite
#Que l'on avait laissees a cet effet au moment du traitement de TAB

#Puissance
TAB$vh_din<-(TAB$vh_din>120)   
TAB$vh_din<-factor(TAB$vh_din,levels=c(FALSE,TRUE),labels=c(0,1))

#Age du vehicule
TAB$vh_age<-(TAB$vh_age>11)   
TAB$vh_age<-factor(TAB$vh_age,levels=c(FALSE,TRUE),labels=c(0,1))

#Type de vehicule
TAB$vh_type<-(TAB$vh_type=="Commercial")   
TAB$vh_type<-factor(TAB$vh_type,levels=c(FALSE,TRUE),labels=c(0,1))

#Fin de campagne marketing
sale_end_6_11<-(TAB$vh_sale_end>6 & TAB$vh_sale_end<=11)   
sale_end_11_plus<-(TAB$vh_sale_end>11)    
TAB<-cbind(TAB,sale_end_6_11,sale_end_11_plus)
TAB$sale_end_6_11<-factor(TAB$sale_end_6_11,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$sale_end_11_plus<-factor(TAB$sale_end_11_plus,levels=c(FALSE,TRUE),labels=c(0,1))
TAB$vh_sale_end<-NULL 

TAB$vh_din<-as.numeric(TAB$vh_din)-1
TAB$vh_age<-as.numeric(TAB$vh_age)-1
TAB$vh_type<-as.numeric(TAB$vh_type)-1
TAB$sale_end_6_11<-as.numeric(TAB$sale_end_6_11)-1
TAB$sale_end_11_plus<-as.numeric(TAB$sale_end_11_plus)-1

TAB$vh_fuel_sev<-TAB$vh_fuel    #Elle sert pour la freq et la sev, on fait la distinction

#Le calcul de la prime pure, par son caractere atypique doit cette fois se faire à la main (cf 4.5 du rapport)

COEFF<-c(coeffFreq,coeffsev)

MAT<-cbind(TAB$vh_fuel, TAB$pol_pay_freq, TAB$drv_sex1, TAB$pol_coverage_Median , TAB$pol_coverage_Mini , 
           TAB$age_lic_1_20 , TAB$age_lic_36etplus,TAB$value9600,TAB$value9600_16200)
MAT<-cbind(c(rep(1,nrow(MAT))),MAT,rep(1,nrow(MAT)))
MAT<-cbind(MAT,TAB$vh_din, TAB$vh_age, TAB$vh_type, TAB$sale_end_6_11 , TAB$sale_end_11_plus, TAB$vh_fuel_sev)

#Les coeffs du vecteur COEFF sont bien dans le meme ordre que les colonnes de MAT (ie chaque coeff va bien avec sa variable
#associee donc on peut effectuer le produit)

#Il ne faut pas oublier que l'on doit additionner la probabilite pour l'assure de causer un sinistre grave, on a donc
FREQnb<-predict(modelefreq,newdata=TAB,level=0.95,type='response')
a<-c(as.vector(FREQnb))   #Probabilite de causer un sinistre pour l'assure i 
a<-q1*EC*a     #Probabilite de causer une catastrophe*montant de la catastrophe (cf 4.5)
PRIMEPURE<-MAT%*%COEFF
PRIMEPURE<-exp(PRIMEPURE) + a
summary(PRIMEPURE)

#Comparaison du montant total de prime pure predit et du vrai montant de sinistre
sum(TabSin$claim_amount)+sum(t2$claim_amount)
sum(PRIMEPURE)

#On observe les classes pour lesquelles la prime pure est la plus elevee/faible
class<-cbind(TAB$vh_fuel, TAB$pol_pay_freq , TAB$drv_sex1 ,TAB$pol_coverage_Median , TAB$pol_coverage_Mini , 
             TAB$age_lic_1_20 ,TAB$age_lic_36etplus , TAB$value9600 , TAB$value9600_16200 ,
              TAB$vh_age, TAB$vh_din , TAB$vh_type ,  TAB$sale_end_6_11 , TAB$sale_end_11_plus,PRIMEPURE)
colnames(class)<-c("Fuel","Pay_Freq","Sexe","Cov_Median","Cov_Mini","Age_Lic_1_20","Age_Lic_36+","Value9600-",
                    "Value9600_16200", "Age_vh","Puiss_Vh", "Type_vh" ,"SaleEnd6-11" , "SaleEnd11+","PrimePure")
class<-data.frame(class)

class<-cbind(aggregate(. ~ PrimePure, class,mean)[1],round(aggregate(. ~ PrimePure, class,mean)[,-1],1))

#Enfin, nous allons par police, renseigner la prime pure, et le departement
#Cela nous servira pour les comparaisons avec l'ONISR
PP<-cbind(TAB$id_policy, TAB$pol_insee_code,round(PRIMEPURE,2))
PP<-PP[order(PP[,2]),]


#### Données de l'ONISR ####

par(mfrow=(c(1,1)))
#On nettoie un peu l'environnement
remove(modelefreq_poiss,modelefreq_poiss.0,modelefreq.0,modelesev.0,modelesev.lognorm,a,accident,age_lic_1_20,age_lic_36etplus, age.d,agelic.d,bonus.d,breaksAgeLic,
       breaksCyl,breaksDin,breaksDuration,breaksSaleBegin,breaksSaleEnd,breaksSpeed,breaksValue,breaksVhage,breaksWeight,cpt1,cpt2,cyl.d,din.d,d_freq,d_sev,duration.d,FREQ,garder,
       i,Nb_Sinistres,ni,outil,point.names,pol_coverage_Median, pol_coverage_Mini,q,sale_end_11_plus,sale_end_6_11,sb.d,se.d,SitDu.d,speed.d,t1,t3,tab,
       theta,ti,tirage,Value.d,value.d,value9600,value9600_16200,vhage.d,weight.d)
library(dplyr)

mesclass <- c("character", rep("numeric", 10), "character", "character", rep("numeric", 3))

#On upload les donnees de l'ONISR
carac17=read.csv("/Users/nicolaswagner/Desktop/Mémoire/caracteristiques-2017.csv", colClasses=mesclass)
lieux17=read.csv("/Users/nicolaswagner/Desktop/Mémoire/lieux-2017.csv",sep=",",colClasses = c("character","numeric","character","numeric","character",rep("numeric",13)))
usa17=read.csv("/Users/nicolaswagner/Desktop/Mémoire/usagers-2017.csv",sep=",",colClasses = c("character",rep("numeric",10),"character"))
veh17=read.csv("/Users/nicolaswagner/Desktop/Mémoire/vehicules-2017.csv",sep=",",colClasses = c("character",rep("numeric",7),"character"))

carac17$dep<-carac17$dep/10  #Les departements avaient un 0 en plus, on le supprime

#### Prise en main des donnees ####

#Il ne faut garder que les accidents impliquant des vehicules qu'on assure
#Astuce pour y parvenir : on prend les categories qui nous interesse dans le dataframe veh17
#En multipliant par 100 les indices correspondants
t1<-which(veh17$catv==7 | veh17$catv==10 | veh17$catv==13 | veh17$catv==14 | veh17$catv==15 )
veh17$catv[t1]<-veh17$catv[t1]*100
#A l'aide d'un aggregate, on garde, pour chaque accident l'indice max
#Si ce max n'est pas egal a une de nos valeurs modifiees, on sait alors qu'aucun de nos vehicules assures n'est implique
tc2<-aggregate(veh17$catv,by=list(veh17$Num_Acc),max)
t1<-which(tc2$x<700)

#t1 represente tous les accidents que nous n'assurons pas
#Comme ils nous ne interessent pas, on les supprime de notre tableau de caracteristiques
carac17<-carac17[-t1,]
lieux17<-lieux17[-t1,]

#On ne conserve que les accidents ne survenant pas en Outre Mer, car aucun d'eux n'est present dans les donnees initiales
#(C'est a dire les donnees pour la prime pure)
t1<-which(carac17$dep==97.1 | carac17$dep==97.2 | carac17$dep==97.3 | carac17$dep==97.4 | carac17$dep==97.6 )
carac17<-carac17[-t1,]
lieux17<-lieux17[-t1,]


#Une fois cela fait, on supprime les accidents correspondants dans les tableaux usagers et vehicules
#En supprimant les occurences de Numero Accident n'apparaissant pas dans le tableau de caracteristiques
suppression<-function(x){
  accidents<-(x$Num_Acc %in% carac17$Num_Acc)
  t1<-which(accidents==0)
  return(x[-t1,])
}
usa17<-suppression(usa17)
veh17<-suppression(veh17)

#On traite de maniere a part les deux Corses pour plus de lisibilite
rename_dep<-function(x,y){
  t1<-which(carac17$dep==x)
  carac17$dep[t1]<-y
  return(carac17)
}

carac17<-rename_dep(20.1,20)      #Corse du Sud 20
carac17<-rename_dep(20.2,20.5)    #Corse du Nord 20.5

#On regroupe les informations dans differents tableaux
tabacc<-merge(carac17,lieux17,by='Num_Acc')
tabveh<-merge(tabacc,veh17)
maxitab<-merge(tabveh,usa17)



#### Test des variables précisant la fréquence ####

#FREQUENCE : les variables etudiees sont les suivantes

#0)Nb acc par dep :
nb_acc_par_dep<-table(carac17$dep) #nb_acc_par_dep est de taille 96
barplot(nb_acc_par_dep,xlab="departement",ylab="Nombre d'accident",main="Nombre d'accident par departement")
#On remarque, comme on pouvait s'y attendre, un pic dans le departement 75 (Paris)


#1)Nb acc moyen par personne par dep :
pop_par_dep<-c(643.350,534.490,337.988,163.915,141.284,1083.310,325.712,273.579,153.153,310.020,370.260,279.206,2024.162,
               694.002,145.143,352.335,644.303,304.256,241.464,157.249,177.689,533.819,598.814,118.638,413.606,539.067,511.553,
               601.843,433.233,909.028,744.178,1362.672,191.091,1583.384,1144.892,1060.199,222.232,606.511,1258.722,260.188,
               407.444,331.915,762.941,227.283,1394.909,678.105,173.828,332.842,76.601,813.493,496.883,568.895,175.640,307.445,
               733.481,187.187,750.863,1043.522,207.182,2604.361,824.503,283.372,1468.018,653.742,677.309,228.530,474.452,1125.559,
               764.030,1843.319,236.659,553.595,566.506,431.174,807.360,2187.526,1254.378,1403.997,1438.266,374.351,572.443,387.890,258.349,
               1058.740,559.479,675.247,436.876,374.426,367.673,338.291,142.622,1296.130,1609.306,1623.111,1387.926,1228.618)
#pop_par_dep est de taille 96 sans les DOM TOM
nb_moy_acc_par_pers_par_dep<-nb_acc_par_dep/(pop_par_dep*1000)
barplot(nb_moy_acc_par_pers_par_dep,xlab="departement",ylab="Nombre d'accident moyen",main="Nombre d'accident moyen par personne par departement")
max_acc_par_pers<-max(nb_moy_acc_par_pers_par_dep)
score_acc_par_pers<-nb_moy_acc_par_pers_par_dep/max_acc_par_pers


#2) On regarde si le population d'un departement influe le nb moyen d'accident par personne
v<-c(seq(1,20),20.5,seq(21,95))
M<-matrix(c(pop_par_dep*1000,nb_moy_acc_par_pers_par_dep,v),nrow=96,ncol=3) 
M<-M[order(M[, 1]), ] #On trie M par ordre croissant de pop par dep
plot(M[,1],M[,2],type='p',xlab="population du dep",ylab="Nombre d'accident moyen par personne correspondant")
text(M[,1],M[,2],as.character(M[,3]))
reg.pop<-lm(M[,2] ~ M[,1])   #On ajoute une droite de regression pour voir s'il y a une tendance
abline(reg.pop, col="blue")
#Ccl : On peut degager une tendance, croissance entre la population et le nb moyen d'accidents/personne
max_pop<-max(pop_par_dep*1000)
score_pop<-(pop_par_dep*1000)/max_pop  #On va donc creer un score de population par department, qu'on normalise

#On observe une relation non lineaire entre la pop et le nombre de sinistres a l'aide d'une courbe de lissage
x<-pop_par_dep
y<-nb_acc_par_dep
plot(x,y,xlab='pop par dep (*1000)',ylab='nombre d accidents',main='relation approximative en echelle logarithmique',log="y",yaxt="n")
lines(smooth.spline(x[-60], y[-60]),col="red",lwd=2) 

#3)Ensoleillement par departement (ici j'ai ajoute les donnees sur les dom tom a la main)
num_dep_metropolitain_soleil<-c(13,83,84,20,6,34,30,4,20.5,5,66,7,26,82,81,12,11,15,32,17,46,
                                48,38,31,42,33,19,24,47,73,16,65,79,69,1,74,9,63,39,43,64,86,91,
                                87,3,40,71,36,37,25,92,21,18,94,10,58,89,85,88,70,41,56,62,23,77,72,
                                90,68,95,45,51,93,28,49,44,27,52,55,78,54,67,75,53,35,80,14,60,59,61,2,57,76,22,29,50,8)

h_soleil<-c(2801,2793,2753,2726,2668,2618,2616,2596,2533,2439,2392,2390,2354,2143,2134,2121,2106,2084,2069,2055,2054,2025,
            2020,2010,2007,1992,1976,1964,1957,1957,1943,1940,1934,1932,1928,1901,1900,1898,1889,1885,1877,1867,1866,1860,1857,
            1852,1849,1835,1799,1797,1796,1789,1787,1774,1771,1764,1759,1756,1743,1743,1737,1736,1734,1733,1731,1728,1724,1721,
            1719,1710,1705,1704,1697,1690,1690,1684,1682,1676,1664,1638,1633,1630,1629,1626,1624,1624,1622,1617,1615,1609,1605,1518,1512,1492,1460,1440)

M1<-cbind(num_dep_metropolitain_soleil,h_soleil)
M1<-M1[order(M1[, 1]), ] #On trie M1 par numero de dep croissant (de 1 a 95)
M1<-cbind(M1,nb_moy_acc_par_pers_par_dep)
#enlever<-c(76,93,94,95)
#M1<-M1[-enlever,]
plot(M1[,2],M1[,3],type='p',xlab="nb d'heures d'ensoleillement du dep",ylab="Nombre d'accident moyen par personne correspondant",ylim=c(0.0003,0.0027))
text(M1[,2],M1[,3],as.character(M1[,1]))
reg.soleil<-lm(M1[,3] ~ M1[,2])
abline(reg.soleil, col="blue")
#Ccl : il semble y avoir une tendance 
#si on retire les dep parisiens 75, 92, 93, 94 alors cette tendance est d'autant plus marquee
max_soleil<-max(h_soleil)
score_soleil<-M1[,2]/max_soleil

#4)Precipitations par departement 
#Nous ne l'avons finalement pas utilise

num_dep_metropolitain_pluie<-c(64,15,25,73,90,50,40,29,87,39,70,65,74,38,19,23,88,76,46,18,9,8,1,48,22,24,3,55,62,80,85,
                               82,14,71,75,54,12,31,58,16,56,92,94,33,52,51,27,32,77,35,10,59,57,91,44,53,47,81,30,7,17,
                               78,68,61,21,49,28,60,67,5,2,42,93,20.5,79,36,72,45,95,69,26,43,89,63,41,86,37,11,34,20,4,6,66,84,83,13)
haut_des_precipitations<-c(1073,1058,1009,995,958,957,951,947,933,926,919,918,906,898 ,894 ,865 ,831 ,
                           813 ,810 ,808,807,800,796,788,787,772,764,764,763,759,756,749,749,746,739,738,737,725,722,722,
                           712 ,702 ,700 ,694 ,686 ,685 ,675 ,674 ,673 ,670 ,666 ,662 ,660 ,660 ,657 ,654 ,654 ,644 ,644 ,641 ,
                           640,637,632,625,622,615,609,603,602,602,602,601,598,597,597,596,596,594,594,592,589,587,582,579,555,554,550,516,489,469,450,442,441,340,338,321)
M2<-cbind(num_dep_metropolitain_pluie,haut_des_precipitations)
M2<-M2[order(M2[, 1]), ] #On trie M2 par numero de dep croissant
M2<-cbind(M2,nb_moy_acc_par_pers_par_dep)
enlever<-c(76,93,94,95)
M2<-M2[-enlever,]
plot(M2[,2],M2[,3],type='p',xlab="hauteur des precipitations du dep",ylab="Nombre d'accident moyen par personne correspondant")
text(M2[,2],M2[,3],as.character(M2[,1]))
reg.pluie<-lm(M2[,3] ~ M2[,2])
abline(reg.pluie, col="blue")
#Ccl : faible tendance
max_pluie<-max(haut_des_precipitations)
score_pluie<-M2[,2]/max_pluie
#On ne la conserve pas


#5) Surf : l'etat de la surface 
#Ces pistes sont : surf, larrout, infra, env1

#La surface, on commence avec un barplot, en retirant les donnees valant 0 (NA), et celles valant 9 (trop vagues)
#Et aussi les valeur 1 et 2 (conditions normales donc tres courantes)
tab<-table(tabacc$surf)
barplot(tab[-c(1,2,3,10)],main="Fréquence des conditions d'accidents selon la surface")
legend("topleft",c("3=Flaques","4=innondée","5=enneigée","6=boue","7=boue","8=corps gras"),pch=21,cex=0.5)
#Les conditions neige et verglas, bien que rares, presentes de nombreux cas, on les considere comme etant un facteur de sinistralite
# Par departement, on fait le rapport d'accidents dans ces conditions sur le nb total d'accidents
# Cela donne une idee des departements les plus exposes a ces conditions meteo
tab<-table(tabacc$dep,tabacc$surf)
tab<-tab[,-c(1,10)]   #On enleve les NA et les "autres conditions"
tab<-tab[,c(5,7)]/apply(tab,1,sum)  #5 et 7 sont donc "neige" et "verglas"
tab<-tab[,1]+tab[,2]     #tab n'a plus que deux colonnes, qu'on somme
plot(tab,main="Proportions de conditions de neiges par département",xlab="Numéro du dép")  
#On obtient, par dep, les proportions d'accidents en conditions neige et verglas
#On considere 3 groupes : <0.025, entre 0.025 et 0.04, et >0.04
score_surf<-tab/max(tab) #On fait notre score de surface


#6)On considere aussi env1, les accidents ayant lieu a proximite d'une ecole
#Une proportion elevee traduirait une aggressivite au volant plus marquee et un manque de prudence
tab<-table(tabacc$dep,tabacc$env1) 
tab<-tab[,-1]   #On retire de nouveau les donnes manquantes
tab<-tab[,1]/(tab[,1]+tab[,2])
plot(tab,main="Nb d'accidents près d'une école/ Nb d'accidents total") #Nb d'accidents pres d'une ecole / Nb d'accidents total
score_ecole<-tab/max(tab)

#7)On ne prend pas larrout, car on n'a pas la certitude que les chaussees etroites sont plus susceptibles a la sinistralite

#8)En revanche, on regarde int, car les intersections sont bien evidemment plus dangeureuses que les les routes droites
tab<-table(carac17$dep,carac17$int) 
tab<-tab[,-1]   #On retire de nouveau les donnes manquantes
tab<-tab[,seq(2,8)]/apply(tab,1,sum) #On observe toutes les proprtions d'accidents en intersection
tab<-apply(tab,1,sum)  #On somme toutes ces intersections
plot(tab,main="Proportions d'intersection perilleuses par departement",xlab="Numéro du dép") 
score_int<-tab/max(tab)


#Score de frequence : on somme tous les scores obtenus
score_freq<-score_acc_par_pers+score_pop+score_soleil+score_surf +score_ecole +score_int
max_score_freq<-max(score_freq)
score_freq<-score_freq/max_score_freq #Et on les normalise
sort(score_freq)
barplot(score_freq,main="Score de Fréquence général par département")

# A PARTIR DE CE POINT, ON VA CONFRONTER LES DEUX METHODES DIFFERENTES (cf rapport cf 5.1.2)
#METHODE 1 : UN SCORE UNIQUE          METHODE 2 : UN SCORE POUR CHAQUE NOUVELLE VARIABLE

TAB2<-TAB  #On duplique donc nos dataframe pour effectuer les deux methodes sans encombres
TabSin2<-TabSin
maxitab2<-maxitab

#METHODE 1

#### Test des variables precisant la severite ####


#SEVERITE : 5 va testees

#1)col : type de collision ; 1=collision frontale
cf<-which(carac17$col==1) #cf est de taille 4936
plot(table(carac17$dep[cf])/(pop_par_dep*1000),xlab="departement",ylab="nombre d'accident frontal par personne par departement")
max_col<-max(table(carac17$dep[cf])/(pop_par_dep*1000))
score_col<-(table(carac17$dep[cf])/(pop_par_dep*1000))/max_col
#Ccl : On remarque que les 2 departements corse ont un nombre d'accident frontal par personne tres eleve

#2)obsm : obstacle mobile heurtre ; 1=pieton
mh<-which(tabveh$obsm==1) #le pieton ne sera pas compte 2 fois ; mh est de taille 8765
plot(table(tabveh$dep[mh])/(pop_par_dep*1000),xlab="departement",ylab="nombre de pieton tue par personne par departement")
max_obsm<-max(table(tabveh$dep[mh])/(pop_par_dep*1000))
score_obsm<-table(tabveh$dep[mh])/(pop_par_dep*1000)/max_obsm
#Ccl : les plus eleves : 75,92,93,94

#3)choc : point de choc initial ; 9=chocs multiples, tonneaux
pci<-which(tabveh$choc==9) #pci est de taille 1450
plot(table(tabveh$dep[pci])/(pop_par_dep*1000),xlab="departement",ylab="nombre de pieton tue par personne par departement")
max_choc<-max(table(tabveh$dep[pci])/(pop_par_dep*1000))
score_choc<-table(tabveh$dep[pci])/(pop_par_dep*1000)/max_choc

#4)l'agglomeration ou non (un accident hors agglomeration est plus couteux qu'en agglomeration) il n'y a que 2 modalites
agg<-which(carac17$agg==1) #agg est de taille 18602
table(carac17$dep[agg])
plot(table(carac17$dep[agg])/(pop_par_dep*1000),xlab="departement",ylab="nombre d'accidents hors agglo par personne par departement")
max_agg<-max(table(carac17$dep[agg])/(pop_par_dep*1000))
score_agg<-table(carac17$dep[agg])/(pop_par_dep*1000)/max_agg


#5)grav : gravite de l'accident ; 2 : tue, 3: blesse hospitalise
gda<-which(maxitab$grav==2 | maxitab$grav==3) #gda est de taille 26513
table(maxitab$dep[gda])
plot(table(maxitab$dep[gda])/(pop_par_dep*1000),xlab="departement",ylab="nombre de personnes tuees ou hospitalisees par personne par departement")
max_grav<-max(table(maxitab$dep[gda])/(pop_par_dep*1000))
score_grav<-table(maxitab$dep[gda])/(pop_par_dep*1000)/max_grav

#Ccl : Score severite
score_sev<-score_col+score_obsm+score_choc+score_agg+score_grav
#sort(score_sev)
max_score_sev<-max(score_sev)
score_sev<-score_sev/max_score_sev
sort(score_sev)
barplot(score_sev)

#On decoupe le score de la sev en classe de deciles :
breaks.sev<-quantile(score_sev,seq(0,1,0.1))
score.sev<-cut(score_sev, breaks=breaks.sev, include.lowest = TRUE)
summary(score.sev)




#### GLM FREQUENCE avec l'ONISR ####

#Partie 1 : Modelisation du score freq

#Dendogramme pour choisir le nombre de classes
res.freq.1=hclust(dist(score_freq)^2,method="ward.D2")
plot(res.freq.1)
#On choisit 3 classes

#Methode des K-means

#tableau_a_supp<-data.frame(score_freq)
#which.min(tableau_a_supp$Freq)
#s<-quantile(tableau_a_supp[,2],1/2)
#which.min( abs(tableau_a_supp[,2]-s) )
#which.max(tableau_a_supp$Freq)
#remove(tableau_a_supp)

res.freq.2<-kmeans(score_freq,c(score_freq[49],score_freq[12],score_freq[76]))
res.freq.2

#Creation du tableau contenant les dep et leurs categories
tableau_dep_categorie_freq<-data.frame(res.freq.2$cluster)
categorie_des_dep<-tableau_dep_categorie_freq[1:96,]

pol_insee_code<-c(1:20,20.5,21:95)
tableau_dep_categorie<-cbind(pol_insee_code,categorie_des_dep)

#Merge des tableaux pour la freq
TAB<-merge(TAB,tableau_dep_categorie,by="pol_insee_code")
TAB<-TAB %>% arrange(id_policy)
u1<-which(TAB$categorie_des_dep==1)
u2<-which(TAB$categorie_des_dep==2)
u3<-which(TAB$categorie_des_dep==3)
TAB$categorie_des_dep[u2]<-0 #on met la categorie mil a 0 car c'est celle ou il y a le + de dep : sert de reference

categorie_des_dep_2<-TAB$categorie_des_dep #on cree la 2eme colonne des categories : 3 categories mais 2 colonnes car
#categorie min vaut 0 pour les degres de liberte

TAB<-cbind(TAB,categorie_des_dep_2) #la categorie mil vaudra donc bien 0 dans les 2 colonnes

TAB$categorie_des_dep[u1]<-1 #categorie_des_dep vaut 1 si categorie min
TAB$categorie_des_dep[u3]<-0

TAB$categorie_des_dep_2[u1]<-0
TAB$categorie_des_dep_2[u3]<-1


#Partie 2 : Test modele frequence onisr

#Backward Forward
modelefreq_onisr.0<-glm.nb(formula=claim_nb~1,data=TAB)

#select.variables.stepwise=step(modelefreq_onisr.0, scope=~vh_fuel + pol_pay_freq + drv_sex1 + pol_coverage_Median 
                               #+ pol_coverage_Mini +  age_lic_1_20 + age_lic_36etplus + value9600 + value9600_16200 
                               #+ categorie_des_dep + categorie_des_dep_2, direction="both",data=TAB)

#Le Backward Forward garde dans cet ordre : pol_coverage_Mini + pol_coverage_Median + vh_fuel + value9600 
#+ value9600_16200 + pol_pay_freq + age_lic_36etplus + categorie_des_dep_2 + age_lic_1_20 + drv_sex1 
#+ categorie_des_dep
#ie il les garde TOUTES : A NOTER cependant qu'il ajoute categorie_des_dep et categorie_des_dep_2 dans les dernieres

#Tests des variables
modelefreq_onisr<-glm.nb(formula=claim_nb~vh_fuel + pol_pay_freq + drv_sex1 + pol_coverage_Median 
                         + pol_coverage_Mini + age_lic_1_20 + age_lic_36etplus + value9600 + value9600_16200 
                         + categorie_des_dep + categorie_des_dep_2, data=TAB)

summary(modelefreq_onisr)
#On remarque que toutes les variables passent le test
coeff_onisr<-coefficients(modelefreq_onisr)
FREQnb_onisr<-predict(modelefreq_onisr,newdata=TAB,level=0.95,type='response')



#### SEVERITE avec l'ONISR ####


#Partie 1 : Modelisation du score sev


#Dendogramme pour choisir le nombre de classes
res.sev.1=hclust(dist(score_sev)^2,method="ward.D2")
plot(res.sev.1)
#On choisit 3 classes

#Methode des K-means

#tableau_a_supp<-data.frame(score_sev)
#which.min(tableau_a_supp$Freq) #la colonne s'appelle Freq alors qu'on est dans la sev (pas grave)
#s<-quantile(tableau_a_supp[,2],1/2)
#which.min( abs(tableau_a_supp[,2]-s) )
#which.max(tableau_a_supp$Freq)
#remove(tableau_a_supp)

res.sev.2=kmeans(score_sev,c(score_sev[22],score_sev[46],score_sev[21]))
res.sev.2

#Creation du tableau contenant les dep et leurs categories
tableau_dep_categorie_sev<-data.frame(res.sev.2$cluster)
categorie_des_dep_sev<-tableau_dep_categorie_sev[1:96,]

pol_insee_code<-c(1:20,20.5,21:95)
tableau_dep_categorie_sev<-cbind(pol_insee_code,categorie_des_dep_sev)


#Merge des tableaux pour la sev
TabSin<-merge(TabSin,tableau_dep_categorie_sev,by="pol_insee_code")
TabSin<-TabSin %>% arrange(id_policy)
u1<-which(TabSin$categorie_des_dep_sev==1)
u2<-which(TabSin$categorie_des_dep_sev==2)
u3<-which(TabSin$categorie_des_dep_sev==3)
TabSin$categorie_des_dep_sev[u2]<-0 #on met la categorie mil a 0 car c'est celle ou il y a le + de dep : sert de reference

categorie_des_dep_sev_2<-TabSin$categorie_des_dep_sev #on cree la 2eme colonne des categories : 3 categories mais 2 colonnes car
#categorie min vaut 0 pour les degres de liberte

TabSin<-cbind(TabSin,categorie_des_dep_sev_2) #la categorie mil vaudra donc bien 0 dans les 2 colonnes

TabSin$categorie_des_dep_sev[u1]<-1 #categorie_des_dep_sev vaut 1 si categorie min
TabSin$categorie_des_dep_sev[u3]<-0

TabSin$categorie_des_dep_sev_2[u1]<-0
TabSin$categorie_des_dep_sev_2[u3]<-1

#Partie 2 : Test modele sev onsir

#Backward Forward
modelesev_onisr.0<-glm(claim_amount~1,family=Gamma(link=log),data=TabSin)

#select.variables.stepwise=step(modelesev_onisr.0, scope=~ vh_din + vh_age + vh_type + sale_end_6_11 + sale_end_11_plus 
                               #+ vh_fuel + categorie_des_dep_sev + categorie_des_dep_sev_2, direction="both", data=TabSin)

#Le Backward Forward garde dans cet ordre : vh_age + vh_type + categorie_des_dep_sev + sale_end_6_11 
#+ sale_end_11_plus + vh_fuel + vh_din + categorie_des_dep_sev_2
#ie TOUTES les variables : A NOTER que categorie_des_dep_sev arrive au debut mais que categorie_des_dep_sev_2 arrive a la fin

#Tests des variables
modelesev_onisr<-glm(claim_amount~vh_din + vh_age + vh_type + sale_end_6_11 + sale_end_11_plus 
                     + vh_fuel + categorie_des_dep_sev + categorie_des_dep_sev_2, family=Gamma(link=log),data=TabSin)

summary(modelesev_onisr)
#On remarque que toutes les variables passent le test
coeffsev_onisr<-coefficients(modelesev_onisr)
SEVval_onisr<-predict(modelesev_onisr,newdata=TabSin,level=0.95,type='response')


#### PRIME PURE avec l'ONISR ####

#Partie 1 : Calcul de la prime pure avec l'ONISR

TAB<-merge(TAB,tableau_dep_categorie_sev,by="pol_insee_code")
TAB<-TAB %>% arrange(id_policy)
u1<-which(TAB$categorie_des_dep_sev==1)
u2<-which(TAB$categorie_des_dep_sev==2)
u3<-which(TAB$categorie_des_dep_sev==3)
TAB$categorie_des_dep_sev[u2]<-0 #on met la categorie mil a 0 : sert de reference

categorie_des_dep_sev_2<-TAB$categorie_des_dep_sev #on cree la 2eme colonne des categories : 3 categories mais 2 colonnes car
#categorie min vaut 0 pour les degres de liberte

TAB<-cbind(TAB,categorie_des_dep_sev_2) #la categorie mil vaudra donc bien 0 dans les 2 colonnes

TAB$categorie_des_dep_sev[u1]<-1
TAB$categorie_des_dep_sev[u3]<-0

TAB$categorie_des_dep_sev_2[u1]<-0
TAB$categorie_des_dep_sev_2[u3]<-1



COEFF_ONISR<-c(coeff_onisr,coeffsev_onisr)

MAT_ONISR<-cbind(TAB$vh_fuel, TAB$pol_pay_freq, TAB$drv_sex1, TAB$pol_coverage_Median, TAB$pol_coverage_Mini, 
                 TAB$age_lic_1_20, TAB$age_lic_36etplus, TAB$value9600, TAB$value9600_16200, TAB$categorie_des_dep,
                 TAB$categorie_des_dep_2)
MAT_ONISR<-cbind(c(rep(1,nrow(MAT_ONISR))),MAT_ONISR,rep(1,nrow(MAT_ONISR)))

MAT_ONISR<-cbind(MAT_ONISR,TAB$vh_din, TAB$vh_age, TAB$vh_type, TAB$sale_end_6_11 , TAB$sale_end_11_plus, TAB$vh_fuel, 
                 TAB$categorie_des_dep_sev, TAB$categorie_des_dep_sev_2)

#Les coeffs du vecteur COEFF_ONISR sont bien dans le meme ordre que les colonnes de MAT_ONISR (ie chaque coeff va bien avec sa variable
#associee donc on peut effectuer le produit)

FREQnb_onisr<-predict(modelefreq_onisr,newdata=TAB,level=0.95,type='response')
a_onisr<-c(as.vector(FREQnb_onisr))
a_onisr<-q1*EC*a_onisr #q1 et EC ne varient pas entre les 2 modeles

PRIMEPURE_ONISR<-MAT_ONISR%*%COEFF_ONISR
PRIMEPURE_ONISR<-exp(PRIMEPURE_ONISR) + a_onisr

summary(PRIMEPURE_ONISR)

#Partie 2 : Comparaison rapide avec le premier modele

#Comparaison des 2 modeles
#Freq
summary(FREQnb)
summary(FREQnb_onisr)
c(var(FREQnb),var(FREQnb_onisr))
c(sum(FREQnb),sum(FREQnb_onisr))
#Les valeurs sont extremement proches

#Sev
summary(SEVval)
summary(SEVval_onisr)
#La mediane est significativement plus faible pour SEVval_onisr
c(sum(SEVval),sum(SEVval_onisr))

#Prime pure
summary(PRIMEPURE)
summary(PRIMEPURE_ONISR)
c(var(PRIMEPURE),var(PRIMEPURE_ONISR))
#La variance est plus elevee pour la PRIMEPURE_ONISR

#Montant de primes
c(sum(PRIMEPURE),sum(PRIMEPURE_ONISR))
#Les 2 sommes sont tres proches

#On repart du tableau initial tc car dans TabSin on a supp les montants <=0
t<-which(tc$claim_amount==0)
tc$claim_amount[t]<-mean(PRIMEPURE)
sum(abs(tc$claim_amount))
#On constate un ecart de 1Million100 000

#Decoupage selon les deciles pour faire des sous groupes et comparer les primes
breaks.primepure<-quantile(PRIMEPURE,seq(0,1,0.1))
primepure.dec<-cut(PRIMEPURE, breaks=breaks.primepure, include.lowest = TRUE)
summary(primepure.dec)

breaks.primepure.onisr<-quantile(PRIMEPURE_ONISR,seq(0,1,0.1))
primepure.onisr.dec<-cut(PRIMEPURE_ONISR, breaks=breaks.primepure.onisr, include.lowest = TRUE)
summary(primepure.onisr.dec)

plot(tapply(PRIMEPURE, primepure.dec, mean) , col="blue", type="p", ylab="Prime pure moyenne", main="Prime pure moyenne selon la classe")
lines(tapply(PRIMEPURE_ONISR, primepure.onisr.dec, mean), col="red",type="p")
#De meme on constate que pour les 2 modeles les primes predites pour les sous groupes sont tres proches



##### METHODE 2 #####

par(mfrow= c(1,1))
remove(usa17,veh17,lieux17)

TAB<-TAB2
TabSin<-TabSin2
maxitab<-maxitab2
remove(TAB2,TabSin2,maxitab2)   #On recupere les dataframes qu'on a conserves intacts avant de debuter la METHODE 1

##### Etude de la Frequence Methode 2 ####

#On va creer un tableau qui, par departement, va prendre toutes nos variables de frequence
# On le concatenera a notre tableau initial, TAB2
TABonisr<-c(seq(1,20),20.5,seq(21,95))
TABonisr<-cbind(TABonisr,round(score_acc_par_pers,2),
                round(score_pop,2),round(score_soleil,2),round(score_surf,2),round(score_ecole,2),
                round(score_int,2))
colnames(TABonisr)<-c("pol_insee_code","Acc_Moy","Population","Ensoleillement","Surface","Ecoles","Intersection")

TABonisr<-data.frame(TABonisr)

cv.test(TABonisr$Ensoleillement,TABonisr$Surface)  #On peut faire des tests de V_Cramer
#Mais les valeurs sont faibles en raison du faible nb de donnees

TAB<-merge(TAB,TABonisr,by="pol_insee_code")
TAB<-TAB[order(TAB[,2]),]

#On discretise les donnees obtenues comme on l'a fait jusqu'a maintenant, mais cette fois-ci, en utilisant la methode des k-means

# On se contentera de faire 2 groupes pour eviter de trop nombreuses covariables
k=2

discrete<-function(vec){    #On va faire une fonction speciale pour s'assurer que le groupe a la valeur 0
  res2 = kmeans(vec,k)      # est bien celui de reference (donc celui avec le plus d'occurences)
  while (res2$size[1]<res2$size[2]){
    res2=kmeans(vec,k)
  }
  return (res2)
}
#Si apres plusieurs executions les barycentres ne sont pas les memes, rentrera les points d'initialisation a la main

res2<-discrete(TAB$Ensoleillement)
TAB$Ensoleillement<-res2$cluster-1  #On discretise
res2<-discrete(TAB$Surface)  #Celle ci varie, on va donc donner les valeurs "a la main"
res2<-kmeans(TAB$Surface,centers=c(0.107,0.528))
TAB$Surface<-res2$cluster-1  
res2<-discrete(TAB$Ecoles)    #Valeur a la main 
res2<-kmeans(TAB$Ecoles,centers=c(0.696,0.302))
TAB$Ecoles<-res2$cluster-1  
res2<-discrete(TAB$Intersection) 
res2<-kmeans(TAB$Intersection,centers=c(0.34,0.558))   #L'intersection est la variable la plus instable
TAB$Intersection<-res2$cluster-1                    #On va donc definir precisement l'initialisation
res2<-discrete(TAB$Population)         #Celle ci varie, on va donc donner les valeurs a la main
res2<-kmeans(TAB$Population,centers=c(0.203,0.595))
TAB$Population<-res2$cluster-1  

#Nous avons eu la confirmation que nous modeles fonctionnent de maniere convaincante, on definit donc notre GLM
#Sur le dataframe entier

modelefreq<-glm.nb(formula=claim_nb~vh_fuel + pol_pay_freq +
                     drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                     age_lic_36etplus + value9600 + value9600_16200 +Population +Ensoleillement + Ecoles +Surface +Intersection ,data=TAB)
summary(modelefreq)

#Toutes les variables pre-ONISR + la population et l'ensoleillement ont des p-values < 0.05, donc on les conserve
modelefreq<-glm.nb(formula=claim_nb~vh_fuel + pol_pay_freq +
                     drv_sex1 + pol_coverage_Median + pol_coverage_Mini + age_lic_1_20 +
                     age_lic_36etplus + value9600 + value9600_16200 +Population +Ensoleillement ,data=TAB)
summary(modelefreq)
FREQnb2<-predict(modelefreq,newdata=TAB,level=0.95,type='response') #On recupere nos predictions
coeffFreq2<-modelefreq$coefficients    #Et nos coefficients

# On va identifier les classes pour lesquelles la proba d'avoir un accident est la plus faible
# On recree un tableau a part avec nos variables et nos estimations de frequence
class2<-cbind(TAB$vh_fuel , TAB$pol_pay_freq , TAB$drv_sex1 ,
             TAB$pol_coverage_Median , TAB$pol_coverage_Mini , TAB$age_lic_1_20 ,
             TAB$age_lic_36etplus , TAB$value9600 , TAB$value9600_16200 , TAB$Population, TAB$Ensoleillement,round(FREQnb2,5))
colnames(class2)<-c("vh_fuel" , "pol_pay_freq" , "drv_sex1" ,
                   "pol_coverage_Median" , "pol_coverage_Mini" , "age_lic_1_20" ,
                   "age_lic_36etplus" , "value9600" , "value9600_16200" ,"Population", "Ensoleillement" ,"Frequence")
class2<-data.frame(class2)
class2<-class2[order(class2[,12]),]
#On observe les meilleurs et les pires types d'usagers

class2<-cbind(aggregate(. ~ Frequence, class2,mean)[1],round(aggregate(. ~ Frequence, class2,mean)[,-1],1))
# La classe la plus a risque : véhicule cher, homme, paiement haute freq, diesel, population elevee, ensoleillement eleve

#On affiche la proba de non accident pour les classes extremes
theta<-modelefreq$theta
dnbinom(0,size=theta,prob=theta/(theta+class2$Frequence[c(1,2,3,length(class2[,1])-2,length(class2[,1])-1,length(class2[,1]))]))


##### Etude Severite METHODE 2  ####

#TabSin<-TabSin2
#maxitab<-maxitab2
#TabSin<-rbind(TabSin,t2)  A NE PAS EXECUTER 
# Comme pour la frequence, on va recenser les donnees trouvees dans un tableau
TabSinOnisr<- c(seq(1,20),20.5,seq(21,95))

SevDep<-tapply(TabSin$claim_amount,TabSin$pol_insee_code,mean) #On recupere les severites moyennes par departement

#La discretisation des variables se fait de maniere differente legerement differente
# par rapport a la methode 1

# 1) Le type de collision
#On repartit les types de collision selon le nombre de vehicules impliques 
faibleAcc<-(carac17$col==7)   #pas de collision
moyenAcc<-2*(carac17$col>=1 & carac17$col<=3)     #Collision entre deux vehicules
fortAcc<-3*(carac17$col>3 & carac17$col<7)       #Collision entre trois vehicules ou plus
aggregCol<-(faibleAcc+moyenAcc+fortAcc)
t1<-which(is.na(aggregCol))
aggregCol[t1]<-1   #On remplace les NA par la valeur la plus presente dans le tableau
carac17$col<-aggregCol
t1<-tapply(carac17$col,carac17$dep,mean)
plot(t1,main="Envergure des collisions par departement",xlab="Num du departement",ylab="Envergure moyenne des collisions")
TabSinOnisr<-cbind(TabSinOnisr,round(t1,2))

#2) La gravite
#On repartit les gravites selon les gravites 
faibleGrav<-(maxitab$grav==1)   #usagers indemne
moyenGrav<-2*(maxitab$grav==4)     #usager legerement blesse
fortGrav<-3*(maxitab$grav==2 | maxitab$grav==3)       #usager hospitalise ou mort
aggregGrav<-(faibleGrav+moyenGrav+fortGrav)
maxitab$grav<-aggregGrav
t1<-tapply(maxitab$grav,maxitab$dep,mean)
SevGrav<-t1     #Servira a faire un plot (cf fonction tendance + bas)
plot(t1,main="Gravite par departement",xlab="Num du departement",ylab="Gravite moyenne")
TabSinOnisr<-cbind(TabSinOnisr,round(t1,2))


#3) L'agglomeration ou non (un accident hors agglomeration est plus couteux qu'en agglomeration)
faibleGrav<-0*(carac17$agg==2) #accident en agglomeration
fortGrav<-(carac17$agg==1) #Hors agglo
aggregGrav<-faibleGrav+fortGrav
carac17$agg<-aggregGrav
t1<-tapply(carac17$agg,carac17$dep,mean)
SevHorsAgglo<-t1    #Servira a faire un plot (cf fonction tendance + bas)
plot(t1,main="Proportion d'accidents hors agglo",xlab="Num du departement") #Naturellement, le 75 a une proportion tres faible
TabSinOnisr<-cbind(TabSinOnisr,round(t1,2))

#4) Choc : des chocs multiples ou de cote sont plus graves que les chocs arrieres
na<-which(is.na(tabveh$choc))
tabveh<-tabveh[-na,]
faibleGrav<-0*(tabveh$choc==4 | tabveh$choc==5 | tabveh$choc==6 | tabveh$choc==7| tabveh$choc==8) #Le choc est arriere ou de cote
fortGrav<-1*( tabveh$choc==0|tabveh$choc==1|tabveh$choc==2| tabveh$choc==3| tabveh$choc==9) #Le choc est avant (grosses chances d'etre grievement blesse) ou multiples
#1 choc sur 7 environ est plus dangereux que les autres
aggregGrav<-faibleGrav+fortGrav
tabveh$choc<-aggregGrav
t1<-tapply(tabveh$choc,tabveh$dep,mean)
SevChoc<-t1 #Servira a faire un plot 
plot(t1,main="Gravite des chocs par departement",xlab="Num du departement") 
TabSinOnisr<-cbind(TabSinOnisr,round(t1,2))

#Le type de choc joue un role tres important dans la survenance des risques extremes
q1<-q1*(t1/sort(t1)[48])  #On va donc agir sur la proba de causer un sinistre grave q1
#On l'augmente (ou la diminue) selon que la moyenne de gravite des collisions soit au dessus de la moyenne ou non (donc le 48eme des departements tries)
q1<-cbind(c(seq(1,20),20.5,seq(21,95)),q1) #On ajoute a la probabilite modifie le departement correspondant
colnames(q1)<-c("pol_insee_code","ProbaSinistreGrave")

#5) Obstacle mobile : un pieton est bien plus fragile qu'un vehicule, et le heurter coutera plus cher
na<-which(is.na(tabveh$obsm))
tabveh<-tabveh[-na,]
#Si l'obstacle heurte n'est pas un pieton, les 
faibleGrav<-0*(tabveh$obsm==0 | tabveh$obsm==2 | tabveh$obsm==4 | tabveh$obsm==5| tabveh$obsm==6| tabveh$obsm==9) 
fortGrav<-1*( tabveh$obsm==1)  #1 accident sur 6 implique un pieton heurte
aggregGrav<-faibleGrav+fortGrav
tabveh$obsm<-aggregGrav
t1<-tapply(tabveh$obsm,tabveh$dep,mean)
plot(t1,main="Proportions pietons heurtes",xlab="Num du departement") 
TabSinOnisr<-cbind(TabSinOnisr,round(t1,2))

#Une fois nos donnees traitees, on les incorpore a TabSin pour y observer des scores par departement lies 
#aux variables de severite que l'on vient de choisir

TabSinOnisr<-data.frame(TabSinOnisr)
colnames(TabSinOnisr)<-c("pol_insee_code","Collision","Gravite","HorsAgglo","Choc","ObstacleMobile")

par(mfrow=c(2,2))      
tendance<-function(u){
  Tend<-lm(u ~ SevDep)
  abline(Tend, col="blue")
}
plot(SevDep,SevChoc,main="Sévérité et Gravité des chocs")
tendance(SevChoc)
plot(SevDep,SevGrav,main="Sévérité et Gravité Générale")
tendance(SevGrav)
plot(SevDep,SevHorsAgglo,main="Sévérité et Proportion d'Acc HA")
tendance(SevHorsAgglo)
par(mfrow=c(1,1))      

TabSin<-merge(TabSin,TabSinOnisr,by="pol_insee_code")
TabSin<-TabSin[order(TabSin[,2]),]

#On passe a la discretisation avec la methode des k-means comme pour la frequence
# NOTE : on discretise la va Hors Agglo en 3 groupes, pour bien illuster les departements tres urbains
# (On peut se le permettre vu le nombre modere de variables explicatives de la severite dans le modele initial)

res2<-discrete(TabSin$Collision)   #Les barycentres changent, donc on remplit les donnees "a la main"
res2<-kmeans(TabSin$Collision,centers=c(2.32,2.41))
TabSin$Collision<-res2$cluster-1
res2<-discrete(TabSin$Gravite)   #Pareil ici
res2<-kmeans(TabSin$Gravite,centers=c(1.938,1.732))
TabSin$Gravite<-res2$cluster-1
res2<-discrete(TabSin$Choc)     #Pareil ici
res2<-kmeans(TabSin$Choc,centers=c(0.755,.674))
TabSin$Choc<-res2$cluster-1
res2<-discrete(TabSin$ObstacleMobile)     #Pareil ici
res2<-kmeans(TabSin$ObstacleMobile,centers=c(0.081,0.132))
TabSin$ObstacleMobile<-res2$cluster-1


#Avec Hors Agglo, on fait k=3
HA<-function(X){
  res2<-kmeans(X$HorsAgglo,centers=c(0.6277,0.226,0.4187))    #0.6277 (reference) : bcp d'accidents hors agglo
  X$HorsAgglo<-res2$cluster-1
  t1<-(X$HorsAgglo==1)   #On Repartit enfin la proportion en accidents hors agglo en 3 classes
  X$HAfaible<-1*t1       # La modalite de reference est celle d'une forte proportion, elle n'apparait
  t1<-(X$HorsAgglo==2)   # que de facon implicite dans le tableau (i.e. les deux autres modalites valent 0)
  X$HAinter<-1*t1
  X$HorsAgglo<-NULL     # on a donc separe la colonne HA en deux colonnes : HAfaible et HAinter
  return(X)
}
TabSin<-HA(TabSin)

# On fait nos GLM Sur 100% des donnees comme pour la frequence

modelesev<-glm(claim_amount~ vh_age +vh_din + vh_fuel+ vh_type + sale_end_6_11 + sale_end_11_plus + Collision +
                 Gravite + Choc + ObstacleMobile + HAfaible + HAinter ,family=Gamma(link=log),data=TabSin)
summary(modelesev)  # La plupart des nouvelles variables ne sont pas conservees par le modele, on ne garde que HAfaible et HAinter

modelesev<-glm(claim_amount~ vh_age +vh_din + vh_fuel+ vh_type + sale_end_6_11 + sale_end_11_plus +
                 HAfaible + HAinter ,family=Gamma(link=log),data=TabSin)
summary(modelesev) 

SEV2<-predict(modelesev,newdata=TabSin,level=0.95,type='response')
coeffsev2<-modelesev$coefficients

# On va identifier les classes pour lesquelles les accidents sont les plus couteux
# On recree un tableau a part avec nos variables et nos estimations de severite
class2<-cbind(TabSin$vh_din , TabSin$vh_age , TabSin$vh_type ,
             TabSin$sale_end_6_11 , TabSin$sale_end_11_plus , TabSin$vh_fuel, TabSin$HAfaible, TabSin$HAinter , SEV2)
colnames(class2)<-c("vh_dinl" , "vh_age" , "vh_type" ,"sale_end_6_11" , "sale_end_11_plus" , "vh_fuel","AccHAfaible" ,"AccHAmoyen","Montant")
class2<-data.frame(class2)

class2<-cbind(aggregate(. ~ Montant, class2,mean)[1],round(aggregate(. ~ Montant, class2,mean)[,-1],1))
# La classe la plus a risque : moteur puissant, vh peu age, vh de tourisme, en vente depuis peu, roulant a l'essence, dans un departement urbain

#### Calcul de la prime pure ####

#Il faut ajouter dans TAB les varaibles de la severite issues de l'ONISR (comme dans la premiere parite)

#La variable HorsAgglo est gardee, et il faut le rediscretiser avec la methode des k-means
TabSinOnisr<-HA(TabSinOnisr) #On rediscretise la variable HA
     
garder<-c("pol_insee_code","HAfaible","HAinter")    #La variable HorsAgglo est gardee, et il faut le rediscretiser 
soustab<-subset(TabSinOnisr,select=garder)  

TAB<-merge(TAB,soustab,by='pol_insee_code')
TAB<-TAB[order(TAB[,2]),]

#Le calcul de la prime pure, comme en premiere partie
COEFF2<-c(coeffFreq2,coeffsev2)

MAT<-cbind(TAB$vh_fuel, TAB$pol_pay_freq, TAB$drv_sex1
           , TAB$pol_coverage_Median , TAB$pol_coverage_Mini , TAB$age_lic_1_20 ,
           TAB$age_lic_36etplus,TAB$value9600,TAB$value9600_16200, TAB$Population, TAB$Ensoleillement)
MAT<-cbind(c(rep(1,nrow(MAT))),MAT,rep(1,nrow(MAT)))
MAT<-cbind(MAT, TAB$vh_age, TAB$vh_din,TAB$vh_fuel_sev, TAB$vh_type, TAB$sale_end_6_11 , TAB$sale_end_11_plus, TAB$HAfaible, TAB$HAinter)

#Les coeffs du vecteur COEFF sont bien dans le meme ordre que les colonnes de MAT (ie chaque coeff va bien avec sa variable
#associee donc on peut effectuer le produit)

#Il ne faut pas oublier que l'on doit additionner la probabilite pour l'assure de causer un sinistre grave, on a donc
b<-c(as.vector(FREQnb2))  #La probabilite d'un accident pour chaque police 
a<-cbind(TAB$id_policy,TAB$pol_insee_code,b)
colnames(a)<-c("id_policy","pol_insee_code","Frequence")
a<-merge(a,round(q1,4),by="pol_insee_code")   #On rentre la proba modifiee par departement
a$pol_insee_code<-NULL
a$Frequence<-NULL
TAB<-merge(TAB,a,by='id_policy')    #On l'integre dans TAB
q1<-TAB$ProbaSinistreGrave
a<-q1*EC*b     #On a notre impact de catastrophe modifiee

#On va suppimer les colonnes inutiles de TAB
TAB$Acc_Moy<-NULL
TAB$Surface<-NULL
TAB$Ecoles<-NULL         
TAB$Intersection<-NULL

PRIMEPURE2<-MAT%*%COEFF2
PRIMEPURE2<-exp(PRIMEPURE2) + a
summary(PRIMEPURE2)

# On va identifier les classes pour lesquelles les primes pures sont les plus elevees
# On recree un tableau a part avec nos variables et nos estimations de severite
class2<-cbind(TAB$vh_fuel, TAB$pol_pay_freq , TAB$drv_sex1 ,
              TAB$pol_coverage_Median , TAB$pol_coverage_Mini , TAB$age_lic_1_20 ,
              TAB$age_lic_36etplus , TAB$value9600 , TAB$value9600_16200 , TAB$Population, TAB$Ensoleillement,
              TAB$vh_age, TAB$vh_din , TAB$vh_type ,  TAB$sale_end_6_11 , TAB$sale_end_11_plus, TAB$HAfaible, TAB$HAinter ,PRIMEPURE2)
colnames(class2)<-c("Fuel","Pay_Freq","Sexe","Cov_Median","Cov_Mini","Age_Lic_1_20","Age_Lic_36+","Value9600-",
                    "Value9600_16200","Population","Ensoleillement", "Age_vh","Puiss_Vh", "Type_vh" ,"SaleEnd6-11" , "SaleEnd11+","AccHAfaible" ,"AccHAmoyen","PrimePure")
class2<-data.frame(class2)

class2<-cbind(aggregate(. ~ PrimePure, class2,mean)[1],round(aggregate(. ~ PrimePure, class2,mean)[,-1],1))
# La classe la plus a risque : elle combine les caracteristiques de la classe la plus a risque pour la frequence et pour la severite

#On recupere ensuite pour chaque police son departement et sa nouvelle prime qu'on comparera a l'ancienne
PP2<-cbind(TAB$id_policy,TAB$pol_insee_code,round(PRIMEPURE2,2))
PP2<-PP2[order(PP2[,2]),]

#Comparaison des modeles

#1)Premieres comparaisons

#Frequence
summary(FREQnb)
summary(FREQnb)
c(sum(FREQnb),sum(FREQnb2))

#Severite
summary(SEVval)
summary(SEV2)
c(sum(SEVval),sum(SEV2))

#Prime pure
summary(PRIMEPURE)
summary(PRIMEPURE2)
c(sum(PRIMEPURE),sum(PRIMEPURE2))
c(var(PRIMEPURE),var(PRIMEPURE2))

sum(abs(tc$claim_amount)) 
#On compare avec la somme totale des sinistres : on constate une difference de 1Million100000

#2)Nuage de primes predites
n<-50
t<-sample(c(1:99999),n,replace=FALSE,prob=rep(1/99999,99999)) #on tire n polices au hasard
t<-sort(t)
plot(PRIMEPURE[t],col="blue",type="l")
lines(PRIMEPURE2[t],col="red")


#3)Boxplot selon les modalites des variables pour comparer les primes
indice<-seq(1,99999)
Tableau_general<-cbind(indice,TAB,PRIMEPURE,PRIMEPURE2)

par(mfrow= c(1,2))
boxplot(PRIMEPURE)
boxplot(PRIMEPURE2)

par(mfrow= c(1,2))
aff<-function(x){
  boxplot(Tableau_general$PRIMEPURE~x,ylim=c(0,300))
  boxplot(Tableau_general$PRIMEPURE2~x,ylim=c(0,300))
}
#a)Variables de frequence
aff(Tableau_general$vh_fuel)
aff(Tableau_general$pol_pay_freq)
aff(Tableau_general$drv_sex1)
aff(Tableau_general$pol_coverage_Median) #On constate une difference
aff(Tableau_general$pol_coverage_Mini)
aff(Tableau_general$age_lic_1_20)
aff(Tableau_general$age_lic_36etplus)
aff(Tableau_general$value9600) #On constate une difference
aff(Tableau_general$value9600_16200) #On constate une difference

#b)Variables de severite
aff(Tableau_general$vh_din) #On constate une difference
aff(Tableau_general$vh_age)
aff(Tableau_general$vh_type)
aff(Tableau_general$sale_end_11_plus) #On constate une difference
aff(Tableau_general$vh_fuel)

#c)Variable pol_insee_code
aff(Tableau_general$pol_insee_code)
par(mfrow= c(1,1))
plot(tapply(Tableau_general$PRIMEPURE,Tableau_general$pol_insee_code,mean),col="blue",type='l',
     xlab="numero du dep",main="Prime pure moyenne pour chaque departement",xlim=c(0,100),ylim=c(0,300))
lines(tapply(Tableau_general$PRIMEPURE2,Tableau_general$pol_insee_code,mean),col="red")

sort(tapply(Tableau_general$PRIMEPURE,Tableau_general$pol_insee_code,mean))
sort(tapply(Tableau_general$PRIMEPURE2,Tableau_general$pol_insee_code,mean))

#d)Variables ONISR
par(mfrow= c(1,2))
aff(Tableau_general$Population)
aff(Tableau_general$Ensoleillement)
aff(Tableau_general$HAfaible)
aff(Tableau_general$HAinter)

#4)Methode des K-means pour faire des sous groupes dans la PRIMEPURE2

#q<-quantile(PRIMEPURE2,seq(0,1,0.1))
#Nous choisissons K=10. Nous avons au prealable chercher les 10 indices des polices d'assurance 
#correspondants aux deciles de PRIMEPURE2 afin d'initialiser notre methode.
#Ainsi les groupes crees seront tjs les memes et seront ranges dans l'ordre croissant.
res.primepure2=kmeans(PRIMEPURE2,c(PRIMEPURE2[24078],PRIMEPURE2[21673],PRIMEPURE2[73583]
                                   ,PRIMEPURE2[2639],PRIMEPURE2[325],PRIMEPURE2[2098]
                                   ,PRIMEPURE2[16578],PRIMEPURE2[33],PRIMEPURE2[15034]
                                   ,PRIMEPURE2[1447])) 
res.primepure2

tab<-data.frame(res.primepure2$cluster)
indice<-seq(1,99999)
tab<-cbind(indice,tab)
Tableau_general<-merge(Tableau_general,tab,by="indice")

par(mfrow= c(1,2))
aff(Tableau_general$res.primepure2.cluster)

#Enfin on voit qu'en prenant les indices d'une classe d'assures bien choisie, on est quasiment certains d'etre dans les 5 niveaux de prime pure les plus eleves
t1<-which(TAB$vh_fuel==0 & TAB$sale_end_11_plus==0 & TAB$sale_end_6_11==0 & TAB$pol_coverage_Mini==0 & TAB$pol_coverage_Median==0 & TAB$Population==1)

#On affiche enfin la grille de primes determinee par la methode des k-means
res2<-kmeans(PRIMEPURE2,10)
res2$centers   #Les barycentres changent souvent en raison du nombre de groupes importants
#Un choix possible est : 1) 29.42    2) 63.59    3) 88.21      4) 111.93     5) 134.61 
#                        6) 156.27   7) 177.61   8) 198.35     9) 221.44     10)251.34
