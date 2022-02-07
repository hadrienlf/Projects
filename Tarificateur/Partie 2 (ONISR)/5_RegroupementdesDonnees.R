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
