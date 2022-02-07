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
