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
populations = read_excel("donnees/populations.xlsx")
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

