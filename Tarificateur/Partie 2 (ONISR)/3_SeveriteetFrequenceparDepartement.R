######################Severite et frequence par departement###########################################
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