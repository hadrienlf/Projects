############################ONISR######################################
library(maptools)
library(RColorBrewer)
library(classInt)
library(raster)
library(rgdal)

#on a la carte de france decoupee en departements avec:
#carte = readOGR(dsn = "epartements-20180101.shp", layer = "SHAPEFILE")
#spdf_departement <- readOGR(
#"https://www.data.gouv.fr/fr/datasets/r/eb36371a-761d-44a8-93ec-3d728bec17ce",
#"data/departements-20140306-100m-shp/", 
# layer= "departements-20140306-100m"
#)
#on a la carte de france decoupee en departements avec:
#FranceFormes = getData(name="GADM", country="FRA", level=2)
#plot(FranceFormes, main="Carte de la France, départements")

#ONISR: on a des donnees separees en 4 categories: Caracteristiques, lieux, vehicules, usagers
caract=read.csv("donnees/caracteristiques-2017.csv")
lieux=read.csv("donnees/lieux-2017.csv")
usagers=read.csv("donnees/usagers-2017.csv")
vehicules=read.csv("donnees/vehicules-2017.csv")

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
