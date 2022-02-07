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