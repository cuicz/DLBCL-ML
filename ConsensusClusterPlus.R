library(limma)
library(ConsensusClusterPlus)

rt=read.table("matrix.txt", header=T, sep="\t", check.names=F, row.names=1)
rt=as.matrix(rt)
workDir=getwd()
#开始聚类
maxK=9
results=ConsensusClusterPlus(rt,
              maxK=maxK,
              reps=50,
              pItem=0.8,
              pFeature=1,
              title=workDir,
              clusterAlg="km",
              distance="euclidean",
              seed=123456,
              plot="png")
calcICL(results, title="consensusScore", plot="png")

#根据算法选择最佳k，见：https://www.bioinfo-scrounger.com/archives/734/
Kvec = 2:maxK
x1 = 0.1; x2 = 0.9                     # threshold defining the intermediate sub-interval
PAC = rep(NA,length(Kvec)) 
names(PAC) = paste("K=",Kvec,sep="")          # from 2 to maxK
for(i in Kvec){
  M = results[[i]]$consensusMatrix
  Fn = ecdf(M[lower.tri(M)])
  PAC[i-1] = Fn(x2) - Fn(x1)
}                                 
# 最佳k值
optK = Kvec[which.min(PAC)]


#提取指定k值的聚类结果，需自行根据情况调整
clusterNum=optK        
cluster=results[[clusterNum]][["consensusClass"]]
write.table(cluster, file="cluster.txt", sep="\t", quote=F, col.names=F)

### by af  ###
##307686155@qq.com###