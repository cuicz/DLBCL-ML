library(NMF)

data <- read.csv("NMFradtype.csv",row.names = 1,header = T)


res=nmf(data, rank=2:10, method="brunet", nrun=20, seed=12345)
pdf(file="cophenetic.pdf", width=8, height=7, onefile=F)
plot(res)
dev.off()

pdf(file="heatmap.all.pdf", width=15, height=15, onefile=F)
consensusmap(res,
             annRow=NA,
             annCol=NA,
             #tracks=c("consensus:"),
             main="Consensus matrix",
             info=FALSE)
dev.off()




clusterNum=3        
res=nmf(data, rank=clusterNum, method="brunet", nrun=20, seed=12345)
Cluster=predict(res)
Cluster=as.data.frame(Cluster)
Cluster$Cluster=paste0("C", Cluster$Cluster)
clusterOut=rbind(ID=colnames(Cluster), Cluster)
write.table(clusterOut, file="cluster3.txt", sep="\t", quote=F, col.names=F)

pdf(file="heatmap3.pdf", width=6, height=6, onefile=F)
consensusmap(res,
             annRow=NA,
             annCol=NA,
             #tracks=c("consensus:"),
             main="Consensus matrix", 
             info=FALSE)
dev.off()


