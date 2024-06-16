library(ggalluvial)
library(ggplot2)
library(dplyr)

data <- read.csv("ggalluvial.csv",header = T,row.names = 1)
corLodes=to_lodes_form(data, axes=1:ncol(data), id = "Cohort")

mycol=rep(c("#0066FF","#FF9900","#FF0000","#029149","#6E568C","#E0367A","#D8D155","#223D6C","#D20A13","#431A3D","#91612D","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#64495D","#7CC767"),15)
p = ggplot(corLodes, aes(x = x, stratum = stratum, alluvium = Cohort,fill = stratum, label = stratum)) +
  scale_x_discrete(expand = c(0, 0)) +  

  geom_flow(width = 2/10,aes.flow = "backward") + 
  geom_stratum(alpha = .9,width = 2/10) +
  scale_fill_manual(values = mycol) +

  geom_text(stat = "stratum", size = 3,color="black") +
  xlab("") + ylab("") + theme_bw() + 
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank()) + #ȥ????????
  theme(panel.grid =element_blank()) + 
  theme(panel.border = element_blank()) + 
  ggtitle("") + guides(fill = FALSE)
p

library("ggsci")
library("ggplot2")
library("gridExtra")
p_lancet = p + scale_fill_lancet()
p_lancet

p_npg = p + scale_fill_npg()
p_npg

p_nejm = p + scale_fill_nejm()
p_nejm

p_jama = p + scale_fill_jama()
p_jama



