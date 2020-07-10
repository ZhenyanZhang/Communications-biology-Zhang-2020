library(vegan)
library(permute)
library(lattice)
library(labdsv)
library(mgcv)
library(nlme)
library(MASS)

spe.pc=read.csv(file.choose(),row.names=1)
gene.pc=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)

#spe=otu file
#gene=ABGs file
# group=the group information of samples

spe=t(spe.pc)
gene=t(gene.pc)
spe.dis <- dsvdis(spe,'bray/curtis')
spe.pc <- pco(spe.dis,k=11)
#k=n-1(n=sample numbers)
gene.dis <- dsvdis(gene,'bray/curtis')
gene.pc <- pco(gene.dis,k=11)

spe.2.pc <- scores(spe.pc, choices = 1:2)
gene.2.pc <- scores(gene.pc, choices = 1:2)
pro <- protest(spe.2.pc,gene.2.pc,permutation=9999,scores = "sites")

point.color <- as.integer(group$treatment) + 1


plot(pro,type = "text")

pro
summary(pro)
plot(pro,
     ar.col="red", len="0.02", pch = 21,bg=point.color,cex=2,
     to.target = TRUE,display = c("target", "rotated"),main="",)
points(pro,display = "target",pch = 24,bg=point.color,cex=1.5)
legend("bottomright",c("SC","S70","S140","S280","GC","G70","G140","G280"),bty="n",
       pch = 22,pt.bg=c("black","gray","pink","yellow","blue","green3","red","cyan"),
       cex=1.1)
legend("bottomleft",c("16S","ABG"), pch = c(24,21),
       bty="n",cex=1.1)
pro
summary(pro)
residuals(pro)
?protest
plot(spe.pc)
mantel(spe.dis,gene.dis,method="spear")

