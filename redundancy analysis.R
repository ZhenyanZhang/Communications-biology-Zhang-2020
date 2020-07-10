# DCA to choose RDA or CCA analysis
library(vegan)
otu<-read.csv(file.choose(), row.names=1)
otu=t(otu)
otu.dca<-decorana(otu)
otu.dca
#if DCA1 < 3,RDA better than CCA;if DCA1 > 4,CCA better than RDA;3 < DCA1 < 4, both ok.

# analysis by vegen
genus=read.csv(file.choose(),row.names=1)
env=read.csv(file.choose(),row.names=1)

genus.cca=cca(genus,env)
genus.cca
genus.cca=cca(genus,env)
ef=envfit(genus.cca,env,permu=999)
ef
plot(genus.cca) 

#prepare for visualization by ggplot2
new<-genus.cca$CCA
new
#obtain sample information
samples<-data.frame(sample=row.names(new$u),RDA1=new$u[,1],RDA2=new$u[,2])
samples
#obtain species information
species<-data.frame(spece=row.names(new$v),RDA1=new$v[,1],RDA2=new$v[,2])
species
#obtain environmental factors information
envi<-data.frame(en=row.names(new$biplot),RDA1=new$biplot[,1],RDA2=new$biplot[,2])
envi

#bulid coordinate frame
line_x = c(0,envi[1,2],0,envi[2,2],0,envi[3,2],0,envi[4,2],0,envi[5,2],0,envi[6,2])
line_x
line_y = c(0,envi[1,3],0,envi[2,3],0,envi[3,3],0,envi[4,3],0,envi[5,3],0,envi[6,3])
line_y
line_g = c("pH","pH","T","T","S2","S2","NH4","NH4","NO2","NO2","Fe2","Fe2")
line_g
line_data = data.frame(x=line_x,y=line_y,group=line_g)
line_data

#visualized by ggplot2
library(ggplot2)

ggplot(data=samples,aes(RDA1,RDA2)) + geom_point(aes(color=sample),size=2) +
  geom_point(data=species,aes(shape=spece),size=2) + 
  geom_text(data=envi,aes(label=en),color="blue") +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)+
  geom_line(data=line_data,aes(x=x,y=y,group=group),color="green") +
  theme_bw() + theme(panel.grid=element_blank()) 

ggsave("RDA2.PDF")

