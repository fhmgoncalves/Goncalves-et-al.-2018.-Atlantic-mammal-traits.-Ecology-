###Script 6

##Graficos

##Carregando planilhas de dados para graficar

#atb.v2<-read.table("AtlanticBirdtraits_forgraphs_2018_07_d16.txt",sep=",",header =T)

#atb.v2<-atb.v2[,-1]


##Para quem esta rodando todos os scripts na sequencia
atb.v2<-m3.final

dim(atb.v2)
head(atb.v2)
names(atb.v2)

names(atb.v2)
##Aproveitando script do felipe martello "script_descriptive_figures_martello 2018_04_d14.R"

library(ggplot2)

list.files()

##Quantificando quantos registros em  cada coluna  (!= NAs)
x<-NA

for(i in 1:dim(atb.v2)[2]){
  x[i]<-  sum(is.na(atb.v2[,i])==F)
  x
}

counts.ABT<-data.frame(x)

counts.ABT$variables<-names(atb.v2)

dim(counts.ABT)
counts.ABT

str(counts.ABT)
dim(counts.ABT)
head(counts.ABT)    

counts.ABT$type<-NA

counts.ABT[order(counts.ABT$x,decreasing = T),]
counts.ABT

"continuos" -> counts.ABT[c(10:14,18,21:25,27,30,33),"type"]
"categorical" -> counts.ABT[c(35:42,44:49),"type"]


##calculando numero de especies por trait
counts.ABT[1,"variables"]

x<-as.vector(NA)

for(i in 1:dim(counts.ABT)[1]){
  x[i]<-length(unique(atb.v2[-which(is.na(atb.v2[,which(colnames(atb.v2)==counts.ABT[i,"variables"])])),"Binomial"]))
  
}

x
counts.ABT$n.spp<-x

summary(counts.ABT)
str(counts.ABT)

counts.ABT
dim(counts.ABT)

counts.ABT[order(counts.ABT$x,decreasing = T),]

write.table(counts.ABT,"counts_ABT_variables.xls",sep=",")

sum(table(m3.final$Binomial)==1)
sum(table(m3.final$Binomial)==2)


###Fazendo calculos do numero de localidades
names(atb.v2)
summary(atb.v2)

range(atb.v2$Latitude_decimal_degrees)
range(atb.v2$Longitude_decimal_degrees)

##number of localities sampled
length(table(sort(paste(atb.v2$Latitude_decimal_degrees,atb.v2$Longitude_decimal_degrees,sep=";"),decreasing = T)))

str(atb.v2)



#####
###Fazendo graficos de barras para registros por ordens e familias
##

library(scales)

x11(width=12,height=8)

g <- ggplot(atb.v2, aes(Order))
g + geom_histogram(stat = "count",binwidth = 1) + 
  theme(axis.text.x = element_text(angle=65,hjust = 1, vjust=1))+
  scale_y_continuous(breaks=c(0,1,10,100,1000,10000,50000),trans = 'log10')

savePlot("Barplot_order_ind.tiff",type="tiff")


g <- ggplot(atb.v2, aes(Family))
g + geom_histogram(stat = "count",binwidth = 1,boundary=-1.5) + 
  theme(axis.text.x = element_text(angle=65,hjust = 1, vjust=1))+
scale_y_continuous(breaks=c(0,1,10,100,1000,10000),trans = 'log10')


savePlot("Barplot_family_ind.tiff",type="tiff")


######
##Fazendo graficos de densidade para medidas de massa de corpo e comprimento de bico 
####

dim(atb.v2[which(is.na(atb.v2$Body_mass.g.)==F),])
table(atb.v2[which(is.na(atb.v2$Body_mass.g.)==F),"Order"])

order_bodym<-aggregate(atb.v2[which(is.na(atb.v2$Body_mass.g.)==F),"Body_mass.g."],by=list(atb.v2[which(is.na(atb.v2$Body_mass.g.)==F),"Order"]),median)

order_bodym
order_bodym[order(order_bodym[,2]),"Group.1"]
unique(atb.v2$Order)

order(order_bodym[,2])

library(ggridges)
library(ggplot2)

x11(width=14,height=8)

order_bodym[order(order_bodym[,2]),"Group.1"]

g1.1<-ggplot(atb.v2[which(is.na(atb.v2$Body_mass.g.)==F),], aes(x=Body_mass.g., y=factor(Order, levels=order_bodym[order(order_bodym[,2],decreasing = T),"Group.1"])))+
  geom_density_ridges(scale = 2,fill="firebrick3") + theme_ridges()+
  scale_x_continuous(trans='log10',breaks=c(1,10,100,1000))+
  labs(y="Order")+
  theme_bw(base_size = 15)

g1.1

names(atb.v2)
unique(atb.v2[which(is.na(atb.v2$Bill_length.mm.)==F),"Order"])

order_billl<-aggregate(atb.v2[which(is.na(atb.v2$Bill_length.mm.)==F),"Bill_length.mm."],by=list(atb.v2[which(is.na(atb.v2$Bill_length.mm.)==F),"Order"]),median)

order_billl

g1.2<-ggplot(atb.v2[which(is.na(atb.v2$Bill_length.mm.)==F & atb.v2$Order!="Cariamiformes"),], aes(x=Bill_length.mm., y=factor(Order, levels=order_billl[order(order_billl[,2],decreasing = T),"Group.1"])))+
  geom_density_ridges(scale = 2,fill="dodger blue3") + theme_ridges()+
  scale_x_reverse(limits = c(150,0))+
  scale_y_discrete(position = "right")+
  labs(y="Order")+
  theme_bw(base_size = 15)

g1.2


library(cowplot)

plot_grid(g1.1,g1.2,ncol=2)

savePlot("Density_bodybill.tiff",type="tiff")


#####
###Fazendo graficos de barras para numero de registros por variaveis
##

names(atb.v2)
counts.ABT
palette()

x11(width=14,height=8)
par(mfrow=c(1,1))

g1 <- ggplot(counts.ABT[which(counts.ABT$type=="continuos"),], aes(x=reorder(variables,x),y=x))

g1 <- g1 + geom_col(stat = "identity",fill="dodger blue3") + 
  theme(axis.text.x = element_text(hjust = 1, vjust=1))+
  xlab("Continuous Variables")+ylab("No. of Records (log10)")+
  scale_y_continuous(breaks=c(1,10,100,1000,10000,50000),trans = 'log10')+
  coord_flip()

g1

counts.ABT
require(grid)

##antes de rodar o  grafico 2, precisa carregar esta funcao abaixo
##esta funcao permite reverte o eixo e plotar as contagens em escala log10
##dica retirada de https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2#0.29459675542211294

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

g2 <- ggplot(counts.ABT[which(counts.ABT$type=="categorical"),])


g2 <- g2 + geom_col(mapping= aes(x=reorder(variables,-x),y=x),stat = "identity",fill="firebrick3") + 
  theme(axis.text.x = element_text(hjust = 1, vjust=1))+
  xlab("Categorical Variables")+ylab("No. of Records (log10)")+
  scale_y_continuous(breaks=c(1,10,100,1000,10000,50000),trans=reverselog_trans(10))+
  scale_x_discrete(position = 'top')+
  coord_flip()
  
g2


plot_grid(g1,g2,ncol=2)


savePlot("Barplot_contcat_ind.tiff",type="tiff")


#####Agora graficos de barras de numeros de especies por variaveis
x11(width=14,height=8)

g2.1 <- ggplot(counts.ABT[which(counts.ABT$type=="continuos"),], aes(x=reorder(variables,n.spp),y=n.spp))

g2.1 <- g2.1 + geom_col(stat = "identity",fill="dodger blue3") + 
  theme(axis.text.x = element_text(hjust = 1, vjust=1))+
  xlab("Continuous Variables")+ylab("No. of Species")+
  scale_y_continuous()+
  coord_flip()

g2.1

counts.ABT



g2.2 <- ggplot(counts.ABT[which(counts.ABT$type=="categorical"),])


g2.2 <- g2.2 + geom_col(mapping= aes(x=reorder(variables,-n.spp),y=n.spp),stat = "identity",fill="firebrick3") + 
  theme(axis.text.x = element_text(hjust = 1, vjust=1))+
  xlab("Categorical Variables")+ylab("No. of Species")+
  scale_y_reverse()+
  scale_x_discrete(position = 'top')+
  coord_flip()

g2.2

plot_grid(g2.1,g2.2,ncol=2)

savePlot("Barplot_contcat_spp.tiff",type="tiff")



#########
##Por ultimo, plotando numero de registros e de localidades por especies

sort(table(atb.v2$Binomial),decreasing=T)[1:25]

length(table(sort(paste(atb.v2$Latitude_decimal_degrees,atb.v2$Longitude_decimal_degrees,sep=";"),decreasing = T)))

atb.v2$Localities.per.coords<-paste(atb.v2$Latitude_decimal_degrees,atb.v2$Longitude_decimal_degrees,sep=";")

sort(table(atb.v2$Localities.per.coords),decreasing=T)[1:25]

atb.v2[which(atb.v2$Localities.per.coords=="-25.587558;-48.541493"),"Binomial"]

unique(atb.v2$Localities.per.coords)



##Vou fazer um for para calcular o numero de ocorrencias e de localidades de todas as especies
##Tb calcula estatisticas descritivas das medidas de todas as especies

length(table(atb.v2$Binomial))
length(unique(atb.v2$Binomial))

counts.ABT

summary(m3.final$Body_length.mm.)
variables<-as.vector(c("Body_mass.g.","Body_length.mm.","Wing_length.mm.","Tail_length.mm.",
                                  "Tarsus_length.mm.","Bill_length.mm.","Bill_depth.mm.","Bill_width.mm."))
variables


s<-data.frame(NA)
counts.ABT
dim(s)
head(s)


##
library(psych)


for (i in 1:length(table(atb.v2$Binomial))){
  
  s[i,1]<-i
  s[i,2]<-names(sort(table(atb.v2$Binomial),decreasing=T)[i])
  s[i,3]<-dim(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),])[1]
  s[i,4]<-length(unique(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T)[i])),"Localities.per.coords"]))

  for (j in 1:56){

    s[i,4+j]<-as.numeric(cbind(round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[1]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[2]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[3]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[4]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[5]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[6]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[7]],na.rm = T)[c(2:5,8:9,13)][1,],2),
                      round(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[i]),variables[8]],na.rm = T)[c(2:5,8:9,13)][1,],2)))[j]    
  }
}


variables.descriptive<-names(describe(atb.v2[which(atb.v2$Binomial==names(sort(table(atb.v2$Binomial),decreasing=T))[1]),variables[8]],na.rm = T)[c(2:5,8:9,13)][1,]  )
length(variables.descriptive)
dim(s)
head(s)

paste(rep(variables,each=7),variables.descriptive,sep = "")

names(s)<-c("Spp_ID","Binomial","No.Records","No.Localities",paste(rep(variables,each=7),variables.descriptive,sep = ""))

head(s)
tail(s)

s[716:722,]



##Recortando o BD somente para as especies do ABT
s<-s[c(1:718),]
dim(s)
head(s)
tail(s)

names(s)


##Exportando a planilha para verificacao e descricao do Luis Fabio

write.csv(s,"ATLANTIC_BIRD_TRAITS_Spp_Info.xls")

write.csv(s,"ATLANTIC_BIRD_TRAITS_Spp_Info.csv")


##Agora plotando os graficos
data.frame(sort(table(atb.v2$Binomial),decreasing=T)[1:25])
(table(s$Binomial))

x11(width=14,height=8)

g3.1 <- ggplot(data.frame(sort(table(atb.v2$Binomial),decreasing=T)[1:25]))

g3.1 <- g3.1 + aes(x=reorder(Var1,Freq),y=Freq) + geom_col(stat = "identity",fill="dodger blue3") + 
  theme(axis.text.x = element_text(hjust = 1, vjust=1))+
  xlab("Binomial")+ylab("No. of Records")+
  scale_y_continuous()+
  coord_flip()

g3.1

counts.ABT
s[order(s$No.Localities,decreasing = T),"No.Localities"][1:25]
s[order(s$No.Localities,decreasing = T)[1:25],]

g3.2 <- ggplot(s[order(s$No.Localities,decreasing = T)[1:25],])


g3.2 <- g3.2 + geom_col(mapping= aes(x=reorder(Binomial,-No.Localities),y=No.Localities),stat = "identity",fill="firebrick3") + 
  theme(axis.text.x = element_text(hjust = 1, vjust=1))+
  xlab("Binomial")+ylab("No. of Localities")+
  scale_y_reverse()+
  scale_x_discrete(position = 'top')+
  coord_flip()

g3.2

x11(width=14,height=8)

plot_grid(g3.1,g3.2,ncol=2)

savePlot("Barplot_reclocal_spp.tiff",type="tiff")

head(s)

head(s[order(s$No.Localities,decreasing=T),])
length(table(m3.final$Reference))
length(table(m3.final$Main_researcher))
unique(m3.final[grep(m3.final$Binomial,pattern="sp.",fixed = T),"Binomial"])


###
#Fimmm
###