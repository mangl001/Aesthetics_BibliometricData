#Bibliometrix
library(bibliometrix)
update.packages("bibliometrix")
library(bibliometrix)

file <- "data/Analysis1_AestheticsAsAWhole/Allpublications.bib"

M <- convert2df(file = file, dbsource = "wos", format = "bibtex") #convert bib file to df
results<- biblioAnalysis(M) #ran analysis
S20=summary(object=results, k=25, pause=FALSE) #summary
plot(x=results, k= 25, pause=FALSE) #plot
##dataset
View(M)
write.csv(M, file = "M.csv")

#Growth TP
library(car)
library(QuantPsyc)
lm.tp <- lm(Figure_Growth_TP$Articles ~ Figure_Growth_TP$Year)
lm.tp3 <- lm(Figure_Growth_TP$Articles ~ poly(Figure_Growth_TP$Year,3))
summary(lm.tp)
summary(lm.tp3)
##Figure 2
library(ggplot2)
a <- ggplot(data=figure2, aes(x=year, y=aesthetics)) +
    geom_bar(stat="identity", color="black")+
    geom_line(aes(x = year, y = liking), size = 0.5, color="red", group = 1) +
    geom_line(aes(x = year, y = creativity), size = 0.5, color="blue", group = 1) +
    geom_line(aes(x = year, y = neuroeconmics), size = 0.5, color="green", group = 1) +
    geom_line(aes(x = year, y = hedonic), size = 0.5, color="orange", group = 1) +
    ylim(0,5000) +
    labs(x="Year", y = "Total number of publications")+
    theme_classic()
a

a2 <- a + scale_fill_brewer(palette="Greys") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ii=1970:2018
a2 + scale_x_continuous(breaks=ii) + 
    theme(axis.text=element_text(size=10), 
          axis.title=element_text(size=12))

ggsave("figure2.pdf", width=29, height=15, units = c("cm"),
       dpi=300, device = "pdf")

#TC
TC <- M$TC
Year <- M$PY
newdata.M <- data.frame(TC,Year)
View(newdata.M)
write.csv(newdata.M, file = "TC.csv")

SumCited<-aggregate(newdata.M[,c("TC")],by=list(newdata.M$Year),sum,na.rm=TRUE)
TcitedM<- aggregate(newdata.M[,c("TC")],by=list(newdata.M$Year),mean,na.rm=TRUE)
TcitedSD<- aggregate(newdata.M[,c("TC")],by=list(newdata.M$Year),sd,na.rm=TRUE)
MedianCited<-aggregate(newdata.M[,c("TC")],by=list(newdata.M$Year),median,na.rm=TRUE)
QuantileCited<-aggregate(newdata.M[,c("TC")],by=list(newdata.M$Year),quantile,na.rm=TRUE)
SECited<-aggregate(newdata.M[,c("TC")],by=list(newdata.M$Year),se,na.rm=TRUE)

sum(newdata.M$TC,na.rm=TRUE)
mean(newdata.M$TC,na.rm=TRUE)
sd(newdata.M$TC,na.rm=TRUE)
median(newdata.M$TC,na.rm=TRUE)
quantile(newdata.M$TC,na.rm=TRUE)

rTC <- lm(Figure3_TC_AV$Mean ~ Figure3_TC_AV$Year)
summary(rTC)

Figure3_TC_AV$Mean2 <- Figure3_TC_AV$Year^2
rTC.2 <- lm(Figure3_TC_AV$Mean ~ Figure3_TC_AV$Year + Figure3_TC_AV$Year2)
summary(rTC.2)

lm.TC3 <- lm(Figure3_TC_AV$Mean ~ poly(Figure3_TC_AV$Year,3))
summary(lm.TC3)

#fit first degree polynomial equation:
fit  <- lm(Figure3_TC_AV$Mean ~ Figure3_TC_AV$Year)
#second degree
fit2 <- lm(Figure3_TC_AV$Mean ~ poly(Figure3_TC_AV$Year,2, raw=TRUE))
#third degree
fit3 <- lm(Figure3_TC_AV$Mean ~ poly(Figure3_TC_AV$Year,3, raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
png(filename="citations_mini.png", 
    units="in", 
    width=10, 
    height=5, 
    pointsize=12, 
    res=300) 

png(filename="citations_mini.png", 
    units="in", 
    width=10, 
    height=8, 
    pointsize=12, 
    res=300) 
plot(Figure3_TC_AV$Year,Figure3_TC_AV$Mean,pch=19,ylim=c(0,40))
lines(Figure3_TC_AV$Year, predict(fit, data.frame(x=Figure3_TC_AV$Year)), col="red")
lines(Figure3_TC_AV$Year, predict(fit2, data.frame(x=Figure3_TC_AV$Year)), col="green")
lines(Figure3_TC_AV$Year, predict(fit3, data.frame(x=Figure3_TC_AV$Year)), col="blue")
dev.off()

summary(fit)
summary(fit2)
summary(fit3)

anova(fit3,fit4)

##Figure 3
c <- ggplot(data=Figure3_TC_AV, aes(x=Year, y=Mean)) +
    geom_bar(stat="identity", color="black")+
    geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                  position=position_dodge(0.005)) +
    ylim(0,40) +
    labs(x="Year", y = "Average citations per year")+
    theme_classic()
c

c2 <- c + scale_fill_brewer(palette="Greys") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ii=1970:2018
c2 + scale_x_continuous(breaks=ii) +
    theme(axis.text=element_text(size=10), 
          axis.title=element_text(size=12))

ggsave("figure2.pdf", width=29, height=15, units = c("cm"),
       dpi=300, device = "pdf")

ggsave("figure2.jpg", width=29, height=15, units = c("cm"),
       dpi=300, device = "jpg")

#Authors
M2 <- convert2df(D, dbsource = "isi", format = "bibtex") #convert to data frame
topAU <- authorProdOverTime(M2, k = 20, graph = TRUE)

#Geographical distribution
#NO CORRECTION
library(rworldmap)
library(data.table)
matched2 <- joinCountryData2Map(F6a_Country1, joinCode="NAME", nameJoinColumn="Country")
library(RColorBrewer)
colourPalette<-brewer.pal(9,'YlGnBu')
mapCountryData(matched2, nameColumnToPlot="Articles", catMethod = "fixedWidth", numCats= 9, mapTitle = "",  colourPalette = colourPalette)

png(filename="map1.png", 
    units="in", 
    width=33, 
    height=10, 
    pointsize=12, 
    res=300)
mapCountryData(matched2, nameColumnToPlot="Articles", catMethod = "fixedWidth", numCats= 9, mapTitle = "",  colourPalette = colourPalette)
dev.off()
#now with population controlled
matched3 <- joinCountryData2Map(Figure6b_corrected, joinCode="NAME", nameJoinColumn="Country")
mapCountryData(matched3, nameColumnToPlot="AV2", catMethod = "fixedWidth", numCats= 9, mapTitle = "",  colourPalette = colourPalette)

png(filename="map2.png", 
    units="in", 
    width=33, 
    height=10, 
    pointsize=12, 
    res=300)
mapCountryData(matched3, nameColumnToPlot="AV2", catMethod = "fixedWidth", numCats= 9, mapTitle = "",  colourPalette = colourPalette)
dev.off()


#TRYING
###Bibliographic coupling
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
###Bibliographic co-citaion
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

# Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, stemming=FALSE, labelsize=10, documents=10)

# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 10, sep = ";")
net <- histPlot(histResults, n=15, size = 10, labelsize=5)

