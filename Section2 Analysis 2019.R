#Bibliometrix
library(bibliometrix)
update.packages("bibliometrix")
library(bibliometrix)
#A&H
options(digits=5)

D.AandH<- readFiles("A&H_ALL.bib")
M.AndH <- convert2df(D.AandH, dbsource = "isi", format = "bibtex") #convert to data frame
results.AandH<- biblioAnalysis(M.AndH)
S12=summary(object=results.AandH, k=12, pause=FALSE)
plot(x=results.AandH, k= 12, pause=FALSE)

TC.AandH <- M.AndH$TC
Year.AandH <- M.AndH$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data.AandH)
#LS&B
setwd("/Users/manu/Documents/RProject/Aesthetics/datasets/Analysis 2019/Section2/*WoS Categories/LS&B")
D.LSandB<- readFiles("LS&B_ALL.bib")
M.LSandB <- convert2df(D.LSandB, dbsource = "isi", format = "bibtex") #convert to data frame
results.LSandB<- biblioAnalysis(M.LSandB)
S12=summary(object=results.LSandB, k=12, pause=FALSE)
plot(x=results.LFandB, k= 12, pause=FALSE)

TC.AandH <- M.LSandB$TC
Year.AandH <- M.LSandB$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data.AandH)

#PS
setwd("/Users/manu/Documents/RProject/Aesthetics/datasets/Analysis 2019/Section2/*WoS Categories/PS")
D.PS<- readFiles("PS_ALL.bib")
M.PS <- convert2df(D.PS, dbsource = "isi", format = "bibtex") #convert to data frame
results.PS<- biblioAnalysis(M.PS)
S12=summary(object=results.PS, k=12, pause=FALSE)
plot(x=results.LFandB, k= 12, pause=FALSE)

TC.AandH <- M.PS$TC
Year.AandH <- M.PS$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data.AandH)
#SS
setwd("/Users/manu/Documents/RProject/Aesthetics/datasets/Analysis 2019/Section2/*WoS Categories/SS")
D.SS<- readFiles("SS_ALL.bib")
M.SS <- convert2df(D.SS, dbsource = "isi", format = "bibtex") #convert to data frame
results.SS<- biblioAnalysis(M.SS)
S12=summary(object=results.SS, k=12, pause=FALSE)
plot(x=results.LFandB, k= 12, pause=FALSE)

TC.AandH <- M.SS$TC
Year.AandH <- M.SS$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data.AandH)
#T
setwd("/Users/manu/Documents/RProject/Aesthetics/datasets/Analysis 2019/Section2/*WoS Categories/T")
D.T<- readFiles("T_ALL.bib")
M.T <- convert2df(D.T, dbsource = "isi", format = "bibtex") #convert to data frame
results.T<- biblioAnalysis(M.T)
S12=summary(object=results.T, k=12, pause=FALSE)

TC.AandH <- M.T$TC
Year.AandH <- M.T$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data.AandH)
#P
setwd("/Users/manu/Documents/RProject/Aesthetics/datasets/Analysis 2019/Section2/*WoS Categories/P&N")
D.P <- readFiles("P&N_ALL.bib")
M.P <- convert2df(D.P, dbsource = "isi", format = "bibtex") #convert to data frame
results.P<- biblioAnalysis(M.P)
S12=summary(object=results.P, k=12, pause=FALSE)

TC.AandH <- M.P$TC
Year.AandH <- M.P$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data.AandH)

#Create dataset
library(tidyverse)

TC.AandH <- M.AndH$TC
Year.AandH <- M.AndH$PY
newdata.M.AandH <- data.frame(TC.AandH,Year.AandH)
data.AandH <- newdata.M.AandH %>% 
    group_by(Year.AandH) %>%
    summarize(MeanTC = mean(TC.AandH),
              TotalTC = sum(as.numeric(TC.AandH)))
View(data1)


library(readxl)
figure_fact <- read_excel("~/Documents/RProject/Aesthetics/datasets/Analysis 2019/Section2/*WoS Categories/Figure.Factfulness.xlsx")
View(figure_fact)

###ANALYSIS BY DECADES
figure_fact <- Figure_Factfulness 
library(tidyverse)
figure_fact_final <- mutate(figure_fact, 
                             Decades = ifelse(Year %in% 1970:1979, "1970s",
                                        ifelse(Year %in% 1980:1989, "1980s",
                                        ifelse(Year %in% 1990:1999, "1990s",
                                        ifelse(Year %in% 2000:2009, "2000s","2010s")))))


data1 <- figure_fact_final %>%
    group_by(Decades, ResearchArea, Year) %>%
    summarise(n=n(), TC.mean = mean(SumTC, na.rm=TRUE),
              TP.mean= mean(TP, na.rm= TRUE))

data1$ResearchArea <- ordered(data1$ResearchArea, levels = c("A&H", "LS&B", "PS","SS","T","P"))
data1$ResearchArea <- as.facotr(data1$ResearchArea)

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
figure7 <- ggplot(data1, aes(x= TP.mean, y= TC.mean,fill= ResearchArea,color= ResearchArea,shape= ResearchArea)) +
    geom_point(size=2.5) + 
    expand_limits(y=0) +
    facet_wrap(~ Decades) + 
    theme_bw()


figure7 + scale_colour_manual(values=c("#56B4E9", "#D55E00", "#009E73",
                                     "#E69F00", "#CC79A7", "#F0E442")) +
    theme(legend.title=element_text(size=14), 
                legend.text = element_text(size = 14),
                strip.text = element_text(size=14), 
                axis.text=element_text(size=14), 
                axis.title=element_text(size=14))

ggsave("figure7.pdf", width=29, height=15, units = c("cm"),
       dpi=300, device = "pdf")
