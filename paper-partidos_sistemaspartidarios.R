library(tidyverse)
library(ggplot2)
library(emdist)
library(haven)
library(magrittr)
library(corrgram)
library(psych)

setwd("C:/Users/test/Desktop/2018.2 - Partidos políticos e sistemas partidários/scripts")

# loading datasets
ches <- read.csv("ches_rrpp_westeu_intvar.csv")
ess <- read.csv("ess_rrpp_westeu_intvar.csv")

##### dividing datasets before and after 2008 #####
ches0814 <- ches[ches$year > 2008,]
ches0208 <- ches[ches$year <= 2008,]
ess0814 <- ess[ess$essround > 4,]
ess0208 <- ess[ess$essround <= 4,]

# creating function in emdist package. This function is used to calculate the
# EMD for two individual variables

emd.dis <- function(var1, var2){
  X <- as.matrix(var1[which(is.na(var1) == F)])
  Y <- as.matrix(var2[which(is.na(var2) == F)])
  weight.x <- rep(1/nrow(X),nrow(X))
  weight.y <- rep(1/nrow(Y),nrow(Y))
  emdw(X,weight.x,Y,weight.y,dist="manhattan")
}

##### joining variables regarding immigration feelings into one variable ##### 
##### factor analysis for immigration #####

# creating a small dataframe with the variables regarding immigration feelings
immig <- as.tibble(na.omit(cbind(ess$rejectsameimmig, ess$rejectdiffimmig, 
                         ess$rejectpoorimmig)))

# correlation matrix 
cormat <- cor(immig)
print(cormat, digits = 2)
corrgram(cormat, type = "cor", lower.panel = panel.shade, 
         upper.panel = panel.pie)

# principal component analysis
fit <- princomp(immig, cor = T)
plot(fit, type = "lines")

# PCA rotated with varimax
fit <- principal(immig, nfactors = 3, rotate = "varimax")
plot(fit)

# analysis
analysis <- as.tibble(list(nomes = c("Left-Right", "Supp. EU integration",
                                     "Supp. EP powers", "Postmat-Tradition",
                                     "Anti-Immigrants", "Assimilationism",
                                     "Envir. not important", "Liberties vs Law and Order",
                                     "Traditionalism", "Cosmopolitism", "Rel. principles",
                                     "Mkt Intervention")))

analysis$EMDGeneral <- c(
  emd.dis(ches$lrgen, ess$lrgen), emd.dis(ches$fav_intEU, ess$supportEU), 
  emd.dis(ches$fav_ep_power, ess$supportEP), emd.dis(ches$postmat_trad, ess$libcustoms),
  emd.dis(ches$reject_immig, ess$rejectdiffimmig), emd.dis(ches$assimil, ess$assimil),
  emd.dis(ches$envnotimp, ess$envnotimp), emd.dis(ches$laworder, ess$civliberties),
  emd.dis(ches$oppos_libpol, ess$libcustoms), emd.dis(ches$nation, ess$immgcntrybetter),
  emd.dis(ches$religious, ess$religious), emd.dis(ches$dereg_mkt, ess$govnotint)
                         )


##### plots #####
Graf10 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = ess, aes(x = VW, fill = 'orange'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(analysis$EMDGeneral[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
gen1 <- Graf10(ches$lrgen, ess$lrgen, "Left\n", "Right\n", 1, "Left-Right Self-positionment")

# support for EU integration
gen2 <- Graf10(ches$fav_intEU, ess$supportEU, "Disapprove EU integration\n", 
               "Support EU integration", 2, "Support to EU integration process")

# approves European Parliament powers
gen3 <- Graf10(ches$fav_ep_power, ess$supportEP, "Does not support the European Parliament\n", 
               "Supports the European Parliament>>\n", 3, 
               "How much support the powers of the European Parliament")

# Post-materialist or traditional values
ess$libcustoms <- max(ess$libcustoms, na.rm = T) - ess$libcustoms
gen4 <- Graf10(ches$postmat_trad, ess$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
gen5 <- Graf10(ches$reject_immig, ess$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants belonging to the same ethnic group")

# multiculturalism (separation of people belonging to different cultures) or assimilation
ess$assimil <- max(ess$assimil, na.rm = T) - ess$assimil
gen6 <- Graf10(ches$assimil, ess$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism (separation of people belonging to different cultures) or assimilation")

# importance of environment despite economic development
gen7 <- Graf10(ches$envnotimp, ess$envnotimp, "Important to take care of the environment\n", 
               "Environment is not that important", 7, "Importance to take care of the environment")

# promotion of civil liberties or tough measures to fight crime
ess$civliberties <- max(ess$civliberties, na.rm = T) - ess$civliberties
gen8 <- Graf10(ches$laworder, ess$civliberties, "Promotion of civil liberties\n", 
               "Promotion of tough policies to fight crime", 8, "Promotion of civil liberties or tough measures to fight crime") +
  scale_fill_identity(name = "", labels = c("Parties","Voters"), guide = "legend")

# opposition or agreement to liberal policies in social lifestyle
gen9 <- Graf10(ches$oppos_libpol, ess$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Opposition or agreement to liberal policies in social lifestyle")

# favours cosmopolitanism or nationalism
ess$immgcntrybetter <- max(ess$immgcntrybetter, na.rm = T)
gen10 <- Graf10(ches$nation, ess$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Favours cosmopolitanism or nationalism")

# religious principles
gen11 <- Graf10(ches$religious, ess$religious, "A little religious\n", "Very religious>>\n",
                11, "Religious principles")

# market regulation
gen12 <- Graf10(ches$dereg_mkt, ess$govnotint, "Supports regulation\n",
                "Opposes regulation>>\n", 12, "Market regulation")

##### analysis of congruence before 2008 #####
analysis$EMDbefore08 <- c(
  emd.dis(ches0208$lrgen, ess0208$lrgen), emd.dis(ches0208$fav_intEU, ess0208$supportEU), 
  emd.dis(ches0208$fav_ep_power, ess0208$supportEP), emd.dis(ches0208$postmat_trad, ess0208$libcustoms),
  emd.dis(ches0208$reject_immig, ess0208$rejectdiffimmig), emd.dis(ches0208$assimil, ess0208$assimil),
  emd.dis(ches0208$envnotimp, ess0208$envnotimp), emd.dis(ches0208$laworder, ess0208$civliberties),
  emd.dis(ches0208$oppos_libpol, ess0208$libcustoms), emd.dis(ches0208$cosmop_nation, ess0208$immgcntrybetter),
  emd.dis(ches0208$religious, ess0208$religious), emd.dis(ches0208$dereg_mkt, ess0208$govnotint)
)

# changing color of voter's distribution
Graf10b <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches0208, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = ess0208, aes(x = VW, fill = 'green'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(analysis$EMDbefore08[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
before1 <- Graf10b(ches0208$lrgen, ess0208$lrgen, "Left\n", "Right\n", 1, "Left-Right Self-positionment")

# support for EU integration
before2 <- Graf10b(ches0208$fav_intEU, ess0208$supportEU, "Disapprove EU integration\n", 
               "Support EU integration", 2, "Support to EU integration process0208")

# approves European Parliament powers
before3 <- Graf10b(ches0208$fav_ep_power, ess0208$supportEP, "Does not support the European Parliament\n", 
             "Supports the European Parliament>>\n", 3, 
               "How much support the powers of the European Parliament")

# Post-materialist or traditional values
ess0208$libcustoms <- max(ess0208$libcustoms, na.rm = T) - ess0208$libcustoms
before4 <- Graf10b(ches0208$postmat_trad, ess0208$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
before5 <- Graf10b(ches0208$reject_immig, ess0208$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants belonging to the same ethnic group")

# multiculturalism (separation of people belonging to different cultures) or assimilation
ess0208$assimil <- max(ess0208$assimil, na.rm = T) - ess0208$assimil
before6 <- Graf10b(ches0208$assimil, ess0208$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism (separation of people belonging to different cultures) or assimilation")

# promotion of civil liberties or tough measures to fight crime
ess0208$civliberties <- max(ess0208$civliberties, na.rm = T) - ess0208$civliberties
before8 <- Graf10b(ches0208$laworder, ess0208$civliberties, "Promotion of civil liberties\n", 
               "Promotion of tough policies to fight crime", 8, "Promotion of civil liberties or tough measures to fight crime") +
  scale_fill_identity(name = "", labels = c("Parties","Voters"), guide = "legend")

# opposition or agreement to liberal policies in social lifestyle
before9 <- Graf10b(ches0208$oppos_libpol, ess0208$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Opposition or agreement to liberal policies in social lifestyle")

# favours cosmopolitanism or nationalism
ess0208$immgcntrybetter <- max(ess0208$immgcntrybetter, na.rm = T)
before10 <- Graf10b(ches0208$cosmop_nation, ess0208$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Favours cosmopolitanism or nationalism")

# religious principles
before11 <- Graf10b(ches0208$religious, ess0208$religious, "A little religious\n", "Very religious>>\n",
                11, "Religious principles")

# market regulation
before12 <- Graf10b(ches0208$dereg_mkt, ess0208$govnotint, "Supports regulation\n",
                "Opposes regulation>>\n", 12, "Market regulation")

##### analysis of congruence after 2008 #####
analysis$EMDafter08 <- c(
  emd.dis(ches0814$lrgen, ess0814$lrgen), emd.dis(ches0814$fav_intEU, ess0814$supportEU), 
  emd.dis(ches0814$fav_ep_power, ess0814$supportEP), emd.dis(ches0814$postmat_trad, ess0814$libcustoms),
  emd.dis(ches0814$reject_immig, ess0814$rejectdiffimmig), emd.dis(ches0814$assimil, ess0814$assimil),
  emd.dis(ches0814$envnotimp, ess0814$envnotimp), emd.dis(ches0814$laworder, ess0814$civliberties),
  emd.dis(ches0814$oppos_libpol, ess0814$libcustoms), emd.dis(ches0814$nation, ess0814$immgcntrybetter),
  emd.dis(ches0814$religious, ess0814$religious), emd.dis(ches0814$dereg_mkt, ess0814$govnotint)
)

# changing color of voter's distribution
Graf10c <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches0814, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = ess0814, aes(x = VW, fill = 'red'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=6,label= paste("EMD =", round(analysis$EMDafter08[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
after1 <- Graf10c(ches0814$lrgen, ess0814$lrgen, "Left\n", "Right\n", 1, "Left-Right Self-positionment")

# support for EU integration
after2 <- Graf10c(ches0814$fav_intEU, ess0814$supportEU, "Disapprove EU integration\n", 
                   "Support EU integration", 2, "Support to EU integration process")

# approves European Parliament powers
after3 <- Graf10c(ches0814$fav_ep_power, ess0814$supportEP, "Does not support the European Parliament\n", 
                   "Supports the European Parliament>>\n", 3, 
                   "How much support the powers of the European Parliament")

# Post-materialist or traditional values
ess0814$libcustoms <- max(ess0814$libcustoms, na.rm = T) - ess0814$libcustoms
after4 <- Graf10c(ches0814$postmat_trad, ess0814$libcustoms, "Post-materialist values\n",
                   "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
after5 <- Graf10c(ches0814$reject_immig, ess0814$rejectdiffimmig, "Allow many immigrants\n",
                   "Allow few immigrants>>\n", 5, 
                   "Opposition to immigrants belonging to the same ethnic group")

# multiculturalism (separation of people belonging to different cultures) or assimilation
ess0814$assimil <- max(ess0814$assimil, na.rm = T) - ess0814$assimil
after6 <- Graf10c(ches0814$assimil, ess0814$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
                   "Multiculturalism (separation of people belonging to different cultures) or assimilation")

# promotion of civil liberties or tough measures to fight crime
ess0814$civliberties <- max(ess0814$civliberties, na.rm = T) - ess0814$civliberties
after8 <- Graf10c(ches0814$laworder, ess0814$civliberties, "Promotion of civil liberties\n", 
                   "Promotion of tough policies to fight crime", 8, "Promotion of civil liberties or tough measures to fight crime") +
  scale_fill_identity(name = "After 2008", labels = c("Parties","Voters"), guide = "legend")

# opposition or agreement to liberal policies in social lifestyle
after9 <- Graf10c(ches0814$oppos_libpol, ess0814$libcustoms, "Supports liberal lifestyle\n",
                   "Supports traditional lifestyle>>\n", 9, "Opposition or agreement to liberal policies in social lifestyle")

# favours cosmopolitanism or nationalism
ess0814$immgcntrybetter <- max(ess0814$immgcntrybetter, na.rm = T)
after10 <- Graf10c(ches0814$nation, ess0814$immgcntrybetter, "cosmopolitanism\n", 
                    "nationalism>>\n", 10, "Favours cosmopolitanism or nationalism")

# religious principles
after11 <- Graf10c(ches0814$religious, ess0814$religious, "A little religious\n", "Very religious>>\n",
                    11, "Religious principles")

# market regulation
after12 <- Graf10c(ches0814$dereg_mkt, ess0814$govnotint, "Supports regulation\n",
                    "Opposes regulation>>\n", 12, "Market regulation")
