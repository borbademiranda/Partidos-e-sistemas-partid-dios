library(tidyverse)
library(ggplot2)
library(GGally)
library(emdist)
library(haven)
library(magrittr)
library(corrgram)
library(psych)

##### loading dataset for west european countries #####
cheswesteu <- read.csv("ches_westeu_intvar.csv")
esswesteu <- read.csv("ess_westeu_intvar.csv")

# only governing parties
gov <- read.csv("ches_gov_westeu_intvar.csv")
gov <- gov[!gov$country %in% c("esp", "irl", "lux", "por"),]

# creating dataset to insert EMD's
EMD <- as.tibble(list(nomes = c("Left-Right", "Supp. EU integration",
                                "Supp. EP powers", "Postmat-Tradition",
                                "Anti-Immigrants", "Assimilationism",
                                "Envir. not important", "Liberties vs Law and Order",
                                "Traditionalism", "Cosmopolitism", "Rel. principles",
                                "Mkt Intervention")))

# creating function in emdist package. This function is used to calculate the
# EMD for two individual variables. Based in Meira, 2018

emd.dis <- function(var1, var2){
  X <- as.matrix(var1[which(is.na(var1) == F)])
  Y <- as.matrix(var2[which(is.na(var2) == F)])
  weight.x <- rep(1/nrow(X),nrow(X))
  weight.y <- rep(1/nrow(Y),nrow(Y))
  emdw(X,weight.x,Y,weight.y,dist="manhattan")
}

##### subsetting by countries #####
chesfr <- gov[gov$country == "fr",]
essfr <- esswesteu[esswesteu$cntry == "FR",]

chesat <- gov[gov$country == "aus",]
essat <- esswesteu[esswesteu$cntry == "AT",]

chesbe <- gov[gov$country == "be",]
essbe <- esswesteu[esswesteu$cntry == "BE",]

chesdk <- gov[gov$country == "dk",]
essdk <- esswesteu[esswesteu$cntry == "DK",]

chesde <- gov[gov$country == "ge",]
essde <- esswesteu[esswesteu$cntry == "DE",]

# selecting random rows from the German dataset (the emd cannot calculate distances in big datasets)
essde <- essde[sample(nrow(essde), 15000),]

chesfi <- gov[gov$country == "fin",]
essfi <- esswesteu[esswesteu$cntry == "FI",]

chesuk <- gov[gov$country == "uk",]
essuk <- esswesteu[esswesteu$cntry == "GB",]

chesgr <- gov[gov$country == "gr",]
essgr <- esswesteu[esswesteu$cntry == "GR",]

chesit <- gov[gov$country == "it",]
essit <- esswesteu[esswesteu$cntry == "IT",]

chesnl <- gov[gov$country == "nl",]
essnl <- esswesteu[esswesteu$cntry == "NL",]

chessv <- gov[gov$country == "sv",]
esssv <- esswesteu[esswesteu$cntry == "SE",]

##### inserting EMD by country in a dataframe #####
EMD$fr <- c(
  emd.dis(chesfr$lrgen, essfr$lrgen), emd.dis(chesfr$fav_intEU, essfr$supportEU), 
  emd.dis(chesfr$fav_ep_power, essfr$supportEP), emd.dis(chesfr$postmat_trad, essfr$libcustoms),
  emd.dis(chesfr$reject_immig, essfr$rejectdiffimmig), emd.dis(chesfr$assimil, essfr$assimil),
  emd.dis(chesfr$envnotimp, essfr$envnotimp), emd.dis(chesfr$laworder, essfr$civliberties),
  emd.dis(chesfr$oppos_libpol, essfr$libcustoms), emd.dis(chesfr$nation, essfr$immgcntrybetter),
  emd.dis(chesfr$religious, essfr$religious), emd.dis(chesfr$dereg_mkt, essfr$govnotint)
)

EMD$at <- c(
  emd.dis(chesat$lrgen, essat$lrgen), emd.dis(chesat$fav_intEU, essat$supportEU), 
  emd.dis(chesat$fav_ep_power, essat$supportEP), emd.dis(chesat$postmat_trad, essat$libcustoms),
  emd.dis(chesat$reject_immig, essat$rejectdiffimmig), emd.dis(chesat$assimil, essat$assimil),
  emd.dis(chesat$envnotimp, essat$envnotimp), emd.dis(chesat$laworder, essat$civliberties),
  emd.dis(chesat$oppos_libpol, essat$libcustoms), emd.dis(chesat$nation, essat$immgcntrybetter),
  emd.dis(chesat$religious, essat$religious), emd.dis(chesat$dereg_mkt, essat$govnotint)
)

EMD$be <- c(
  emd.dis(chesbe$lrgen, essbe$lrgen), emd.dis(chesbe$fav_intEU, essbe$supportEU), 
  emd.dis(chesbe$fav_ep_power, essbe$supportEP), emd.dis(chesbe$postmat_trad, essbe$libcustoms),
  emd.dis(chesbe$reject_immig, essbe$rejectdiffimmig), emd.dis(chesbe$assimil, essbe$assimil),
  emd.dis(chesbe$envnotimp, essbe$envnotimp), emd.dis(chesbe$laworder, essbe$civliberties),
  emd.dis(chesbe$oppos_libpol, essbe$libcustoms), emd.dis(chesbe$nation, essbe$immgcntrybetter),
  emd.dis(chesbe$religious, essbe$religious), emd.dis(chesbe$dereg_mkt, essbe$govnotint)
)

EMD$dk <- c(
  emd.dis(chesdk$lrgen, essdk$lrgen), emd.dis(chesdk$fav_intEU, essdk$supportEU), 
  emd.dis(chesdk$fav_ep_power, essdk$supportEP), emd.dis(chesdk$postmat_trad, essdk$libcustoms),
  emd.dis(chesdk$reject_immig, essdk$rejectdiffimmig), emd.dis(chesdk$assimil, essdk$assimil),
  emd.dis(chesdk$envnotimp, essdk$envnotimp), emd.dis(chesdk$laworder, essdk$civliberties),
  emd.dis(chesdk$oppos_libpol, essdk$libcustoms), emd.dis(chesdk$nation, essdk$immgcntrybetter),
  emd.dis(chesdk$religious, essdk$religious), emd.dis(chesdk$dereg_mkt, essdk$govnotint)
)

EMD$de <- c(
  emd.dis(chesde$lrgen, essde$lrgen), emd.dis(chesde$fav_intEU, essde$supportEU), 
  emd.dis(chesde$fav_ep_power, essde$supportEP), emd.dis(chesde$postmat_trad, essde$libcustoms),
  emd.dis(chesde$reject_immig, essde$rejectdiffimmig), emd.dis(chesde$assimil, essde$assimil),
  emd.dis(chesde$envnotimp, essde$envnotimp), emd.dis(chesde$laworder, essde$civliberties),
  emd.dis(chesde$oppos_libpol, essde$libcustoms), emd.dis(chesde$nation, essde$immgcntrybetter),
  emd.dis(chesde$religious, essde$religious), emd.dis(chesde$dereg_mkt, essde$govnotint)
)

EMD$fi <- c(
  emd.dis(chesfi$lrgen, essfi$lrgen), emd.dis(chesfi$fav_intEU, essfi$supportEU), 
  emd.dis(chesfi$fav_ep_power, essfi$supportEP), emd.dis(chesfi$postmat_trad, essfi$libcustoms),
  emd.dis(chesfi$reject_immig, essfi$rejectdiffimmig), emd.dis(chesfi$assimil, essfi$assimil),
  emd.dis(chesfi$envnotimp, essfi$envnotimp), emd.dis(chesfi$laworder, essfi$civliberties),
  emd.dis(chesfi$oppos_libpol, essfi$libcustoms), emd.dis(chesfi$nation, essfi$immgcntrybetter),
  emd.dis(chesfi$religious, essfi$religious), emd.dis(chesfi$dereg_mkt, essfi$govnotint)
)

EMD$uk <- c(
  emd.dis(chesuk$lrgen, essuk$lrgen), emd.dis(chesuk$fav_intEU, essuk$supportEU), 
  emd.dis(chesuk$fav_ep_power, essuk$supportEP), emd.dis(chesuk$postmat_trad, essuk$libcustoms),
  emd.dis(chesuk$reject_immig, essuk$rejectdiffimmig), emd.dis(chesuk$assimil, essuk$assimil),
  emd.dis(chesuk$envnotimp, essuk$envnotimp), emd.dis(chesuk$laworder, essuk$civliberties),
  emd.dis(chesuk$oppos_libpol, essuk$libcustoms), emd.dis(chesuk$nation, essuk$immgcntrybetter),
  emd.dis(chesuk$religious, essuk$religious), emd.dis(chesuk$dereg_mkt, essuk$govnotint)
)

EMD$gr <- c(
  emd.dis(chesgr$lrgen, essgr$lrgen), emd.dis(chesgr$fav_intEU, essgr$supportEU), 
  emd.dis(chesgr$fav_ep_power, essgr$supportEP), emd.dis(chesgr$postmat_trad, essgr$libcustoms),
  emd.dis(chesgr$reject_immig, essgr$rejectdiffimmig), emd.dis(chesgr$assimil, essgr$assimil),
  emd.dis(chesgr$envnotimp, essgr$envnotimp), emd.dis(chesgr$laworder, essgr$civliberties),
  emd.dis(chesgr$oppos_libpol, essgr$libcustoms), emd.dis(chesgr$nation, essgr$immgcntrybetter),
  emd.dis(chesgr$religious, essgr$religious), emd.dis(chesgr$dereg_mkt, essgr$govnotint)
)

EMD$it <- c(
  emd.dis(chesit$lrgen, essit$lrgen), emd.dis(chesit$fav_intEU, essit$supportEU), 
  emd.dis(chesit$fav_ep_power, essit$supportEP), emd.dis(chesit$postmat_trad, essit$libcustoms),
  emd.dis(chesit$reject_immig, essit$rejectdiffimmig), emd.dis(chesit$assimil, essit$assimil),
  emd.dis(chesit$envnotimp, essit$envnotimp), emd.dis(chesit$laworder, essit$civliberties),
  emd.dis(chesit$oppos_libpol, essit$libcustoms), emd.dis(chesit$nation, essit$immgcntrybetter),
  emd.dis(chesit$religious, essit$religious), emd.dis(chesit$dereg_mkt, essit$govnotint)
)

EMD$nl <- c(
  emd.dis(chesnl$lrgen, essnl$lrgen), emd.dis(chesnl$fav_intEU, essnl$supportEU), 
  emd.dis(chesnl$fav_ep_power, essnl$supportEP), emd.dis(chesnl$postmat_trad, essnl$libcustoms),
  emd.dis(chesnl$reject_immig, essnl$rejectdiffimmig), emd.dis(chesnl$assimil, essnl$assimil),
  emd.dis(chesnl$envnotimp, essnl$envnotimp), emd.dis(chesnl$laworder, essnl$civliberties),
  emd.dis(chesnl$oppos_libpol, essnl$libcustoms), emd.dis(chesnl$nation, essnl$immgcntrybetter),
  emd.dis(chesnl$religious, essnl$religious), emd.dis(chesnl$dereg_mkt, essnl$govnotint)
)

EMD$sv <- c(
  emd.dis(chessv$lrgen, esssv$lrgen), emd.dis(chessv$fav_intEU, esssv$supportEU), 
  emd.dis(chessv$fav_ep_power, esssv$supportEP), emd.dis(chessv$postmat_trad, esssv$libcustoms),
  emd.dis(chessv$reject_immig, esssv$rejectdiffimmig), emd.dis(chessv$assimil, esssv$assimil),
  emd.dis(chessv$envnotimp, esssv$envnotimp), emd.dis(chessv$laworder, esssv$civliberties),
  emd.dis(chessv$oppos_libpol, esssv$libcustoms), emd.dis(chessv$nation, esssv$immgcntrybetter),
  emd.dis(chessv$religious, esssv$religious), emd.dis(chessv$dereg_mkt, esssv$govnotint)
)

EMD$means <- rowMeans(EMD[,-1])

write.csv(EMD, "emdbycountry.csv")

##### plots #####

# creating functions
plotgov1 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = gov, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = esswesteu, aes(x = VW, fill = 'lightblue'), alpha = .3, adjust = 5) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,6), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(EMD$means[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

plotgov2 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = gov, aes(x = VB, fill = 'black'), alpha = .3, adjust = 1) +
    geom_density(data = esswesteu, aes(x = VW, fill = 'lightblue'), alpha = .3, adjust = 5) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,6), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(EMD$means[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

plotgov3 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = gov, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = esswesteu, aes(x = VW, fill = 'lightblue'), alpha = .3, adjust = 5) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,6), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(EMD$means[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

plotgov4 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = gov, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = esswesteu, aes(x = VW, fill = 'lightblue'), alpha = .3, adjust = 5) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,6), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(EMD$means[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

plotgov5 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = gov, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = esswesteu, aes(x = VW, fill = 'lightblue'), alpha = .3, adjust = 7) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,6), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(EMD$means[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
gov1 <- plotgov1(gov$lrgen, esswesteu$lrgen, "Left\n", "Right\n", 1, "Left-Right Self-positionment") +
  scale_fill_identity(name = "Government parties", labels = c("Parties","Voters"), guide = "legend")

# support for EU integration
gov2 <- plotgov2(gov$fav_intEU, esswesteu$supportEU, "Disapprove EU integration\n", 
               "Support EU integration>>\n", 2, "Support to EU integration process")

# approves European Parliament powers
gov3 <- plotgov2(gov$fav_ep_power, esswesteu$supportEP, "Does not support the European Parliament\n", 
               "Supports the European Parliament>>\n", 3, 
               "Support powers of the European Parliament")

# Post-materialist or traditional values
esswesteu$libcustoms <- max(esswesteu$libcustoms, na.rm = T) - esswesteu$libcustoms
gov4 <- plotgov2(gov$postmat_trad, esswesteu$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
gov5 <- plotgov3(gov$reject_immig, esswesteu$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants")

# multiculturalism (separation of people belonging to different cultures) or assimilation
gov$assimil <- max(gov$assimil, na.rm = T) - gov$assimil
gov6 <- plotgov2(gov$assimil, esswesteu$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism  or assimilation")

# importance of environment despite economic development
gov7 <- plotgov4(gov$envnotimp, esswesteu$envnotimp, "Important to take care of the environment\n", 
               "Environment is not that important>>\n", 7, "Environment")

# promotion of civil liberties or tough measures to fight crime
esswesteu$civliberties <- max(esswesteu$civliberties, na.rm = T) - esswesteu$civliberties
gov8 <- plotgov5(gov$laworder, esswesteu$civliberties, "Promotion of civil liberties\n", 
               "Promotion of tough policies to fight crime", 8, "Law and Order")
  
# opposition or agreement to liberal policies in social lifestyle
esswesteu$libcustoms <- max(esswesteu$libcustoms, na.rm = T) - esswesteu$libcustoms
gov9 <- plotgov2(gov$oppos_libpol, esswesteu$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Social lifestyle")

# favours cosmopolitanism or nationalism
esswesteu$immgcntrybetter <- max(esswesteu$immgcntrybetter, na.rm = T) - esswesteu$immgcntrybetter
gov10 <- plotgov2(gov$nation, esswesteu$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Favours cosmopolitanism or nationalism")

# religious principles
gov11 <- plotgov2(gov$religious, esswesteu$religious, "A little religious\n", "Very religious>>\n",
                11, "Religious principles")

# market regulation
gov12 <- plotgov3(gov$dereg_mkt, esswesteu$govnotint, "Supports regulation\n",
                "Opposes regulation>>\n", 12, "Market regulation")

##### multiplot function #####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##### saving plots #####
ggsave("gen1.png", plot = multiplot(gov1, gov2, gov3, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("gen2.png", plot = multiplot(gov4, gov5, gov6, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("gen3.png", plot = multiplot(gov7, gov8, gov9, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("gen4.png", plot = multiplot(gov10, gov11, gov12, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")

##### saving compared plots joint with the plots of the principal script #####
################################################################################
# ATENTION!!! remember to load the script "paper-partidos_sistemaspartidarios #
################################################################################

setwd("./plotsgov_rrpp")

ggsave("lr.png", plot = multiplot(gov1, gen1, cols = 2), 
       device = "png",  width = 12, height = 3, units = "in")
ggsave("euint.png", plot = multiplot(gov2, gen2, cols = 2), device = "png",
       width = 12, height = 3, units = "in")
ggsave("eppower.png", plot = multiplot(gov3, gen3, cols = 2), device = "png",
       width = 12, height = 3, units = "in")
ggsave("post-trad.png", plot = multiplot(gov4, gen4, cols = 2), device = "png",
       width = 12, height = 3, units = "in")
ggsave("antimmig.png", plot = multiplot(gov5, gen5, cols = 2), 
       device = "png", width = 12, height = 3, units = "in")
ggsave("assimil.png", plot = multiplot(gov6, gen6, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
ggsave("envnotimp.png", plot = multiplot(gov7, gen7, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
ggsave("laworder.png", plot = multiplot(gov8, gen8, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
ggsave("tradition.png", plot = multiplot(gov9, gen9, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
ggsave("cosmop.png", plot = multiplot(gov10, gen10, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
ggsave("religion.png", plot = multiplot(gov11, gen11, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
ggsave("market.png", plot = multiplot(gov12, gen12, cols = 2),
       device = "png", width = 12, height = 3, units = "in")
