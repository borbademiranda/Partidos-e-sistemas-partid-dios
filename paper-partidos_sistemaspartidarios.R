library(tidyverse)
library(ggplot2)
library(emdist)
library(haven)
library(magrittr)
library(corrgram)
library(psych)
library(lme4)

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
  ggplot() + geom_density(data = ches, aes(x = VB, fill = 'black'), alpha = .3, adjust = 3) +
    geom_density(data = ess, aes(x = VW, fill = 'orange'), alpha = .3, adjust = 2) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.3,y=5, label = paste("EMD =", round(analysis$EMDGeneral[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

Graf5 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches, aes(x = VB, fill = 'black'), alpha = .3, adjust = 8) +
    geom_density(data = ess, aes(x = VW, fill = 'orange'), alpha = .3, adjust = 3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.3,y=5,label= paste("EMD =", round(analysis$EMDGeneral[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

Graf10b <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches, aes(x = VB, fill = 'black'), alpha = .3, adjust = 5) +
    geom_density(data = ess, aes(x = VW, fill = 'orange'), alpha = .3, adjust = 3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.3,y=5,label= paste("EMD =", round(analysis$EMDGeneral[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
gen1 <- Graf10(ches$lrgen, ess$lrgen, "Left\n", "Right>>\n", 1, "Left-Right Self-positionment") +
  scale_fill_identity(name = "Total time lapse", labels = c("Parties","Voters"), guide = "legend")


# support for EU integration
gen2 <- Graf10(ches$fav_intEU, ess$supportEU, "Disapprove EU integration\n", 
               "Support EU integration>>\n", 2, "Support to EU integration process")

# approves European Parliament powers
gen3 <- Graf10(ches$fav_ep_power, ess$supportEP, "Does not support the E.P.\n", 
               "Supports the E.P.>>\n", 3, "Support powers of the E.P.")

# Post-materialist or traditional values
ess$libcustoms <- max(ess$libcustoms, na.rm = T) - ess$libcustoms
gen4 <- Graf10(ches$postmat_trad, ess$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
gen5 <- Graf5(ches$reject_immig, ess$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants")

# multiculturalism (separation of people belonging to different cultures) or assimilation
ess$assimil <- max(ess$assimil, na.rm = T) - ess$assimil
gen6 <- Graf5(ches$assimil, ess$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism or assimilation")

# importance of environment despite economic development
gen7 <- Graf10(ches$envnotimp, ess$envnotimp, "Important to take care of the environment\n", 
               "Environment is not that important", 7, "Importance to take care of the environment")

# promotion of civil liberties or tough measures to fight crime
ess$civliberties <- max(ess$civliberties, na.rm = T) - ess$civliberties
gen8 <- Graf10(ches$laworder, ess$civliberties, "Promotion of civil liberties\n", 
               "Tough policies to fight crime>>\n", 8, "Law and Order")

# opposition or agreement to liberal policies in social lifestyle
ess$libcustoms <- max(ess$libcustoms, na.rm = T) - ess$libcustoms
gen9 <- Graf10(ches$oppos_libpol, ess$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Social lifestyle")

# favours cosmopolitanism or nationalism
ess$immgcntrybetter <- max(ess$immgcntrybetter, na.rm = T) - ess$immgcntrybetter
gen10 <- Graf10b(ches$nation, ess$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Cosmopolitanism or nationalism")

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
Graf10d <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches0208, aes(x = VB, fill = 'black'), alpha = .3, adjust = 3) +
    geom_density(data = ess0208, aes(x = VW, fill = 'green'), alpha = .3, adjust = 2) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.3,y=5,label= paste("EMD =", round(analysis$EMDbefore08[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
before1 <- Graf10d(ches0208$lrgen, ess0208$lrgen, "Left\n", "Right>>\n", 1, "Left-Right Self-positionment") +
  scale_fill_identity(name = "Before 2008", labels = c("Parties","Voters"), guide = "legend")

# support for EU integration
before2 <- Graf10d(ches0208$fav_intEU, ess0208$supportEU, "Disapprove EU integration\n", 
               "Support EU integration>>\n", 2, "Support to EU integration process")

# approves European Parliament powers
before3 <- Graf10d(ches0208$fav_ep_power, ess0208$supportEP, "Does not support the E.P.\n", 
                   "Supports the E.P.>>\n", 3, "Support powers of the E.P.")

# Post-materialist or traditional values
ess0208$libcustoms <- max(ess0208$libcustoms, na.rm = T) - ess0208$libcustoms
before4 <- Graf10d(ches0208$postmat_trad, ess0208$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
before5 <- Graf10d(ches0208$reject_immig, ess0208$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants")

# multiculturalism (separation of people belonging to different cultures) or assimilation
ess0208$assimil <- max(ess0208$assimil, na.rm = T) - ess0208$assimil
before6 <- Graf10d(ches0208$assimil, ess0208$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism or assimilation")

# promotion of civil liberties or tough measures to fight crime
ess0208$civliberties <- max(ess0208$civliberties, na.rm = T) - ess0208$civliberties
before8 <- Graf10d(ches0208$laworder, ess0208$civliberties, "Promotion of civil liberties\n", 
               "Tough policies to fight crime>>\n", 8, "Law and Order") 

# opposition or agreement to liberal policies in social lifestyle
ess0208$libcustoms <- max(ess0208$libcustoms, na.rm = T) - ess0208$libcustoms
before9 <- Graf10d(ches0208$oppos_libpol, ess0208$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Social Lifestyle")

# favours cosmopolitanism or nationalism
ess0208$immgcntrybetter <- max(ess0208$immgcntrybetter, na.rm = T) - ess0208$immgcntrybetter
before10 <- Graf10d(ches0208$cosmop_nation, ess0208$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Cosmopolitanism or nationalism")

# religious principles
before11 <- Graf10d(ches0208$religious, ess0208$religious, "A little religious\n", "Very religious>>\n",
                11, "Religious principles")

# market regulation
before12 <- Graf10d(ches0208$dereg_mkt, ess0208$govnotint, "Supports regulation\n",
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
  ggplot() + geom_density(data = ches0814, aes(x = VB, fill = 'black'), alpha = .3, adjust = 3) +
    geom_density(data = ess0814, aes(x = VW, fill = 'red'), alpha = .3, adjust = 2) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.3,y=5,label= paste("EMD =", round(analysis$EMDafter08[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

Graf10cc <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = ches0814, aes(x = VB, fill = 'black'), alpha = .3, adjust = 5) +
    geom_density(data = ess0814, aes(x = VW, fill = 'red'), alpha = .3, adjust = 2) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.3,y=5,label= paste("EMD =", round(analysis$EMDafter08[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
after1 <- Graf10c(ches0814$lrgen, ess0814$lrgen, "Left\n", "Right>>\n", 1, "Left-Right Self-positionment") +
  scale_fill_identity(name = "After 2008", labels = c("Parties","Voters"), guide = "legend")

# support for EU integration
after2 <- Graf10c(ches0814$fav_intEU, ess0814$supportEU, "Disapprove EU integration\n", 
                   "Support EU integration>>\n", 2, "Support to EU integration process")

# approves European Parliament powers
after3 <- Graf10c(ches0814$fav_ep_power, ess0814$supportEP, "Does not support the E.P.\n", 
                   "Supports the E.P.>>\n", 3, "Support powers of the E.P.")

# Post-materialist or traditional values
ess0814$libcustoms <- max(ess0814$libcustoms, na.rm = T) - ess0814$libcustoms
after4 <- Graf10c(ches0814$postmat_trad, ess0814$libcustoms, "Post-materialist values\n",
                   "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
after5 <- Graf10cc(ches0814$reject_immig, ess0814$rejectdiffimmig, "Allow many immigrants\n",
                   "Allow few immigrants>>\n", 5, "Opposition to immigrants")

# multiculturalism (separation of people belonging to different cultures) or assimilation
ess0814$assimil <- max(ess0814$assimil, na.rm = T) - ess0814$assimil
after6 <- Graf10cc(ches0814$assimil, ess0814$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
                   "Multiculturalism or assimilation")

# importance of environment despite economic development
after7 <- Graf10c(ches0814$envnotimp, ess0814$envnotimp, "Important to take care of the environment\n", 
               "Environment is not that important>>\n", 7, "Importance to take care of the environment")

# promotion of civil liberties or tough measures to fight crime
ess0814$civliberties <- max(ess0814$civliberties, na.rm = T) - ess0814$civliberties
after8 <- Graf10c(ches0814$laworder, ess0814$civliberties, "Promotion of civil liberties\n", 
                   "Tough policies to fight crime>>\n", 8, "Law and Order")
  
# opposition or agreement to liberal policies in social lifestyle
ess0814$libcustoms <- max(ess0814$libcustoms, na.rm = T) - ess0814$libcustoms
after9 <- Graf10c(ches0814$oppos_libpol, ess0814$libcustoms, "Supports liberal lifestyle\n",
                   "Supports traditional lifestyle>>\n", 9, "Social Lifestyle")

# favours cosmopolitanism or nationalism
ess0814$immgcntrybetter <- max(ess0814$immgcntrybetter, na.rm = T) - ess0814$immgcntrybetter
after10 <- Graf10cc(ches0814$nation, ess0814$immgcntrybetter, "cosmopolitanism\n", 
                    "nationalism>>\n", 10, "Cosmopolitanism or nationalism")

# religious principles
after11 <- Graf10c(ches0814$religious, ess0814$religious, "A little religious\n", "Very religious>>\n",
                    11, "Religious principles")

# market regulation
after12 <- Graf10c(ches0814$dereg_mkt, ess0814$govnotint, "Supports regulation\n",
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

##### saving datasets #####
setwd("./complots")
ggsave("lr.png", plot = multiplot(gen1, before1, after1, cols = 3), 
       device = "png",  width = 8, height = 3, units = "in")
ggsave("euint.png", plot = multiplot(gen2, before2, after2, cols = 3), device = "png",
       width = 8, height = 3, units = "in")
ggsave("eppower.png", plot = multiplot(gen3, before3, after3, cols = 3), device = "png",
       width = 8, height = 3, units = "in")
ggsave("post-trad.png", plot = multiplot(gen4, before4, after4, cols = 3), device = "png",
       width = 8, height = 3, units = "in")
ggsave("antimmig.png", plot = multiplot(gen5, before5, after5, cols = 3), 
       device = "png", width = 8, height = 3, units = "in")
ggsave("assimil.png", plot = multiplot(gen6, before6, after6, cols = 3),
       device = "png", width = 8, height = 3, units = "in")
ggsave("envnotimp.png", plot = multiplot(gen7, after7, cols = 2),
       device = "png", width = 8, height = 3, units = "in")
ggsave("laworder.png", plot = multiplot(gen8, before8, after8, cols = 3),
       device = "png", width = 8, height = 3, units = "in")
ggsave("tradition.png", plot = multiplot(gen9, before9, after9, cols = 3),
       device = "png", width = 8, height = 3, units = "in")
ggsave("cosmop.png", plot = multiplot(gen10, before10, after10, cols = 3),
       device = "png", width = 8, height = 3, units = "in")
ggsave("religion.png", plot = multiplot(gen11, before11, after11, cols = 3),
       device = "png", width = 8, height = 3, units = "in")
ggsave("market.png", plot = multiplot(gen12, before12, after12, cols = 3),
       device = "png", width = 8, height = 3, units = "in")

##### models #####

ess <- read.csv("ess_large.csv")
ess2 <- na.omit(ess)
attach(ess)

corr <- ggpairs(ess[, c("lrgen", "supportEU", "rejectdiffimmig", "immgcntrybetter",
                        "stfdem", "stfeco", "stfgov")])

# multi-level logit model 1 - general
logit1 <- glmer(data = ess, populist_vt ~ agea + gndr + eduyrs + lrgen + stfdem + 
                  stfgov + stfeco + supportEU + supportEP + rejectsameimmig + 
                  immgcntrybetter + antirefugee + civliberties + libcustoms + 
                  trstplt + trstprt + uemp3m + uemp5yr + hinctnta + (1 | cntry), 
                family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(logit1)
print(summary(logit1), corr = FALSE)

# multi-level model 2: demographic variables
logit3 <- glmer(populist_vt ~ agea + factor(gndr) + eduyrs + (1 | cntry), 
                family = "binomial", control = glmerControl(optimizer = "bobyqa"))
print(summary(logit3), corr = F)

# multi-level model 3: immigration feelings
logit4 <- glmer(populist_vt ~ immgcntrybetter + rejectdiffimmig + rejectsameimmig +
                  antirefugee + assimil + (1 | cntry), data = ess, family = binomial("logit"),
                control = glmerControl(optimizer = "bobyqa"))
print(summary(logit4), corr = F)

# multilevel model 4: ideological values
summary(prtyban)
logit5 <- glmer(populist_vt ~ lrgen + civliberties + libcustoms + freehms + 
                  prtyban + prtdgcl + mmbprty + (1 | cntry), data = ess, family = 
                  binomial("logit"), control = glmerControl(optimizer = "bobyqa"))
summary(logit5)


# logit model 2
logit2 <- glm(data = ess, populist_vt ~ agea + gndr + eduyrs + lrgen + stfdem + 
                stfgov + stfeco + supportEU + supportEP + rejectsameimmig + 
                immgcntrybetter + antirefugee + civliberties + libcustoms + 
                trstplt + trstprt + uemp5yr + hinctnta, family = "binomial")
summary(logit2)

# confidence intervals
se <- sqrt(diag(vcov(logit1)))

# table of estimates with 95% CI
(tab <- cbind(Est = fixef(logit1), LL = fixef(logit1) - 1.96 * se, 
              UL = fixef(logit1) + 1.96 * se))

# odd ratios instead of log odds
exp(tab)

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

##### saving plots #####
ggsave("gen1.png", plot = multiplot(gov1, gov2, gov3, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("gen2.png", plot = multiplot(gov4, gov5, gov6, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("gen3.png", plot = multiplot(gov7, gov8, gov9, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("gen4.png", plot = multiplot(gov10, gov11, gov12, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")

##### saving compared plots between parties in government and populist parties #####
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

##### descriptive stats #####
setwd("C:/Users/test/Desktop/2018.2 - Partidos políticos e sistemas partidários/scripts")

# position of government by survey year
gpos <- ggplot(gov, aes(lrgen)) +
  geom_line(stat = "density", aes(color = factor(year))) +
  theme_classic()

# average percentage of votes by year
ches$electionyear <- as.factor(ches$electionyear)

plot(tapply(ches$vote, ches$year, mean) ~ c(2002, 2006, 2010, 2014),
     type = 'l', ylab = "Percantage of vote (mean)", ylim = c(0, 12),
     xlab = "Year", lwd = 2, cex.lab = 1, cex.axis = 1, family = 'serif',
     main = "Percantage of RRPPs votes - 2002-2014",
     sub = "Figure 1 - Author's ellaboration")

line1 <- ggplot(data = ches, aes(x = factor(electionyear), y = vote)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(limits = c(1, 25)) +
  labs(x = "Election year", y = "Average percentage of votes") +
  stat_summary(fun.y = median, geom = "line", colour = "darkred", aes(group = 1)) +
  coord_cartesian(ylim = c(0, 20))

line2 <- ggplot(data = ches, aes(x = factor(year), y = vote)) +
  theme_classic() +
  labs(x = "Survey year", y = "Average percentage of votes") +
  stat_summary(fun.y = mean, geom = "line", colour = "darkred", aes(group = 1)) +
  coord_cartesian(ylim = c(5, 12))

ggsave("averagevotes.png", plot = multiplot(line1, line2, cols = 2),
       device = "png", width = 8, height = 3, units = "in")

##### rrpp voters and general electorate #####
Graf20 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = esswesteu, aes(x = VB, fill = 'black'), alpha = .3, adjust = 3) +
    geom_density(data = ess, aes(x = VW, fill = 'gray'), alpha = .3, adjust = 1) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("RRPP voter", "General electorate")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

Graf21 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = esswesteu, aes(x = VB, fill = 'black'), alpha = .3, adjust = 5) +
    geom_density(data = ess, aes(x = VW, fill = 'gray'), alpha = .3, adjust = 3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("RRPP voter", "General electorate")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    ggtitle(Titulo) +
    theme_classic(base_size = 8) + theme(legend.position = c(.3,.55), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plots comparing RRPP voters and general electorate
vot1 <- Graf20(esswesteu$lrgen, ess$lrgen, "Left\n", "Right>>\n", 1, 
               "Left-Right general") +
  scale_fill_identity(name = "Voters", labels = c("General voters","RPPP voters"), 
                      guide = "legend")
vot2 <- Graf20(esswesteu$supportEU, ess$supportEU, "Opposes EU integration\n", 
               "Favors EU integration>>\n", 2, "Support to EU integration process")
vot3 <- Graf20(esswesteu$supportEP, ess$supportEP, "Opposes EP powers\n", 
               "Favors EP powers>>\n", 3, "Support powers of EP")
vot4 <- Graf21(esswesteu$rejectdiffimmig, ess$rejectdiffimmig, "Allow many immigrants\n", 
               "Allow few immigrants>>\n", 4, "Opposition to immigrants")

ggsave("rrppvot_genelectorate.png", plot = multiplot(vot1, vot2, cols = 2),
       device = "png", width = 8, height = 3)
ggsave("rrppvot_genelectorate2.png", plot = multiplot(vot3, vot4, cols = 2),
       device = "png", width = 8, height = 3)
