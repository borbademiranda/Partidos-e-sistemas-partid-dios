##### loading dataset for west european countries #####
cheswesteu <- read.csv("ches_westeu_intvar.csv")
esswesteu <- read.csv("ess_westeu_intvar.csv")

# only governing parties
gov <- read.csv("ches_gov_westeu_intvar.csv")

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


EMD$fr <- c(
  emd.dis(chesfr$lrgen, essfr$lrgen), emd.dis(chesfr$fav_intEU, essfr$supportEU), 
  emd.dis(chesfr$fav_ep_power, essfr$supportEP), emd.dis(chesfr$postmat_trad, essfr$libcustoms),
  emd.dis(chesfr$reject_immig, essfr$rejectdiffimmig), emd.dis(chesfr$assimil, essfr$assimil),
  emd.dis(chesfr$envnotimp, essfr$envnotimp), emd.dis(chesfr$laworder, essfr$civliberties),
  emd.dis(chesfr$oppos_libpol, essfr$libcustoms), emd.dis(chesfr$nation, essfr$immgcntrybetter),
  emd.dis(chesfr$religious, essfr$religious), emd.dis(chesfr$dereg_mkt, essfr$govnotint)
)

plotfr <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = chesfr, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = essfr, aes(x = VW, fill = 'orange'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(EMD$fr[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

plotfr2 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = chesfr, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = essfr, aes(x = VW, fill = 'orange'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=6,label= paste("EMD =", round(EMD$fr[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

plotfr3 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = chesfr, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = essfr, aes(x = VW, fill = 'orange'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.65,y=4,label= paste("EMD =", round(EMD$fr[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}
# plot to EMD in left-right general position
fr1 <- plotfr(chesfr$lrgen, essfr$lrgen, "Left\n", "Right\n", 1, "Left-Right Self-positionment")

# support for EU integration
fr2 <- plotfr(chesfr$fav_intEU, essfr$supportEU, "Disapprove EU integration\n", 
               "Support EU integration", 2, "Support to EU integration process")

# approves European Parliament powers
fr3 <- plotfr(chesfr$fav_ep_power, essfr$supportEP, "Does not support the European Parliament\n", 
               "Supports the European Parliament>>\n", 3, 
               "How much support the powers of the European Parliament")

# Post-materialist or traditional values
essfr$libcustoms <- max(essfr$libcustoms, na.rm = T) - essfr$libcustoms
fr4 <- plotfr(chesfr$postmat_trad, essfr$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
fr5 <- plotfr2(chesfr$reject_immig, essfr$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants")

# multiculturalism (separation of people belonging to different cultures) or assimilation
essfr$assimil <- max(essfr$assimil, na.rm = T) - essfr$assimil
fr6 <- plotfr(chesfr$assimil, essfr$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism  or assimilation")

# importance of environment despite economic development
fr7 <- plotfr(chesfr$envnotimp, essfr$envnotimp, "Important to take care of the environment\n", 
               "Environment is not that important", 7, "Importance to take care of the environment")

# promotion of civil liberties or tough measures to fight crime
essfr$civliberties <- max(essfr$civliberties, na.rm = T) - essfr$civliberties
fr8 <- plotfr2(chesfr$laworder, essfr$civliberties, "Promotion of civil liberties\n", 
               "Promotion of tough policies to fight crime", 8, "Law and Order") +
  scale_fill_identity(name = "", labels = c("Parties","Voters"), guide = "legend")

# opposition or agreement to liberal policies in social lifestyle
fr9 <- plotfr(chesfr$oppos_libpol, essfr$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Social lifestyle")

# favours cosmopolitanism or nationalism
essfr$immgcntrybetter <- max(essfr$immgcntrybetter, na.rm = T)
fr10 <- plotfr(chesfr$nation, essfr$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Favours cosmopolitanism or nationalism")

# religious principles
fr11 <- plotfr(chesfr$religious, essfr$religious, "A little religious\n", "Very religious>>\n",
                11, "Religious principles")

# market regulation
fr12 <- plotfr3(chesfr$dereg_mkt, essfr$govnotint, "Supports regulation\n",
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
ggsave("fr1.png", plot = multiplot(fr1, fr2, fr3, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("fr2.png", plot = multiplot(fr4, fr5, fr6, cols = 3), device = "png", 
                                   width = 12, height = 3, units = "in")
ggsave("fr3.png", plot = multiplot(fr7, fr8, fr9, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
ggsave("fr4.png", plot = multiplot(fr10, fr11, fr12, cols = 3), device = "png", 
       width = 12, height = 3, units = "in")
