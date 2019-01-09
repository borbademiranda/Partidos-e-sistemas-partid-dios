##### loading dataset for west european countries #####
cheswesteu <- read.csv("ches_westeu_intvar.csv")
esswesteu <- read.csv("ess_westeu_intvar.csv")

gov <- read.csv("ches_gov_westeu_intvar.csv")

fr <- gov[gov$country == "fr",]

essfr <- esswesteu[esswesteu$cntry == "FR",]

analysis <- as.tibble(list(nomes = c("Left-Right", "Supp. EU integration",
                                     "Supp. EP powers", "Postmat-Tradition",
                                     "Anti-Immigrants", "Assimilationism",
                                     "Envir. not important", "Liberties vs Law and Order",
                                     "Traditionalism", "Cosmopolitism", "Rel. principles",
                                     "Mkt Intervention")))

analysis$EMDfr <- c(
  emd.dis(fr$lrgen, essfr$lrgen), emd.dis(fr$fav_intEU, essfr$supportEU), 
  emd.dis(fr$fav_ep_power, essfr$supportEP), emd.dis(fr$postmat_trad, essfr$libcustoms),
  emd.dis(fr$reject_immig, essfr$rejectdiffimmig), emd.dis(fr$assimil, essfr$assimil),
  emd.dis(fr$envnotimp, essfr$envnotimp), emd.dis(fr$laworder, essfr$civliberties),
  emd.dis(fr$oppos_libpol, essfr$libcustoms), emd.dis(fr$nation, essfr$immgcntrybetter),
  emd.dis(fr$religious, essfr$religious), emd.dis(fr$dereg_mkt, essfr$govnotint)
)

Graf10 <- function(VB, VW, Lab1, Lab10, Numero, Titulo) {
  ggplot() + geom_density(data = fr, aes(x = VB, fill = 'black'), alpha = .3) +
    geom_density(data = essfr, aes(x = VW, fill = 'orange'), alpha = .3) + 
    scale_x_continuous(name = "",breaks = c(2/9,7/9),limits = c(0,1), labels = c(paste0("<<",Lab1), Lab10), expand = c(.01,.01)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    scale_y_continuous(name = "", breaks = NULL, limits = c(0,7), labels = NULL, expand = c(.01,.01)) +
    annotate("text",x=.35,y=4,label= paste("EMD =", round(analysis$EMDfr[Numero], 3)),family="serif",size=4) +
    ggtitle(Titulo) +
    theme_bw(base_size = 8) + theme(legend.position = c(.4,.5), legend.background = element_blank(), legend.text = element_text(size = 7), plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), plot.margin = unit(c(0,.1,0,.1), "cm"), axis.text.x = element_text(size = 7))
}

# plot to EMD in left-right general position
gen1 <- Graf10(fr$lrgen, essfrfr$lrgen, "Left\n", "Right\n", 1, "Left-Right Self-positionment")

# support for EU integration
gen2 <- Graf10(fr$fav_intEU, essfr$supportEU, "Disapprove EU integration\n", 
               "Support EU integration", 2, "Support to EU integration processfr")

# approves European Parliament powers
gen3 <- Graf10(fr$fav_ep_power, essfr$supportEP, "Does not support the European Parliament\n", 
               "Supports the European Parliament>>\n", 3, 
               "How much support the powers of the European Parliament")

# Post-materialist or traditional values
essfr$libcustoms <- max(essfr$libcustoms, na.rm = T) - essfr$libcustoms
gen4 <- Graf10(fr$postmat_trad, essfr$libcustoms, "Post-materialist values\n",
               "Traditional values>>\n", 4, "Post-materialist or Traditional values")

# anti-immigrant feelings
gen5 <- Graf10(fr$reject_immig, essfr$rejectdiffimmig, "Allow many immigrants\n",
               "Allow few immigrants>>\n", 5, 
               "Opposition to immigrants belonging to the same ethnic group")

# multiculturalism (separation of people belonging to different cultures) or assimilation
essfr$assimil <- max(essfr$assimil, na.rm = T) - essfr$assimil
gen6 <- Graf10(fr$assimil, essfr$assimil, "Multiculturalism\n", "Assimilation>>\n", 6,
               "Multiculturalism (separation of people belonging to different cultures) or assimilation")

# importance of environment despite economic development
gen7 <- Graf10(fr$envnotimp, essfr$envnotimp, "Important to take care of the environment\n", 
               "Environment is not that important", 7, "Importance to take care of the environment")

# promotion of civil liberties or tough measures to fight crime
essfr$civliberties <- max(essfr$civliberties, na.rm = T) - essfr$civliberties
gen8 <- Graf10(fr$laworder, essfr$civliberties, "Promotion of civil liberties\n", 
               "Promotion of tough policies to fight crime", 8, "Promotion of civil liberties or tough measures to fight crime") +
  scale_fill_identity(name = "", labels = c("Parties","Voters"), guide = "legend")

# opposition or agreement to liberal policies in social lifestyle
gen9 <- Graf10(fr$oppos_libpol, essfr$libcustoms, "Supports liberal lifestyle\n",
               "Supports traditional lifestyle>>\n", 9, "Opposition or agreement to liberal policies in social lifestyle")

# favours cosmopolitanism or nationalism
essfr$immgcntrybetter <- max(essfr$immgcntrybetter, na.rm = T)
gen10 <- Graf10(fr$nation, essfr$immgcntrybetter, "cosmopolitanism\n", 
                "nationalism>>\n", 10, "Favours cosmopolitanism or nationalism")

# religious principles
gen11 <- Graf10(fr$religious, essfr$religious, "A little religious\n", "Very religious>>\n",
                11, "Religious principles")

# market regulation
gen12 <- Graf10(fr$dereg_mkt, essfr$govnotint, "Supports regulation\n",
                "Opposes regulation>>\n", 12, "Market regulation")
