# setting working directory
setwd("C:/Users/test/Desktop/manifestoproject")

# loading manifesto package
library(manifestoR)
mp_setapikey("manifesto_apikey.txt")

# importing dataset
manifesto <- read.csv("mpdataset2018.csv", sep = ",")

# excluding non-utilized countries
eu <- manifesto[!(manifesto$country %in% c("13", "23", "34", "54", "55", "61", 
                                           "62", "63", "64", "71", "72", "73", 
                                           "74", "75", "76", "77", "79", "78", 
                                           "80", "81", "84", "87", "89", "90", 
                                           "91", "93", "94", "95", "96", "98", 
                                           "113", "171", "181")),
                           ]

# selecting data from 2012
eu2012 <- eu[eu$date >= 201201,]

# selecting nationalist and special issue parties
eu2012nat <- eu2012[eu2012$parfam %in% c(70, 95),]

# selecting right parties
eu2012right <- eu2012nat[eu2012nat$rile >= 0,]

# test analysis

