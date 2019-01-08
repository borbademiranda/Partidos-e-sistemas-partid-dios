library()

# setting working directory
setwd("C:/Users/test/Desktop/manifestoproject")

# loading manifesto package
library(manifestoR)
mp_setapikey("manifesto_apikey.txt")

# importing dataset
manifesto <- read.csv("mpdataset2018.csv", sep = ",")

# excluding non-utilized countries
eu <- manifesto[manifesto$countryname %in% c("Austria", "Belgium", "Germany", "Denmark",
                                        "Finland", "France", "United Kingdom",
                                        "Greece", "Italy", "Netherlands", "Sweden"),]

# selecting data from 2002
eu2002 <- eu[eu$date >= 200201,]

# selecting parties for analysis
rrpp02 <- eu2002[eu2002$partyabbrev %in% c("DF", "SD", "FN", "PVV", "LN",
                                         "XA", "FPÃ–", "UKIP", "PS", "AfD", 
                                         "VB"),]

# selecting interest variables
vars <- c("country", "countryname", "edate", "date", "partyname", "partyabbrev", 
          "coderyear", "pervote", "presvote", "absseat", "absseat", "rile", "planeco", 
          "markeco", "welfare", "per102", "per109", "per110", "per201", 
          "per305", "per401", "per402", "per403", "per501", "per503", "per601", 
          "per603", "per605", "per608", "per7062", "per201_2", "per202_4", 
          "per305_3", "per601_1", "per601_2", "per605_1", "per608_1", "per608_2")

man_iv <- rrpp02[vars]


# renaming error values for the variable "partyabbrev"
man_iv$partyabbrev <- as.character(man_iv$partyabbrev)
man_iv$partyabbrev[man_iv$partyabbrev == "FPÃ–"] <- "FPO"

# removing non used datasets
rm(eu, eu2002, manifesto, eu2002nat, man_iv_rrpp, vars)

# loading ess dataset to make the analysis
setwd("C:/Users/test/Desktop/2018.2 - Partidos políticos e sistemas partidários/scripts")

ess <- read.csv("ess_rrpp_westeu_intvar.csv")

# test analysis

