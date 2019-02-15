library(tidyverse)
library(ggplot2)
library(emdist)
library(haven)
library(magrittr)

setwd("C:/Users/test/Desktop/dados/ess/essintegrated")

# loading original dataset
ess <- read.csv("ESS1-8e01.csv")

# subsetting by countries analyzed
weu <- ess[ess$cntry %in% c("AT", "BE", "DE", "DK", "FI", "FR", "GB", "GR", "IT",
                            "NL", "SE"),]

# "vars" object to extract interest variables
vars <- c("cntry", "gndr", "agea", "cregion", "regionat", "regionbe", "regionde",
          "regiondk", "regioadk", "regionfi", "regioafi", "regionfr", "regiongb",
          "regiongr", "regioagr", "regionit", "regionnl", "regionse", "prtvtat",
          "prtvtaat", "prtvtbat", "prtvtbe", "prtvtabe", "prtvtbbe", "prtvtcbe",
          "prtvtdk", "prtvtadk", "prtvtbdk", "prtvtcdk", "prtvede1", "prtvede2", 
          "prtvtcgr", "prtvtfr", "prtvtafr", "prtvtbfr", "prtvtcfr", "prtvtit", 
          "prtvtait", "prtvtbit", "prtvtcnl", "prtvtdnl", "prtvtenl", "prtvtfnl",
          "prtvtagb", "prtvtbgb", "prtvtfi", "prtvtafi", "prtvtbfi", "prtvtcfi",
          "prtvtase", "prtvtbse", "polintr", 
          "psppsgv", "psppsgva", "psppipl", "psppipla", "trstprl", "trstplt", 
          "trstprt", "vote", "clsprty", "prtdgcl", "mmbprty", "stfeco", "stfgov", 
          "stfdem", "freehms", "prtyban", "crvctwr", "crvctef", "trrenyr", 
          "trrcnyr", "eisced", "eduyrs", "iscoco", "isco08", "uemp3m", "uemp5yr", 
          "hinctnt", "hinctnta")

weuvars <- weu[vars]

# saving dataset
write.csv(weuvars, "addvar_esswesteu.csv")

# loading large dataset
ess <- read.csv("ess_westeu_intvar_large.csv")

##### transforming variables #####

# gender
ess$gndr[ess$gndr > 2] <- NA 
ess$gndr <- ifelse(ess$gndr == 2, 0, 1)

# age calculated
ess$agea[ess$agea == 999] <- NA

# education levels
ess$eisced[ess$eisced %in% c(0, 7)] <- NA
ess$educ <- ess$eisced

# years of education
ess$eduyrs[ess$eduyrs >= 77] <- NA

# creating variable for blue-collars from the iscoco variable (ess1 to ess5)
ess$bluecol <-  ifelse(ess$iscoco %in% c(6000 : 8340), 1, 0)
                      
# variable blue-collar from isco08 variable (ess6 to ess8)
ess$bluecol2 <- ifelse(ess$isco08 %in% c(6000 : 8350), 1, 0)

# ever unemployed for 3 months or more
ess$uemp3m[ess$uemp3m > 2] <- NA
ess$uemp3m[ess$uemp3m == 2] <- 0

# unemployed in last 5 years
ess$uemp5yr[ess$uemp5yr > 2] <- NA
ess$uemp5yr <- ifelse(ess$uemp5yr == 1, 1, 0)

# total net household income
ess$hinctnt[ess$hinctnt > 12] <- NA
ess$hinctnta[ess$hinctnta > 10] <- NA

# satisfaction with democracy
ess$stfdem[ess$stfdem > 10] <- NA

# satisfaction with economy
ess$stfeco[ess$stfeco > 10] <- NA

# satisfaction with government
ess$stfgov[ess$stfgov > 10] <- NA

# trust in national parliament
ess$trstprl[ess$trstprl > 10] <- NA

# trust in politicians
ess$trstplt[ess$trstplt > 10] <- NA

# trust in political parties
ess$trstprt[ess$trstprt > 10] <- NA

# making "populist vote" variable
ess$cntry <- as.character(ess$cntry)
ess$prtvtat <- as.numeric(ess$prtvtat)
ess$prtvtaat <- as.numeric(ess$prtvtaat)
ess$prtvtbat <- as.numeric(ess$prtvtbat)

ess$populist_vote <- ifelse(ess$cntry == "AT", ifelse(ess$prtvtat == 3, 
                            ifelse(ess$prtvtaat == 3, ifelse(ess$prtvtbat == 3, 1, 0), 0), 0), 0)

table(ess$populist_vote)

ess$populist_vote <- ess[ess$cntry == "AT" & ess$prtvtat == 3 & ess$prtvtaat == 3 & 
                           ess$prtvtbat == 3,] <- 1

ess$populist_vote[ess$cntry == "FN"] <- ifelse(ess$prtv)

# 
