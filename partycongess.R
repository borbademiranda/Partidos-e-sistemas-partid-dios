library(tidyverse)

# loading ess harmonized 1st to 7th wave
essharm <- read.csv("ESS1-8e01.csv")

# filtering countries to match with ches data
esswest <- essharm[essharm$cntry %in% c("AT", "BE", "DK", "DE", "GR", "FR", "IT",
                                        "NL", "GB", "FI", "SE"),]

##### filtering by party voted in last election #####
##### (only radical right populist parties) #####

# Austria - FPO
rrpat1 <- subset(esswest, cntry == "AT" & prtvtat == 3)
rrpat2 <- subset(esswest, cntry == "AT" & prtvtaat == 3)
rrpat3 <- subset(esswest, cntry == "AT" & prtvtbat == 3)                     
rrpat <- rbind(rrpat1, rrpat2)
rrpat <- rbind(rrpat, rrpat3)
rrpat$party_voted <- rep("FPO")
rm(rrpat1, rrpat2, rrpat3)

# Belgium - VB
rrpbe1 <- subset(esswest, cntry == "BE" & prtvtbe == 8)
rrpbe2 <- subset(esswest, cntry == "BE" & prtvtabe == 7)
rrpbe3 <- subset(esswest, cntry == "BE" & prtvtbbe == 7)
rrpbe4 <- subset(esswest, cntry == "BE" & prtvtcbe == 7)
rrpbe <- rbind(rrpbe1, rrpbe2)
rrpbe <- rbind(rrpbe, rrpbe3)
rrpbe <- rbind(rrpbe, rrpbe4)
rrpbe$party_voted <- rep("VB")
rm(rrpbe1, rrpbe2, rrpbe3, rrpbe4)

# Denmark - DF
rrpdk1 <- subset(esswest, cntry == "DK" & prtvtdk == 6)
rrpdk2 <- subset(esswest, cntry == "DK" & prtvtadk == 6)
rrpdk3 <- subset(esswest, cntry == "DK" & prtvtbdk == 5)
rrpdk4 <- subset(esswest, cntry == "DK" & prtvtcdk == 5)
rrpdk <- rbind(rrpdk1, rrpdk2)
rrpdk <- rbind(rrpdk, rrpdk3)
rrpdk <- rbind(rrpdk, rrpdk4)
rrpdk$party_voted <- rep("DF")
rm(rrpdk1, rrpdk2, rrpdk3, rrpdk4)

# Germany - NPD
rrpge1 <- subset(esswest, cntry == "DE" & prtvede1 == 8)
rrpge2 <- subset(esswest, cntry == "DE" & prtvede2 == 8)  
rrpge <- rbind(rrpge1, rrpge2)  
rrpge$party_voted <- rep("NPD")
rm(rrpge1, rrpge2)

# Greece - XA
rrpgr <- subset(esswest, cntry == "GR" & prtvtcgr == 10)
rrpgr$party_voted <- rep("XA")

# France - FN
rrpfr1 <- subset(esswest, cntry == "FR" & prtvtfr == 3)
rrpfr2 <- subset(esswest, cntry == "FR" & prtvtafr == 3)
rrpfr3 <- subset(esswest, cntry == "FR" & prtvtbfr == 2)
rrpfr4 <- subset(esswest, cntry == "FR" & prtvtcfr == 2)
rrpfr <- rbind(rrpfr1, rrpfr2)
rrpfr <- rbind(rrpfr, rrpfr3)
rrpfr <- rbind(rrpfr, rrpfr4)
rrpfr$party_voted <- rep("FN")
rm(rrpfr1, rrpfr2, rrpfr3, rrpfr4)

# Italy - LN
rrpit1 <- subset(esswest, cntry == "IT" & prtvtit == 11)
rrpit2 <- subset(esswest, cntry == "IT" & prtvtait == 11)
rrpit3 <- subset(esswest, cntry == "IT" & prtvtbit == 9)
rrpit <- rbind(rrpit1, rrpit2)
rrpit <- rbind(rrpit, rrpit3)
rrpit$party_voted <- rep("LN")
rm(rrpit1, rrpit2, rrpit3)

# The Netherlands - PVV
rrpnl1 <- subset(esswest, cntry == "NL" & prtvtcnl == 11)
rrpnl2 <- subset(esswest, cntry == "NL" & prtvtdnl == 3)
rrpnl3 <- subset(esswest, cntry == "NL" & prtvtenl == 3)
rrpnl4 <- subset(esswest, cntry == "NL" & prtvtfnl == 3)
rrpnl <- rbind(rrpnl1, rrpnl2)
rrpnl <- rbind(rrpnl, rrpnl3)
rrpnl <- rbind(rrpnl, rrpnl4)
rrpnl$party_voted <- rep("PVV")
rm(rrpnl1, rrpnl2, rrpnl3, rrpnl4)

# United Kingdom - UKIP
rrpuk1 <- subset(esswest, cntry == "GB" & prtvtagb == 8)
rrpuk2 <- subset(esswest, cntry == "GB" & prtvtbgb == 7)
rrpuk <- rbind(rrpuk1, rrpuk2)
rrpuk$party_voted <- rep("UKIP")
rm(rrpuk1, rrpuk2)

# Finland - True Finns
rrpfi1 <- subset(esswest, cntry == "FI" & prtvtfi == 5)
rrpfi2 <- subset(esswest, cntry == "FI" & prtvtafi == 5)
rrpfi3 <- subset(esswest, cntry == "FI" & prtvtbfi == 5)
rrpfi4 <- subset(esswest, cntry == "FI" & prtvtcfi == 4)
rrpfi <- rbind(rrpfi1, rrpfi2)
rrpfi <- rbind(rrpfi, rrpfi3)
rrpfi <- rbind(rrpfi, rrpfi4)
rrpfi$party_voted <- rep("PS")
rm(rrpfi1, rrpfi2, rrpfi3, rrpfi4)

# Sweden - Sweden Democrats
rrpsw <- subset(esswest, cntry == "SE" & prtvtase == 10)
rrpsw <- rbind(subset(esswest, cntry == "SE" & prtvtbse == 10))
rrpsw$party_voted <- rep("SD")

##### binding datasets #####
essrrpwest <- do.call("rbind", list(rrpat, rrpbe, rrpdk, rrpfi, rrpfr, rrpge, 
                                    rrpgr, rrpit, rrpnl, rrpsw, rrpuk))

# saving dataset
write.csv(essrrpwest, "ess_rrp_westeurope.csv")

# removing temporary datasets
rm(rrpat, rrpbe, rrpdk, rrpfi, rrpfr, rrpge, rrpgr, rrpit, rrpnl, rrpsw, rrpuk)

##### treating variables #####

# renaming and reshaping variable left-right scale
essrrpwest$lrgen <- essrrpwest$lrscale
essrrpwest$lrgen[essrrpwest$lrgen > 10] <- NA
essrrpwest$lrgen <- essrrpwest$lrgen / 10
table(essrrpwest$lrgen)

# renaming and adjusting values for support for European integration
essrrpwest$euftf[essrrpwest$euftf > 10] <- NA
essrrpwest$supportEU <- essrrpwest$euftf / 10
table(essrrpwest$supportEU)

# renaming and reshaping values for "trust in European Parliament"
# (used as proxy to support for Europarliament)
essrrpwest$trstep[essrrpwest$trstep > 10] <- NA
essrrpwest$supportEP <- essrrpwest$trstep / 10
table(essrrpwest$supportEP)

# allow many or few immigrants of different ethnic groups
essrrpwest$imdfetn[essrrpwest$imdfetn > 4] <- NA
essrrpwest$rejectdiffimmig <- (essrrpwest$imdfetn - 1) / 3
table(essrrpwest$rejectdiffimmig)

# allow many or few immigrants of the same ethnic group
essrrpwest$imsmetn[essrrpwest$imsmetn > 4] <- NA
essrrpwest$rejectsameimmig <- (essrrpwest$imsmetn - 1) / 3
table(essrrpwest$rejectsameimmig)

# allow many or few immigrants from poorer countries outside Europe
essrrpwest$impcntr[essrrpwest$impcntr > 4] <- NA
essrrpwest$rejectpoorimmig <- (essrrpwest$impcntr - 1) / 3
table(essrrpwest$rejectpoorimmig)

# immigration makes country a worse or better place to live
essrrpwest$imwbcnt[essrrpwest$imwbcnt > 10] <- NA
essrrpwest$immgcntrybetter <- essrrpwest$imwbcnt / 10
table(essrrpwest$immgcntrybetter)

# national culture is undermined or enriched by immigrants
# proxy to favours multiculturalism or assimilation
essrrpwest$imueclt[essrrpwest$imueclt > 10] <- NA
essrrpwest$assimil <- essrrpwest$imueclt / 10
table(essrrpwest$assimil)

# take care of environment and nature or it doesn't matter
essrrpwest$impenv[essrrpwest$impenv > 6] <- NA
essrrpwest$envnotimp <- (essrrpwest$impenv - 1) / 5
table(essrrpwest$envnotimp)

# promotes civil liberties instead of tough policies on crime
essrrpwest$ipstrgv[essrrpwest$ipstrgv > 6] <- NA
essrrpwest$civliberties <- (essrrpwest$ipstrgv - 1) / 5
table(essrrpwest$civliberties)

# importance to follow traditions and customs
# (proxy to accessrrpwest how much the repondent favours liberal policies)
essrrpwest$imptrad[essrrpwest$imptrad > 6] <- NA
essrrpwest$libcustoms <- (essrrpwest$imptrad - 1) / 5
table(essrrpwest$libcustoms)

# how religious you are
essrrpwest$rlgdgr[essrrpwest$rlgdgr > 10] <- NA
essrrpwest$religious <- essrrpwest$rlgdgr / 10
table(essrrpwest$religious)

# government should be generous judging applications for refugees
essrrpwest$gvrfgap[essrrpwest$gvrfgap > 5] <- NA
essrrpwest$antirefugee <- (essrrpwest$gvrfgap - 1) / 4
table(essrrpwest$antirefugee)

# government should reduce differences in income levels
essrrpwest$gincdif[essrrpwest$gincdif > 5] <- NA
essrrpwest$govnotint <- (essrrpwest$gincdif - 1) / 4
table(essrrpwest$govnotint)

##### making dataset with only interest variables #####
vars <- c("cntry", "cname", "essround", "vote", "clsprty", "mmbprty", 
          "party_voted", "lrgen", "supportEU", "supportEP", "rejectdiffimmig",
          "rejectsameimmig", "rejectpoorimmig", "immgcntrybetter", "antirefugee", 
          "assimil", "envnotimp", "civliberties", "libcustoms", "religious",
          "govnotint")

essrrpw2 <- essrrpwest[vars]

##### treating variables of the esswest dataset #####
##### treating variables #####

# renaming and reshaping variable left-right scale
esswest$lrgen <- esswest$lrscale
esswest$lrgen[esswest$lrgen > 10] <- NA
esswest$lrgen <- esswest$lrgen / 10
table(esswest$lrgen)

# renaming and adjusting values for support for European integration
esswest$euftf[esswest$euftf > 10] <- NA
esswest$supportEU <- esswest$euftf / 10
table(esswest$supportEU)

# renaming and reshaping values for "trust in European Parliament"
# (used as proxy to support for Europarliament)
esswest$trstep[esswest$trstep > 10] <- NA
esswest$supportEP <- esswest$trstep / 10
table(esswest$supportEP)

# allow many or few immigrants of different ethnic groups
esswest$imdfetn[esswest$imdfetn > 4] <- NA
esswest$rejectdiffimmig <- (esswest$imdfetn - 1) / 3
table(esswest$rejectdiffimmig)

# allow many or few immigrants of the same ethnic group
esswest$imsmetn[esswest$imsmetn > 4] <- NA
esswest$rejectsameimmig <- (esswest$imsmetn - 1) / 3
table(esswest$rejectsameimmig)

# allow many or few immigrants from poorer countries outside Europe
esswest$impcntr[esswest$impcntr > 4] <- NA
esswest$rejectpoorimmig <- (esswest$impcntr - 1) / 3
table(esswest$rejectpoorimmig)

# immigration makes country a worse or better place to live
esswest$imwbcnt[esswest$imwbcnt > 10] <- NA
esswest$immgcntrybetter <- esswest$imwbcnt / 10
table(esswest$immgcntrybetter)

# national culture is undermined or enriched by immigrants
# proxy to favours multiculturalism or assimilation
esswest$imueclt[esswest$imueclt > 10] <- NA
esswest$assimil <- esswest$imueclt / 10
table(esswest$assimil)

# take care of environment and nature or it doesn't matter
esswest$impenv[esswest$impenv > 6] <- NA
esswest$envnotimp <- (esswest$impenv - 1) / 5
table(esswest$envnotimp)

# promotes civil liberties instead of tough policies on crime
esswest$ipstrgv[esswest$ipstrgv > 6] <- NA
esswest$civliberties <- (esswest$ipstrgv - 1) / 5
table(esswest$civliberties)

# importance to follow traditions and customs
# (proxy to accesswest how much the repondent favours liberal policies)
esswest$imptrad[esswest$imptrad > 6] <- NA
esswest$libcustoms <- (esswest$imptrad - 1) / 5
table(esswest$libcustoms)

# how religious you are
esswest$rlgdgr[esswest$rlgdgr > 10] <- NA
esswest$religious <- esswest$rlgdgr / 10
table(esswest$religious)

# government should be generous judging applications for refugees
esswest$gvrfgap[esswest$gvrfgap > 5] <- NA
esswest$antirefugee <- (esswest$gvrfgap - 1) / 4
table(esswest$antirefugee)

# government should reduce differences in income levels
esswest$gincdif[esswest$gincdif > 5] <- NA
esswest$govnotint <- (esswest$gincdif - 1) / 4
table(esswest$govnotint)

##### making dataset with only interest variables #####
vars <- c("cntry", "cname", "essround", "vote", "clsprty", "mmbprty", "lrgen", "supportEU", "supportEP", "rejectdiffimmig",
          "rejectsameimmig", "rejectpoorimmig", "immgcntrybetter", "antirefugee", 
          "assimil", "envnotimp", "civliberties", "libcustoms", "religious",
          "govnotint")

esswest2 <- esswest[vars]


# saving dataset
write.csv(esswest2, "ess_westeu_intvar.csv")
write.csv(essrrpw2, "ess_rrpp_westeu_intvar.csv")
