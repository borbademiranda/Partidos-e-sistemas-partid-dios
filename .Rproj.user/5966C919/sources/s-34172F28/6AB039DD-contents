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

# Belgium - VB
rrpbe1 <- subset(esswest, cntry == "BE" & prtvtbe == 8)
rrpbe2 <- subset(esswest, cntry == "BE" & prtvtabe == 7)
rrpbe3 <- subset(esswest, cntry == "BE" & prtvtbbe == 7)
rrpbe4 <- subset(esswest, cntry == "BE" & prtvtcbe == 7)
rrpbe <- rbind(rrpbe1, rrpbe2)
rrpbe <- rbind(rrpbe, rrpbe3)
rrpbe <- rbind(rrpbe, rrpbe4)

# Denmark - DF
rrpdk1 <- subset(esswest, cntry == "DK" & prtvtdk == 6)
rrpdk2 <- subset(esswest, cntry == "DK" & prtvtadk == 6)
rrpdk3 <- subset(esswest, cntry == "DK" & prtvtbdk == 5)
rrpdk4 <- subset(esswest, cntry == "DK" & prtvtcdk == 5)
rrpdk <- rbind(rrpdk1, rrpdk2)
rrpdk <- rbind(rrpdk, rrpdk3)
rrpdk <- rbind(rrpdk, rrpdk4)

# Germany - NPD
rrpge1 <- subset(esswest, cntry == "DE" & prtvede1 == 8)
rrpge2 <- subset(esswest, cntry == "DE" & prtvede2 == 8)  
rrpge <- rbind(rrpge1, rrpge2)  

# Greece - XA
rrpgr <- subset(esswest, cntry == "GR" & prtvtcgr == 10)

# France - FN
rrpfr1 <- subset(esswest, cntry == "FR" & prtvtfr == 3)
rrpfr2 <- subset(esswest, cntry == "FR" & prtvtafr == 3)
rrpfr3 <- subset(esswest, cntry == "FR" & prtvtbfr == 2)
rrpfr4 <- subset(esswest, cntry == "FR" & prtvtcfr == 2)
rrpfr <- rbind(rrpfr1, rrpfr2)
rrpfr <- rbind(rrpfr, rrpfr3)
rrpfr <- rbind(rrpfr, rrpfr4)

# Italy - LN
rrpit1 <- subset(esswest, cntry == "IT" & prtvtit == 11)
rrpit2 <- subset(esswest, cntry == "IT" & prtvtait == 11)
rrpit3 <- subset(esswest, cntry == "IT" & prtvtbit == 9)
rrpit <- rbind(rrpit1, rrpit2)
rrpit <- rbind(rrpit, rrpit3)

# The Netherlands - PVV
rrpnl1 <- subset(esswest, cntry == "NL" & prtvtcnl == 11)
rrpnl2 <- subset(esswest, cntry == "NL" & prtvtdnl == 3)
rrpnl3 <- subset(esswest, cntry == "NL" & prtvtenl == 3)
rrpnl4 <- subset(esswest, cntry == "NL" & prtvtfnl == 3)
rrpnl <- rbind(rrpnl1, rrpnl2)
rrpnl <- rbind(rrpnl, rrpnl3)
rrpnl <- rbind(rrpnl, rrpnl4)

# United Kingdom - UKIP
rrpuk1 <- subset(esswest, cntry == "GB" & prtvtagb == 8)
rrpuk2 <- subset(esswest, cntry == "GB" & prtvtbgb == 7)
rrpuk <- rbind(rrpuk1, rrpuk2)

# Finland - True Finns
rrpfi1 <- subset(esswest, cntry == "FI" & prtvtfi == 5)
rrpfi2 <- subset(esswest, cntry == "FI" & prtvtafi == 5)
rrpfi3 <- subset(esswest, cntry == "FI" & prtvtbfi == 5)
rrpfi4 <- subset(esswest, cntry == "FI" & prtvtcfi == 4)
rrpfi <- rbind(rrpfi1, rrpfi2)
rrpfi <- rbind(rrpfi, rrpfi3)
rrpfi <- rbind(rrpfi, rrpfi4)

# Sweden - Sweden Democrats
rrpsw <- subset(esswest, cntry == "SE" & prtvtase == 10)
rrpsw <- rbind(subset(esswest, cntry == "SE" & prtvtbse == 10))

##### binding datasets #####
essrrpwest <- do.call("rbind", list(rrpat, rrpbe, rrpdk, rrpfi, rrpfr, rrpge, 
                                    rrpgr, rrpit, rrpnl, rrpsw, rrpuk))

# saving final dataset
write.csv(essrrpwest, "ess_rrp_westeurope.csv")
