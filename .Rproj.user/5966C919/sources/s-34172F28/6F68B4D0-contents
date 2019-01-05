setwd("C:/Users/test/Desktop/2018.2 - Partidos políticos e sistemas partidários/scripts")

# loading CHES data 1999-2014
ches <- read.csv("1999-2014_CHES_dataset_means.csv")

# filtering data from 2002 to 2014 to match with observations of the ESS. Also 
# filtering for EU members only
ches0214 <- ches[ches$year >= 2002 | ches$eumember == 1,]

# filtering only radical right parties
rrp <- ches0214[ches0214$family == "rad right" | ches0214$party == "LN",]

# filtering for west european countries
rrpwest <- rrp[rrp$eastwest == "west",]

# exporting final dataset
write.csv(rrpwest, "ches_rrp_westeurope.csv")
