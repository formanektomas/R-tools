rm(list=ls())
# install.packages("SmarterPoland")
# install.packages("reshape")
library("SmarterPoland") 
library("reshape")
help(package=SmarterPoland)
#
#
# NUTS2 <- grepEurostatTOC("NUTS 2")
# write.csv(NUTS2, "NUTS2.csv")
#
#### GDP #### 
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nama_10r_2gdp
# see web for metadata....
#
GDP <- getEurostatRCV(kod = "nama_10r_2gdp")
# select time
GDP$time <- as.numeric(as.character(GDP$time))
GDP <- GDP[GDP$time >= 2010 & GDP$time <= 2016, ]
# Million euro, Euro per inhabitant, Euro per inhabitant in percentage of the EU average
GDP <- GDP[GDP$unit == "MIO_EUR" | GDP$unit == "EUR_HAB" | GDP$unit == "EUR_HAB_EU", ]
#
MainDF <- cast(GDP, geo+time ~ unit)
rm(GDP)
#
#
#
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nama_10r_2gdp
#
#
#### Population  #### 
POP <- getEurostatRCV(kod = "demo_r_pjangroup") # Total pop per NUTS2, M/F/T, age groups
# POP2 <- getEurostatRCV(kod = "demo_r_pjanind2") # Age group proportions
str(POP)
summary(POP)
# select time
POP$time <- as.numeric(as.character(POP$time))
POP <- POP[POP$time >= 2010 & POP$time <= 2016, ]
# Select Total and Female
POP <- POP[POP$sex == "T" | POP$sex == "F", ]
# Disregard age structuring
POP <- POP[POP$age=="TOTAL",]
POP$age <- NULL
# Dump unit variable - "Number" for all obs.
POP$unit <- NULL
# Export to MainDF
PopDF <- cast(POP, geo+time ~ sex)
colnames(PopDF) <- c("geo", "time", "FemPopNr", "TotPopNr")
rm(POP)
#
MainDF <- merge(MainDF, PopDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(PopDF)
MainDF$GDP_PC <- (MainDF$MIO_EUR/MainDF$TotPopNr)*1000000 # GDP Per capita - manually
#
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjangroup
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_r_pjanind2
#
#
#
#
#
#### Population Activity status #### 
ACTIVES <- getEurostatRCV(kod = "lfst_r_lfp2act") # Economically active % by sex, age NUTS2
str(ACTIVES)
summary(ACTIVES)
ACTIVES$time <- as.numeric(as.character(ACTIVES$time))
ACTIVES <- ACTIVES[ACTIVES$time >= 2010 & ACTIVES$time <= 2016, ]
# Select Total and Female
ACTIVES <- ACTIVES[ACTIVES$sex == "T" | ACTIVES$sex == "F", ]
# Disregard units / all thousands
ACTIVES$unit <- NULL
# Select Age: 15-64 & 65+
ACTIVES <- ACTIVES[ACTIVES$age == "Y15-64" | ACTIVES$age == "Y_GE65", ]
ACTIVES$Act <- "Act" # Make var-name understandable
ActDF <- cast(ACTIVES, geo+time ~ Act+sex+age)
rm(ACTIVES)
#
MainDF <- merge(MainDF, ActDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
colnames(MainDF) <- c("geo","time","EUR_HAB","EUR_HAB_EU","MIO_EUR","FemPopNr","TotPopNr",
                      "GDP_PC","Act_F_Y_GE65","Act_F_Y15_64","Act_T_Y_GE65","Act_T_Y15_64")
rm(ActDF)
# Share of economically active
MainDF$Act_Sh_T_Y15_64 = (MainDF$Act_T_Y15_64/MainDF$TotPopNr)*1000
MainDF$Act_sh_T_GE65   = (MainDF$Act_T_Y_GE65/MainDF$TotPopNr)*1000
MainDF$Act_Sh_F_Y15_64 = (MainDF$Act_F_Y15_64/MainDF$FemPopNr)*1000
MainDF$Act_sh_F_GE65   = (MainDF$Act_F_Y_GE65/MainDF$FemPopNr)*1000
#
#
#
FOREIGN <- getEurostatRCV(kod = "lfst_r_lfp2actrc") # foreigner % by educ/origin and sector
str(FOREIGN)
summary(FOREIGN)
FOREIGN$time <- as.numeric(as.character(FOREIGN$time))
FOREIGN <- FOREIGN[FOREIGN$time >= 2010 & FOREIGN$time <= 2016, ]
# Total only (disregard sex structure)
FOREIGN <- FOREIGN[FOREIGN$sex == "T", ]
# Y15-64 only
FOREIGN <- FOREIGN[FOREIGN$age == "Y15-64", ]
# EU expats & non_EU expats
FOREIGN <- FOREIGN[FOREIGN$c_birth == "EU28_FOR" | FOREIGN$c_birth == "NEU28_FOR", ]
# Tertiary education (levels 5-8), of the ISCED 2011 levels
FOREIGN <- FOREIGN[FOREIGN$isced11 == "ED5-8", ]
# rm redundant /single-value/ variables
FOREIGN$isced11 <- NULL
FOREIGN$age <- NULL
FOREIGN$sex <- NULL
FOREIGN$unit <- NULL
FOREIGN$For <- "FoAc"
ForDF <- cast(FOREIGN, geo+time ~ For+c_birth)
rm(FOREIGN)
MainDF <- merge(MainDF, ForDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(ForDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=lfst_r_lfp2act
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=lfst_r_lfp2actrc
#
# Save MainDF
write.csv(MainDF, "Main_s1.csv", row.names = F)
#
#### Fixed Capital Formation #### 
CAP <- getEurostatRCV(kod = "nama_10r_2gfcf") # Capital formation by NACE sectors !!!!
str(CAP)
summary(CAP)
CAP$time <- as.numeric(as.character(CAP$time))
CAP <- CAP[CAP$time >= 2010 & CAP$time <= 2016, ]
# MIO EUR only
CAP <- CAP[CAP$currency == "MIO_EUR", ]
CAP$currency <- NULL
# Select NACE2 sectors
# 	B-E	Industry (except construction)
#   M_N	Professional, scientific and technical activities; administrative and support service activities
CAP <- CAP[CAP$nace_r2 == "TOTAL" | CAP$nace_r2 == "B-E" | CAP$nace_r2 == "J" | CAP$nace_r2 == "M_N", ]
# Formatting
CAP$GCF <- "GCF"
CapDF <- cast(CAP, geo+time ~ GCF+nace_r2)
# Final merging
colnames(CapDF) <- c("geo","time","GCF_B_E","GCF_J","GCF_M_N","GCF_TOTAL")
MainDF <- merge(MainDF, CapDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(CAP)
rm(CapDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nama_10r_2gfcf
#
#
#
#### Employment & Wages #### 
WAGE <- getEurostatRCV(kod = "nama_10r_2coe") # Compensations total per region
str(WAGE)
summary(WAGE)
WAGE$time <- as.numeric(as.character(WAGE$time))
WAGE <- WAGE[WAGE$time >= 2010 & WAGE$time <= 2016, ]
# MIO EUR only
WAGE <- WAGE[WAGE$currency == "MIO_EUR", ]
WAGE$currency <- NULL
# Select NACE2 sectors
WAGE <- WAGE[WAGE$nace_r2 == "TOTAL" | WAGE$nace_r2 == "B-E" | WAGE$nace_r2 == "J" | WAGE$nace_r2 == "M_N", ]
# Formatting
WAGE$Wg <- "Wg"
WgDF <- cast(WAGE, geo+time ~ Wg+nace_r2)
# Final merging
colnames(WgDF) <- c("geo","time","Wg_B_E","Wg_J","Wg_M_N","Wg_TOTAL")
MainDF <- merge(MainDF, WgDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(WAGE)
rm(WgDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nama_10r_2coe 
#
#
HRSEMP <- getEurostatRCV(kod = "nama_10r_2emhrw") # 000s hours worked per region
str(HRSEMP)
summary(HRSEMP)
HRSEMP$time <- as.numeric(as.character(HRSEMP$time))
HRSEMP <- HRSEMP[HRSEMP$time >= 2010 & HRSEMP$time <= 2016, ]
# only 000 hrs available
HRSEMP$unit <- NULL
# Employed persons vs Employees - select Employed persons
HRSEMP <- HRSEMP[HRSEMP$wstatus == "EMP", ]
HRSEMP$wstatus <- NULL
# Select NACE2 sectors
HRSEMP <- HRSEMP[HRSEMP$nace_r2 == "TOTAL" | HRSEMP$nace_r2 == "B-E" | HRSEMP$nace_r2 == "J" | HRSEMP$nace_r2 == "M_N", ]
# Formatting
HRSEMP$Hrs <- "Hrs"
HrsDF <- cast(HRSEMP, geo+time ~ Hrs+nace_r2)
colnames(HrsDF) <- c("geo","time","Hrs_B_E","Hrs_J","Hrs_M_N","Hrs_TOTAL")
# Final merging
MainDF <- merge(MainDF, HrsDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(HRSEMP)
rm(HrsDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nama_10r_2emhrw 
#
# Labor Force Productivity 
MainDF$LFP = (MainDF$MIO_EUR/MainDF$Hrs_TOTAL)*1000
#
write.csv(MainDF, "Main_s2.csv", row.names = F)
#
#
# TECHEMP <- getEurostatRCV(kod = "htec_emp_reg2") # employment in tech-intensive , NACE
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=htec_emp_reg2
# EMPL <- getEurostatRCV(kod = "lfst_r_lfe2emp") # Employment by age/sex, Fem. work part. rates
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=lfst_r_lfe2emp
#
EMN2 <- getEurostatRCV(kod = "lfst_r_lfe2en2") # Employment by age/NACE
str(EMN2)
summary(EMN2)
EMN2$time <- as.numeric(as.character(EMN2$time))
EMN2 <- EMN2[EMN2$time >= 2010 & EMN2$time <= 2016, ]
# Y15-64 only
EMN2 <- EMN2[EMN2$age == "Y15-64", ]
EMN2$age <- NULL
# Unit is 1 000 individuals only
EMN2$unit <- NULL
# NACE2 sectors
EMN2 <- EMN2[EMN2$nace_r2 == "TOTAL" | EMN2$nace_r2 == "B-E" | EMN2$nace_r2 == "J" | EMN2$nace_r2 == "M_N", ]
# Formatting
EMN2$Emp <- "Emp"
Emn2DF <- cast(EMN2, geo+time ~ Emp+nace_r2)
colnames(Emn2DF) <- c("geo","time","Emp_B_E","Emp_J","Emp_M_N","Emp_TOTAL")
# Final merging
MainDF <- merge(MainDF, Emn2DF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(EMN2)
rm(Emn2DF)
# Share of employees in HighTech Nace2 sectors
MainDF$Rel_Emp_B_E = (MainDF$Emp_B_E/MainDF$Emp_TOTAL)
MainDF$Rel_Emp_J   = (MainDF$Emp_J/MainDF$Emp_TOTAL)
MainDF$Rel_Emp_M_N = (MainDF$Emp_M_N/MainDF$Emp_TOTAL)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=lfst_r_lfe2en2
#
#
UNEM <-  getEurostatRCV(kod = "lfst_r_lfu3rt") # Unemployment rates
str(UNEM)
summary(UNEM)
UNEM$time <- as.numeric(as.character(UNEM$time))
UNEM <- UNEM[UNEM$time >= 2010 & UNEM$time <= 2016, ]
# Only percentage values available
UNEM$unit <- NULL
# Y15-74 only
UNEM <- UNEM[UNEM$age == "Y15-74", ]
UNEM$age <- NULL
# sex: Total only
UNEM <- UNEM[UNEM$sex == "T", ]
UNEM$sex <- NULL
# Formatting
UNEM$unit <- "Unem"
UnDF <- cast(UNEM, geo+time ~ unit)
# Final merging
MainDF <- merge(MainDF, UnDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(UNEM)
rm(UnDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=lfst_r_lfu3rt
#
write.csv(MainDF, "Main_s3.csv", row.names = F)
# MainDF <- read.csv("usableDF.csv")
#
#### R & D #### 
RDEX <- getEurostatRCV(kod = "rd_e_gerdreg") # R&D Expenditures by gen.sector, up to 2015
str(RDEX)
summary(RDEX)
RDEX$time <- as.numeric(as.character(RDEX$time))
RDEX <- RDEX[RDEX$time >= 2010 & RDEX$time <= 2016, ]
# general sector - business & enterprise + TOTAL
RDEX <- RDEX[RDEX$sectperf == "TOTAL", ]
RDEX$sectperf <- NULL #redundant
# unit - EUR/Hab for BES, Perc of GDP for TOTAL
RDEX <- RDEX[RDEX$unit == "MIO_EUR", ]
# Formatting
RDEX$RD <- "RD"
RDDF <- cast(RDEX, geo+time ~ RD+unit)
summary(RDDF)
# Disregard variable difficult to interpret
# Final merging
MainDF <- merge(MainDF, RDDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(RDDF)
rm(RDEX)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=rd_e_gerdreg
#
#
# RDEM <- getEurostatRCV(kod = "rd_p_persreg") # R&D Personnel by gen sector, up to 2015
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=rd_p_persreg
#
write.csv(MainDF, "Main_s4.csv", row.names = F)
#
#
#### Transportation Infrastructure #### 
TNET <- getEurostatRCV(kod = "tran_r_net") # Transport & infrastructure
str(TNET)
summary(TNET)
TNET$time <- as.numeric(as.character(TNET$time))
TNET <- TNET[TNET$time >= 2010 & TNET$time <= 2016, ]
# Motorways & Total raiway lines
TNET <- TNET[TNET$tra_infr == "MWAY" | TNET$tra_infr == "RL", ]
# unit ... keep both KM and KM per 1 000 KMSQ
# Formatting
TrDF <- cast(TNET, geo+time ~ tra_infr+unit)
summary(TrDF)
# Final merging
MainDF <- merge(MainDF, TrDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(TNET)
rm(TrDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=tran_r_net
#
#
# RAIL <- getEurostatRCV(kod = "tran_r_rago") # Railway transp. volumes... useless
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=tran_r_rago
#
#
#
#### Weather #### 
ACDAY <- getEurostatRCV(kod = "nrg_chddr2_a") # cooling and heating days (000???)
str(ACDAY)
summary(ACDAY)
ACDAY$time <- as.numeric(as.character(ACDAY$time))
ACDAY <- ACDAY[ACDAY$time >= 2010 & ACDAY$time <= 2016, ]
# disregard units - all Number of days per year
ACDAY$unit <- NULL
# Formatting
ACDF <- cast(ACDAY, geo+time ~ indic_nrg)
summary(ACDF)
# Final merging
MainDF <- merge(MainDF, ACDF, by.x = c("geo", "time"), by.y = c("geo", "time"), all.x = TRUE)
rm(ACDAY)
rm(ACDF)
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nrg_chddr2_a
#
#
#
write.csv(MainDF, "LFP_data.csv", row.names = F)









