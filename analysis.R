library(tidyverse)
# LOAD DATA
# The ESS8 integrated file, edition 2.2 is used for the below and can be downloaded via the ESS data portal (https://ess-search.nsd.no/en/study/f8e11f55-0c14-4ab3-abde-96d3f14d3c76).
data <- read.csv("Desktop/Thesis_R_Code/ESS8e02_2.csv")
# FILTER DATA
# select necessary columns: country, all PVQ statements, age, gender, the two # dependent variables - ccrdprs & rdcenr
data_selection <- data %>% select(cntry, ipcrtiv:impfun, ccrdprs, rdcenr, gndr, agea, eisced)
# remove all values that are not specific answers
# values are on a 1 - 6 scale
# cccrdprs (level of responsibility to reduce climate change) is on a 0 - 10 scale # rdcenr (frequency of action to reduce energy use) is on a 1 - 6 scale
# agea < 999
# eisced (education level) < 8
truncate <- data_selection %>% filter(ipcrtiv < 7) %>% filter(imprich < 7) %>% filter(ipeqopt < 7) %>% filter(ipshabt < 7) %>%
filter(impsafe < 7) %>% filter(impdiff < 7) %>% filter(ipfrule < 7) %>% filter(ipudrst < 7) %>% filter(ipmodst < 7) %>% filter(ipgdtim < 7) %>% filter(impfree < 7) %>% filter(iphlppl < 7) %>% filter(ipsuces < 7) %>% filter(ipstrgv < 7) %>% filter(ipadvnt < 7) %>% filter(ipbhprp < 7) %>% filter(iprspot < 7) %>% filter(iplylfr < 7) %>% filter(impenv < 7) %>% filter(imptrad < 7) %>% filter(impfun < 7) %>% filter(ccrdprs < 11) %>% filter(rdcenr < 7) %>% filter(agea < 999) %>% filter(eisced < 8)

# TRANSFORM DATA
# transform 21 value statements from PVQ into the core ten human values
df <- truncate %>% rowwise() %>%
mutate(univ = mean(c(ipeqopt, ipudrst, impenv)), trad = mean(c(ipmodst, imptrad)),
stim = mean(c(impdiff, ipadvnt)),
sdir = mean(c(ipcrtiv, impfree)),
 sec = mean(c(impsafe, ipstrgv)), pow = mean(c(imprich, iprspot)), hedo = mean(c(ipgdtim, impfun)), conf = mean(c(ipfrule, ipbhprp)), benv = mean(c(iphlppl, iplylfr)), achv = mean(c(ipshabt, ipsuces)), stran = mean(c(univ, benv)),
cons = mean(c(trad, conf, sec)), otc = mean(c(stim, sdir, hedo)),
senh = mean(c(achv, pow))) %>%
select(cntry, ccrdprs, rdcenr, agea, gndr, eisced, univ, trad, stim, sdir,
sec, pow, hedo, conf, benv, achv, stran, cons, otc, senh) 

# FREQUENCY TABLES

# Values

val_stran <- round(df$stran) table(val_stran) round(prop.table(table(val_stran)), 3)
val_senh <- round(df$senh) table(val_senh) round(prop.table(table(val_senh)), 3)
val_otc <- round(df$otc) table(val_otc) round(prop.table(table(otc)), 3)
val_cons <- round(df$cons) table(val_cons) round(prop.table(table(cons)), 3)

# Demographics

# age summary(df$agea) round(sd(df$agea), 3)

#education
table(df$eisced) round(prop.table(table(df$eisced)), 3)

# Dependent Variable # Frequency of Action table(df$rdcenr)
 round(prop.table(table(df$rdcenr)), 3) 

# Level of Responsibility
table(df$ccrdprs) round(prop.table(table(df$ccrdprs)), 3)

# BUILD DUMMY VARIABLES 

df$c_1 <- ifelse(df$cntry == "AT", 1, 0)
df$c_2 <- ifelse(df$cntry == "BE", 1, 0)
df$c_3 <- ifelse(df$cntry == "CH", 1, 0)
df$c_4 <- ifelse(df$cntry == "CZ", 1, 0)
df$c_5 <- ifelse(df$cntry == "DE", 1, 0)
df$c_6 <- ifelse(df$cntry == "EE", 1, 0)
df$c_7 <- ifelse(df$cntry == "ES", 1, 0)
df$c_8 <- ifelse(df$cntry == "FI", 1, 0)
df$c_9 <- ifelse(df$cntry == "FR", 1, 0)
df$c_10 <- ifelse(df$cntry == "GB", 1, 0)
df$c_11 <- ifelse(df$cntry == "HU", 1, 0)
df$c_12 <- ifelse(df$cntry == "IE", 1, 0) # Ireland: reference country df$c_13 <- ifelse(df$cntry == "IE", 1, 0)
df$c_14 <- ifelse(df$cntry == "IS", 1, 0) df$c_15 <- ifelse(df$cntry == "IT", 1, 0) df$c_16 <- ifelse(df$cntry == "LT", 1, 0) df$c_17 <- ifelse(df$cntry == "NL", 1, 0) df$c_18 <- ifelse(df$cntry == "NO", 1, 0) df$c_19 <- ifelse(df$cntry == "PL", 1, 0) df$c_20 <- ifelse(df$cntry == "PT", 1, 0) df$c_21 <- ifelse(df$cntry == "RU", 1, 0) df$c_22 <- ifelse(df$cntry == "SE", 1, 0) df$c_23 <- ifelse(df$cntry == "SI", 1, 0)

# BUILD MODELS

# Basic Linear Models
n_rdcenr <- lm(rdcenr ~ senh + stran + otc + cons,
data = df)

n_ccrdprs <- lm(ccrdprs ~ senh + stran + otc + cons, data = df)

# Basic Fixed Effects Model
fe_rdcenr <- lm(rdcenr ~ senh + stran + otc + cons + c_1 + c_2 + c_3 + c_4 +
c_5 + c_6 + c_7 + c_8 + c_9 + c_10 + c_11 + c_13 + c_14 + c_15 + c_16 + c_17 + c_18 + c_19 + c_20 + c_21 + c_22 + c_23,
data = df)

fe_ccrdprs <- lm(ccrdprs ~ senh + stran + otc + cons + c_1 + c_2 + c_3 + c_4 + c_5 + c_6 + c_7 + c_8 + c_9 + c_10 + c_11 + c_13 + c_14 +
c_15 + c_16 + c_17 + c_18 + c_19 + c_20 + c_21 + c_22 + c_23, data = df)

# Full Fixed Effects with age, gender and education

fe_full_rdcenr <- lm(rdcenr ~ senh + stran + otc + cons + agea + gndr + eisced +
c_1 + c_2 + c_3 + c_4 + c_5 + c_6 + c_7 + c_8 + c_9 + c_10 + c_11 + c_13 + c_14 + c_15 + c_16 + c_17 + c_18 + c_19 + c_20 + c_21 + c_22 + c_23, data = df)

fe_full_ccrdprs <- lm(ccrdprs ~ senh + stran + otc + cons + agea + gndr + eisced + c_1 + c_2 + c_3 + c_4 + c_5 + c_6 + c_7 + c_8 + c_9 + c_10 + c_11 + c_13 + c_14 + c_15 + c_16 + c_17 + c_18 + c_19 + c_20 + c_21 + c_22 + c_23, data = df)

# SUMMARIES

# Reduce Energy
summary(n_rdcenr) summary(fe_rdcenr) summary(fe_full_rdcenr)

# Level of Responsibility
summary(n_ccrdprs) summary(fe_ccrdprs) summary(fe_full_ccrdprs)
