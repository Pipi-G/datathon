#### PPH CLEANING ####

library(tidyverse)

### Load data --------------------------------------------------------------------------------------------------------------------------------------------------------------
NIS_2016_Core <- read_csv("NIS_2016_Core.csv") %>%
  mutate(key = round(key_nis))
NIS_2016_Severity <- read_csv("NIS_2016_Severity.csv") %>%
  mutate(key = round(key_nis))

NIS_2017_Core <- read_csv("NIS_2017_Core.csv") %>%
  mutate(key = round(key_nis))
NIS_2017_Severity <- read_csv("NIS_2017_Severity.csv") %>%
  mutate(key = round(key_nis))

NIS_2018_Core <- read_csv("NIS_2018_Core.csv") %>%
  mutate(key = round(key_nis))
NIS_2018_Severity <- read_csv("NIS_2018_Severity.csv") %>%
  mutate(key = round(key_nis))

NIS_2019_Core <- read_csv("NIS_2019_Core.csv") %>%
  mutate(key = round(key_nis))
NIS_2019_Severity <- read_csv("NIS_2019_Severity.csv") %>%
  mutate(key = round(key_nis))



### Get PPH cases per year ---------------------------------------------------------------------------------------------------------------------------------------------------------
# by O720/1/2/3 in diagnosis
icd_2016 <- sprintf("i10_dx%d",seq(1:30)) #2016 has 10_dx1-30, the other three year has 1-40
PPH_Core_2016 <- NIS_2016_Core %>% 
  filter_at(icd_2016, any_vars(.%in% c("O720","O721", "O722", "O723"))) #26588 patients diagonsed with PPH in 2016

icd_2017 <- sprintf("i10_dx%d",seq(1:40)) 
PPH_Core_2017 <- NIS_2017_Core %>% 
  filter_at(icd_2017, any_vars(.%in% c("O720","O721", "O722", "O723"))) #28375 patients diagonsed with PPH in 2017

icd_2018 <- sprintf("i10_dx%d",seq(1:40))
PPH_Core_2018 <- NIS_2018_Core %>% 
  filter_at(icd_2018, any_vars(.%in% c("O720","O721", "O722", "O723"))) #30204 patients diagonsed with PPH in 2018

icd_2019 <- sprintf("i10_dx%d",seq(1:40)) 
PPH_Core_2019 <- NIS_2019_Core %>% 
  filter_at(icd_2019, any_vars(.%in% c("O720","O721", "O722", "O723"))) #32416 patients diagonsed with PPH in 2019



### Join severity --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
PPH_2016 <- left_join(PPH_Core_2016, NIS_2016_Severity, by = "key") %>% 
  select(-c("key_nis.x","hosp_nis.y")) %>% 
  select(-c("dxver","i10_ecause1","i10_ecause2", "i10_ecause3" ,"i10_ecause4" ,"i10_necause","prver")) %>% 
  add_column("i10_dx31" = NA,"i10_dx32" = NA,"i10_dx33" = NA,"i10_dx34"= NA, "i10_dx35"= NA,"i10_dx36"= NA,"i10_dx37"= NA, "i10_dx38"= NA,"i10_dx39"= NA,"i10_dx40"= NA, .after = "i10_dx30") %>%
  add_column("i10_pr16" = NA, "i10_pr17" = NA,"i10_pr18" = NA,"i10_pr19"= NA,"i10_pr20"= NA,"i10_pr21"= NA,"i10_pr22"= NA,"i10_pr23"= NA, "i10_pr24"= NA,"i10_pr25"= NA, .after = "i10_pr15") %>%
  add_column("prday16"= NA , "prday17"= NA,  "prday18"= NA , "prday19" = NA,
             "prday20" = NA,"prday21"= NA, "prday22"= NA,"prday23"= NA, "prday24"= NA, "prday25"= NA, .after = "prday15")
#the variables that 2016 has but no longer exists for later years："dxver""i10_ecause1" "i10_ecause2" "i10_ecause3" "i10_ecause4" "i10_necause" "prver"， remove them for now

PPH_2017 <- left_join(PPH_Core_2017, NIS_2017_Severity, by = "key") %>% select(-c("key_nis.x","hosp_nis.y","dxver","prver")) #the dxver and prver in 2017 are all 10,no these two columns in years after

PPH_2018 <- left_join(PPH_Core_2018, NIS_2018_Severity, by = "key") %>% select(-c("key_nis.x","hosp_nis.y"))

PPH_2019 <- left_join(PPH_Core_2019, NIS_2019_Severity, by = "key") %>% select(-c("key_nis.x","hosp_nis.y")) %>% 
  select(-c("i10_birth", "i10_delivery", "i10_injury",    "i10_multinjury" , "i10_serviceline" ,"pclass_orproc")) #there are 6 new variables in 2019 "i10_birth", "i10_delivery", "i10_injury"     "i10_multinjury" , "i10_serviceline" ,"pclass_orproc", drop them for now



### Join all years -----------------------------------------------------------------------------------------------------------------------------------------
PPH <- rbind(PPH_2016, PPH_2017, PPH_2018, PPH_2019)



### Create variables for Severe Maternal Morbidity (CDC) ------------------------------------------------------------------------------------------------------------------------------------------
# The new variables are labeled cdc_s1-cdc_s21

# 1. Acute myocardial infarctionTransfusion: 
# I21.01, I21.02, I21.09, I21.11, I21.19, I21.21, I21.29, I21.3, I21.4, I21.9, I21.A1 and I21.A9(I21.xx, I22.x)
PPH$cdc_s1 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^I21|^I22")),1,0)

# 2.  Aneurysm: I71.xx,I79.0
PPH$cdc_s2 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^I71|^I790$")),1,0)

# 3. Acute renal failure: N17.0, N17.1, N17.2, N17.8, N17.9, O90.4 (N17.x, O90.4)
PPH$cdc_s3 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^N17|^O904$")),1,0)

# 4. Adult respiratory distress syndrome: J80, J95.1, J95.2, J95.3, J95.821, J95.822, J96.00, J96.01, J96.02,
# J96.20, J96.21, J96.22, R09.2 (J80, J95.1, J95.2, J95.3, J95.82x, J96.0x, J96.2x R09.2)
PPH$cdc_s4 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^J80$|^J951$|^J952$|^J953$|^J9582|^J960|^J962|^R092$")),1,0)

# 5. Amniotic fluid embolism: O88.111,O88.112,O88.113, O88.12 (childbirth), O88.13 (O88.1x)
PPH$cdc_s5 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O881")),1,0)

# 6. Cardiac arrest/ventricular fibrillation: I46.2, I46.8, I46.9, I49.01, I49.02(I46.x, I49.0x)
PPH$cdc_s6 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^I46|^I490")),1,0)

# 7. Conversion of cardiac rhythm(PR):	5A2204Z, 5A12012
PPH$cdc_s7 <- ifelse(!!rowSums(sapply(PPH[59:83], grepl, pattern = "5A2204Z|5A12012")),1,0)

# 8. Disseminated intravascular coagulation: D65, D68.8, D68.9, O72.3
PPH$cdc_s8 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^D65$|^D688$|^D689$|^O723$")),1,0)

# 9. Eclampsia:	O15.00, O15.02, O15.03, O15.1, O15.2, O15.9 (O15. X)
PPH$cdc_s9 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O15")),1,0)

# 10. Heart failure/arrest during surgery or procedure: 
# I97.120, I97.121, I97.130, I97.131, I97.710, I97.711 (I97.12x, I97.13x, I97.710, I97.711)
PPH$cdc_s10 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^I9712|I9713|^I97710$|^I97711$")),1,0)

# 11. Puerperal cerebrovascular disorders: I60.xx- I68.xx, O22.51, O22.52, O22.53, I97.81x, I97.82x, O87.3
PPH$cdc_s11 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^I60|^I61|^I62|^I63|^I64|^I65|^I66|^I67|^I68|^O2251$|^O2252$|^O2253$|^I9781|^I9782|^O873$")),1,0)

# 12. Pulmonary edema / Acute heart failure: J81.0, I50.1, I50.20, I50.21, I50.23, I50.30, I50.31, I50.33, I50.40, I50.41, I50.43, I50.9
PPH$cdc_s12 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^J810$|^I501$|^I5020$|^I5021$|^I5023$|^I5030$|^I5031$|^I5033$|^I5040$|^I5041$|^I5043$|^I509$")),1,0)

# 13. Severe anesthesia complications: O74.0 , O74.1, O74.2, O74.3, O89.01, O89.09, O89.1, O89.2 (O74.0 , O74.1 , O74.2, O74.3, O89.0x, O89.1, O89.2
PPH$cdc_s13 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O740$|^O741$|^O742$|^O743$|^I5023$|^O890|^O891$|^O892$")),1,0)

# 14. Sepsis: O85, O86.04, T80.211A, T81.4XXA, T81.44xx,
# or R65.20 or A40.x, A41.x, A32.7
PPH$cdc_s14 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O85$|^O8604$|^T80211A$|^T814XXA$|^T8144$|^R6520$ |^A40|^A41|^A327$")),1,0)

# 15. Shock: O75.1, R57.x, R65.21, T78.2XXA, T88.2 XXA, T88.6 XXA, T81.10XA , T81.11XA, T81.19XA
PPH$cdc_s15 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O751$|^R57|^R6521$|^T782XXA$|^T882XXA$|^T886XXA$ |^T8110XA$|^T8111XA$|^T8119XA$")),1,0)

# 16. Sickle cell disease with crisis: D57.00 , D57.01, D57.02, D57.211, D57.212, D57.219, D57.411, D57.412, D57.419, D57.811, D57.812, D57.819 (D57.0x, D57.21x, D57.41x, D57.81x)
PPH$cdc_s16 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^D570|^D5721|^D5741|^D5781")),1,0)

# 17. Air and thrombotic embolism: I26.01, I26.02, I26.09, I26.90, I26.92, I26.99, O88.011-O88.019, 088.02, O88.03, O88.211-O88.219, O88.22, O88.23, O88.311-O88.319, O88.32, O88.33, O88.81, O88.82, O88.83 (I26.x, O88.0x, O88.2x, O88.3x, O88.8x)
PPH$cdc_s17 <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^I26|^O880|^O882|^O883|^O888")),1,0)

# 18. Blood products transfusion(PR): 30233 Peripheral vein, percutaneous(7th digit: x=1: nonautologous)30240 Central Vein, open(7th digit: x=1: nonautologous)30243 Central Vein, percutaneous (full codes starts with 30233, 30230, 30243, 30240, 30253, 30250, 30263,30260)
PPH$cdc_s18 <- ifelse(!!rowSums(sapply(PPH[59:83], grepl, pattern = "^30233|^30230|^30240|^30243|^30250|^30253|^30260|^30263")),1,0)

# 19. Hysterectomy(PR): 0UT90ZZ, 0UT94ZZ, 0UT97ZZ, 0UT98ZZ, 0UT9FZZ
PPH$cdc_s19 <- ifelse(!!rowSums(sapply(PPH[59:83], grepl, pattern = "^0UT90ZZ$|^0UT94ZZ$|^0UT97ZZ$|^0UT98ZZ$|^0UT9FZZ$")),1,0)

# 20. Temporary tracheostomy(PR): 0B110Z4, 0B110F4, 0B113Z4, 0B113F4, 0B114Z4, 0B114F4
PPH$cdc_s20 <- ifelse(!!rowSums(sapply(PPH[59:83], grepl, pattern = "^0B110Z4$|^0B110F4$|^0B113Z4$|^0B113F4$|^0B114Z4$|^0B114F4$")),1,0)

# 21.Ventilation(PR): 5A1935Z, 5A1945Z, 5A1955Z
PPH$cdc_s21 <- ifelse(!!rowSums(sapply(PPH[59:83], grepl, pattern = "^5A1935Z$|^5A1945Z$|^5A1955Z$")),1,0)

# Turn into composite binary
PPH <- PPH %>% 
  mutate(SMM = as.factor(ifelse(!!rowSums(PPH[126:146]), 1, 0)))



### Create predictor variables ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Obesity: O99.214; Unspecified obesity E66.9
PPH$obesity <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O99214$|^E669$")),1,0)

# 2. Gestational diabetes: O24.4, O24.4xx
# History of GD: Z86.32
PPH$diabetes <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O244|^Z8632$")),1,0)

# 3. Gestational hypertension:  O13.x Gestational [pregnancy-induced] hypertension without significant proteinuria
PPH$gh <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O13")),1,0)

# 4. Multiple gestation: O30
PPH$mg <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O30")),1,0)

# 5. Placenta previa (may not be at admission): O44.xx
PPH$pp <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O44")),1,0)

# 6. Placental abruption (may not be at admission): O45.xxx
PPH$pa <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O45")),1,0)

# 7. Prolonged labor: O63.x
PPH$pl <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O63")),1,0)

# 8. Hypertrophy of uterus: N85.2
PPH$hu <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^N852$")),1,0)

# 9. Pre-eclampsia: 
PPH$pe <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O14|^O10919$|^O119$")),1,0)

# 10. Anemia associated with pregnancy
PPH$aap <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O99")),1,0)

# 11. Uterine fibroid
PPH$uf <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^D25")),1,0)

# 12. Uterine rupture during labor
PPH$urdl <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O71")),1,0)

# 13. Assisted reproductive tech: ^O09.81
PPH$art <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O0981")),1,0)

# 14. Chorioamnionitis: ^O41.12 
PPH$cra <- ifelse(!!rowSums(sapply(PPH[17:56], grepl, pattern = "^O4112")),1,0)

