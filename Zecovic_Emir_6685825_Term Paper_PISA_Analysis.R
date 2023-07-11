#######################################################################
# Emir Zecovic, 6685825                                               #
# R code: 2nd assignment                                              #
# Date 11.07.2023                                                     #
#######################################################################

# Packages-----
packages <- c("tidyverse", "psych", "foreign", "convenience",
              "haven", "sjlabelled", "margins",
              "readxl","performance", "sjPlot", "car", "lattice",
              "marginaleffects", "parameters", "lubridate", 
              "magrittr", "jtools", "expss","stargazer",
              "estimatr", "panelr", "plm", "survival")

## Install if necessary
install.packages(setdiff(packages,
                         rownames(installed.packages()))) 

## Load
lapply(packages, library, character.only = TRUE)



# Read in PISA 2018 student questionnaire (main file) ----------
PISA2018 <- read_sav("CY07_MSU_STU_QQQ.sav")

## Select variables of interest -----------------------------
P18 <- dplyr::select(
  PISA2018,
  c(
    "CNTRYID":"OECD",         # Various ID identifier 
    "ST004D01T",              # Gender
    "AGE",                    # AGE
    "ESCS",                   # SES
    
    # Immigrant background
    "ST019AQ01T",             # You
    "ST019BQ01T",             # Mother
    "ST019CQ01T",             # Father
    "ST021Q01TA",             # Age arrival host country
    
    # ICT availability
    "ICTHOME",                # ICT available at home
    "ICTSCH",                 # ICT available at school
    
    # ICT general use
    "ENTUSE",                 # ICT use outside of school (leisure) (WLE)
    "HOMESCH",                # Educational ICT use outside school
    "USESCH",                 # Use of ICT at school in general (WLE)
    
    
    # ICT subject related use
    "ICTCLASS",               # Subject-related ICT use durring lessons (WLE)
    "ICTOUTSIDE",             # Subject-related ICT use outside of of lessons (WLE)
    
    # ICT engagement
    "COMPICT",                # Perceived ICT competence
    "INTICT",                 # Interest to ICT
    "SOIAICT",                # ICT in social interaction
    "AUTICT",                 # ICT autonomy
    
    # Meta cognition
    "METASPAM",               # Meta-cognition: assess credibility
    
    # Academic achievement
    intersect(starts_with("PV"), ends_with("MATH")),  # Math
    intersect(starts_with("PV"), ends_with("SCIE")),  # Science
    intersect(starts_with("PV"), ends_with("READ")),  # Reading
  ))

rm(PISA2018) # Remove main dataset to free memory
gc()         # Free additional memory


## Change column names --------------------------------------
P18 <- rename(
  P18,
  "GNDR"                = "ST004D01T",
  "CNTRY"               = "CNT",
  "SCHOOLID"            = "CNTSCHID",
  "STUDENTID"           = "CNTSTUID",
  "Origin_cntry_you"    = "ST019AQ01T",
  "Origin_cntry_mother" = "ST019BQ01T",
  "Origin_cntry_father" = "ST019CQ01T",
  "Age_arrival_cntry"   = "ST021Q01TA"
)

names(P18) # Check newly created dataset


# Subset dataset -----------------------------------------
## OECD countries only -----------------------------------
#P18_OECD <- subset (P18, OECD == 1)
#table(P18$CNTRY)

# Remaining countries
#AUS   AUT   BEL   CAN   CHE   CHL   COL   CZE 
#DEU   DNK   ESP   EST   FIN   FRA   GBR   GRC 
#HUN   IRL   ISL   ISR   ITA   JPN   KOR   LTU 
#LUX   LVA   MEX   NLD   NOR   NZL   POL   PRT 
#SVK   SVN   SWE   TUR   USA



## Immigrant background ----------------------------------
P18 <- P18 %>%
  mutate(immback = case_when(
    (Origin_cntry_mother == 1 & Origin_cntry_father == 1) ~ 0, # native
    (Origin_cntry_mother == 2 & Origin_cntry_father == 1) ~ 1, # immigrant background
    (Origin_cntry_mother == 1 & Origin_cntry_father == 2) ~ 1, # immigrant background
    (Origin_cntry_you    == 2 & Age_arrival_cntry   >= 7) ~ 1, # immigrant background
    TRUE ~ NA
  ))



## Achievement scores ----------------------------
P18$MATH <- rowMeans(subset(P18, select = c(PV1MATH:PV10MATH)))
P18$SCIE <- rowMeans(subset(P18, select = c(PV1SCIE:PV10SCIE)))
P18$READ <- rowMeans(subset(P18, select = c(PV1READ:PV10READ)))

### Check
min(P18$MATH)
max(P18$MATH)

min(P18$SCIE)
max(P18$SCIE)

min(P18$READ)
max(P18$READ)


## Gen dummy of gender ----------------------------
alabs(P18$GNDR)
P18$male <- P18$GNDR

P18$male [P18$GNDR == 1] <- 0
P18$male [P18$GNDR == 2] <- 1
P18$male [P18$GNDR >= 5] <- NA


# Clean NAs ------------------------------------------------
names(P18)

## Gender
P18$GNDR [P18$GNDR >= 5]    <- NA
P18$GNDR [P18$AGE  >= 9995] <- NA
P18$ESCS [P18$ESCS >= 95]   <- NA
P18$ICTHOME [P18$ICTHOME >= 95] <- NA
# This step can be skipped


table(is.na(P18)) # Check
P18 <- P18[complete.cases(P18), ]
table(is.na(P18)) # Check again


# Subset 
P18_immback <- subset (P18, immback == 1)


################################################################
# Save as RDS file for faster loading                          #
#saveRDS(P18_immback,"P18_immback.rds")                        #
#                                                              #
# Load P18 -------------------------------------------------   #
#P18_immback <- readRDS("P18_immback.rds")                     #
################################################################






# ANALYSIS ----------------------------------------------
## DESCRIPTIVE  -----------------------------------------
modelvars <- cbind(# ICT availability
  P18_immback$ICTHOME,                # ICT available at home
  P18_immback$ICTSCH,                 # ICT available at school
  
  # ICT general use
  P18_immback$ENTUSE,                 # ICT use outside of school (leisure) (WLE)
  P18_immback$HOMESCH,                # Educational ICT use outside school
  P18_immback$USESCH,                 # Use of ICT at school in general (WLE)
  
  # ICT subject related use
  P18_immback$ICTCLASS,
  P18_immback$ICTOUTSIDE,
  
  # ICT engagement
  P18_immback$COMPICT,
  P18_immback$INTICT,
  P18_immback$SOIAICT,
  P18_immback$AUTICT,
  
  # Meta cognition
  P18_immback$METASPAM,
  
  # Achievemtn score
  P18_immback$MATH,
  P18_immback$SCIE,
  P18_immback$READ,
  
  # Control
  P18_immback$GNDR,
  P18_immback$AGE,
  P18_immback$ESCS
)


descrhelp <- t(round(pastecs::stat.desc(modelvars), 2))
descrhelp

descr <- cbind(
  descrhelp[,3],  # NAs
  descrhelp[,4],  # Min
  descrhelp[,5],  # Max
  descrhelp[,9],  # Mean
  descrhelp[,13], # SD
  descrhelp[,1])  # N

### Name of
colnames(descr) <- c("NAs","Min", "Max", "Mean", "SD", "N")

### Variable names
rownames(descr) <- c(# Dependent variables
  "ICTHOME",
  "ICTSCH",
  "ENTUSE",
  "HOMESCH",
  "USESCH",
  "ICTCLASS",
  "ICTOUTSIDE",
  "COMPICT",
  "INTICT",
  "SOIAICT",
  "AUTICT",
  "METASPAM",
  # Independent
  "Math",
  "Science",
  "Reading",
  # Control variables
  "Gender",
  "Age",
  "ESCS")



print(descr)
# descr_1 <- as.data.frame(descr)
# writexl::write_xlsx(descr_1, "C:/Users/Emir-/Desktop//D1.xlsx")


## OLS Regression ----------------------------------------

### ICT
#### ICT availability ----
P18_immback$ICT_availability <- P18_immback$ICTHOME + P18_immback$ICTSCH

#### ICT general use ----
P18_immback$ICT_general_use <- P18_immback$ENTUSE + P18_immback$HOMESCH + P18_immback$USESCH

#### ICT subject related use ----
P18_immback$ICT_subject_rel_use <- P18_immback$ICTCLASS + P18_immback$ICTOUTSIDE

#### ICT engagement ----
P18_immback$ICT_engagement <- P18_immback$COMPICT + P18_immback$INTICT+ P18_immback$SOIAICT + P18_immback$AUTICT

#### ICT scenario-based assessment ----
P18_immback$METASPAM


#### M1: ICT availability

M1_Math<- lm(MATH ~ ICT_availability + male + AGE + ESCS, data = P18_immback)
M1_Read<- lm(READ ~ ICT_availability + male + AGE + ESCS, data = P18_immback)
M1_Scie<- lm(SCIE ~ ICT_availability + male + AGE + ESCS, data = P18_immback)

tab_model(M1_Math, M1_Read, M1_Scie,
          digits = 2,
          p.style = "stars",
          show.ci = F,
          show.se = T,
          collapse.se = T,
          title = "OLS Regression: ICT availability on academic achievement",
          dv.labels = c("Math", "Reading", "Science"))


#### M2: ICT general use
M2_Math<- lm(MATH ~ ICT_general_use + male + AGE + ESCS, data = P18_immback)
M2_Read<- lm(READ ~ ICT_general_use + male + AGE + ESCS, data = P18_immback)
M2_Scie<- lm(SCIE ~ ICT_general_use + male + AGE + ESCS, data = P18_immback)

tab_model(M2_Math, M2_Read, M2_Scie,
          digits = 2,
          p.style = "stars",
          show.ci = F,
          show.se = T,
          collapse.se = T,
          title = "OLS Regression: ICT general use on academic achievement",
          dv.labels = c("Math", "Reading", "Science"))


#### M3: ICT subject related use
M3_Math<- lm(MATH ~ ICT_subject_rel_use + male + AGE + ESCS, data = P18_immback)
M3_Read<- lm(READ ~ ICT_subject_rel_use + male + AGE + ESCS, data = P18_immback)
M3_Scie<- lm(SCIE ~ ICT_subject_rel_use + male + AGE + ESCS, data = P18_immback)

tab_model(M3_Math, M3_Read, M3_Scie,
          digits = 2,
          p.style = "stars",
          show.ci = F,
          show.se = T,
          collapse.se = T,
          title = "OLS Regression: ICT subject related use on academic achievement",
          dv.labels = c("Math", "Reading", "Science"))


#### M4: ICT engagement
M4_Math<- lm(MATH ~ ICT_engagement + male + AGE + ESCS, data = P18_immback)
M4_Read<- lm(READ ~ ICT_engagement + male + AGE + ESCS, data = P18_immback)
M4_Scie<- lm(SCIE ~ ICT_engagement + male + AGE + ESCS, data = P18_immback)

tab_model(M4_Math, M4_Read, M4_Scie,
          digits = 2,
          p.style = "stars",
          show.ci = F,
          show.se = T,
          collapse.se = T,
          title = "OLS Regression: ICT engagement on academic achievement",
          dv.labels = c("Math", "Reading", "Science"))