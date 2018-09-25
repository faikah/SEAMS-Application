# === HSV2_EstimatingTransmissionParameters.R === #
# Set working directory
setwd(getwd())
# File to read in all functions and run the different sceanrios for HSV2/HIV ----------

# === Load all necessary packages
library(actuar)
library(MASS)
library(boot)
library(reshape2)
library(ggplot2)
library(tidyverse)

# === Declaring Variables === #
# sample size
sample.size <- 100000

# duration of trial
time.trial <- 4

# = Age limits : 15-24
start.age <- 11
end.age <- 25

# === Run Initialising Files === #
# === Initial Variables : no changes required
source("HSV2_Variables_Initialising.R")
## CHECKED!!

# === Variables for distribution parameters for the Base Charactersitics: change if needed
source("HSV2_Variables_BaseCharacteristics.R")
## CHECKED!!

# === Variables for rates : change if needed        
source("HSV2_Variables_Rates.R")
## CHECKED!!

# === Parameter Values for Prevention Methods - acceptability, adherence, efficacy
# === for both control (sim.run=0) and intervention sims (sim.run=1)
source("HSV2_Variables_Interventions.R")
## CHECKED!!

# === Run Function files === #
# === Setting the seeds for all the random numbers in all the functions === #
source("HSV2_SetSeed.R")                            
## CHECKED!!

# === Base Characteristics Functions (includes beta parameter estimation)
source("HSV2_Function_BaseCharacteristics.R")
## CHECKED!!

# === Time to event
source("HSV2_Function_TimeAndEvent.R")
# CHECKED!!

# === HIV and HSV-2 Incidence calculations
source("HSV2_Function_Incidence.R")
# NOT CHECKED!!

# === Final Cohort = returns DATA file
source("HSV2_Function_FinalCohort.R")
# === Relative Risk: Intervention vs Control
#source("Function_RelativeRisk.R")

# === START OF SIMS === #
# CREATE A POPULATION: COHORT
source("HSV2_InitialPop.R") # output is HSV2_COHORT
HSV2_InitialPopulation <- HSV2_COHORT
head(HSV2_COHORT,5)




# Control
S3 <- HSV2_InitialPopulation
S3.f <- subset(HSV2_InitialPopulation[HSV2_InitialPopulation$Sex==0,]) # CONTROL: Females
S3.m <- subset(HSV2_InitialPopulation[HSV2_InitialPopulation$Sex==1,]) # CONTROL: Males
# Intervention
S4 <- HSV2_InitialPopulation
S4.f <- subset(HSV2_InitialPopulation[HSV2_InitialPopulation$Sex==0,]) # INTERVENTION: Females
S4.m  <- subset(HSV2_InitialPopulation[HSV2_InitialPopulation$Sex==1,]) # INTERVENTION: Males


# Output for Odds Ratio of HIv given HSV-2 infection for partners
reg.m.p1 <- glm(Partner1.HIV ~  Partner1.HSV2, data=S3.m, family=binomial)
exp(summary(reg.m.p1)$coefficients)
reg.m.p2 <- glm(Partner2.HIV ~  Partner2.HSV2, data=S3.m, family=binomial)
exp(summary(reg.m.p2)$coefficients)


reg.f.p1 <- glm(Partner1.HIV ~  Partner1.HSV2, data=S3.f, family=binomial)
exp(summary(reg.f.p1)$coefficients)
reg.f.p2 <- glm(Partner2.HIV ~  Partner2.HSV2, data=S3.f, family=binomial)
exp(summary(reg.f.p2)$coefficients)


reg.f.p1 <- glm(Partner1.HIV ~  Partner1.HSV2, data=Con.S3.f, family=binomial)
exp(summary(reg.f.p1)$coefficients)
reg.f.p2 <- glm(Partner2.HIV ~  Partner2.HSV2, data=Con.S3.f, family=binomial)
exp(summary(reg.f.p2)$coefficients)

# # === CONTROL COHORT ----------------------------------------------------
# = SCENARIO 3: Simulation Run - for CONTROL COHORT
# sim.run to 0 for CONTROL ---> sets choice of Intervention to "NA"
# sim.run to 1 for INTERVENTION ---> sets choice of Intervention to "PrEP/Microbicides"
# For estimating parameters - set sim.run <- 0

sim.run <- 0

# FOR LOOP TO BEST ESTIMATE THE PARAMETER COMBINATION FOR HIV AND HSV-2 TRANSMISSION PARAMETERS
# === FEMALES

hivprev.f <- matrix(0, ncol=19, nrow=1)
hsv2prev.f <- matrix(0, ncol=19, nrow=1)  

hivprev.f <- as.data.frame(hivprev.f)
colnames(hivprev.f) <- c("hiv", "hsv2", "age.15", "age.16", "age.17", "age.18", "age.19", "age.20", "age.21", "age.22",
                         "age.23", "age.24", "age.25", "age.26", "age.27", "age.28", "age.15to19", "age.20to24", "age.15to24")
hsv2prev.f <- as.data.frame(hsv2prev.f)
colnames(hsv2prev.f) <- c("hiv", "hsv2", "age.15", "age.16", "age.17", "age.18", "age.19", "age.20", "age.21", "age.22",
                          "age.23", "age.24", "age.25", "age.26", "age.27", "age.28", "age.15to19", "age.20to24", "age.15to24")

hivm2f.combo <-  c(seq(0.09, 0.99, 0.1))
hsv2m2f.combo <- c(seq(0.05, 0.05, 0.01))
total.run.f <- length(hsv2m2f.combo)
for (ii in (1:length(hivm2f.combo))) 
{

  hiv.m2f <- hivm2f.combo[ii]
  for(jj in (1:length(hsv2m2f.combo))) 
  {
    hsv2.m2f <- hsv2m2f.combo[jj]
    print(paste0('Females: HSV-2 value: ', hsv2.m2f, ' number ',jj,' out of ',length(hsv2m2f.combo), '. HIV value: ', hiv.m2f, ' number ',jj,' out of ', length(hivm2f.combo)))
    Con.S3.f <- HSV2_Final_Cohort(S3.f)
    # = HIV final prev, by age 
    CON.HIV.prev.f.final <- Con.S3.f %>%
      group_by(Age=End.Age.Yr) %>%
      summarise(total= n(),
                Final.f.prev = mean(All.HIV==1))
    CON.HSV2.prev.f.final <- Con.S3.f %>%
      group_by(Age=End.Age.Yr) %>%
      summarise(total= n(),
                Final.f.prev = mean(All.HSV2==1))
      
    # RUN MODEL AND OUTPUT AGE-SPECIFIC HIV PREVALENCE FINAL ESTMATES 
    hivprev.f <- rbind(hivprev.f, c(hiv.m2f, hsv2.m2f, as.vector(t(CON.HIV.prev.f.final[,3])),
                                    mean(Con.S3.f$All.HIV[Con.S3.f$End.Age.Yr %in% c(15:19)]),# = HIV final prev,  age-group 15 to 19
                                    mean(Con.S3.f$All.HIV[Con.S3.f$End.Age.Yr %in% c(19:24)]),# = HIV final prev,  age-group 19 to 24
                                    mean(Con.S3.f$All.HIV[Con.S3.f$End.Age.Yr %in% c(15:24)]) ))# = HIV final prev,  age-group 15 to 24
    hsv2prev.f <- rbind(hsv2prev.f, c(hiv.m2f, hsv2.m2f, as.vector(t(CON.HSV2.prev.f.final[,3])),
                                     mean(Con.S3.f$All.HSV2[Con.S3.f$End.Age.Yr %in% c(15:19)]),# = HSV2 final prev,  age-group 15 to 19
                                     mean(Con.S3.f$All.HSV2[Con.S3.f$End.Age.Yr %in% c(19:24)]),# = HSV2 final prev,  age-group 19 to 24
                                     mean(Con.S3.f$All.HSV2[Con.S3.f$End.Age.Yr %in% c(15:24)]) ))# = HSV2 final prev,  age-group 15 to 24
  print(hivprev.f)
  print(hsv2prev.f)
  }
}
# Use output to calculate "min LSS residuals" in order to select best parameters that fit the HIV and HSV-2 Prevalence Data
write.csv(hivprev.f, "CSV_Files/HIVandHSV2estimates_HIVprevfemales.csv")
write.csv(hsv2prev.f, "CSV_Files/HIVandHSV2estimates_HSV2prev_females.csv")
