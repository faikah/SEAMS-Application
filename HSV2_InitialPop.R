# === HSV2_InitialPop.R === #
# Creates a cohort to be used in sims
print(Sys.time())

# SIMULATION OF BASELINE POPULATION ----------------------------------------
  # === Start of Simulation
for (i in 1:sample.size)
{  
  ## === CHARACTERISTICS
  ## GENDER
  ind.sex[i] <- sex(i)
  ## AGE  
  ind.age[i] <- age(i)
  ## MALE CIRCUMCISION STATUS
  ind.mc.sett[i] <- mc.setting(ind.sex[i], i)
  ind.mc.age[i] <- age.circum(ind.mc.sett[i], i)
  ind.mc.status[i] <- circum.status(ind.sex[i], ind.mc.sett[i], ind.age[i], ind.mc.age[i])
  ## AGE OF SEXUAL DEBUT
  ind.sex.debut[i] <- age.sexual.debut(ind.sex[i], i)
  ## SEXUALLY ACTIVE STATUS
  ind.sex.status[i] <- sexual.status(ind.age[i],ind.sex.debut[i])
  ## RISK LEVEL	
  ind.risk[i] <- risk(ind.sex[i], ind.sex.debut[i], i)
  ## HIV STATUS
  ind.hiv.status[i] <- hiv(ind.sex[i], ind.age[i], ind.sex.status[i], i)
  ind.hiv.init.status[i] <- ind.hiv.status[i]
  ## TIME SINCE HIV INFECTION
  ind.hiv.time[i] <- time.since.hiv(ind.hiv.status[i])
  ## HSV2 STATUS
  ind.hsv2.status[i] <- hsv2(ind.sex[i], ind.age[i], ind.sex.status[i], i) 
  ind.hsv2.init.status[i] <- ind.hsv2.status[i]
  ## TIME SINCE hsv2 INFECTION
  ind.hsv2.time[i] <- time.since.hsv2(ind.hsv2.status[i])
  ## NUMBER OF PARTNERS
  ind.num.part[i] <- number.partners(ind.sex[i],ind.sex.status[i], ind.risk[i], i)
  
  if (ind.num.part[i]==0)
  {
    part1.sex[i] <- NA
    part1.age[i] <- NA
    part1.hiv[i] <- NA
    part1.hsv2[i] <- NA
    part2.sex[i] <- NA
    part2.age[i] <- NA
    part2.hiv[i] <- NA
    part2.hsv2[i] <- NA
  }
  if (ind.num.part[i]==1)
  {
    part1.sex[i] <- partner.sex(ind.sex[i])
    part1.age[i] <- partner1.age(ind.sex[i],ind.age[i])
    if(part1.sex[i] == 1)
    {
      part1.hiv[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.m.odds)[1]
      part1.hsv2[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.m.odds)[2]   
    }
    else
    {
      part1.hiv[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.f.odds)[1]
      part1.hsv2[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.f.odds)[2]   
    }
    part2.sex[i] <- NA
    part2.age[i] <- NA
    part2.hiv[i] <- NA
    part2.hsv2[i] <- NA
  }
  if (ind.num.part[i]==2)
  {
    part1.sex[i] <- partner.sex(ind.sex[i])
    part1.age[i] <- partner1.age(ind.sex[i],ind.age[i])
    if(part1.sex[i] == 1)
    {
      part1.hiv[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.m.odds)[1]
      part1.hsv2[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.m.odds)[2]   
    }
    else
    {
      part1.hiv[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.f.odds)[1]
      part1.hsv2[i] <- partner1.hsv2.hiv.function(part1.sex[i], part1.age[i], i, hiv.f.odds)[2]   
    }
    part2.sex[i] <- partner.sex(ind.sex[i])
    part2.age[i] <- partner2.age(ind.sex[i],ind.age[i])
    if(part2.sex[i] == 1)
    {
      part2.hiv[i] <- partner2.hsv2.hiv.function(part2.sex[i], part2.age[i], i, hiv.m.odds)[1]
      part2.hsv2[i] <- partner2.hsv2.hiv.function(part2.sex[i], part2.age[i], i, hiv.m.odds)[2]   
    }
    else
    {
      part2.hiv[i] <- partner2.hsv2.hiv.function(part2.sex[i], part2.age[i], i, hiv.f.odds)[1]
      part2.hsv2[i] <- partner2.hsv2.hiv.function(part2.sex[i], part2.age[i], i, hiv.f.odds)[2]   
    }
  }
  # == Additional columns for dataframe 
  Choice.mmc[i] <- 0
  time.total[i] <- 0
  
  score.con[i] <- 0
  score.mc[i] <- 0
  choice[i] <- NA
  score.adh[i] <- 0
  Start.Age[i] <- ind.age[i]
  End.Age[i] <- 0
  Start.Age.Yr[i] <- floor(ind.age[i])
  End.Age.Yr[i] <- 0
  Age.At.HIV[i] <- NA
  Num.HIV.Inf[i] <- 0
  Age.HIV[i] <- NA
  Age.At.HSV2[i] <- NA
  Num.HSV2.Inf[i] <- 0
  Age.HSV2[i] <- NA
  
  # if(i %% sample.size != 0) print(paste0('Completed individual  ', i, ' of ', sample.size))
  
} # === end of for loop simulation === #
  print(Sys.time())
# RANDOMIZATION INTO 2 GROUPS - CONTROL and INTERVENTION ------------------  
# USE SAME BASE COHORT FOR CONTROL AND INTERVENTION
HSV2_COHORT <- data.frame(ID=(1:sample.size), Sex = ind.sex, Age = ind.age, #Base.Age = ind.base.age,
                     MC.Setting = ind.mc.sett, MC.Age = ind.mc.age, MC.Stat = ind.mc.status,
                     Age.Sex.Debut = ind.sex.debut, Sex.Stat = ind.sex.status, Risk = ind.risk, 
                     HIV = ind.hiv.status, Time.HIV = ind.hiv.time, Initial.HIV.Status = ind.hiv.init.status,
                     HSV2 = ind.hsv2.status, Time.HSV2 = ind.hsv2.time, Initial.HSV2.Status = ind.hsv2.init.status,
                     Num.Partners = ind.num.part,
                     Partner1.Sex=part1.sex,Partner1.Age=part1.age, Partner1.HIV=part1.hiv, Partner1.HSV2=part1.hsv2, 
                     Partner2.Sex=part2.sex,Partner2.Age=part2.age, Partner2.HIV=part2.hiv, Partner2.HSV2=part2.hsv2,
                     Choice.mmc = Choice.mmc,
                     time.total=time.total,
                     score.con=score.con,
                     score.mc=score.mc,
                     choice=choice,
                     score.adh=score.adh,
                     Start.Age=Start.Age,
                     End.Age=End.Age,
                     Start.Age.Yr=Start.Age.Yr,
                     End.Age.Yr=End.Age.Yr,
                     Age.At.HIV=Age.At.HIV,
                     Num.HIV.Inf=Num.HIV.Inf,
                     Age.HIV=Age.HIV,
                     Age.At.HSV2=Age.At.HSV2,
                     Num.HSV2.Inf=Num.HSV2.Inf,
                     Age.HSV2=Age.HSV2)
