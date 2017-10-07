#summary function

library(data.table)
library(stringr)
library(plyr)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(bit64)
library(foreach)
library(doParallel)
library(reshape2)
library(xlsx)


wd <- "C:/Users/39492/Desktop/HEM S2D R"
setwd(wd)

control.file="test0.txt"


# read.control file reads the settings for the current S2D run

read.control.file = function(control.file) {
  if (is.null(control.file)) control.file <- "S2D_control_file.txt"
  lc <- str_length(control.file)
  if(!substr(control.file,lc-3,lc)=='.txt') control.file <- paste0(control.file,".txt")
  x <- fread(paste0("input/",control.file),header=FALSE)
  setnames(x,c("key","setting"))
  x$key          <- tolower(x$key)
  chem.list      <- list(c(x$setting[x$key=="chem"]))
  puc.list       <- list(c(x$setting[x$key=="puc"]))
  fug.file       <- x$setting[x$key=="fug.file"] 
  chem.file      <- x$setting[x$key=="chem.file"]
  chem.frac.file <- x$setting[x$key=="chem.frac.file"]
  puc.type.file  <- x$setting[x$key=="puc.type.file"]
  compart.file   <- x$setting[x$key=="compart.file"]
  puc.met.file   <- x$setting[x$key=="puc.met.file"]
  skin.area.file <- x$setting[x$key=="skin.area.file"]
  removal.file   <- x$setting[x$key=="removal.file"]
  vent.file      <- x$setting[x$key=="vent.file"]
  diary.prefix   <- x$setting[x$key=="diary.prefix"]
  run.name       <- x$setting[x$key=="run.name"]
  house.seed     <- x$setting[x$key=="house.seed"]
  puc.seed       <- x$setting[x$key=="puc.seed"]
  chem.seed      <- x$setting[x$key=="chem.seed"]
  init.seed      <- x$setting[x$key=="init.seed"]
  hab.prac       <- x$setting[x$key=="hab.prac"]
  if (length(init.seed)>0) { 
    house.seed   <- init.seed
    puc.seed     <- init.seed
    chem.seed    <- init.seed
  } else init.seed <- 0
  mode(house.seed)  <- "integer"
  mode(puc.seed)    <- "integer"
  mode(chem.seed)   <- "integer"
  first.house       <- x$setting[x$key=="first.house"]
  if (length(first.house)==0) first.house <- 1
  mode(first.house) <- "integer"
  last.house        <- x$setting[x$key=="last.house"]
  if (length(last.house)==0) last.house <- NA
  mode(last.house)  <- "integer"
  n.houses          <- x$setting[x$key=="n.houses"]
  if (length(n.houses)==0)   n.houses   <- NA
  mode(n.houses)    <- "integer"
  if (is.na(last.house) & !is.na(n.houses)) last.house <- first.house+n.houses-1
  if (is.na(n.houses) & !is.na(last.house)) n.houses   <- last.house-first.house+1
  comp.method       <- x$setting[x$key=="comp.method"]
  if (length(comp.method)==0) comp.method <- 1
  mode(comp.method) <- "integer" 
  puc.offset        <- x$setting[x$key=="puc.offset"]
  if (length(puc.offset)==0) puc.offset <- 0
  mode(puc.offset)  <- "integer"
  prog              <- substr(x$setting[x$key=="show.progress"],1,1)
  if (length(prog)==0)  prog <- "n"                     # default is no progress messages
  parallel          <- substr(x$setting[x$key=="parallel"],1,1)
  if (length(parallel)==0) parallel <- "n"              # default is not parallel
  save.r.objects    <- substr(x$setting[x$key=="save.r.objects"],1,1)
  if (length(save.r.objects)==0) save.r.objects <- "n"  # default is not to save
  out <- paste0("output/S2D/",run.name)
  g <- as.data.table(list(chem.list,hab.prac,puc.list,fug.file,chem.file,chem.frac.file,puc.type.file,compart.file,puc.met.file,
                          skin.area.file,removal.file,vent.file,diary.prefix,run.name,n.houses,init.seed,prog,parallel,
                          puc.offset,first.house,last.house,comp.method,save.r.objects,house.seed,puc.seed,chem.seed,out))
  setnames(g,c("chem.list","hab.prac","puc.list","fug.file","chem.file","chem.frac.file","puc.type.file","compart.file",
               "puc.met.file","skin.area.file","removal.file","vent.file","diary.prefix","run.name","n.houses",
               "init.seed","prog","parallel","puc.offset","first.house","last.house","comp.method","save.r.objects",
               "house.seed","puc.seed","chem.seed","out"))
  return(g)
}


read.vent.file = function(vent.file){
  if (is.null(vent.file)) vent.file <- g$vent.file
  x <- fread(paste0("input/",vent.file))
  setnames(x,tolower(names(x)))
  return(x)
}

# trimzero removes leading zeroes from CAS numbers

trimzero = function(x) {
  y <- x
  for (j in 1:length(x)) {
    i <- 1
    while(substr(x[j],i,i)=="0" & substr(x[j],i+1,i+1)!="_") {
      substr(y[j],i,i)<- " "
      i <- i+1
    }  
  }
  return(str_trim(y))
}

read.chem.props = function(chem.file,chem.list) {
  if (is.null(chem.file)) chem.file <- g$chem.file
  # Read chemical properties input file
  dt <- as.data.table(fread(paste0("input/",chem.file),na.strings=c("",". ","."," .","NA")))
  setnames(dt,names(dt),tolower(str_replace_all(names(dt),"_",".")))
  mode(dt$cas) <- "character"
  mode(dt$kp) <- "numeric"
  dt$cas <- trimzero(str_replace_all(dt$cas,"-","_"))
  dt$cas <- ifelse(substr(dt$cas,1,1)=="_",paste0("0",dt$cas),dt$cas)
  if(!exists("x",dt))               dt$x       <- 1:nrow(dt)
  if(is.na(dt[1]$x))                dt[1]$x    <- 1
  y <- lag(dt$x) + 1
  dt$x[is.na(dt$x)] <- y[is.na(dt$x)]
  if (length(chem.list)>0)  dt <- dt[dt$dtxsid %in% chem.list]
  dt <- unique(dt,by="dtxsid",fromLast=TRUE)
  if(nrow(dt)==0) cat("\n  No chemicals left in fugacity calcs \n")
  if(exists("name",dt))             setnames(dt,"name","chem.name")
  if(exists("vp.pa",dt))            setnames(dt,"vp.pa","vapor")
  if(exists("mw",dt))               setnames(dt,"mw","molwt")
  if(exists("water.sol.mg.l",dt))   dt$solub   <- dt$water.sol.mg.l/dt$molwt
  if(exists("log.kow",dt))          dt$kow     <- 10^dt$log.kow
  if(exists("half.sediment.hr",dt)) dt$decay.s <- 24*log(2)/dt$half.sediment.hr 
  if(exists("half.air.hr",dt))      dt$decay.a <- 24*log(2)/dt$half.air.hr 
  return(dt) 
}


# read.puc.types reads the .csv file listing the product type for each PUC

read.puc.types = function(puc.type.file,puc.list) {
  if (is.null(puc.type.file)) puc.type.file <- g$puc.type.file
  x <- fread(paste0("input/",puc.type.file))
  setnames(x,tolower(names(x)))
  if(length(puc.list>0)) {
    x <- x[x$source.id %in% puc.list] 
    for (i in 1:length(puc.list)) {
      if (!puc.list[i] %in% x$source.id) cat("  PUC ",puc.list[i]," not on PUC types file")
    }
  }
  x$code    <- toupper(x$code)
  puc.types <- x[x$code!="XXX"]
  return(puc.types)
}


# read.diary loads the product-use diary for one household

read.diary = function(diary.prefix,run.name,house.num,persons) {
  if (is.null(diary.prefix)) diary.prefix <- g$diary.prefix
  if (is.null(run.name))         run.name <- g$run.name
  format <- c("Clusters"="Character","Appliances"="Character","Impacted_by"="character")
  diary <- fread(paste0("input/abm/", diary.prefix, house.num, ".csv"),stringsAsFactors = FALSE, na.strings = c("","NA"),colClasses=format)
  setnames(diary,tolower(names(diary)))
  if(exists("person_household_index",diary))              setnames(diary,"person_household_index","p")
  if(exists("day_of_the_year",diary))                     setnames(diary,"day_of_the_year","daynum")
  if(exists("sheds_id",diary))                            setnames(diary,"sheds_id","source.id")
  if(exists("start_time_hr_using_military_time",diary))   setnames(diary,"start_time_hr_using_military_time","start")
  if(exists("duration_min",diary))                        setnames(diary,"duration_min","hand.dur")
  if(exists("household_index",diary))                     setnames(diary,"household_index","house.num")
  if(exists("person_gender",diary))                       setnames(diary,"person_gender","sex")
  if(exists("person_age",diary))                          setnames(diary,"person_age","age")
  if(exists("primary_person",diary))                      setnames(diary,"primary_person","primary")
  primenum <- persons$pnum[persons$primary==1]
  diary$person[diary$primary==1] <- primenum
  diary$person[diary$primary==0 & diary$p<=primenum] <- diary$p[diary$primary==0 & diary$p<=primenum]-1
  diary$person[diary$primary==0 & diary$p >primenum] <- diary$p[diary$primary==0 & diary$p >primenum]           
  diary$mass[is.na(diary$mass)] <- 0
  diary$source.id[is.na(diary$source.id)] <- "none"
  diary$mass[diary$source.id=="none"]     <- 0
  diary$hand.dur[diary$source.id=="none"] <- 0
  diary$product_type[diary$source.id=="none"] <- "none"
  diary$indoor_outdoor[diary$source.id=="none"] <- "I"
  diary$hour           <- 0
  diary$hour <- 1 + floor(diary$start)
  diary$hournum  <- 24*(diary$daynum-1) + diary$hour
  setorder(diary,person,hournum,start)
  diary$row <- 1:nrow(diary)
  return(diary)
}

# list.persons details all the persons in this household 

list.persons = function(ph) {
  gens  <- vector("character",20)
  ages  <- vector("integer",20)
  prime <- rep(0,20)
  pset  <- 0
  pg   <- substr(ph$gender,1,1)
  pa   <- ph$age_years
  for (p in 1:20) {
    gens[p] <- substr(ph$genders,p,p)
    if(gens[p]==".") break
    ages[p] <- as.integer(substr(ph$ages,2*p-1,2*p))
    if(gens[p]==pg & ages[p]==pa & pset==0) {
      prime[p] <- 1
      pset     <- 1
    }  
  }
  np      <- p-1
  persons <- as.data.table(data.frame(1:np,gens[1:np],ages[1:np],prime[1:np]))
  setnames(persons,c("pnum","sex","age","primary"))
  persons$basal.vent[persons$primary==1] <- ph$basal.vent
  persons$skin.area[persons$primary==1]  <- ph$BSA_adj
  if(pset==0) cat("\n  Primary person not identified in this house") 
  return(persons)
}


# read.pophouse reads the output from the HEM RPGen module 


read.pophouse = function(run.name) {
  if (cf$prog=="y") cat("\n  Reading pophouse...")
  if (is.null(run.name)) run.name <- cf$run.name
  pophouse <- fread(paste0("input/pophouse.csv"))
  if (!exists("basa.vent",pophouse)) pophouse$basal.vent <- 5+0.5*pmin(14,floor(pophouse$age_years/2))
  pophouse$unitsf[pophouse$unitsf<0] <- 1500
  pophouse$house <- 1:nrow(pophouse)
  return(pophouse)
}

#read.puc.use reads the PUC_use_data
read.puc.use = function(hab.prac,puc.list){
  if (is.null(hab.prac)) hab.prac <- cf$hab.prac
  x <- fread(paste0("input/",hab.prac))
  setnames(x,tolower(names(x)))
  #if (exists("source.id",x)) setnames(x,"puc","source.id")
  if(length(puc.list>0)) {
    x <- x[x$source.id %in% puc.list] #??
    for (i in 1:length(puc.list)) {
      if (!puc.list[i] %in% x$source.id) cat("  PUC ",puc.list[i]," not on PUC MET file")
    }
  }
  setnames(x,c("source.id","source_description","HT","HT_CV","AT","Prev_M","Prev_F","Prev_child","Freq","Freq_CV","Mass","Mass_CV"))
  return(x)
}



pop <- read.pophouse(cf$run.name)

cf <- read.control.file(control.file)

vent <- read.vent.file(cf$vent.file)

chemp <- read.chem.props(cf$chem.file,cf$chem.list)



#data frames to hold data 
HH.data            <- data.frame("INFO"=character(),"PRIMARY ONLY"=integer(),"EVERYBODY"=integer(),stringsAsFactors = FALSE)
chem.data          <- data.frame("DTXID"=character(),"Chemical Name"=character(),"CAS"=integer(),"Product name"=character(),"Code"=character(),stringsAsFactors = FALSE)
PUC.data           <- data.frame("PUC"=character(),"PUC name"=character(),"Households that use PUC"=integer(),"Households that don't use PUC"=integer(),"Primaries that use PUC, and others do"=integer(),"Primaries that don't use PUC, but others do"=integer(),"Code"=character(), stringsAsFactors = FALSE)
HP.data            <- data.frame("PUC"=character(),"Description"=character(),"E_Prev_M"=integer(),"A_Prev_M"=integer(),"E_Prev_F"=integer(),"A_Prev_F"=integer(),"E_Prev_Ch"=integer(),"A_Prev_Ch"=integer(),"E_Freq"=integer(),"A_Freq"=integer(),"E_Mass"=integer(),"A_Mass"=integer(), stringsAsFactors = FALSE)
                      

#OPTIONAL.data      <- data.frame()


max_age <- 0
min_age <- 0
min_M_age <- 0
max_M_age <- 0
min_F_age <- 0
max_F_age <- 0
max_Ch_age <- 0
min_Ch_age <- 0


#this is looking at only primary individuals

if ('Male' %in% person.data$gender && 'Female' %in% person.data$gender){
  G = "M and F"
}else if (('Male' %in% person.data$gender&&!"Female"%in%person.data)){#add female not in
  G= "M only"
}else if (('Female' %in% person.data$gender&&!"Male"%in%person.data)){#add male not in
  G= "F only"
}


#Write sheet 1 alt.

HH.data[nrow(HH.data)+1, ] <- c("No. of househods",cf$last.house-cf$first.house+1,cf$last.house-cf$first.house+1)
HH.data[nrow(HH.data)+1, ] <- c("Min age",min_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Max age",max_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Max age - Min age",max_age-min_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Min male age",min_M_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Max male age",max_M_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Min female age",min_F_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Max female age",max_F_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Min child age",min_Ch_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Max child age",max_Ch_age,0)
HH.data[nrow(HH.data)+1, ] <- c("Gender","","")
 




#-------------

#H&P

#habits and practices code
for (a in 1:length(unlist(cf$puc.list))){
  
  
  #print (unlist(cf$puc.list)[u])
  puc <- read.puc.types(cf$puc.type.file,unlist(cf$puc.list)[a])
  hp <- read.puc.use(cf$hab.prac,unlist(cf$puc.list)[a])
  
  
  apuc <- unlist(cf$puc.list)[a]
  non_PUC <- 0
  notP_O <- 0

#-------------H&P begins  
  
  #apuc <- unlist(cf$puc.list)[a]
  
  pop_m12 <- 0.0 #counter for the population of males above 12 yrs
  pop_f12 <- 0.0 #counter for the population of females above 12 yrs
  pop_ch <- 0.0 #counter for the population of children (<=12)
  
  puc_user <- 0.0 #counter for puc users only
  
  x_puc <- 0.0 # counter for the number of times puc used
  
  mass_puc <- 0.0 #counter for sum of mass of puc used
  
  p_puc_m12 <- 0.0 # counter for prevalence of use of puc in m12 demographic
  p_puc_f12 <- 0.0 #counter for prevalence of use of puc in f12 demographic
  p_puc_ch <- 0.0 # counter for prevalence of use of puc in ch demographic
  
 #----------------------------------- 
  for (hn in (cf$first.house):(cf$last.house)){
    
    pd <- person.data[person.data$house==hn]
    pp <- list.persons(pd)
   # print(pp$age)
    
    abm <- read.diary(cf$diary.prefix,cf$run.name,hn,pp)
    
    maxxx <- max(abm$age)
    max_age <- max(max_age,maxxx)
    minnn <- min(abm$age)
    min_age <- min(min_age,minnn)
    
    M_abm <- abm[abm$sex=="M"]
    max_M_age <- max(max_M_age, max(M_abm$age)) 
    min_M_age <- min(min_M_age, min(M_abm$age))
    
    F_abm <- abm[abm$sex=="F"]
    max_F_age <- max(max_F_age, max(F_abm$age))
    min_F_age <- min(min_F_age, min(F_abm$age))
    
    Ch_abm <- abm[abm$age<=12]
    max_Ch_age <- max(12, max(Ch_abm$age))
    min_Ch_age <- min(12, min(Ch_abm$age))
    
    if (!(n%in%abm$source.id)){
      non_PUC= non_PUC+1 # no of households that are non-users of the PUCs in model run
    }
    
    prim <- abm[abm$primary==1]
    other <- abm[abm$primary==0]
    
    # print("PUC #:")
    # print (u)
    # 
    # print("house number:")
    # print(hn)
    # 
    # print("APUC:")
    # print(apuc)
    # 
    if ((!is.element(apuc,prim$source.id)&&is.element(apuc,other$source.id))==TRUE){
      
      notP_O= notP_O+1
    }
    
    
    #------------------------------H&P begins
    pd <- person.data[person.data$house==hn]
    pp <- list.persons(pd)

    HH <- read.diary(cf$diary.prefix,cf$run.name,hn,pp)
    
    females <- HH[HH$sex=="F"]    # sort for females in HH
    fem <- females[females$age>12]# sort for females > 12 yrs
    pop_f12 <- pop_f12+length(unique(fem$p)) #the count 
    
    
    males <- HH[HH$sex=="M"]      # sort for males in HH
    mal <- males[males$age>12]    #sort for males in HH > 12 yrs
    pop_m12 <- pop_m12+length(unique(mal$p)) ##############the count
    #print("HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    #print(length(unique(mal$p)))
    
    
    children <- HH[HH$age<=12] #sort for children (<=12)
    pop_ch <- pop_ch+length(unique(children$p)) #the count

    uniq <- unique(HH$person) # each person in this HH
    
    for (u in uniq){
      #print(u)
      HHsub <- HH[HH$person==u] #sort by this person
      if (apuc%in%HHsub$source.id){
        puc_user <- puc_user+1.0
        
        HHsub <- HHsub[HHsub$source.id==apuc] #sort by this puc
        x_puc <- x_puc + nrow(HHsub)
        mass_puc <- mass_puc+sum(as.numeric(HHsub$mass))
      } 
      if (HHsub$sex[1]=="M"&&HHsub$age[1]>12){
        p_puc_m12 <- p_puc_m12+1.0
      }
      if (HHsub$sex[1]=="F"&&HHsub$age[1]>12){
        p_puc_f12 <- p_puc_f12+1.0
      }
      if (HHsub$age[1]<=12){
        p_puc_ch <- p_puc_ch+1.0
      }
    }
    
  }
  PUC.data[nrow(PUC.data)+1, ] <- c(apuc,puc$description,0,non_PUC,0,notP_O,puc$code)
  
  actual_freq_puc <- x_puc/puc_user
  actual_mass_puc <- mass_puc/x_puc
  prev_puc_m12 <- p_puc_m12/pop_m12 #################
  
  prev_puc_f12 <- p_puc_f12/pop_f12
  prev_puc_ch <- p_puc_ch/pop_ch
  
  HP.data[nrow(HP.data)+1, ] <- c(apuc,hp$source_description,hp$Prev_M,prev_puc_m12,hp$Prev_F,prev_puc_f12,hp$Prev_child,prev_puc_ch,hp$Freq,actual_freq_puc,hp$Mass,actual_mass_puc)

  print("HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  print(hp$source_description)
  
  
  HH.data[nrow(HH.data)+1, ] <- c("population of M >12","",pop_m12)
  HH.data[nrow(HH.data)+1, ] <- c("population of F >12","",pop_f12)
  HH.data[nrow(HH.data)+1, ] <- c("population of M/F <=12","",pop_ch)
  
  HH.data[nrow(HH.data)+1, ] <- c("No of people who use the PUC","",puc_user)
  HH.data[nrow(HH.data)+1, ] <- c("No of times people use the PUC","",x_puc)
  HH.data[nrow(HH.data)+1, ] <- c("sum of mass of puc used","",mass_puc)
  
  
  HH.data[nrow(HH.data)+1, ] <- c("Prevalence of puc use in M >12","",p_puc_m12)
  HH.data[nrow(HH.data)+1, ] <- c("Prevalence of puc use in F >12","",p_puc_f12)
  HH.data[nrow(HH.data)+1, ] <- c("Prevalence of puc use in M/F <=12","",p_puc_ch)
  
  HH.data[nrow(HH.data)+1, ] <- c("End of this household","","")
  
}

#HH info
write.xlsx((HH.data),file="C:/Users/39492/Desktop/HEM S2D R/summary sheet.xlsx",sheetName="Household Summary",row.names=FALSE)

#PUC info
write.xlsx((PUC.data),file="C:/Users/39492/Desktop/HEM S2D R/summary sheet.xlsx",sheetName="PUC_use",append = TRUE,row.names = FALSE)

#H&P info
write.xlsx((HP.data),file="C:/Users/39492/Desktop/HEM S2D R/summary sheet.xlsx",sheetName="H&P",append = TRUE,row.names = FALSE)

#
