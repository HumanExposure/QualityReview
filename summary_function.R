---
  header-includes:
  - \usepackage{caption}
title: "HEM S2D Summary"
output:
  pdf_document: default
html_document: default
classoption: landscape
---
  
  
  \captionsetup[table]{labelformat=empty}

```{r include=FALSE, echo=FALSE, results='asis'}
#classoption: landscape
#Copy and paste whole file into a new R markdown file
#11/16/2017 - 12:36 pm
#HEM Summary Function

#libraries
library(latticeExtra)
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
library(pander)
library(kableExtra)

#setting working directory
wd <- "C:/Users/39492/Desktop/temp HEM testing folder/update to common__HEM summary function" 
setwd(wd)

#name of control file for optional output
optional.file="ocf.txt"

#name of control file for S2D run
control.file="airfreshener_OPP.txt"

#panderOptions('table.continues','')
#panderOptions('table.caption.prefix', 'Table:')
#panderOptions('digits',1)#attempt to fomat decimal places

#many functions below were recycled from S2D code:

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
  g <- as.data.table(list(chem.list,puc.list,fug.file,chem.file,chem.frac.file,puc.type.file,compart.file,puc.met.file,
                          skin.area.file,removal.file,vent.file,diary.prefix,run.name,n.houses,init.seed,prog,parallel,
                          puc.offset,first.house,last.house,comp.method,save.r.objects,house.seed,puc.seed,chem.seed,out))
  setnames(g,c("chem.list","puc.list","fug.file","chem.file","chem.frac.file","puc.type.file","compart.file",
               "puc.met.file","skin.area.file","removal.file","vent.file","diary.prefix","run.name","n.houses",
               "init.seed","prog","parallel","puc.offset","first.house","last.house","comp.method","save.r.objects",
               "house.seed","puc.seed","chem.seed","out"))
  return(g)
}

#read the control file
cf <- read.control.file(control.file)


#read optional file reads the settings for the optional output
read.optional.file = function(optional.file){
  if (is.null(optional.file)) optional.file <- "optional_control_file.txt"
  lc <- str_length(optional.file)
  if (!substr(optional.file,lc-3,lc)=='.txt') optional.file <- paste0(optional.file, ".txt")
  x <- fread(paste0("input/",optional.file),header = FALSE)
  setnames(x,c("key","setting"))
  x$key  <- tolower(x$key)
  #start of possible rows
  run.all.housholds                                    <- x$setting[x$key=="run.all.households"] 
  output.plots                                         <- x$setting[x$key=="output.plots"]
  total.absorbed.dose                                  <- x$setting[x$key=="total.absorbed.dose"]                      
  dermal.absorbed.dose.total                           <- x$setting[x$key=="dermal.absorbed.dose.total"] 
  dermal.absorbed.dose.direct                          <- x$setting[x$key=="dermal.absorbed.dose.direct"] 
  dermal.absorbed.dose.indirect                        <- x$setting[x$key=="dermal.absorbed.dose.indirect"] 
  inhalation.absorbed.dose.total                       <- x$setting[x$key=="inhalation.absorbed.dose.total"] 
  inhalation.absorbed.dose.direct                      <- x$setting[x$key=="inhalation.absorbed.dose.direct"] 
  inhalation.absorbed.dose.indirect                    <- x$setting[x$key=="inhalation.absorbed.dose.indirect"] 
  ingestion.absorbed.dose.total                        <- x$setting[x$key=="ingestion.absorbed.dose.total"] 
  ingestion.absorbed.dose.direct                       <- x$setting[x$key=="ingestion.absorbed.dose.direct"] 
  ingestion.absorbed.dose.indirect                     <- x$setting[x$key=="ingestion.absorbed.dose.indirect"] 
  mass.down.the.drain                                  <- x$setting[x$key=="mass.down.the.drain"] 
  mass.out.the.window                                  <- x$setting[x$key=="mass.out.the.window"] 
  mass.in.solid.waste                                  <- x$setting[x$key=="mass.in.solid.waste"] 
  #start of possible columns
  age.groups.of.interest                               <- x$setting[x$key=="age.groups.of.interest"] 
  population.average.of.annual.mean.for.all.population <- x$setting[x$key=="population.average.of.annual.mean.for.all.population"] 
  population.average.of.annual.mean.for.users.only     <- x$setting[x$key=="population.average.of.annual.mean.for.users.only"] 
  population.average.of.maximum.daily.dose             <- x$setting[x$key=="population.average.of.maximum.daily.dose"]
  hab.prac.file                                        <- x$setting[x$key=="hab.prac.file"]
  s2d.output.folder.location                           <- x$setting[x$key=="s2d.output.folder.location"]
  abm.folder.location                                  <- x$setting[x$key=="abm.folder.location"]
  
  
  g <- as.data.table(list(run.all.housholds,output.plots,total.absorbed.dose,dermal.absorbed.dose.total,dermal.absorbed.dose.direct,dermal.absorbed.dose.indirect,inhalation.absorbed.dose.total,inhalation.absorbed.dose.direct,inhalation.absorbed.dose.indirect,ingestion.absorbed.dose.total,ingestion.absorbed.dose.direct,
                          ingestion.absorbed.dose.indirect,mass.down.the.drain,mass.out.the.window,mass.in.solid.waste,age.groups.of.interest,population.average.of.annual.mean.for.all.population,population.average.of.annual.mean.for.users.only,population.average.of.maximum.daily.dose,hab.prac.file,s2d.output.folder.location,abm.folder.location))
  setnames(g,c("run.all.households","output.plots","total.absorbed.dose","dermal.absorbed.dose.total","dermal.absorbed.dose.direct","dermal.absorbed.dose.indirect","inhalation.absorbed.dose.total","inhalation.absorbed.dose.direct","inhalation.absorbed.dose.indirect","ingestion.absorbed.dose.total","ingestion.absorbed.dose.direct",
               "ingestion.absorbed.dose.indirect","mass.down.the.drain","mass.out.the.window","mass.in.solid.waste","age.groups.of.interest","population.average.of.annual.mean.for.all.population","population.average.of.annual.mean.for.users.only",
               "population.average.of.maximum.daily.dose","hab.prac.file","s2d.output.folder.location","abm.folder.location"))
  return(g)
}

#read the optional control file
od <- read.optional.file(optional.file)


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

#read chemical properties file
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
  diary <- fread(paste0(od$abm.folder.location, diary.prefix, house.num, ".csv"),stringsAsFactors = FALSE, na.strings = c("","NA"),colClasses=format)
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
    #original code
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

#read.s2d.annual reads data contained in the files in the s2d annual folder
read.s2d.annual = function(house_number,chemical) {
  annual <- fread(paste0(od$s2d.output.folder.location,"Annual/", "House_", house_number, ".csv"),stringsAsFactors = FALSE, na.strings = c("","NA"))
  if(chemical=="NA"){
    return(annual)
  }else{
    annual <- annual[annual$dtxsid==chemical]
    return(annual)
  }
}

#read.s2d.daily reads data contained in the files in the s2d daily folder
read.s2d.daily = function(house_number,chemical){
  daily <- fread(paste0(od$s2d.output.folder.location,"Daily/","Daily_",house_number,".csv"),stringsAsFactors = FALSE,na.strings = c("","NA"))
  if (chemical=="NA"){
    return(daily)
  }else{
    daily <- daily[daily$dtxsid==chemical]
    return(daily)
  }
}


#read.prod.chem reads the s2d prod_chem files
read.prod.chem = function(house_num,chem,puccc){
  prodchem <- fread(paste0(od$s2d.output.folder.location,"Prod_chem/","Prod_chem_",house_num,".csv"),stringsAsFactors = FALSE, na.strings = c("","NA"))
  if (chem=="NA"&&puccc=="NA"){
    return(prodchem)
  }else if (chem=="NA"&&!(puccc=="NA")){
    prodchem <- prodchem[prodchem$puc==puccc]
    return(prodchem)
  }else if (puccc=="NA"&&!(chem=="NA")){
    prodchem <- prodchem[prodchem$chemical==chem]
    return(prodchem)
  }else{
    prodchem <- prodchem[prodchem$puc==puccc]
    prodchem <- prodchem[prodchem$chemical==chem]
    return(prodchem)
  } 
}

#read.comp.s2d reads the composition for s2d file
read.comp.s2d = function(puc,chem){
  comp.file <- fread(paste0("input/",cf$chem.frac.file),stringsAsFactors = FALSE)
  if(puc=="NA"&&chem=="NA"){
    return (comp.file)
  }else if (puc=="NA"&&!(chem=="NA")){
    comp.file <- comp.file[comp.file$DTXSID==chem]
    return(comp.file)
  }else if (chem=="NA"&&!(puc=="NA")){
    comp.file <- comp.file[comp.file$source.id==puc]
    return(comp.file)
  }else{
    comp.file <- comp.file[comp.file$source.id==puc]
    comp.file <- comp.file[comp.file$DTXSID==chem]
    return(comp.file)
  }
}

#variables to hold files

pop <- read.pophouse(cf$run.name)

HHno <- cf$last.house - cf$first.house + 1 #no of households in this run


#data frames to hold data: 
HH.data            <- data.frame("Info"=character(),"PrimaryHH"=integer(),"AllHH"=integer(),stringsAsFactors = FALSE)
chem.data          <- data.frame("DTXSID"=character(),"Chemical Name"=character(),"CAS"=integer(),stringsAsFactors = FALSE)
PUC.data           <- data.frame("PUC"=character(),"PUC Name"=character(),"PUC_U"=integer(),"PUC_not"=integer(),"P_O_PUC"=integer(),"NP_O_PUC"=integer(),"Code"=character(), stringsAsFactors = FALSE)
HP.data            <- data.frame("PUC"=character(),"Description"=character(),"E_P_M"=numeric(),"S_P_M"=numeric(),"E_P_F"=numeric(),"S_P_F"=numeric(),"E_P_Ch"=numeric(),"S_P_Ch"=numeric(),"E_Freq"=numeric(),"S_Freq"=numeric(),"E_Mass"=numeric(),"S_Mass"=numeric(), stringsAsFactors = FALSE)
QA.data            <- data.frame("Info"=character(),"Primary Only"=integer(),"EVERYBODY"=integer(),stringsAsFactors = FALSE)
HP_all.data        <- data.frame("PUC"=character(),"Description"=character(),"E_P_M"=numeric(),"A_P_M"=numeric(),"E_P_F"=numeric(),"A_P_F"=numeric(),"E_P_Ch"=numeric(),"A_P_Ch"=numeric(),"E_Freq"=numeric(),"A_Freq"=numeric(),"E_Mass"=numeric(),"A_Mass"=numeric(), stringsAsFactors = FALSE)
PC.data            <- data.frame("PUC"=character(),"Chemical"=character(),"num_PUC"=numeric(),"num_PUC_Chem"=numeric(),"Real_min"=numeric(),"Min_wf"=numeric(),"Real_max"=numeric(),"max_wf"=numeric(),"Real_frac_zero"=numeric(),"frac_zeros"=numeric(),stringsAsFactors = FALSE)
PT.data            <- data.frame("chemical"= character(),"HH"=integer(),"Dermal"=numeric(),"Inhalation"=numeric(),"Ingestion"=numeric(),stringsAsFactors = FALSE)
PD.data            <- data.frame("chemical"= character(),"HH"=integer(),"Dermal"=numeric(),"Inhalation"=numeric(),"Ingestion"=numeric(),stringsAsFactors = FALSE)

#Chemical summary table:
#accumulating chemical properties data and writing to data frame
for (i in 1:length(unlist(cf$chem.list))){
  ach <- unlist(cf$chem.list)[i]
  chemp <- read.chem.props(cf$chem.file,ach)
  chem.data[nrow(chem.data)+1,] <- c(ach,chemp$chemical,chemp$cas)
}


#Household summary table:

#finding genders included in the study
popsub <- pop[cf$first.house:cf$last.house, ]

#this is looking at genders of primary individuals only
if ('Male' %in% popsub$gender && 'Female' %in% popsub$gender){
  G_P <- "M and F"
}else if (('Male' %in% popsub$gender&&!"Female"%in%popsub$gender)){
  G_P <- "M only"
}else if (('Female' %in% popsub$gender&&!"Male"%in%popsub$gender)){
  G_P <- "F only"
}
#this is looking at genders of everyone
gender_id_M <- 0 
gender_id_F <- 0
gender_id_MF <- 0
for (i in 1:length(popsub$genders)){
  if (grepl("M",popsub$genders[i]) && grepl("F",popsub$genders[i])){
    gender_id_MF <- gender_id_MF + 1
    
  }else if (grepl("M",popsub$genders[i])&&!grepl("F",popsub$genders[i])){
    gender_id_M <- gender_id_M + 1
    #
  }else if (grepl("F",popsub$genders[i])&&!grepl("M",popsub$genders[i])){
    gender_id_F <- gender_id_F + 1
    #
  }
}
if (gender_id_MF>0){G_E <- "M and F"}
if (gender_id_MF==0 && gender_id_M==0 && gender_id_F>0) {G_E <- "F only"}
if (gender_id_MF==0 && gender_id_F==0 && gender_id_M>0) {G_E <- "M only"}


#Household summary, PUC summary and Habits and Practices data tables:

#accumulating household data, PUC data and Habits and Practices data simultaneously
#variables to hold ages for primary (P) and everyone (E)
max_age_P <- 0
min_age_P <- 100000 #a large number to help find min age
min_M_age_P <- 100000
max_M_age_P <- 0
min_F_age_P <- 100000
max_F_age_P <- 0
max_Ch_age_P <- 0
min_Ch_age_P <- 100000

max_age_E <- 0
min_age_E <- 100000
min_M_age_E <- 100000
max_M_age_E <- 0
min_F_age_E <- 100000
max_F_age_E <- 0
max_Ch_age_E <- 0
min_Ch_age_E <- 100000


for (a in 1:length(unlist(cf$puc.list))){#for each PUC included
  
  puc <- read.puc.types(cf$puc.type.file,unlist(cf$puc.list)[a])#read PUC file
  hp <- read.puc.use(od$hab.prac.file,unlist(cf$puc.list)[a])#read habits and prac file
  
  #variables to hold PUC data
  apuc <- unlist(cf$puc.list)[a]#this PUC
  non_PUC <- 0 #no. do not use PUC
  notP_O <- 0 #no. primaries who don't use it but others do
  use_PUC <- 0 #no. that use PUC
  P_O <- 0 #no. primaries that use it and others used it as well
  
  #variables to hold H&P data   
  pop_m12 <- 0.0 #counter for the population of males above 12 yrs
  pop_f12 <- 0.0 #counter for the population of females above 12 yrs
  pop_ch <- 0.0 #counter for the population of children (<=12)
  
  puc_user <- 0.0 #counter for puc users only
  
  x_puc <- 0.0 # counter for the number of times puc used
  
  mass_puc <- 0.0 #counter for sum of mass of puc used
  
  p_puc_m12 <- 0.0 # counter for prevalence of use of puc in m12 demographic
  p_puc_f12 <- 0.0 #counter for prevalence of use of puc in f12 demographic
  p_puc_ch <- 0.0 # counter for prevalence of use of puc in ch demographic
  
  
  
  for (hn in (cf$first.house):(cf$last.house)){ #for each household included in the run
    
    pd <- popsub[popsub$house==hn]
    pp <- list.persons(pd)
    
    abm <- read.diary(cf$diary.prefix,cf$run.name,hn,pp) #read data in this household
    
    #calculate ages for this household
    maxxx <- max(abm$age)
    max_age_E <- max(max_age_E,maxxx)#max age thus far
    minnn <- min(abm$age)
    min_age_E <- min(min_age_E,minnn)#min age thus far
    
    max_age_P <- max(popsub$age_years)
    min_age_P <- min(popsub$age_years)
    
    popsub_F  <- popsub[popsub$gender=="Female"]
    popsub_F  <- popsub_F[popsub_F$age_years>12]
    if (nrow(popsub_F)>0){
      max_F_age_P <- max(popsub_F$age_years)
      min_F_age_P <- min(popsub_F$age_years)
    }
    
    popsub_M  <- popsub[popsub$gender=="Male"]
    popsub_M  <- popsub_M[popsub_M$age_years>12]
    if(nrow(popsub_M)>0){
      max_M_age_P <- max(popsub_M$age_years)
      min_M_age_P <- min(popsub_M$age_years)
    }
    
    popsub_Ch <- popsub[popsub$age_years<=12]
    if (nrow(popsub_Ch)>0){
      max_Ch_age_P <- max(popsub_Ch$age_years)
      min_Ch_age_P <- min(popsub_Ch$age_years)
    }
    
    
    M_abm <- abm[abm$sex=="M"]
    M_abm <- M_abm[M_abm$age>12]
    
    max_M_age_E <- max(max_M_age_E, max(M_abm$age)) 
    min_M_age_E <- min(min_M_age_E, min(M_abm$age))
    
    F_abm <- abm[abm$sex=="F"]
    F_abm <- F_abm[F_abm$age>12]
    if(nrow(F_abm)>0){
      max_F_age_E <- max(max_F_age_E, max(F_abm$age))
      min_F_age_E <- min(min_F_age_E, min(F_abm$age))
    }
    
    Ch_abm <- abm[abm$age<=12]
    if(nrow(Ch_abm)>0){
      max_Ch_age_E <- max(max_Ch_age_E, max(Ch_abm$age))
      min_Ch_age_E <- min(min_Ch_age_E, min(Ch_abm$age))
    }
    
    
    if (!(apuc%in%abm$source.id)){
      non_PUC <- non_PUC+1 # no of households that are non-users of the PUCs in model run
    }else{
      use_PUC <- use_PUC+1#no of households that are users of the PUC
    }
    
    prim <- abm[abm$primary==1]
    other <- abm[abm$primary==0]
    
    
    if ((!is.element(apuc,prim$source.id)&&is.element(apuc,other$source.id))==TRUE){
      
      notP_O <- notP_O+1
    }else if ((is.element(apuc,prim$source.id)&&is.element(apuc,other$source.id))==TRUE){
      P_O <- P_O +1
    }
    
    #Calculate H&P data in this household
    HH <- read.diary(cf$diary.prefix,cf$run.name,hn,pp)#same as abm; keeping the variable becasue I merged two functions after writing them separately
    
    females <- HH[HH$sex=="F"]    # sort for females in HH
    fem <- females[females$age>12]# sort for females > 12 yrs
    pop_f12 <- pop_f12+length(unique(fem$p)) #the count 
    
    
    males <- HH[HH$sex=="M"]      # sort for males in HH
    mal <- males[males$age>12]    #sort for males in HH > 12 yrs
    pop_m12 <- pop_m12+length(unique(mal$p)) #the count
    
    
    children <- HH[HH$age<=12] #sort for children (<=12)
    pop_ch <- pop_ch+length(unique(children$p)) #the count
    
    uniq <- unique(HH$person) # each person in this HH
    
    for (u in uniq){#for each unique person
      HHsub <- HH[HH$person==u] #sort by this person
      if (apuc%in%HHsub$source.id){
        puc_user <- puc_user+1.0
        
        HHsub <- HHsub[HHsub$source.id==apuc] #sort by this puc
        x_puc <- x_puc + nrow(HHsub)
        mass_puc <- mass_puc+sum(as.numeric(HHsub$mass))
        
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
  }
  #write PUC data to data frame
  PUC.data[nrow(PUC.data)+1, ] <- c(apuc,hp$source_description,use_PUC,non_PUC,P_O,notP_O,puc$code)
  
  #calculate H&P values
  actual_freq_puc <- signif(x_puc/puc_user,2)
  actual_mass_puc <- signif(mass_puc/x_puc,2)
  
  prev_puc_m12 <- signif(p_puc_m12/pop_m12,2) #################
  prev_puc_f12 <- signif(p_puc_f12/pop_f12,2)
  prev_puc_ch  <- signif(p_puc_ch/pop_ch)
  
  
  #write H&P data to data frame
  HP.data[nrow(HP.data)+1, ] <- c(apuc,hp$source_description,hp$Prev_M,prev_puc_m12,hp$Prev_F,prev_puc_f12,hp$Prev_child,prev_puc_ch,hp$Freq,actual_freq_puc,hp$Mass,actual_mass_puc)
  
  #for QA: remove after using
  QA.data[nrow(QA.data)+1, ] <- c("population of M >12","",pop_m12)
  QA.data[nrow(QA.data)+1, ] <- c("population of F >12","",pop_f12)
  QA.data[nrow(QA.data)+1, ] <- c("population of M/F <=12","",pop_ch)
  
  QA.data[nrow(QA.data)+1, ] <- c("No of people who use the PUC","",puc_user)
  QA.data[nrow(QA.data)+1, ] <- c("No of times people use the PUC","",x_puc)
  QA.data[nrow(QA.data)+1, ] <- c("sum of mass of puc used","",mass_puc)
  
  
  QA.data[nrow(QA.data)+1, ] <- c("Prevalence of puc use in M >12","",p_puc_m12)
  QA.data[nrow(QA.data)+1, ] <- c("Prevalence of puc use in F >12","",p_puc_f12)
  QA.data[nrow(QA.data)+1, ] <- c("Prevalence of puc use in M/F <=12","",p_puc_ch)
  
  QA.data[nrow(QA.data)+1, ] <- c("End of this PUC","","")
  
}

#write household data to data frame
HH.data[nrow(HH.data)+1, ] <- c("No. of households",cf$last.house-cf$first.house+1,"NA")
HH.data[nrow(HH.data)+1, ] <- c("Min age",min_age_P,min_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Max age",max_age_P,max_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Max age - Min age",max_age_P-min_age_P,max_age_E-min_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Min male (>12yrs) age",min_M_age_P,min_M_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Max male (>12yrs) age",max_M_age_P,max_M_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Min female (>12yrs) age",min_F_age_P,min_F_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Max female (>12yrs) age",max_F_age_P,max_F_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Min child (<=12yrs) age",min_Ch_age_P,min_Ch_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Max child (<=12yrs) age",max_Ch_age_P,max_Ch_age_E)
HH.data[nrow(HH.data)+1, ] <- c("Gender",G_P,G_E)

#attempt to format decimal places
options(scipen=999)


#prod_chem table start
relevHHs <- list() #list of relevant HH numbers
prod_files <- list.files(path = paste0(od$s2d.output.folder.location,"Prod_chem"))#list of files in the Prod_chem folder
for (f in 1:length(prod_files)){
  
  #extract household number (n) from file name "Prod_chem_n.csv"
  y <- regexpr("csv",prod_files[f],fixed = TRUE)
  n <- as.numeric(substr(prod_files[f],11,y-2))
  
  if (n<=cf$last.house&&n>=cf$first.house){
    relevHHs <- c(relevHHs,n)
  }
}

for (PUC in unlist(cf$puc.list)){
  
  for (chemicalll in unlist(cf$chem.list)){
    
    cnttpuc <- 0 #counter for files that contain this puc
    HH_u_PUC <- 0 #counter for primaries that use this PUC
    
    HH_PUC_Chem <- 0#counter for HH in which the primary used this PUC and the PUC contained this chemical
    Max_wf <- 0 #maximum weight fraction
    Min_wf <- 1000 #minimum weight fraction
    real_Max_wf <- 0
    real_Min_wf <- 0
    num_zeros <- 0
    cnttchem <- 0
    num_zeros_HH <- 0
    
    for (q in relevHHs){
      prod_puc <- read.prod.chem(q,"NA",PUC)
      if (nrow(prod_puc)>0){
        HH_u_PUC <- HH_u_PUC + 1
      }
      
      prod_f <- read.prod.chem(q,chemicalll,PUC)
      prod_f_all <- read.prod.chem(q,"NA","NA")
      
      prod_f_all_PUC <- prod_f_all[prod_f_all$puc==PUC]
      cnttpuc <- cnttpuc + nrow(prod_f_all_PUC)
      
      prod_f_all_CHEM <- prod_f_all_PUC[prod_f_all_PUC$chemical==chemicalll]
      cnttchem <- cnttchem + nrow(prod_f_all_CHEM)
      
      
      
      
      pd <- popsub[popsub$house==q]
      pp <- list.persons(pd)
      
      abm <- read.diary(cf$diary.prefix,cf$run.name,q,pp) #read data in this household
      prim <- abm[abm$primary==1]
      
      
      if (nrow(prod_f)>0){
        HH_PUC_Chem <- HH_PUC_Chem+1
        
        Max_wf <- signif(max(Max_wf,max(prod_f$mass.fraction)),2)
        Min_wf <- signif(min(Min_wf,min(prod_f$mass.fraction)),2)
      }
    }
    
    num_zeros_HH <- num_zeros_HH + abs(cnttchem - cnttpuc)
    
    
    compfile <- read.comp.s2d(PUC,chemicalll)
    real_Max_wf <- signif(max(as.numeric(compfile$weight_fraction),na.rm = TRUE),2)
    real_Min_wf <- signif(min(as.numeric(compfile$weight_fraction),na.rm = TRUE),2)
    
    compfile_all <- read.comp.s2d("NA","NA")
    compfile_all_f <- compfile_all[compfile_all$formulation_id==0]
    
    compfile_all_f_puc <- (compfile_all_f[compfile_all_f$source.id==PUC])
    cntPUC <- nrow(compfile_all_f_puc)
    
    compfile_all_f_puc_chem <-(compfile_all_f_puc[compfile_all_f_puc$DTXSID==chemicalll])
    cntChem <- nrow(compfile_all_f_puc_chem)
    
    num_zeros <- as.numeric((cntPUC - cntChem))
    
    
    if (cntChem==0){
      PC.data[nrow(PC.data)+1, ] <- c(PUC,chemicalll,signif(HH_u_PUC,2),signif(HH_PUC_Chem,2),0,0,0,0,signif(num_zeros/cntPUC,2),signif(num_zeros_HH/cnttpuc))
    }else if (HH_PUC_Chem==0&&!(cntChem==0)){
      PC.data[nrow(PC.data)+1, ] <- c(PUC,chemicalll,signif(HH_u_PUC,2),signif(HH_PUC_Chem,2),signif(real_Min_wf,2),0,signif(real_Max_wf,2),0,signif(num_zeros/cntPUC,2),signif(num_zeros_HH/cnttpuc))
    }
    else{
      PC.data[nrow(PC.data)+1, ] <- c(PUC,chemicalll,signif(HH_u_PUC,2),signif(HH_PUC_Chem,2),signif(real_Min_wf,2),Min_wf,signif(real_Max_wf,2),Max_wf,signif(num_zeros/cntPUC,2),signif(num_zeros_HH/cnttpuc))
    }
  }
  
}
#----

```



```{r echo=FALSE, results='asis'}

#write data frames to file
#HH info
print(knitr::kable((HH.data),caption = "Table 1: Summary of Simulated Population"))
```


```{r echo=FALSE, results='asis'}

#chem info

print(knitr::kable(chem.data,caption = "Table 2: Chemical Summary"))
```



```{r echo=FALSE, results='asis'}

#PUC info
print(knitr::kable(PUC.data,caption = "Table 3: PUC Summary"))
```



```{r echo=FALSE, results='asis'}
#panderOptions('table.split.cells',10)
#options(width=12)
#options(scipen=999)
#HP.data <- t(HP.data)
#colnames(HP.data) <- NULL 
print(knitr::kable(HP.data,caption = "Table 4: Habits & Practices Summary (households in run)"))
#pander((HP.data),caption = "Table 4: Habits & Practices Summary (Households in run)")
#cat("\n\n\\pagebreak\n") #separating output
```


```{r echo=FALSE, results='asis'}

print(knitr::kable(PC.data,caption = "Table 5: Prod Chem table"))
#cat("\\newpage")
#pander(PC.data,caption = "Table 5: Prod Chem Summary")
```


```{r echo=FALSE, results='asis'}

#for QA purposes
#print(knitr::kable(QA.data,caption = "QA data"))
```

```{r echo=FALSE, results='asis'}
#Habits and practices table for all households:

#run for all 1000 households
if (od$run.all.households=="yes"){
  for (a in 1:length(unlist(cf$puc.list))){
    
    puc <- read.puc.types(cf$puc.type.file,unlist(cf$puc.list)[a])
    hp <- read.puc.use(od$hab.prac.file,unlist(cf$puc.list)[a])
    apuc <- unlist(cf$puc.list)[a]#this PUC
    
    pop_m12 <- 0.0 #counter for the population of males above 12 yrs
    pop_f12 <- 0.0 #counter for the population of females above 12 yrs
    pop_ch <- 0.0 #counter for the population of children (<=12)
    
    puc_user <- 0.0 #counter for puc users only
    
    x_puc <- 0.0 # counter for the number of times puc used
    
    mass_puc <- 0.0 #counter for sum of mass of puc used
    
    p_puc_m12 <- 0.0 # counter for prevalence of use of puc in m12 demographic
    p_puc_f12 <- 0.0 #counter for prevalence of use of puc in f12 demographic
    p_puc_ch <- 0.0 # counter for prevalence of use of puc in ch demographic
    
    for (hn in 1:1000){
      pd <- pop[hn,]
      pp <- list.persons(pd)
      
      HH <- read.diary(cf$diary.prefix,cf$run.name,hn,pp)
      
      females <- HH[HH$sex=="F"]    # sort for females in HH
      fem <- females[females$age>12]# sort for females > 12 yrs
      pop_f12 <- pop_f12+length(unique(fem$p)) #the count 
      
      
      males <- HH[HH$sex=="M"]      # sort for males in HH
      mal <- males[males$age>12]    #sort for males in HH > 12 yrs
      pop_m12 <- pop_m12+length(unique(mal$p)) ##############the count
      
      
      children <- HH[HH$age<=12] #sort for children (<=12)
      pop_ch <- pop_ch+length(unique(children$p)) #the count
      
      uniq <- unique(HH$person) # each person in this HH
      
      for (u in uniq){
        HHsub <- HH[HH$person==u] #sort by this person
        if (apuc%in%HHsub$source.id){
          puc_user <- puc_user+1.0
          
          HHsub <- HHsub[HHsub$source.id==apuc] #sort by this puc
          x_puc <- x_puc + nrow(HHsub)
          mass_puc <- mass_puc+sum(as.numeric(HHsub$mass))
          
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
      
    }
    
    actual_freq_puc <- x_puc/puc_user
    actual_mass_puc <- mass_puc/x_puc
    
    prev_puc_m12 <- p_puc_m12/pop_m12
    prev_puc_f12 <- p_puc_f12/pop_f12
    prev_puc_ch <- p_puc_ch/pop_ch
    
    HP_all.data[nrow(HP_all.data)+1, ] <- c(apuc,hp$source_description,hp$Prev_M,prev_puc_m12,hp$Prev_F,prev_puc_f12,hp$Prev_child,prev_puc_ch,hp$Freq,actual_freq_puc,hp$Mass,actual_mass_puc)
  }
  write.xlsx((HP_all.data),file=paste0(wd,"/hp_all_data.xlsx"),row.names = FALSE,col.names = TRUE)
  #print(knitr::kable(HP_all.data))
}
```





##Optional Summary Tables
```{r echo=FALSE, results='asis', fig.align="center"}
tab <- 5 #count for table number

#Optional data start

#For each chemical in S2D run:
for (a in 1:length(unlist(cf$chem.list))){ 
  
  #Data frame to hold data
  OPT.data <- data.frame("Rows"=character(),"Age group"=character(),"AvgMean AllPop"=integer(),"AvgMean Users"=integer(),"AvgMaxDaily"=integer(),stringsAsFactors = FALSE)
  
  achem <- unlist(cf$chem.list)[a]
  
  #counters
  
  HH_c_use <- 0 #count for the number of houses that use a chemical.
  
  #total absorbed dose
  all_tot_ad <- 0
  max_tot_ad <- 0
  
  #dermal absorbed dose total
  all_der_tot <- 0
  max_der_tot <- 0
  
  #dermal absorbed dorse direct
  all_der_dir <- 0
  max_der_dir <- 0
  
  #dermal absorbed dose indirect
  all_der_ind <- 0
  max_der_ind <- 0
  
  #inhalation absorbed dose total
  all_inh_tot <- 0
  max_inh_tot <- 0
  
  #inhalation absorbed dose direct
  all_inh_dir <- 0
  max_inh_dir <- 0
  
  #inhalation absorbed dose indirect
  all_inh_ind <- 0
  max_inh_ind <- 0
  
  #ingestion absorbed dose total
  all_ing_tot <- 0
  max_ing_tot <- 0
  
  #ingestion absorbed dose direct
  all_ing_dir <- 0
  max_ing_dir <- 0
  
  #ingestion absorbed dose indirect
  all_ing_ind <- 0
  max_ing_ind <- 0
  
  #mass down the drain
  all_mass_drain <- 0
  max_mass_drain <- 0
  
  #mass out the window
  all_mass_window <- 0
  max_mass_window <- 0
  
  #mass in solid waste
  all_mass_waste <- 0
  max_mass_waste <- 0
  
  #list of relevant households
  relevHH <- list()
  
  lf <- list.files(path = paste0(od$s2d.output.folder.location,"Annual"))#list of files in the annual S2D folder
  for (i in 1:length(lf)){
    
    #extract the household number (n) from "House_n.csv"
    y <- regexpr("csv",lf[i],fixed = TRUE)
    n <- as.numeric(substr(lf[i],7,y-2))
    
    if (n<=cf$last.house&&n>=cf$first.house){#if n is within boundaries of the households run
      
      #if interested in only adults, note the adult HH
      if (od$age.groups.of.interest=="adult"){
        
        if (pop$age_years[n]>12){
          relevHH <- c(relevHH,n)
        }
      }  
      
      #if interested in only children, note the child HH
      if (od$age.groups.of.interest=="child"){
        
        if (pop$age_years[n]<=12){
          relevHH <- c(relevHH,n)
        }
      }
      
      #if interested in all HH, note all HH
      if (od$age.groups.of.interest=="no"||od$age.groups.of.interest=="both"){
        relevHH <- c(relevHH,n)
      }
    }
  }
  #for each HH in the relevant HH list
  for (p in relevHH){
    chem_annual <- read.s2d.annual(p,"NA") #read the S2D annual output for this household
    
    
    
    if ((achem%in%chem_annual$dtxsid)){#counting the number of households that used this chemical
      achem_annual <- chem_annual[chem_annual$dtxsid==achem]
      if (achem_annual$total.used > 0){
        HH_c_use<-HH_c_use+1
      }
    }     
    
    house.daily <- read.s2d.daily(p,achem)
    
    if (achem%in%unique(chem_annual$dtxsid)){
      chem_annual <- read.s2d.annual(p,achem)
      
      #total absorbed dose
      all_tot_ad <- signif((all_tot_ad + chem_annual$dir.derm.abs + chem_annual$dir.inhal.abs + chem_annual$dir.ingest.abs + chem_annual$ind.derm.abs + chem_annual$ind.inhal.abs + chem_annual$ind.ingest.abs),2)
      max_tot_ad <- signif((max_tot_ad + max(house.daily$dir.derm.abs) + max(house.daily$dir.inhal.abs) + max(house.daily$dir.ingest.abs) + max(house.daily$ind.derm.abs) + max(house.daily$ind.inhal.abs) + max(house.daily$ind.ingest.abs)),2)
      
      #dermal absorbed dose total
      all_der_tot <- signif((all_der_tot + chem_annual$dir.derm.abs + chem_annual$ind.derm.abs),2)
      max_der_tot <- signif((max_der_tot + max(house.daily$dir.derm.abs) + max(house.daily$ind.derm.abs)),2)
      
      #dermal absorbed dorse direct
      all_der_dir <- signif((all_der_dir + chem_annual$dir.derm.abs),2)
      max_der_dir <- signif((max_der_dir + max(house.daily$dir.derm.abs)),2)
      
      #dermal absorbed dose indirect
      all_der_ind <- signif((all_der_ind + chem_annual$ind.derm.abs),2)
      max_der_ind <- signif((max_der_ind + max(house.daily$ind.derm.abs)),2)
      
      #inhalation absorbed dose total
      all_inh_tot <- signif((all_inh_tot + chem_annual$dir.inhal.abs + chem_annual$ind.inhal.abs),2)
      max_inh_tot <- signif((max_inh_tot + max(house.daily$dir.inhal.abs) + max(house.daily$ind.inhal.abs)),2)
      
      #inhalation absorbed dose direct
      all_inh_dir <- signif((all_inh_dir + chem_annual$dir.inhal.abs),2)
      max_inh_dir <- signif((max_inh_dir + max(house.daily$dir.inhal.abs)),2)
      
      #inhalation absorbed dose indirect
      all_inh_ind <- signif((all_inh_ind + chem_annual$ind.inhal.abs),2)
      max_inh_ind <- signif((max_inh_ind + max(house.daily$ind.inhal.abs)),2)
      
      #ingestion absorbed dose total
      all_ing_tot <- signif((all_ing_tot + chem_annual$dir.ingest.abs + chem_annual$ind.ingest.abs),2)
      max_ing_tot <- signif((max_ing_tot + max(house.daily$dir.ingest.abs + max(house.daily$ind.ingest.abs))),2)
      
      #ingestion absorbed dose direct
      all_ing_dir <- signif((all_ing_dir + chem_annual$dir.ingest.abs),2)
      max_ing_dir <- signif((max_ing_dir + max(house.daily$dir.ingest.abs)),2)
      
      #ingestion absorbed dose indirect
      all_ing_ind <- signif((all_ing_ind + chem_annual$ind.ingest.abs),2)
      max_ing_ind <- signif((max_ing_ind + max(house.daily$ind.ingest.abs)),2)
      
      #mass down the drain
      all_mass_drain <- signif((all_mass_drain + chem_annual$drain),2)
      max_mass_drain <- signif((max_mass_drain + max(house.daily$drain)),2)
      
      #mass out the window
      all_mass_window <- signif((all_mass_window + chem_annual$out.air),2)
      max_mass_window <- signif((max_mass_window + max(house.daily$out.air)),2)
      
      #mass in solid waste
      all_mass_waste <- signif((all_mass_waste + chem_annual$waste),2)
      max_mass_waste <- signif((max_mass_waste + max(house.daily$waste)),2)
      
    }
    #write to annual plot data frame
    testing <- read.s2d.annual(p,"NA")
    
    if (!(achem%in%unique(testing$dtxsid))){
      
      PT.data[nrow(PT.data)+1, ] <- c(achem,p,0,0,0)
      
      
    }else{
      
      PT.data[nrow(PT.data)+1, ] <- c(achem,p,signif((chem_annual$dir.derm.abs + chem_annual$ind.derm.abs),2),signif((chem_annual$dir.inhal.abs + chem_annual$ind.inhal.abs),2), signif((chem_annual$dir.ingest.abs + chem_annual$ind.ingest.abs),2))
    }
    
    #read daily files
    daily.file <- read.s2d.daily(p,achem)
    
    #write to max daily plot data frame
    if (nrow(daily.file)==0){
      PD.data[nrow(PD.data)+1, ] <- c(achem,p,0,0,0)
    }else{
      
      max_der_tot_abs <- signif((max(daily.file$dir.derm.abs) + max(daily.file$ind.derm.abs)),2)
      max_inh_tot_abs <- signif((max(daily.file$dir.inhal.abs) + max(daily.file$ind.inhal.abs)),2)
      max_ing_tot_abs <- signif((max(daily.file$dir.ingest.abs) + max(daily.file$ind.ingest.abs)),2)
      
      PD.data[nrow(PD.data)+1, ] <- c(achem,p,max_der_tot_abs,max_inh_tot_abs,max_ing_tot_abs)
    }
    
    
    
    
  }
  
  #Store data in data frame
  OPT.data[nrow(OPT.data)+1, ] <- c("Total.abs.dose",od$age.groups.of.interest,signif(all_tot_ad/HHno,2),signif(all_tot_ad/HH_c_use,2),signif(max_tot_ad/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Dermal.abs.dose.tot",od$age.groups.of.interest,signif(all_der_tot/HHno,2),signif(all_der_tot/HH_c_use,2),signif(max_der_tot/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Dermal.abs.dose.dir",od$age.groups.of.interest,signif(all_der_dir/HHno,2),signif(all_der_dir/HH_c_use,2),signif(max_der_dir/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Dermal.abs.dose.ind",od$age.groups.of.interest,signif(all_der_ind/HHno,2),signif(all_der_ind/HH_c_use,2),signif(max_der_ind/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Inhalation.abs.dose.tot",od$age.groups.of.interest,signif(all_inh_tot/HHno,2),signif(all_inh_tot/HH_c_use,2),signif(max_inh_tot/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Inhalation.abs.dose.dir",od$age.groups.of.interest,signif(all_inh_dir/HHno,2),signif(all_inh_dir/HH_c_use,2),signif(max_inh_dir/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Inhalation.abs.dose.ind",od$age.groups.of.interest,signif(all_inh_ind/HHno,2),signif(all_inh_ind/HH_c_use,2),signif(max_inh_ind/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Ingestion.abs.dose.tot",od$age.groups.of.interest,signif(all_ing_tot/HHno,2),signif(all_ing_tot/HH_c_use,2),signif(max_ing_tot/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Ingestion.abs.dose.dir",od$age.groups.of.interest,signif(all_ing_dir/HHno,2),signif(all_ing_dir/HH_c_use,2),signif(max_ing_dir/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Ingestion.abs.dose.ind",od$age.groups.of.interest,signif(all_ing_ind/HHno,2),signif(all_ing_ind/HH_c_use,2),signif(max_ing_ind/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Mass.down.drain",od$age.groups.of.interest,signif(all_mass_drain/HHno,2),signif(all_mass_drain/HH_c_use,2),signif(max_mass_drain/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Mass.out.window",od$age.groups.of.interest,signif(all_mass_window/HHno,2),signif(all_mass_window/HH_c_use,2),signif(max_mass_window/HHno,2))
  OPT.data[nrow(OPT.data)+1, ] <- c("Mass.solid.waste",od$age.groups.of.interest,signif(all_mass_waste/HHno,2),signif(all_mass_waste/HH_c_use,2),signif(max_mass_waste/HHno,2))
  
  
  OPT.data <- as.data.table(OPT.data)
  
  OPT_out.data <- data.frame("NA")#creating a new data frame; will collect user-specified columns into this data frame.
  
  OPT_out.data <- cbind(OPT_out.data,OPT.data[ , 1]) #bind the first column containing labels.
  
  
  #possible columns:
  
  #age col
  if (od$age.groups.of.interest=="both"||od$age.groups.of.interest=="adult"||od$age.groups.of.interest=="child"){
    #OPT_out.data <- cbind(OPT_out.data,OPT.data[ , 2]) dropping this column
  }
  
  #tot col
  if (od$population.average.of.annual.mean.for.all.population=="yes"){
    OPT_out.data <- cbind(OPT_out.data,OPT.data[ , 3])
  }
  
  #user col
  if (od$population.average.of.annual.mean.for.users.only=="yes"){
    OPT_out.data <- cbind(OPT_out.data,OPT.data[ , 4])
  }
  
  #max col
  if (od$population.average.of.maximum.daily.dose=="yes"){
    OPT_out.data <- cbind(OPT_out.data,OPT.data[ , 5])
  }
  
  #deleting dummy column
  OPT_out.data$X.NA. <- NULL
  
  #possible rows
  m <- matrix(0, ncol= length(colnames(OPT_out.data)), nrow= 1)
  
  OPT_f_out.data <- data.frame(m) #creating a new data frame; will collect user-specified rows into this data frame (together with interested columns from OPT_out.data)). This is the final data frame of interest.
  
  colnames(OPT_f_out.data) <- colnames(OPT_out.data)
  
  
  if(od$total.absorbed.dose=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[1, ]
  }
  if (od$dermal.absorbed.dose.total=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[2, ]
  }
  if (od$dermal.absorbed.dose.direct=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[3, ]
  }
  if (od$dermal.absorbed.dose.indirect=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[4, ]
  }
  if (od$inhalation.absorbed.dose.total=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[5, ]
  }
  if (od$inhalation.absorbed.dose.direct=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[6, ]
  }
  if (od$inhalation.absorbed.dose.indirect=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[7, ]
  }
  if (od$ingestion.absorbed.dose.total=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[8, ]
  }
  if (od$ingestion.absorbed.dose.direct=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[9, ]
  }
  if (od$ingestion.absorbed.dose.indirect=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[10, ]
  }
  if (od$mass.down.the.drain=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[11, ]
  }
  if (od$mass.out.the.window=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[12, ]
  }
  if (od$mass.in.solid.waste=="yes"){
    OPT_f_out.data[nrow(OPT_f_out.data)+1, ] <- OPT_out.data[13, ]
  }
  #delete dummy data
  OPT_f_out.data <- OPT_f_out.data[-c(1),]
  
  cat("\n") #separating output
  
  chempp <- read.chem.props(cf$chem.file,achem)
  
  print(knitr::kable(OPT_f_out.data, row.names = FALSE,caption = paste0("Table ",tab+1,": ","Summary Table for ",chempp$chemical," (Age Group: ",od$age.groups.of.interest,")")))#, digits=c(2,2,2,2,2),col.names = c("a","b","d","e","f")))  #format decinmal points
  tab <- tab+1
  cat("\n") #separating output
  
  if (od$output.plots=="yes"){
    if (achem%in%PT.data$chemical){
      PTC.data <- PT.data[PT.data$chemical==achem,] 
      
      xlow1 <- 0 #lowest x axis limit
      xhi1  <-  as.numeric(max(as.numeric(PTC.data$Ingestion),as.numeric(PTC.data$Inhalation),as.numeric(PTC.data$Dermal)))#highest x axis limit  
      #curve(pnorm(OPT_f_out.data$AvgMean.AllPop))
      #print(knitr::kable(PTC.data,caption = "QA table for annual exp plot"))
      cat("\n") #separating output
      plot(ecdf(as.numeric(PTC.data$Ingestion)),col="red",ylab = "Cumulative Proportion",xlab = "Annual average by route",main=paste0(("Annual average chemical exposure by route for "),chempp$chemical),verticals = TRUE,xlim = c(xlow1,xhi1))
      lines(ecdf(as.numeric(PTC.data$Inhalation)),col="blue",verticals=TRUE)#,xlim=c(0,max(PTC.data)))
      lines(ecdf(as.numeric(PTC.data$Dermal)),col="green",verticals=TRUE)#,xlim=c(0,max(PTC.data)))
      legend('bottomright',legend = c("Ingestion","Inhalation","Dermal"),col = c("red","blue","green"),pch = 15)
      cat("\n") #separating output
      
      PDC.data <- PD.data[PD.data$chemical==achem,] 
      xlow2 <- 0 #lowest x axis limit
      xhi2  <-  as.numeric(max(as.numeric(PDC.data$Ingestion),as.numeric(PDC.data$Inhalation),as.numeric(PDC.data$Dermal)))#highest x axis limit  
      #print(knitr::kable(PDC.data,caption = "QA table for max daily plot"))
      cat("\n") #separating output
      plot(ecdf(as.numeric(PDC.data$Ingestion)),col="red",ylab = "Cumulative Proportion",xlab = "Maximum daily by route",main=paste0(("Maximum daily chemical exposure by route for "),chempp$chemical),verticals = TRUE,xlim=c(xlow2,xhi2))
      lines(ecdf(as.numeric(PDC.data$Inhalation)),col="blue",verticals=TRUE)#,xlim=c(0,max(PDC.data)))
      lines(ecdf(as.numeric(PDC.data$Dermal)),col="green",verticals=TRUE)#,xlim=c(0,max(PDC.data)))
      legend('bottomright',legend = c("Ingestion","Inhalation","Dermal"),col = c("red","blue","green"),pch = 15)
      cat("\\newpage") #separating output
    }
  }
  
  
}

```

