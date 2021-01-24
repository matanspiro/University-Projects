
##----------------------------------------- 1.  all functions ------------------------------------------------

kupahFunction <- function (district){ # a function that gets the district of the tested person and returns a vector of the Kuppot Holim stations
  if (district=="Center") {
    return (c("KupahCenter1","KupahCenter2","KupahCenter3","KupahCenter4","KupahCenter5","KupahCenter6","KupahCenter7"))
  }
  else if (district=="North") {
    return (c("KupahNorth1","KupahNorth2","KupahNorth3","KupahNorth4"))
  }
  else if (district=="South") {
    return (c("KupahSouth1","KupahSouth2","KupahSouth3","KupahSouth4"))
  }
}

DIFunction <- function (district){ #gets teh tested person's district and returns a vector of the Drive In Stations
  if (district=="Center") {
    return (c("DICenter1","DICenter2","DICenter3","DICenter4"))
  }
  else if (district=="North") {
    return (c("DINorth1","DINorth2","DINorth3"))
  }
  else {
    return (c("DISouth1","DISouth2","DISouth3"))
  }
}

Ambulances <- function (){  # returns a vector of all the ambulance resources
  return(c("Ambulance1","Ambulance2","Ambulance3","Ambulance4","Ambulance5","Ambulance6","Ambulance7","Ambulance8","Ambulance9","Ambulance10"
           ,"Ambulance11","Ambulance12","Ambulance13","Ambulance14","Ambulance15","Ambulance16","Ambulance17","Ambulance18","Ambulance19","Ambulance20"))
}

#trimmed norm
trimmedNorm<-function(mu,sd){ #need only positive output
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

districtAttribute<-function(district){ #determine district by probability to each test
  if(district<=0.23){
    return ("South")
  }
  else if(district>0.23 && district<=0.58){
    return ("North")
  }
  else {
    return ("Center")
  }
}

addService<- function  (path,sname,time){ 
  updatedPath <- seize(path, sname)%>%
    timeout(time) %>%
    release(sname)
  return(updatedPath)
}

SplitAmbulances <- function(path){ # branch the tests to a different ambulance randomly
  split1 <- 
    branch(path,option = function() rdiscrete(1,rep(0.05,20),seq(1, 20, by = 1)),continue=rep(F,20),TrajAmbulance1,TrajAmbulance2,TrajAmbulance3,TrajAmbulance4,TrajAmbulance5,TrajAmbulance6,TrajAmbulance7,TrajAmbulance8,TrajAmbulance9,TrajAmbulance10,TrajAmbulance11,TrajAmbulance12,TrajAmbulance13,TrajAmbulance14,TrajAmbulance15,TrajAmbulance16,TrajAmbulance17,TrajAmbulance18,TrajAmbulance19,TrajAmbulance20)
  return(split1)
}

MDAtrajectoryFunction <- function(path,resource){ # a repeated trajectory in order to shorten the code.
  traj <- 
    seize(path,resource,1)%>%
    timeout(function() rtriangle(1,10,20,15))%>% #time to arrive
    timeout(1.5)%>% #testing time
    set_attribute("TestedTime",value =1,mod="+")%>%
    release(resource,1)%>%
    batch(Inf,timeout=function () {get_global(SimuCorona,"timeForBatch")-now(SimuCorona)})%>% #separated batch to each ambulance
    set_capacity(resource,0)%>%
    timeout(trimmedNorm(15,5))%>% #Driving to the lab
    timeout(10)%>% #transferring tests to the lab
    timeout(10)%>% #restocking the ambulance
    set_capacity(resource,1)%>%
    branch(function() rdiscrete(1,c(0.23,0.35,0.42),c(1,2,3)),c(F,F,F),TrajLabSouth,TrajLabNorth,TrajLabCenter)
  return(traj)
}

DIcapacity2 <- function(path){  #change the Drive In stations from 3 to 2.
  cap2 <- 
    set_capacity(path,"DINorth1",2)%>%set_capacity("DINorth2",2)%>%set_capacity("DINorth3",2)%>%
    set_capacity("DISouth1",2)%>%set_capacity("DISouth2",2)%>%set_capacity("DISouth3",2)%>%
    set_capacity("DICenter1",2)%>%set_capacity("DICenter2",2)%>%set_capacity("DICenter3",2)%>%set_capacity("DICenter4",2)
  return(cap2)
}

DIcapacity3 <- function(path){ #change the Drive In stations from 2 to 3.
  cap3 <- 
    set_capacity(path,"DINorth1",3)%>%set_capacity("DINorth2",3)%>%set_capacity("DINorth3",3)%>%
    set_capacity("DISouth1",3)%>%set_capacity("DISouth2",3)%>%set_capacity("DISouth3",3)%>%
    set_capacity("DICenter1",3)%>%set_capacity("DICenter2",3)%>%set_capacity("DICenter3",3)%>%set_capacity("DICenter4",3)
  return(cap3)
}


numToDistrict<-function(i){ #changes the number of attribute to a String for each attribute of district 
  #1-South, 2-North, 3-Center
  
  if(i==1){
    return("South")
  }
  else if(i==2){
    return("North")
  }
  else{
    return("Center")
  }
}

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime <- 48*60

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

SimuCorona<- simmer("SimuCorona")%>%
  add_resource("DINorth1",capacity=3,queue_size=Inf)%>%
  add_resource("DINorth2",capacity=3,queue_size=Inf)%>%
  add_resource("DINorth3",capacity=3,queue_size=Inf)%>%
  
  add_resource("DISouth1",capacity=3,queue_size=Inf)%>%
  add_resource("DISouth2",capacity=3,queue_size=Inf)%>%
  add_resource("DISouth3",capacity=3,queue_size=Inf)%>%
  
  add_resource("DICenter1",capacity=3,queue_size=Inf)%>%
  add_resource("DICenter2",capacity=3,queue_size=Inf)%>%
  add_resource("DICenter3",capacity=3,queue_size=Inf)%>%
  add_resource("DICenter4",capacity=3,queue_size=Inf)%>%
  
  add_resource("KupahSouth1",capacity=1,queue_size=Inf)%>%
  add_resource("KupahSouth2",capacity=1,queue_size=Inf)%>%
  add_resource("KupahSouth3",capacity=1,queue_size=Inf)%>%
  add_resource("KupahSouth4",capacity=1,queue_size=Inf)%>%
  
  add_resource("KupahNorth1",capacity=1,queue_size=Inf)%>%
  add_resource("KupahNorth2",capacity=1,queue_size=Inf)%>%
  add_resource("KupahNorth3",capacity=1,queue_size=Inf)%>%
  add_resource("KupahNorth4",capacity=1,queue_size=Inf)%>%
  
  add_resource("KupahCenter1",capacity=1,queue_size=Inf)%>%
  add_resource("KupahCenter2",capacity=1,queue_size=Inf)%>%
  add_resource("KupahCenter3",capacity=1,queue_size=Inf)%>%
  add_resource("KupahCenter4",capacity=1,queue_size=Inf)%>%
  add_resource("KupahCenter5",capacity=1,queue_size=Inf)%>%
  add_resource("KupahCenter6",capacity=1,queue_size=Inf)%>%
  add_resource("KupahCenter7",capacity=1,queue_size=Inf)%>%
  
  add_resource("LabNorth",capacity=20,queue_size=Inf,preemptive=F,preempt_order="fifo")%>%
  add_resource("LabSouth",capacity=20,queue_size=Inf,preemptive=F,preempt_order="fifo")%>%
  add_resource("LabCenter",capacity=20,queue_size=Inf,preemptive=F,preempt_order="fifo")%>%
  
  add_resource("Ambulance1",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance2",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance3",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance4",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance5",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance6",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance7",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance8",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance9",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance10",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance11",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance12",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance13",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance14",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance15",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance16",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance17",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance18",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance19",capacity=1,queue_size=Inf,preemptive=T)%>%
  add_resource("Ambulance20",capacity=1,queue_size=Inf,preemptive=T)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

ChangeDIworkstationCAP <- trajectory("ChangeDIworkstationCAP")%>% #every day at 18:00 the Drive Ins are closing one of their stations
  timeout(18*60)%>%
  DIcapacity2()%>%
  timeout(11*60)%>%
  DIcapacity3()

KupahSchedule <- trajectory("KupahSchedule")%>% #every day the Kupot Holim are closing their gates for one hour at 13:00
  deactivate("KupahTest")%>%
  timeout(540)%>%
  activate("KupahTest")%>%
  timeout(240)%>%
  deactivate("KupahTest")%>%
  timeout(60)%>%
  activate("KupahTest")%>%
  timeout(240)%>%
  deactivate("KupahTest")

batchScheduleTrj <- trajectory("batchScheduleTrj") %>%
  set_global(key="timeForBatch",value=(function () now(SimuCorona)+4*60))

DayScheduleTraj <- trajectory("DayScheduleTraj")%>%
  set_global(key="endOfDay",value=(function () now(SimuCorona)+24*60))

SendAmbulancesToLabs <- trajectory("SendAmbulancesToLabs")%>% # If an ambulance was in the middle of a test and it's time to leave to the lab, then the ambulance will stop the test, drive to lab and go back to do the same test after that.
  seize("Ambulance1",1)%>%
  seize("Ambulance2",1)%>%
  seize("Ambulance3",1)%>%
  seize("Ambulance4",1)%>%
  seize("Ambulance5",1)%>%
  seize("Ambulance6",1)%>%
  seize("Ambulance7",1)%>%
  seize("Ambulance8",1)%>%
  seize("Ambulance9",1)%>%
  seize("Ambulance10",1)%>%
  seize("Ambulance11",1)%>%
  seize("Ambulance12",1)%>%
  seize("Ambulance13",1)%>%
  seize("Ambulance14",1)%>%
  seize("Ambulance15",1)%>%
  seize("Ambulance16",1)%>%
  seize("Ambulance17",1)%>%
  seize("Ambulance18",1)%>%
  seize("Ambulance19",1)%>%
  seize("Ambulance20",1)%>%
  release("Ambulance1",1)%>%
  release("Ambulance2",1)%>%
  release("Ambulance3",1)%>%
  release("Ambulance4",1)%>%
  release("Ambulance5",1)%>%
  release("Ambulance6",1)%>%
  release("Ambulance7",1)%>%
  release("Ambulance8",1)%>%
  release("Ambulance9",1)%>%
  release("Ambulance10",1)%>%
  release("Ambulance11",1)%>%
  release("Ambulance12",1)%>%
  release("Ambulance13",1)%>%
  release("Ambulance14",1)%>%
  release("Ambulance15",1)%>%
  release("Ambulance16",1)%>%
  release("Ambulance17",1)%>%
  release("Ambulance18",1)%>%
  release("Ambulance19",1)%>%
  release("Ambulance20",1)

mainMDArep <- trajectory("mainMDArep")%>%
  rollback(21,1)

mainKupahrep <- trajectory("mainKupahrep")%>%
  timeout(function() get_global(SimuCorona,"endOfDay")-now(SimuCorona)+540)%>%
  rollback(15,1)

mainDIrep <- trajectory("mainDIrep")%>%
  timeout(function() get_global(SimuCorona,"endOfDay")-now(SimuCorona)+360)%>%
  rollback(18,1)

Generate2DInight <- trajectory("Generate2DInight")%>%
  timeout(function() get_global(SimuCorona,"endOfDay")-now(SimuCorona)+360)%>%
  deactivate("Exposed_To_Positive")%>%
  activate("Exposed_To_Positive")

Generate2DI <- trajectory("Generate2DI")%>%
  deactivate("Exposed_To_Positive")%>%
  activate("Exposed_To_Positive")

Split <- trajectory("Split")%>% 
  set_attribute(key="times",value=2)%>% 
  branch(function() get_attribute(SimuCorona,"TypeOfTest"),c(F,F,F),mainDIrep,mainKupahrep,mainMDArep)

TrajSick <- trajectory("TrajSick")%>%
  set_attribute(key="Result",value=0)%>%
  branch(function() ifelse(get_global(SimuCorona,"endOfDay")-now(SimuCorona)<=180 || get_global(SimuCorona,"endOfDay")-now(SimuCorona)>=1080,1,2),c(F,F),Generate2DInight,Generate2DI)

TrajHealthy <- trajectory("TrajHealthy")%>%
  set_attribute(key="Result",value=1)

TrajInconclusive <- trajectory("TrajInconclusive")%>% #checks if the test has been already inconclusive or if its the first time.
  set_attribute(key="Result",value=2)%>%
  branch(function() ifelse(get_attribute(SimuCorona,"times")==2,1,2),c(F,F),TrajSick,Split)

TrajLabSouth <- trajectory("TrajLabSouth")%>%
  separate()%>% #tests from Drive Ins and Ambulance are being batched and separated here.
  addService("LabSouth",5)%>%
  branch(function() rdiscrete(1,c(0.75,0.15,0.1),c(1,2,3)),c(F,F,F),TrajHealthy,TrajInconclusive,TrajSick)

TrajLabNorth <- trajectory("TrajLabNorth")%>%
  separate()%>% #tests from Drive Ins and Ambulance are being batched and separated here.
  addService("LabNorth",5)%>%
  branch(function() rdiscrete(1,c(0.75,0.15,0.1),c(1,2,3)),c(F,F,F),TrajHealthy,TrajInconclusive,TrajSick)

TrajLabCenter <- trajectory("TrajLabCenter")%>%
  separate()%>% #tests from Drive Ins and Ambulance are being batched and separated here.
  addService("LabCenter",5)%>%
  branch(function() rdiscrete(1,c(0.75,0.15,0.1),c(1,2,3)),c(F,F,F),TrajHealthy,TrajInconclusive,TrajSick)

TrajAmbulance1 <- trajectory("TrajAmbulance1")%>%
  MDAtrajectoryFunction("Ambulance1")

TrajAmbulance2 <- trajectory("TrajAmbulance2")%>%
  MDAtrajectoryFunction("Ambulance2")

TrajAmbulance3 <- trajectory("TrajAmbulance3")%>%
  MDAtrajectoryFunction("Ambulance3")

TrajAmbulance4 <- trajectory("TrajAmbulance4")%>%
  MDAtrajectoryFunction("Ambulance4")

TrajAmbulance5 <- trajectory("TrajAmbulance5")%>%
  MDAtrajectoryFunction("Ambulance5")

TrajAmbulance6 <- trajectory("TrajAmbulance6")%>%
  MDAtrajectoryFunction("Ambulance6")

TrajAmbulance7 <- trajectory("TrajAmbulance7")%>%
  MDAtrajectoryFunction("Ambulance7")

TrajAmbulance8 <- trajectory("TrajAmbulance8")%>%
  MDAtrajectoryFunction("Ambulance8")

TrajAmbulance9 <- trajectory("TrajAmbulance9")%>%
  MDAtrajectoryFunction("Ambulance9")

TrajAmbulance10 <- trajectory("TrajAmbulance10")%>%
  MDAtrajectoryFunction("Ambulance10")

TrajAmbulance11 <- trajectory("TrajAmbulance11")%>%
  MDAtrajectoryFunction("Ambulance11")

TrajAmbulance12 <- trajectory("TrajAmbulance12")%>%
  MDAtrajectoryFunction("Ambulance12")

TrajAmbulance13 <- trajectory("TrajAmbulance13")%>%
  MDAtrajectoryFunction("Ambulance13")

TrajAmbulance14 <- trajectory("TrajAmbulance14")%>%
  MDAtrajectoryFunction("Ambulance14")

TrajAmbulance15 <- trajectory("TrajAmbulance15")%>%
  MDAtrajectoryFunction("Ambulance15")

TrajAmbulance16 <- trajectory("TrajAmbulance16")%>%
  MDAtrajectoryFunction("Ambulance16")

TrajAmbulance17 <- trajectory("TrajAmbulance17")%>%
  MDAtrajectoryFunction("Ambulance17")

TrajAmbulance18 <- trajectory("TrajAmbulance18")%>%
  MDAtrajectoryFunction("Ambulance18")

TrajAmbulance19 <- trajectory("TrajAmbulance19")%>%
  MDAtrajectoryFunction("Ambulance19")

TrajAmbulance20 <- trajectory("TrajAmbulance20")%>%
  MDAtrajectoryFunction("Ambulance20")

DIboxNorth <- trajectory("DIboxNorth")%>% # each Drive IN has his own box of tests
  batch(n=100,timeout=function () {get_global(SimuCorona,"timeForBatch")-now(SimuCorona)},permanent=FALSE)%>% #after 100 tests or 4 hours- move to lab
  timeout(trimmedNorm(15,5))%>% #Driving to the lab
  leave(1,TrajLabNorth,FALSE)

DIboxCenter <- trajectory("DIboxCenter")%>% # each Drive IN has his own box of tests
  batch(n=100,timeout=function () {get_global(SimuCorona,"timeForBatch")-now(SimuCorona)},permanent=FALSE)%>% #after 100 tests or 4 hours- move to lab
  timeout(trimmedNorm(15,5))%>% #Driving to the lab
  leave(1,TrajLabCenter,FALSE)

DIboxSouth <- trajectory("DIboxSouth")%>% # each Drive IN has his own box of tests
  batch(n=100,timeout=function () {get_global(SimuCorona,"timeForBatch")-now(SimuCorona)},permanent=FALSE)%>% #after 100 tests or 4 hours- move to lab
  timeout(trimmedNorm(15,5))%>% #Driving to the lab
  leave(1,TrajLabSouth,FALSE)

mainDI<-trajectory("mainDI")%>%
  set_attribute(key="times",value=1)%>%
  set_attribute(key="TypeOfTest",value=1)%>% #1-DItests
  set_attribute(key="DIDistrict",value=(function() rdiscrete(1,c(0.23,0.35,0.42),c(1,2,3))))%>%
  select(function() DIFunction(numToDistrict(get_attribute(SimuCorona,"DIDistrict"))) ,policy="random")%>%
  seize_selected(amount=1)%>%
  timeout(1.5)%>% #test time
  set_attribute("TestedTime",value=1,mod="+")%>%
  release_selected(amount=1)%>%
  branch(option=function() get_attribute(SimuCorona,"DIDistrict") ,continue= c(FALSE,FALSE,FALSE),DIboxSouth,DIboxNorth,DIboxCenter)

mainKupah <- trajectory("mainKupah")%>%
  set_attribute(key="times",value=1,mod="+")%>%
  set_attribute(key="TypeOfTest",value=2)%>% #2-Kupahtests
  set_attribute(key="KupahDistrict",value=(function() rdiscrete(1,c(0.23,0.35,0.42),c(1,2,3))))%>%
  select(function() kupahFunction(numToDistrict(get_attribute(SimuCorona,"KupahDistrict"))) ,policy="random", id=1)%>%
  seize_selected(amount=1,id=1)%>%
  timeout(1.5)%>% #testing time
  set_attribute("TestedTime",value =1,mod="+")%>%
  release_selected(amount=1,id=1)%>%
  branch(option=function() get_attribute(SimuCorona,"KupahDistrict") ,continue= c(FALSE,FALSE,FALSE),TrajLabSouth,TrajLabNorth,TrajLabCenter)

mainMDA <- trajectory("mainMDA")%>%
  set_attribute(key="times",value=1)%>%
  set_attribute(key="TypeOfTest",value=3)%>% #3-MDAtests
  set_attribute(key="MDADistrict",value=(function() rdiscrete(1,c(0.23,0.35,0.42),c(1,2,3))))%>%
  SplitAmbulances()




##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

SimuCorona%>%
  add_generator(name="DItest", mainDI,from_to(360,1260, function() rexp(1,0.5788541),every = 24*60), mon=2)%>%
  add_generator("MDAtest", mainMDA, distribution = function() rexp(1,0.463138), mon=2)%>%
  add_generator("KupahTest", mainKupah, distribution = function() rexp(1,8), mon=2,priority=1)%>%
  add_generator("DIworkstationCAP",ChangeDIworkstationCAP,from(0,function () 24*60))%>%
  add_generator("KupahSchedule",KupahSchedule,from(0,function () 24*60))%>%
  add_generator("Exposed_To_Positive",mainDI,when_activated(n=2),mon=2)%>%
  add_generator("batchSchedule",batchScheduleTrj,from (0,function ()(4*60)), mon=2)%>%
  add_generator("DaySchedule",DayScheduleTraj,from (0,function ()(24*60)), mon=2)%>%
  add_generator("StopTesting",SendAmbulancesToLabs,function()(4*60),mon=2,priority=1)
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  reset(SimuCorona)%>%run(until=simulationTime)


  arrivalData <- get_mon_arrivals(SimuCorona)
  arrivalData2 <- get_mon_arrivals(SimuCorona,ongoing = T)
  arrivalData3 <- get_mon_arrivals(SimuCorona,per_resource = T)
  resourceData <- get_mon_resources(SimuCorona)
  attributeData <- get_mon_attributes(SimuCorona)

  ViewResults <-sqldf("select * from attributeData where key='Result'")
  Waiting_for_results <- sqldf("select AR.'name', AT.'time',(case
                               when AT.key='TestedTime' and AT.value=2 then AR.'end_time'
                                when AT.key='TestedTime' and AT.'value'=1 then V.'time'
                               else 0 end) as END,
                               (case when AT.key='TestedTime' and AT.value=2 then (AR.'end_time'-AT.time)
                               when AT.key='TestedTime' and AT.'value'=1 then (V.'time'-AT.time)
                               else 0 end) as 'Wait For Result'
                               From arrivalData as AR join attributeData as AT on AR.name=AT.name
                               join ViewResults as V on AT.name=V.name
                              WHERE AT.key='TestedTime' GROUP BY AT.time")
  Mean_of_WaitForResult <- mean(Waiting_for_results$`Wait For Result`)

  Waiting_for_ambulance <- sqldf("select *,A.'end_time'-A.'start_time'-1.5 as 'Wait for ambulance'
                                 FROM arrivalData3 as A WHERE resource LIKE '%Ambulance%' AND name LIKE '%MDA%'")
  Mean_for_waitForAmbulance <- mean(Waiting_for_ambulance$`Wait for ambulance`)

  Waiting_In_KupatHolim <- sqldf("select *,A.'end_time'-A.'start_time'-1.5 as 'Wait for Kupah'
                                FROM arrivalData3 as A WHERE resource LIKE '%Kupah%'")
  Mean_for_waitInKupahLine <- mean(Waiting_In_KupatHolim$`Wait for Kupah`)


