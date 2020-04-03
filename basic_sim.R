
library(tictoc)

#study design - RCT
# new needles > old needles ? GOAL: new needle mean deflection is 40% less than std needle

#monte carlo simluation i think

n_people <- 10 #this variable will vary 

new_needles <- 500 #limit

#biopsy cores
targeted_bc <- 1 #1-4 of each lesion
random_bc <- 6 #6-12 of entire prostate 

patient_lesions <-  #1-3 per patient, random variable

#new needle tip prelim results on PVC (surrogate healthy) (mm)
std_defl_PVC_mean <- 1.34
std_defl_PVC_sd <- 0.22
new_defl_PVC_mean <- 0.52
new_defl_PVC_sd <- 0.16
# %decrease = 61%, the varaible we're looking for

#new needle tip prelim results on turnips (cancerous tissue surrogate)
std_defl_turn_mean <- 10.6
new_defl_turn_mean <- 2.1

#both needles used in each patient
#12 random needle biopsies (fixed)
#4*(1-3) based on patient_lesions variable biopsies

#specific questions
#q1: engoh power to determine 40% deflection reduction with 500 new needles??
#q2: add 100 more needles, % level of reduction in needle defelction can be detected w/ same power as before

n_mc <- 10000
mean_vector <- matrix(0,n_mc,3) #make it 2d for a dataframe or something
colnames(mean_vector) <- c("new mean","standard mean","n_patients")
#output of each run: mean deflection of std needle and mean deflection of new needle
#loop until exhaust all needles
# if needles < 12 (or amount needed)

for(i in 1:n_mc) {
 mean_vector[i,] <- needle_sim(4,8)
}



#returns the mean of each needle deflections and other stuff and # of patients
needle_sim <- function(targeted_bc, random_bc) {
  patient_num <- 1
  temp_needles <- 500 
  temp_biop <- rep(NA,10)
  new_mean <- rep(NA,85)
  std_mean <- rep(NA,85)
  vec_lesions <- c(1,2,3)
  vec_regions <- c(1,2,3,4)
  n_lesion <- 0
  while(temp_needles > 6) {
    #generate patient lesions (1-3)
    n_lesion <- sample(vec_lesions,1) #leision #
    lesion_loc <- sample(vec_regions,n_lesion)
    #subtract new needles needed
    temp_needles <- temp_needles-(random_bc/2 + n_lesion*targeted_bc/2)
    if(temp_needles >= 0) {
      #perform biopsies
      temp_random <- biopsy(vec_regions,lesion_loc,targeted_bc,random_bc)
      #store results
      new_mean[patient_num] <- temp_random[1]
      std_mean[patient_num] <- temp_random[2]
      #go next patient
      patient_num <- patient_num +1 
    }
  } 
  #return a new_mean,std_mean
  return(c(mean(new_mean,na.rm = TRUE),mean(std_mean, na.rm = TRUE), patient_num-1)) #double check
}

#function to perform biopsies, should return a vector of results
biopsy <- function(vec_regions, lesion_location,tbc,rbc) {
  #op chars
  std_defl_PVC_mean <- 1.34
  std_defl_PVC_sd <- 0.22
  new_defl_PVC_mean <- 0.52
  new_defl_PVC_sd <- 0.16
  std_defl_turn_mean <- 10.6
  new_defl_turn_mean <- 2.1
  per = tbc/2
  per2 = rbc/2
  n_lesions = length(lesion_location)
  #data vectors
  new_bc <- rep(NA,n_lesions*per+per2)
  std_bc <- rep(NA,n_lesions*per+per2)
  #random - stratified by region use the pvc if not a lesion, turnip otherwise
  #currently unable to be more flexible since i'm lazy
  #so we're stuck at intervals of 4 lol....if i wanna do 12 need to modify this bit
  for(i in 1:per2) {
    #new
    new_bc[i] <- rd_strat_bp(lesion_location,i,new_defl_PVC_mean,new_defl_PVC_sd,
                             new_defl_turn_mean, 1) #chage whne we get an estimate
    #std
    std_bc[i] <- rd_strat_bp(lesion_location,i,std_defl_PVC_mean,std_defl_PVC_sd,
                             std_defl_turn_mean, 1)
  }
  #targetted - for each lesion, use the turnip means
  target_b <- n_lesions*per
  if(target_b > 0) {
    for(j in 1:target_b) {
      new_bc[j+per2] <- rnorm(1,new_defl_turn_mean,sd = 1) #figure out which sd to use
      #std
      std_bc[j+per2] <- rnorm(1,std_defl_turn_mean,sd = 1) #same
    }
  }
  #average out and return as matrix
  return(c(mean(new_bc,na.rm = TRUE),mean(std_bc,na.rm = TRUE)))
}

#random stratified biopsy helper function
rd_strat_bp <- function(lesion,i,PVC_mean,PVC_std,turnip_mean,turnip_std) {
  #if in lesion draw from turnip
  if (i %in% lesion) {
   deflect <- rnorm(1,turnip_mean,sd = 1) #change this when can
  }
  #else draw from PVC
  else{
    deflect <- rnorm(1,PVC_mean,PVC_std)
  }
  return(deflect)
}

