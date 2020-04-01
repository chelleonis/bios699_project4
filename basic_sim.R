
print("hello world")

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

n_mc <- 5
needle_means <- matrix(0,n_mc,2) #make it 2d for a dataframe or something

#output of each run: mean deflection of std needle and mean deflection of new needle
#loop until exhaust all needles
# if needles < 12 (or amount needed)

for(i in 1:n_mc) {
 std_means <- needle_sim(4,8)
}

#returns the mean of each needle deflections and other stuff and # of patients
needle_sim <- function(targeted_bc, random_bc) {
  patient_num <- 0
  temp_needles <- 500 
  temp_biop <- rep(NA,10)
  vec_lesions <- c(1,2,3)
  vec_regions <- c(1,2,3,4)
  n_lesion <- 0
  while(temp_needles > 6) { #need to do a check somehwere to ensure we have enough
    #needles
    #generate patient lesions (1-3)
    n_lesion <- sample(vec_lesion,1) #leision #
    lesion_loc <- sample(vec_regions,n_lesion)
    #subtract new needles needed
    temp_needles <- temp_needles-(random_bc/2 + n_lesion*targeted_bc/2)
    #perform biopsies
    temp_random <- biopsy(vec_regions,lesion_loc,temp_needles)
    #store resultss
    
    patient_num <- patient_num + 1
  } #go next patient
  
}

#function to perform biopsies
biopsy <- function(vec_regions, lesion_location) {
  
}



