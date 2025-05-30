library(gsynth)

load("~/Library/CloudStorage/OneDrive-NorthwesternUniversity/Publications/Forrest - Crime Intervention/RR/New Analysis Final/Urban_Alchmey_Analysis_dd-ddd.RData")

#############################################################
####################### Day Only (DD) #######################
#############################################################

dd_drug <- gsynth(Drug_Crime ~ intervention_month_number, data = dd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric",  nboots = 500, 
              parallel = TRUE, cores = 5)

dd_behavior <- gsynth(Behavioral_Health_Crime ~ intervention_month_number, data = dd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)
              
dd_total <- gsynth(Total_Crime ~ intervention_month_number, data = dd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)
              
dd_property <- gsynth(Property_Crime ~ intervention_month_number, data = dd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)
              
dd_violence <- gsynth(Violent_Crime ~ intervention_month_number, data = dd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)

################################################################
####################### Difference (DDD) #######################
################################################################

ddd_drug <- gsynth(Drug_Crime_Diff ~ intervention_month_number, data = ddd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)
 
ddd_behavior <- gsynth(Behavioral_Health_Crime_Diff ~ intervention_month_number, data = ddd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)

ddd_total <- gsynth(Total_Crime_Diff ~ intervention_month_number, data = ddd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 500, 
              parallel = TRUE, cores = 5, normalize = TRUE)

ddd_property <- gsynth(Property_Crime_Diff ~ intervention_month_number, data = ddd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)
              
ddd_violence <- gsynth(Violent_Crime_Diff ~ intervention_month_number, data = ddd_data, 
                  index = c("Intersection_ID","t"), force = "two-way", 
              CV = TRUE, r = c(0, 10), 
              se = TRUE, 
              inference = "parametric", nboots = 5000, 
              parallel = TRUE, cores = 5, normalize = TRUE)
