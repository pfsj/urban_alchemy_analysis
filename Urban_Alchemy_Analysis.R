library(glmmTMB)

# Load data 
analysis_data <- readRDS(file = "Urban_Alchemy_Data.Rds")

# Poisson models without adjacent intersections # 
drug_NoAdj<- glmmTMB(Drug_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")

behavioral_NoAdj <- glmmTMB(Behavioral_Health_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                        data=analysis_data,
                      family="poisson")
total_NoAdj<- glmmTMB(Total_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")

violent_NoAdj<- glmmTMB(Violent_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")
property_NoAdj<- glmmTMB(Property_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")

# Poisson models with adjacent intersections # 
drug_All <- glmmTMB(Drug_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")

behavioral_All <- glmmTMB(Behavioral_Health_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")
total_All <- glmmTMB(Total_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")

violent_All <- glmmTMB(Violent_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")
property_All <- glmmTMB(Property_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data,
                      family="poisson")

########### SENSITIVITY ANALYSES ###########

# Unweighted Poisson models # 
drug_Unweighted <- glmmTMB(Drug_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data,
                      family="poisson")

behavioralArAll_Unweighted <- glmmTMB(Behavioral_Health_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data,
                      family="poisson")

totalArAll_Unweighted <- glmmTMB(Total_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data,
                      family="poisson")

violentArAll_Unweighted <- glmmTMB(Violent_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data,
                      family="poisson")

propertyArAll_Unweighted <- glmmTMB(Property_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data,
                      family="poisson")

# Poisson models with only Urban Alchemy intersections # 
drugArAll_UA <- glmmTMB(Drug_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      family="poisson")

behavioralArAll_UA <- glmmTMB(Behavioral_Health_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      family="poisson")

totalArAll_UA <- glmmTMB(Total_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      family="poisson")

violentArAll_UA <- glmmTMB(Violent_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      family="poisson")

propertyArAll_UA <- glmmTMB(Property_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      family="poisson")

# Poisson models with shorter time frame #
drugAll_Short <- glmmTMB(Drug_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data[analysis_data$t>20 & analysis_data$t <= 71,],
                      family="poisson")

behavioralAll_Short <- glmmTMB(Behavioral_Health_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group)  + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data[analysis_data$t>20 & analysis_data$t <= 71,],
                      family="poisson")
totalAll_Short <- glmmTMB(Total_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data[analysis_data$t>20 & analysis_data$t <= 71,],
                      family="poisson")

violentAll_Short <- glmmTMB(Violent_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data[analysis_data$t>20 & analysis_data$t <= 71,],
                      family="poisson")

propertyAll_Short <- glmmTMB(Property_Crime ~ t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t + (1|group) + ar1(t_fact-1|group),
                      control = glmmTMBControl(optArgs=list(method="BFGS")),
                      zi=~1,
                      REML=TRUE,
                      weights=w,
                      data=analysis_data[analysis_data$t>20 & analysis_data$t <= 71,],
                      family="poisson")

# Poisson models with geospatial error term #
library(spaMM)
total_geo <- spaMM::fitme(Total_Crime ~  t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + adjacent + adjacent_t + Matern(1|Latitude+Longitude) + (1|group),
                      data=analysis_dataGeo,
                      family="poisson")

violent_geo <- spaMM::fitme(Violent_Crime ~  t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + adjacent + adjacent_t + Matern(1|Latitude+Longitude) + (1|group),
                      data=analysis_dataGeo,
                      family="poisson")

property_geo <- spaMM::fitme(Property_Crime ~  t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + adjacent + adjacent_t + Matern(1|Latitude+Longitude) + (1|group),
                      data=analysis_dataGeo,
                      family="poisson")

drug_geo <- spaMM::fitme(Drug_Crime ~  t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + adjacent + adjacent_t + Matern(1|Latitude+Longitude) + (1|group),
                      data=analysis_dataGeo,
                      family="poisson")

disorder_geo <- spaMM::fitme(Disorder_Crime ~  t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + adjacent + adjacent_t + Matern(1|Latitude+Longitude) + (1|group),
                      data=analysis_dataGeo,
                      family="poisson")

behavioral_geo <- spaMM::fitme(Behavioral_Health_Crime ~  t + Urban_Alchemy + Urban_Alchemy_t + deployment + deployment_t + covid + covid_t + adjacent + adjacent_t + Matern(1|Latitude+Longitude) + (1|group),
                      data=analysis_dataGeo,
                      family="poisson")

# Conditional poisson models #
library(gnm)
totalGnm <- gnm(Total_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t, eliminate=group,
                      #data=analysis_data[analysis_data$Urban_Alchemy==1,], # Works with either way
                      data = analysis_data,
                      family="poisson")

violentGnm <- gnm(Violent_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t, eliminate=group,
                      #data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      data = analysis_data,
                      family="poisson")
propertyGnm <- gnm(Property_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t, eliminate=group,
                      #data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      data = analysis_data,
                      family="poisson")

drugGnm <- gnm(Drug_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t, eliminate=group,
                      #data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      data = analysis_data,
                      family="poisson")

disorderGnm <- gnm(Disorder_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t, eliminate=group,
                      #data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      data = analysis_data,
                      family="poisson")

behaviorGnm <- gnm(Behavioral_Health_Crime ~ t + deployment + deployment_t + adjacent + adjacent_t + covid + covid_t, eliminate=group,
                      #data=analysis_data[analysis_data$Urban_Alchemy==1,],
                      data = analysis_data,
                      family="poisson")
