library(ggplot2)
library(dplyr)
library(car)
library(dataverse) 
library(readxl)
library(tidyverse)
library(tidycensus)
library(lme4)
library(tidyr)
library(arm) 
library(sf)
library(modelr)
library(inlmisc)
library(AddSearchButton)
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(readr)
library(maptools)
library(sp)
library(rgeos)
#Load BLW respondents geocoded by district 

dat <- read_csv("blw_geocoded_cong_dat.csv")

#Set states with only one house district to NA

dat$DistrictID[dat$STATE=="AK"] <- "NA"
dat$DistrictID[dat$STATE=="DC"] <- "NA"
dat$DistrictID[dat$STATE=="DE"] <- "NA"
dat$DistrictID[dat$STATE=="ND"] <- "NA"
dat$DistrictID[dat$STATE=="SD"] <- "NA"
dat$DistrictID[dat$STATE=="VT"] <- "NA"
dat$DistrictID[dat$STATE=="WY"] <- "NA"
table(dat$DistrictID)

#recode accept results
dat$biden_winner[dat$biden_winner=="Definitely not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably the rightful winner"] <- "0"
dat$biden_winner[dat$biden_winner=="Definitely the rightful winner"] <- "0"
dat$biden_winner <- as.numeric(dat$biden_winner)
table(dat$biden_winner)

#Make race variable
dat$race <- NA
dat$race[dat$ethnicity=="White"] <- "White"
dat$race[dat$ethnicity=="Asian/Pacific Islander"] <- "Asian"
dat$race[dat$ethnicity=="Hispanic/Latino/Chicano/a" | dat$hispanic=="Yes"] <- "Hispanic"
dat$race[dat$ethnicity=="Black or African American"] <- "Black"
dat$race[dat$ethnicity=="Other" | dat$ethnicity=="Multi-racial" | dat$ethnicity=="American Indian or Alaska Native"] <- "Other"
table(dat$race)
#Convert categorical race variables to dummy variables
#White
dat$WHITE <- NA
dat$WHITE <- 0
dat$WHITE[dat$race=="White"] <- 1
table(dat$WHITE)
#Black
dat$BLACK <- NA
dat$BLACK <- 0
dat$BLACK[dat$race=="Black"] <- 1
table(dat$BLACK)
#Hispanic
dat$HISPANIC <- NA
dat$HISPANIC <- 0
dat$HISPANIC[dat$race=="Hispanic"] <- 1
table(dat$HISPANIC)

#Make HHI variables
#Create dummy variable for if HHI is less than 40k
dat$less40k <- NA
dat$less40k <- 0
dat$less40k[dat$family_income=="Less than $10,000"] <- 1
dat$less40k[dat$family_income=="$10,000 - $19,999"] <- 1
dat$less40k[dat$family_income=="$20,000 - $29,999"] <- 1
dat$less40k[dat$family_income=="$30,000 - $39,999"] <- 1
table(dat$less40k)
#Create dummy variable for if HHI is over 100k
dat$over100k <- NA
dat$over100k <- 0
dat$over100k[dat$family_income=="$100,000 - $119,999"] <- 1
dat$over100k[dat$family_income=="$120,000 - $149,999"] <- 1
dat$over100k[dat$family_income=="$120,000 - $149,999"] <- 1
dat$over100k[dat$family_income=="$150,000 - $199,999"] <- 1
dat$over100k[dat$family_income=="$200,000 - $249,999" | dat$family_income=="$250,000 - $349,999" | dat$family_income=="$350,000 - $499,999" | dat$family_income=="$500,000 or more"] <- 1
table(dat$over100k)

#Make Education variables
#Create dummy for if individual has recieved a highest degree of high school or less
#where those with GED, High school degree, or no highschool degree = 1 
dat$edu <- dat$educ7
dat$highschool <- NA
dat$highschool <- 0 
dat$highschool[dat$edu=="Did not graduate from high school"] <- 1
dat$highschool[dat$edu=="High school diploma or the equivalent (GED)"] <- 1
table(dat$highschool)


#Make Employment variables
table(dat$employ)
dat$employment <- NA
#dat$employment[dat$employ=="Student"] <- "Student"
dat$employment[dat$employ=="Full-time"] <- "Employed"
dat$employment[dat$employ=="Part-time"] <- "Employed"
dat$employment[dat$employ=="Unemployed"] <- "Unemployed"
table(dat$employment)
#Make Not In Labor Force binary variable
dat$NotInLaborForce <- 0
dat$NotInLaborForce[!is.na(dat$employment)] <- 1
table(dat$NotInLaborForce)


## 2020 Vote variables 
table(dat$pres_vote_2020)
dat$pres_vote <- dat$pres_vote_2020
table(dat$pres_vote)
#Make dummy variable for voted for Trump
dat$Trump <- NA
dat$Trump <- 0
dat$Trump[dat$pres_vote_2020=="Donald Trump"] <- 1
table(dat$Trump)

dat$Biden <- NA
dat$Biden <- 0
dat$Biden[dat$pres_vote_2020=="Joe Biden"] <- 1
table(dat$Biden)

#Model 
test.congressional_model.regress <- lm(biden_winner ~  highschool + WHITE + HISPANIC + NotInLaborForce + less40k + over100k + Trump + Biden, data=dat)
test.congressional_model.regress
summary(test.congressional_model.regress)

congressional_model <- glmer(formula = biden_winner ~  highschool + (1 | DistrictID) + WHITE + HISPANIC + NotInLaborForce + less40k + over100k + Trump + Biden, data=dat, family=binomial(link="logit"))

congressional_model
summary(congressional_model)


##### Clean Census data #####
library(sf)
Census <- read_sf("District_level_census_dat.shp")

Census$Trump <- as.numeric(Census$Trump_1)
Census$Trump_voters <- Census$Trump/Census$TotalPp

Census$Biden <- as.numeric(Census$Biden)
Census$Biden_voters <- Census$Biden/Census$TotalPp


Census$highschool <- Census$highschl
Census$NotInLaborForce <- Census$NtInLbrFr



Census$DistrictID <- Census$DstrcID
table(table(Census$DistrictID))
Census$over100k <- Census$ovr100k
Census$HISPANIC <- Census$HISPANI


#manually input Kentucky state level 
Census$Trump_voters[Census$STATE=="Kentucky"] <- 0.38173194288
Census$Biden_voters[Census$STATE=="Kentucky"] <- 0.22227331243




##### Make MrP Model #####
#make state random effects by determining survey n of each district 
dat$sum <- 1
table(dat$sum)
df <- dat %>%
  group_by(DistrictID) %>%
  dplyr::summarize(Mean = mean(Trump*100, na.rm=TRUE),
                   N = sum(Trump, na.rm=TRUE))

summary(df$N)
table(df$N)

head(Census)
Census$DistrictID
df$DistrictID
#Merge survey N data with Census data
Census <- merge(Census, df, by.x="DistrictID", by.y="DistrictID", all.x=TRUE)

# set district names as row names
cong_ranefs <- NA
cong_ranefs <- array(NA, c(429, 1))
dimnames(cong_ranefs) <- list(c(Census$DistrictID), 'effect')
for (i in Census$DistrictID) {
  
  cong_ranefs[i, ] <- ranef(congressional_model)$DistrictID[i, 1]
  
}
#If fixed effects = NA, set to 0
cong_ranefs[, 1][is.na(cong_ranefs[, 1])] <- 0

Census$cellpred <- invlogit((fixef(congressional_model)['(Intercept)'])  + cong_ranefs[Census$DistrictID, 1] + (fixef(congressional_model)['Trump'] *Census$TRUMP) + (fixef(congressional_model)['Biden'] *Census$Biden_voters) + (fixef(congressional_model)['highschool'] *Census$highschool) +  (fixef(congressional_model)['WHITE'] *Census$WHITE) + (fixef(congressional_model)['HISPANIC'] *Census$HISPANIC) + (fixef(congressional_model)['NotInLaborForce'] *Census$NotInLaborForce) + (fixef(congressional_model)['less40k'] *Census$less40k) + (fixef(congressional_model)['over100k'] *Census$over100k))
summary(Census$cellpred)



##### Make Map #####
s.sf <- st_transform(Census, "+proj=longlat +zone=19 +ellps=GRS80 +datum=WGS84")
Census <- s.sf
Census$cellpred <- Census$cellpred*100
CongressMap <- leaflet(Census) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", cellpred)(cellpred),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(
                "District: "
                , Census$DistrictID
                , "<br>"
                , Census$cellpred
                , "% "
                ,"doubting election"
                , "<br>"
              )) 
CongressMap

summary(Census$cellpred)
