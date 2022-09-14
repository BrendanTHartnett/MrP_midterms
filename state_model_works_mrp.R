library(readr)
library(haven)
library(ggplot2)
library(dplyr)
library(car)
library(lme4)
library(arm)

#LOAD DATA
BLWwave13 <- read_csv("BLWwave13.csv")
BLWwave14 <- read_csv("BLWwave14.csv")

#Merge datawaves
data <- bind_rows(BLWwave13, BLWwave14)
dat <- data


table(dat$wave)
#Recode rejection of election results ot binary: 0 = accept, 1 = reject
table(dat$biden_winner)
dat$biden_winner[dat$biden_winner=="Definitely not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably the rightful winner"] <- "0"
dat$biden_winner[dat$biden_winner=="Definitely the rightful winner"] <- "0"
dat$biden_winner <- as.numeric(dat$biden_winner)
table(dat$biden_winner)



## RACE X GENDER ####
#Recode gender to binary (is.MALE = 1 )
dat$male <- NA
dat$male <- 0
dat$male[dat$gender=="Male"] <- 1
table(dat$male)
#Recode race to factor
dat$race <- NA
dat$race[dat$ethnicity=="White"] <- "White"
dat$race[dat$ethnicity=="Asian/Pacific Islander"] <- "Asian"
dat$race[dat$ethnicity=="Hispanic/Latino/Chicano/a" | dat$hispanic=="Yes"] <- "Hispanic"
dat$race[dat$ethnicity=="Black or African American"] <- "Black"
dat$race[dat$ethnicity=="Other" | dat$ethnicity=="Multi-racial" | dat$ethnicity=="American Indian or Alaska Native"] <- "Other"
table(dat$race)
table(dat$hispanic)


#Code for Race - White and Hispanic
dat$WHITE <- NA
dat$WHITE <- 0
dat$WHITE[dat$race=="White"] <- 1
table(dat$WHITE)

dat$HISPANIC <- NA
dat$HISPANIC <- 0
dat$HISPANIC[dat$hispanic=="Yes"] <- 1
dat$HISPANIC[dat$race=="Hispanic"] <- 1
table(dat$HISPANIC)


## EDUCATION ####
#Recode Education
table(dat$educ7)
dat$edu <- NA
dat$edu[dat$educ7=="Did not graduate from high school"] <- "No high school"
dat$edu[dat$educ7=="High school diploma or the equivalent (GED)"] <- "High school"
table(dat$edu)
#Create dummy variable for if their highest level of education was high school
#where those with GED, High school degree, or no highschool degree = 1 
dat$highschool <- NA
dat$highschool <- 0 
dat$highschool[dat$edu=="No high school"] <- 1
dat$highschool[dat$edu=="High school"] <- 1
table(dat$highschool)


## Employment ####
#Recode employment to labor statistics
table(dat$employ)
dat$employment <- NA
dat$employment <- "Not in labor force"
dat$employment[dat$employ=="Student"] <- "Student"
dat$employment[dat$employ=="Full-time"] <- "Employed"
dat$employment[dat$employ=="Part-time"] <- "Employed"
dat$employment[dat$employ=="Unemployed"] <- "Unemployed"
table(dat$employment)
#Create binary for NotInLaborForce
dat$NotInLaborForce <- 0
dat$NotInLaborForce[dat$employment=="Not in labor force"] <- 1

## 2020 VOTE CHOICE ####
table(dat$pres_vote)
#Make dummy variable for voted for Trump
dat$Trump <- NA
dat$Trump <- 0
dat$Trump[dat$pres_vote=="Donald Trump"] <- 1
table(dat$Trump)

dat$Biden <- NA
dat$Biden <- 0
dat$Biden[dat$pres_vote=="Joe Biden"] <- 1
table(dat$Biden) 

# HHI ####
#Make dummy for if HHI is under 40k
dat$less40k <- NA
dat$less40k <- 0
dat$less40k[dat$faminc_new=="Less than $10,000"] <- 1
dat$less40k[dat$faminc_new=="$10,000 - $19,999"] <- 1
dat$less40k[dat$faminc_new=="$20,000 - $29,999"] <- 1
dat$less40k[dat$faminc_new=="$30,000 - $39,999"] <- 1
table(dat$less40k)
#Make dummy for if HHI is over 100k
dat$over100k <- NA
dat$over100k <- 0
dat$over100k[dat$faminc_new=="$100,000 - $119,999"] <- 1
dat$over100k[dat$faminc_new=="$120,000 - $149,999"] <- 1
dat$over100k[dat$faminc_new=="$120,000 - $149,999"] <- 1
dat$over100k[dat$faminc_new=="$150,000 - $199,999"] <- 1
dat$over100k[dat$faminc_new=="$200,000 - $249,999" | dat$faminc_new=="$250,000 - $349,999" | dat$faminc_new=="$350,000 - $499,999" | dat$faminc_new=="$500,000 or more"] <- 1
table(dat$over100k)


#Change input_states to NAME to match tidycensus
dat$State <- dat$inputstate
dat$STATE <- tolower(dat$State)
dat <- subset(dat, STATE != "district of columbia")

#Model Used for prediction
Model <- lm(biden_winner ~ highschool + WHITE + HISPANIC + NotInLaborForce + less40k + over100k + Trump + Biden, data=dat)
Model
summary(Model)

#Inverse Logit
state_model <- glmer(formula = biden_winner ~  (1 | STATE) + highschool + WHITE + HISPANIC + NotInLaborForce + less40k + over100k + Trump + Biden, data=dat, family=binomial(link="logit"))
state_model


## Census Data ####

# Load Census Data : 
Census <- read_csv("ACS_state_data.csv")

state_ranefs <- array(NA, c(50, 1))
Census$STATE <- Census$NAME
dimnames(state_ranefs) <- list(c(Census$NAME), 'effect')
# assign state random effects to array while preserving NAs
for (i in Census$STATE) {
  
  state_ranefs[i, ] <- ranef(state_model)$STATE[i, 1]
  
}
state_ranefs[, 1][is.na(state_ranefs[, 1])] <- 0


#Recode race
Census$WHITE <- NA
Census$WHITE <- Census$WHITEFEMALE + Census$WHITEMALE
Census$HISPANIC <- NA
Census$HISPANIC <- Census$HISPANICFEMALE + Census$HISPANICMALE

#Recode vote
Census$Trump_voters
Census$Trump_voters <- Census$TRUMP #already in % of total VAP
Census$Biden_voters <- Census$candidatevotes/Census$TOTAL_VAP
Census$Trumpers <- Census$TRUMP/Census$voted #Recode to percent of voters to control for turnout which varied most across states

Census$cellpred <- invlogit(fixef(state_model)['(Intercept)']  + state_ranefs[Census$STATE, 1] + (fixef(state_model)['Trump'] *Census$Trumpers) + (fixef(state_model)['Biden'] *Census$Biden_voters) + (fixef(state_model)['highschool'] *Census$highschool)  + (fixef(state_model)['WHITE'] *Census$WHITE) + (fixef(state_model)['HISPANIC'] *Census$HISPANIC) + (fixef(state_model)['NotInLaborForce'] *Census$NotInLaborForce) + (fixef(state_model)['less40k'] *Census$less40k) + (fixef(state_model)['over100k'] *Census$over100k))

summary(Census$cellpred)


#Create .shp
USSTATES <- read_sf("Downloads/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
USSTATES$State <- USSTATES$NAME
USSTATES$NAME <- tolower(USSTATES$NAME)
table(USSTATES$NAME)

usstar <- merge(USSTATES, Census, by.x = "NAME", by.y = "NAME")
table(USSTATES$NAME)
usstar$NAME

Census_exp <- usstar[, c("NAME", "cellpred")]
Census_exp$NAME <- str_to_title(Census_exp$NAME)
Census_exp$NAME

state.sf <- st_transform(Census_exp, "+proj=longlat +zone=19 +ellps=GRS80 +datum=WGS84")
Census <- state.sf

Census$cellpred <- round(Census$cellpred, digits=2)
Census$cellpred <- Census$cellpred*100
StateMap_internal <- leaflet(Census) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", cellpred)(cellpred),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(
                "District: "
                , Census$STATE
                , "<br>"
                , Census$cellpred
                , "% "
                ,"doubting election"
                , "<br>"
              )) 
StateMap_internal
