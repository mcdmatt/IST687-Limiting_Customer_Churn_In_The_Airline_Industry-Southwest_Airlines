# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
getwd()
library(jsonlite)

Southwest <- fromJSON("/Users/mattmcdonnell/Desktop/Spring2020IST687FinalProject.json")
View(Southwest)
str(Southwest)
#Create vector to analyze which rows need to be removed since they are categorical variables and have NAs that cannot be replaced.
CategoricalNAs <- c(which(is.na(Southwest$Destination.City)),
which(is.na(Southwest$Origin.City)),
which(is.na(Southwest$Airline.Status)),
which(is.na(Southwest$Gender)),
which(is.na(Southwest$Type.of.Travel)),
which(is.na(Southwest$Class)),
which(is.na(Southwest$Flight.date)),
which(is.na(Southwest$Partner.Code)),
which(is.na(Southwest$Partner.Name)),
which(is.na(Southwest$Origin.State)),
which(is.na(Southwest$Destination.City)),
which(is.na(Southwest$Flight.cancelled)))

CategoricalNAs
str(CategoricalNAs) # 312 missing values

Southwest <- Southwest[-CategoricalNAs,]
# This is smarter/fastest because it takes into account NA versus row number alone rather than deleting rows individually.

# Lets check to see if we get any more Categorical NAs now 
CategoricalNAs <- c(which(is.na(Southwest$Destination.City)),
                    which(is.na(Southwest$Origin.City)),
                    which(is.na(Southwest$Airline.Status)),
                    which(is.na(Southwest$Gender)),
                    which(is.na(Southwest$Type.of.Travel)),
                    which(is.na(Southwest$Class)),
                    which(is.na(Southwest$Flight.date)),
                    which(is.na(Southwest$Partner.Code)),
                    which(is.na(Southwest$Partner.Name)),
                    which(is.na(Southwest$Origin.State)),
                    which(is.na(Southwest$Destination.City)),
                    which(is.na(Southwest$Flight.cancelled)))
CategoricalNAs


# Now I will replace the NAs in the quantitative columns with the median of each respective value
QuantitativeNAs <- c(which(is.na(Southwest$Age)),
                    which(is.na(Southwest$Price.Sensitivity)),
                    which(is.na(Southwest$Year.of.First.Flight)),
                    which(is.na(Southwest$Flights.Per.Year)),
                    which(is.na(Southwest$Loyalty)),
                    which(is.na(Southwest$Total.Freq.Flyer.Accts)),
                    which(is.na(Southwest$Shopping.Amount.at.Airport)),
                    which(is.na(Southwest$Eating.and.Drinking.at.Airport)),
                    which(is.na(Southwest$Day.of.Month)),
                    which(is.na(Southwest$Scheduled.Departure.Hour)),
                    which(is.na(Southwest$Departure.Delay.in.Minutes)),
                    which(is.na(Southwest$Arrival.Delay.in.Minutes)),
                    which(is.na(Southwest$Flight.time.in.minutes)),
                    which(is.na(Southwest$Flight.Distance)),
                    which(is.na(Southwest$Likelihood.to.recommend)),
                    which(is.na(Southwest$olong)),
                    which(is.na(Southwest$olat)),
                    which(is.na(Southwest$dlong)),
                    which(is.na(Southwest$dlat)))
QuantitativeNAs
str(QuantitativeNAs) # 629 total values missing


# There are 3 numeric columns that have NULL data 
# Can use the median/mean (depending on range) of each column to act as a vector stored median value for good estimation
mean(Southwest$Departure.Delay.in.Minutes) 
median(Southwest$Arrival.Delay.in.Minutes)
median(Southwest$Flight.time.in.minutes)
# All 3 give NA value as answers so we must replace NAs

is.na(Southwest$Departure.Delay.in.Minutes)
Southwest$Departure.Delay.in.Minutes[is.na(Southwest$Departure.Delay.in.Minutes)]

is.na(Southwest$Arrival.Delay.in.Minutes)
Southwest$Arrival.Delay.in.Minutes[is.na(Southwest$Arrival.Delay.in.Minutes)]

is.na(Southwest$Flight.time.in.minutes)
Southwest$Flight.time.in.minutes[is.na(Southwest$Flight.time.in.minutes)]
#These statements pull all values that are "NA" and basically different from the numbers in that they are blank
#These statements work by setting all missing values to NA

# The columns Departure Delay, Arrival Delay, and Flight time all must fix the rows with NAs
Southwest$Departure.Delay.in.Minutes[is.na(Southwest$Departure.Delay.in.Minutes)] <- median(Southwest$Departure.Delay.in.Minutes, na.rm = TRUE)
Southwest$Arrival.Delay.in.Minutes[is.na(Southwest$Arrival.Delay.in.Minutes)] <- median(Southwest$Arrival.Delay.in.Minutes, na.rm = TRUE)
Southwest$Flight.time.in.minutes[is.na(Southwest$Flight.time.in.minutes)] <- median(Southwest$Flight.time.in.minutes, na.rm = TRUE)

QuantitativeNAs <- c(which(is.na(Southwest$Age)),
                     which(is.na(Southwest$Price.Sensitivity)),
                     which(is.na(Southwest$Year.of.First.Flight)),
                     which(is.na(Southwest$Flights.Per.Year)),
                     which(is.na(Southwest$Loyalty)),
                     which(is.na(Southwest$Total.Freq.Flyer.Accts)),
                     which(is.na(Southwest$Shopping.Amount.at.Airport)),
                     which(is.na(Southwest$Eating.and.Drinking.at.Airport)),
                     which(is.na(Southwest$Day.of.Month)),
                     which(is.na(Southwest$Scheduled.Departure.Hour)),
                     which(is.na(Southwest$Departure.Delay.in.Minutes)),
                     which(is.na(Southwest$Arrival.Delay.in.Minutes)),
                     which(is.na(Southwest$Flight.time.in.minutes)),
                     which(is.na(Southwest$Flight.Distance)),
                     which(is.na(Southwest$Likelihood.to.recommend)),
                     which(is.na(Southwest$olong)),
                     which(is.na(Southwest$olat)),
                     which(is.na(Southwest$dlong)),
                     which(is.na(Southwest$dlat)))
QuantitativeNAs
# New output of integer(0) indicated no more NAs
# Mitigate if any partner airlines do not have enough information to statistically infer (<30)
str(unique(Southwest$Partner.Code)) # There are 14 unique partner airlines in the Southwest umbrella
#Looking to identify which airlines are not statistically significant
length(Southwest$Partner.Code[Southwest$Partner.Code == "WN"]) #2199
length(Southwest$Partner.Code[Southwest$Partner.Code == "DL"]) #1639
length(Southwest$Partner.Code[Southwest$Partner.Code == "OO"]) #1229
length(Southwest$Partner.Code[Southwest$Partner.Code == "US"]) #903
length(Southwest$Partner.Code[Southwest$Partner.Code == "OU"]) #947
length(Southwest$Partner.Code[Southwest$Partner.Code == "EV"]) #1147
length(Southwest$Partner.Code[Southwest$Partner.Code == "B6"]) #377
length(Southwest$Partner.Code[Southwest$Partner.Code == "MQ"]) #529
length(Southwest$Partner.Code[Southwest$Partner.Code == "FL"]) #210
length(Southwest$Partner.Code[Southwest$Partner.Code == "VX"]) #115
length(Southwest$Partner.Code[Southwest$Partner.Code == "AS"]) #304
length(Southwest$Partner.Code[Southwest$Partner.Code == "F9"]) #140
length(Southwest$Partner.Code[Southwest$Partner.Code == "AA"]) #505
length(Southwest$Partner.Code[Southwest$Partner.Code == "HA"]) #12
# Remove airline HA because under 30 is stat insignificant
# remove 12 rows in r - subset function with HA condition
Southwest <- subset(Southwest, Partner.Code!='HA')
str(unique(Southwest$Partner.Code))
# now only 13 unique airlines



# Phase 2: Histograms (numeric variables) and Tables (factor variables like gender)
library(arules)
library(arulesViz)
library(ggplot2)
library(kernlab)
library(dplyr)
library(jsonlite)
library(kernlab)

ggplot(Southwest) + # pulls the dataset
  aes(x=Age) + # displays the aesthetic of the axis desired
  geom_histogram(bins=10, fill="blue",col="white") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Age Breakdown")
#Approx normal distribution centered around 40 years old. Potential outliers in elderly age group makes for positive skewness. 

summary(Southwest$Price.Sensitivity)
ggplot(Southwest) + # pulls the dataset
  aes(x=Price.Sensitivity) + # displays the aesthetic of the axis desired
  geom_histogram(bins=5, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Price Sensitivity Breakdown")
#Majority of price sensitivity is 1(majority) or 2. Hard to see skew with type of peak.

summary(Southwest$Year.of.First.Flight)
ggplot(Southwest) + # pulls the dataset
  aes(x=Year.of.First.Flight) + # displays the aesthetic of the axis desired
  geom_histogram(bins=5, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Year of First Flight Breakdown")
#Slightly negative skew. Centered around 2007

summary(Southwest$Flights.Per.Year)
ggplot(Southwest) + # pulls the dataset
  aes(x=Flights.Per.Year) + # displays the aesthetic of the axis desired
  geom_histogram(bins=10, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Flights/Year Breakdown")
# Strong positive skew showing outliers near those flying 50 or more times a year.

summary(Southwest$Loyalty)
ggplot(Southwest) + # pulls the dataset
  aes(x=Loyalty) + # displays the aesthetic of the axis desired
  geom_histogram(bins=10, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Loyalty Proportion to Southeast Airlines")
#Skewed positively. Few people have strong loyalty. Many of their consumers shop around various airlines.

summary(Southwest$Total.Freq.Flyer.Accts)
ggplot(Southwest) + # pulls the dataset
  aes(x=Total.Freq.Flyer.Accts) + # displays the aesthetic of the axis desired
  geom_histogram(bins=8, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Personal Frequent Flyer Accounts")
# Clear positive skew. Many people don't have freq flyer accounts and those that do mainly have 1 versus multiple. 

summary(Southwest$Shopping.Amount.at.Airport)
ggplot(Southwest) + # pulls the dataset
  aes(x=Shopping.Amount.at.Airport) + # displays the aesthetic of the axis desired
  geom_histogram(bins=15, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Airport Shopping Patterns ($)")
#Clear positive skew. Most people are spending little to nothing in airports on shopping. Some large outliers exist though.

summary(Southwest$Eating.and.Drinking.at.Airport)
ggplot(Southwest) + # pulls the dataset
  aes(x=Eating.and.Drinking.at.Airport) + # displays the aesthetic of the axis desired
  geom_histogram(bins=40, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Airport Eating/Drinking Patterns ($)")
# Positive skew. People defintely spend more on eating and drinking than shopping.

summary(Southwest$Day.of.Month)
ggplot(Southwest) + # pulls the dataset
  aes(x=Day.of.Month) + # displays the aesthetic of the axis desired
  geom_histogram(bins=20, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Day of Month Breakdown")
# Approx normally distributed with a single unimodal peak day in the middle of the month.
# Interesting to see about 8 peaks and dips at bins=20 because it could coincide with weekends or busier travel days or cheaper flight days.

summary(Southwest$Scheduled.Departure.Hour)
ggplot(Southwest) + # pulls the dataset
  aes(x=Scheduled.Departure.Hour) + # displays the aesthetic of the axis desired
  geom_histogram(bins=6, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Scheduled Departure Hour Breakdown")
# Travel typically begins 5am onward and dies down in the final hours
#Think it is negative skew but not great for skew. Just know busy in morning and after work.

summary(Southwest$Departure.Delay.in.Minutes)
ggplot(Southwest) + # pulls the dataset
  aes(x=Departure.Delay.in.Minutes) + # displays the aesthetic of the axis desired
  geom_histogram(bins=12, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Departure Delays")
#Positive skew. Typically flight departures are not delayed and if they are, it is mostly under 2 hours.

summary(Southwest$Arrival.Delay.in.Minutes)
ggplot(Southwest) + # pulls the dataset
  aes(x=Arrival.Delay.in.Minutes) + # displays the aesthetic of the axis desired
  geom_histogram(bins=12, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Arrival Delays")
#Positive skew. Typically flight arrival are not delayed and if they are, it is mostly under 2 hours.

summary(Southwest$Flight.time.in.minutes)
ggplot(Southwest) + # pulls the dataset
  aes(x=Flight.time.in.minutes) + # displays the aesthetic of the axis desired
  geom_histogram(bins=50, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Flight Times Breakdown (minutes)")
# Mostly short flights. Positive skew is very clear

summary(Southwest$Flight.Distance)
ggplot(Southwest) + # pulls the dataset
  aes(x=Flight.Distance) + # displays the aesthetic of the axis desired
  geom_histogram(bins=50, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Flight Distances Breakdown")
# Clear positive skew. Again, mainly short flights domestically. Majority under 1000 miles

summary(Southwest$Likelihood.to.recommend)
ggplot(Southwest) + # pulls the dataset
  aes(x=Likelihood.to.recommend) + # displays the aesthetic of the axis desired
  geom_histogram(bins=10, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Likelihood to Recommend Southeast")
#Negative skew. More people seem likely to recommend Southeast airlines.

summary(Southwest$olat)
ggplot(Southwest) + # pulls the dataset
  aes(x=olat) + # displays the aesthetic of the axis desired
  geom_histogram(bins=10, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Origin Latitude Coordinates")

summary(Southwest$olong)
ggplot(Southwest) + # pulls the dataset
  aes(x=olong) + # displays the aesthetic of the axis desired
  geom_histogram(bins=15, fill="blue",col="white")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Origin Longitude Coordinates")

#Begin Working with the factor variables

table(Southwest$Gender)
# 5883 females, 4373 males

table(Southwest$Airline.Status)
#6988 blue, 859 gold, 320 platinum, 2089 silver

table(Southwest$Type.of.Travel)
# 6252 business travel, 858 mileage tickets, 3146 personal travel

table(Southwest$Class)
# 790 business, 1084 economy plus, 8382 economy

table(Southwest$Partner.Code)
#  AA   AS   B6   DL   EV   F9   FL   HA   MQ   OO   OU   US   VX   WN 
# 505  304  377 1639 1147  140  210   12  529 1229  947  903  115 2199 

table(Southwest$Flight.cancelled)
# 10061 no, 195 yes
10049+195 # equals 10244
195/10244*100 # equals 1.9% of flights are cancelled

table(Southwest$AgeBins)
# Young     Young Adult     Adult      Senior 
#  2531        3888         2481       1344 

table(Southwest$NPSBins)
# Detractor  Passive   Promoter 
#   3002      3332      3910


# REGRESSIONS ***************************************************************************************************************

Southwest <- Southwest %>%
  mutate(NPSBins = case_when(Likelihood.to.recommend <= 6 ~ 'Detractor',
                             Likelihood.to.recommend ==7  ~ 'Passive',
                             Likelihood.to.recommend ==8  ~ 'Passive',
                             TRUE ~ 'Promoter'))
Southwest <- Southwest %>%
  mutate(NPSNums = case_when(Likelihood.to.recommend <= 6 ~ '1',
                             Likelihood.to.recommend ==7  ~ '2',
                             Likelihood.to.recommend ==8  ~ '2',
                             TRUE ~ '3')) # so that it can be used as factors still
Southwest$NPSNums <- as.numeric(as.factor(Southwest$NPSNums))

# Earliest insignificant findings **********************************************************************************************
#____________________________________________________________________________________________________________________________
R1 <- lm(formula = NPSNums ~ Year.of.First.Flight, Southwest)
summary(R1)
# This variable is not significant at all so we should not consider it

R2 <- lm(formula = NPSNums ~  Airline.Status, data=Southwest)
summary(R2)
# Subgroup those with platinum because they are less happy than gold and silver but more luxurios so this is a
# ...... good finding to investigate because maybe they dont want to focus on luxury but stable consitency
# Good side note but not too valuable. The value will be found in subgroup the Blue status which is predicted detractors 
R3 <- lm(formula = NPSNums ~ Partner.Code, Southwest)
summary(R3)
#Code EV is hated and makes NPS go from Passive to Detractor so drop this airline

R4 <- lm(formula = NPSNums ~ olat + olong+ dlat+ dlong, data = Southwest)
summary(R4)
R4.5 <- lm(formula = NPSNums ~ olat + olong, data = Southwest)
summary(R4.5)
# Do those originating from flights further north give higher NPS scores???
# Used reverse stepwise regression to figure out that olong/olat are the only 95% statistically significant coordinates



# Begin meaningful bins and regressions_________________________________________________________________________________________________

Southwest <- Southwest %>%
  mutate(OriginLat = case_when(olat < 29.5 ~ 'DeepSouth.Tropics',
                               olat >=29.5 & olat <29.9 ~ 'SATX.GVL.HOU',
                               olat >=29.9 & olat<39 ~ 'Middle',
                               olat >=39 & olat <45 ~ 'North',
                               olat >=45 & olat<50 ~ 'FarNorth',
                               olat >=50 ~ 'Alaska'))
table(Southwest$OriginLat)
#   Alaska    DeepSouth.Tropics      FarNorth       Middle        North      SATX.GVL.HOU 
#    58               751              427           4428         4030           550 
Bad <- lm(formula = NPSNums ~ OriginLat, Southwest)
summary(Bad) # Deep south.tropics not stat significant but does show 3 cities are all detractors so must subgroup them

olatSouth <- Southwest[(Southwest$OriginLat=="SATX.GVL.HOU"),]
unique(olatSouth$Origin.City) #San Antonio, Gainsville, Houston are all dissatisfied olat airports

# *********************************************************
Southwest <- Southwest %>%
  mutate(AgeBins = case_when(Age < 18 ~ 'Minors',
                             Age >= 18 & Age<30 ~ 'Young Adults',
                             Age >= 30 & Age<45 ~ 'Adults',
                             Age >= 45 & Age<65 ~ 'Mature Adults',
                             Age >= 65 ~ 'Seniors'))
table(Southwest$AgeBins)
#   Adults     Mature Adults     Minors     Seniors    Young Adults 
#    3248          3390           435        1670          1501 
BadAges <- lm(formula = NPSNums ~ AgeBins, Southwest)
summary(BadAges)
# It appears clear that those at the extreme ends of the spectrum are both detractors (minors and seniors)

BadSeniors<-(Southwest$OriginLat == 'SATX.GVL.HOU' & Southwest$AgeBins == 'Seniors') 
summary(BadSeniors) # 93 cases
BadMinors<-(Southwest$OriginLat == 'SATX.GVL.HOU' & Southwest$AgeBins == 'Minors') 
summary(BadMinors) # 24 cases
# I would like to take out these combined 117 rows because I think it will increase overall NPS Score
FakeSouthwest <- Southwest 
str(FakeSouthwest)
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$OriginLat != 'SATX.GVL.HOU' | FakeSouthwest$AgeBins != 'Seniors')
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$OriginLat != 'SATX.GVL.HOU' | FakeSouthwest$AgeBins != 'Minors')
dim(FakeSouthwest) # can see it shrunk from 10,244 rows to 10,127 rows...perfect!
((sum(FakeSouthwest$NPSNums == 3))-(sum(FakeSouthwest$NPSNums == 1)))/sum(FakeSouthwest$NPSNums)*100 # NPS Score for this subset is 4.61973%%
((sum(Southwest$NPSNums == 3))-(sum(Southwest$NPSNums == 1)))/sum(Southwest$NPSNums)*100 # Original NPS Score is 4.24378%
4.61973-4.24378 # An increase in NPS Score of 0.37595%

# ****************************************************************************************************************************

Southwest <- Southwest %>%
  mutate(YearlyTripBins = case_when(Flights.Per.Year < 5 ~ 'Infrequent',
                                    Flights.Per.Year >= 5 & Flights.Per.Year<15 ~ 'Occasional',
                                    Flights.Per.Year >= 15 & Flights.Per.Year<52 ~ 'Average',
                                    Flights.Per.Year >= 52 ~ 'HyperFrequent'))
table(Southwest$YearlyTripBins)
BadFlightFrequency <- lm(formula = NPSNums ~ YearlyTripBins, Southwest)
summary(BadFlightFrequency) # Those who fly 52 or more times a year (1 a week) are predicted to give detracting NPS scores while others are at least passive

FakeSouthwest <- Southwest
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$YearlyTripBins != 'HyperFrequent')
dim(FakeSouthwest) # can see it shrunk from 10,244 rows to 9961 rows...perfect!
((sum(FakeSouthwest$NPSNums == 3))-(sum(FakeSouthwest$NPSNums == 1)))/sum(FakeSouthwest$NPSNums)*100 # NPS Score for this subset is 4.884221%%
((sum(Southwest$NPSNums == 3))-(sum(Southwest$NPSNums == 1)))/sum(Southwest$NPSNums)*100 # Original NPS Score is 4.24378%
4.884221-4.24378 # An increase in NPS Score of 0.640441% but was only from 6 rows changing (might still illustrate some power though)

# ****************************************************************************************************************************

Southwest <- Southwest %>%
  mutate(ArrivalDelayBins = case_when(Arrival.Delay.in.Minutes < 6 ~ 'OnTime',
                                      Arrival.Delay.in.Minutes  >=6 ~ 'Late'))
table(Southwest$ArrivalDelayBins)
BadArrivalDelay <- lm(formula = NPSNums ~ ArrivalDelayBins, Southwest)
summary(BadArrivalDelay) # Detractors are predicted when arrival delay is over 5 minutes

FakeSouthwest <- Southwest 
dim(FakeSouthwest)
BadArrival<-(FakeSouthwest$ArrivalDelayBins == 'Late') 
summary(BadArrival) #3605  cases

FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$ArrivalDelayBins != 'Late')
dim(FakeSouthwest) # can see it shrunk from 10,244 rows to 6,639 rows...perfect!
((sum(FakeSouthwest$NPSNums == 3))-(sum(FakeSouthwest$NPSNums == 1)))/sum(FakeSouthwest$NPSNums)*100 # NPS Score for this subset is 8.459152%%
((sum(Southwest$NPSNums == 3))-(sum(Southwest$NPSNums == 1)))/sum(Southwest$NPSNums)*100 # Original NPS Score is 4.24378%
8.459152-4.24378 # An increase in NPS Score of 4.215372%

#Upon further analysis I realized that this is quite an obvious metric and is not very meaningful considering lateness will occur

# ****************************************************************************************************************************
Southwest <- Southwest %>%
  mutate(LoyaltyBins = case_when(Loyalty < -0.8 ~ 'Disloyal',
                                 Loyalty >= -0.8 & Loyalty<=0.8 ~ 'Nuetral',
                                 Loyalty >0.8 ~ 'Loyal'))
table(Southwest$LoyaltyBins)
BadLoyalty <- lm(formula = NPSNums ~ LoyaltyBins, Southwest)
summary(BadLoyalty) # Those who fly on our airline least in proportion to their total yearly flights are disloyal
FakeSouthwest <- Southwest 
dim(FakeSouthwest)
NoLoyalty <- (FakeSouthwest$LoyaltyBins == 'Disloyal') 
summary(NoLoyalty) # 1448 cases
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$LoyaltyBins != 'Disloyal')
dim(FakeSouthwest) # can see it shrunk from 10,244 rows to 8,796 rows...perfect!
((sum(FakeSouthwest$NPSNums == 3))-(sum(FakeSouthwest$NPSNums == 1)))/sum(FakeSouthwest$NPSNums)*100 # NPS Score for this subset is 5.672922%%
((sum(Southwest$NPSNums == 3))-(sum(Southwest$NPSNums == 1)))/sum(Southwest$NPSNums)*100 # Original NPS Score is 4.24378%
5.672922-4.24378 # An increase in NPS Score of 1.429142%
# Although this change in NPS Score looks promising, I am disspleased with predictive fit in the model when putting it into my final regression model

# ****************************************************************************************************************************
table(Southwest$Airline.Status)
# Blue     Gold    Platinum   Silver 
# 6978      858      320      2088 
badStatus <- lm(formula=NPSNums ~ Airline.Status, Southwest)
summary(badStatus)

FakeSouthwest <- Southwest 
dim(FakeSouthwest)
BlueStatus <- (FakeSouthwest$Airline.Status == 'Blue')
summary(BlueStatus) #6978 cases seems like a lot at +68%
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$Airline.Status != 'Blue')
dim(FakeSouthwest)
((sum(FakeSouthwest$NPSNums == 3))-(sum(FakeSouthwest$NPSNums == 1)))/sum(FakeSouthwest$NPSNums)*100 # NPS Score for this subset is 4.61973%%
((sum(Southwest$NPSNums == 3))-(sum(Southwest$NPSNums == 1)))/sum(Southwest$NPSNums)*100 # Original NPS Score is 4.24378%
18.3602-4.24378 # An increase in NPS Score of 14.11642%

# ****************************************************************************************************************************
Southwest <- Southwest %>%
  mutate(DepartureTimeBins = case_when(Scheduled.Departure.Hour < 5 | Scheduled.Departure.Hour>=22 ~ 'LateNight', #Before 5am or after 9pm
                                       Scheduled.Departure.Hour >= 5 & Scheduled.Departure.Hour<12 ~ 'Morning',
                                       Scheduled.Departure.Hour >=12 & Scheduled.Departure.Hour<17 ~ 'Midday',
                                       Scheduled.Departure.Hour >=17 & Scheduled.Departure.Hour<22 ~ 'Evening'))
table(Southwest$DepartureTimeBins)
#  Evening    LateNight    Midday    Morning 
#   2660         244        3068      4272 
BadHour <- lm(formula = NPSNums ~ DepartureTimeBins, Southwest)
summary(BadHour) # Those who fly on our airline least in proportion to their total yearly flights are disloyal
# This model does not show any statistical significance aor explanation in the variability of the predicted upon variable

# ****************************************************************************************************************************
# Test to see if we should remove any airlines from the data
summary(lm(formula = NPSNums ~ Partner.Code, Southwest))
sum(Southwest$Partner.Code =='EV') # Shows strongest stat significance of any airline and also associated with detractors while all others are at least passive (NPS 2)

# ****************************************************************************************************************************
summary(Southwest$Flight.Distance)
Southwest <- Southwest %>%
  mutate(TripDistance = case_when(Flight.Distance<600 ~ 'Short',
                                  Flight.Distance >=600~ 'Long'))
table(Southwest$TripDistance)
#  Evening    LateNight    Midday    Morning 
#   2660         244        3068      4272 
TripDist <- lm(formula = NPSNums ~ TripDistance, Southwest)
summary(TripDist)
# No impact or meaning derived from this based on summary(TripDist) significance stats and relation to NPS bin 2 Passive vs. Detractor

# ****************************************************************************************************************************
summary(Southwest$Total.Freq.Flyer.Accts == 3)
Southwest <- Southwest %>%
  mutate(FlyerAcctBins = case_when(Total.Freq.Flyer.Accts == 0 ~ 'None',
                                   Total.Freq.Flyer.Accts == 1 ~ 'One',
                                   Total.Freq.Flyer.Accts == 2 | Total.Freq.Flyer.Accts == 3 | Total.Freq.Flyer.Accts == 4 ~ 'Some',
                                   Total.Freq.Flyer.Accts >= 5 ~ 'Multiple'))
table(Southwest$FlyerAcctBins)
#  Multiple     None       One     Some 
#     296       5284      2048     2616 
FlyerAccts <- lm(formula = NPSNums ~ FlyerAcctBins, Southwest)
summary(FlyerAccts)
#Those with multiple Frequent Flyer Accounts (5+ are likely to be detractors)

# ****************************************************************************************************************************
Southwest$Type.of.Travel <- as.character(Southwest$Type.of.Travel)
str(Southwest)
TravelType <- lm(formula = NPSNums ~ Type.of.Travel, Southwest)
summary(TravelType) # Personal Travel is when people are less satsfied 
# This means maybe spending less marketingon vacationers and more on business and mileage consumers. Maybe increase spending on amenities associated with vacationing!!!!


### ***************************** Final Impact on the dataset ***************************
# ****************************************************************************************************************************
FakeSouthwest <- Southwest 
dim(FakeSouthwest)
# I would like to take out any rows under these classifications because I think it will increase overall NPS Score for Southwest airlines

FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$OriginLat != 'SATX.GVL.HOU')
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$AgeBins != 'Minors' & FakeSouthwest$AgeBins != 'Seniors')
# FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$YearlyTripBins != 'HyperFrequent')
#       -- Became insignificant and did not fit into the model when test final regressions in stepwise fashion on impact of coefficients and R^2
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$Airline.Status != 'Blue')
# FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$Partner.Code != 'EV') #Removes an airline that seems to mainly have detractors
#       -- Will not use status because it is not justified in the final regression MYMODEL by substantial increase in stat significance or R^2. Also locations are widespread to many regions
# FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$FlyerAcctBins != 'Multiple') # Removes those with 5+ frequent flyer accounts (only 11 more rows removed than EV)
#       -- Will not use FlyerAcctBins becuse low predictability compared to other bins I found in the regression
FakeSouthwest <- filter(FakeSouthwest, FakeSouthwest$Type.of.Travel != 'Personal Travel')

dim(FakeSouthwest)
((sum(FakeSouthwest$NPSNums == 3))-(sum(FakeSouthwest$NPSNums == 1)))/sum(FakeSouthwest$NPSNums)*100 # NPS Score for this subset is 24.15051%%
((sum(Southwest$NPSNums == 3))-(sum(Southwest$NPSNums == 1)))/sum(Southwest$NPSNums)*100 # Original NPS Score is 4.24378%
# 11.28569-4.24378 # 7.04191% increase in total NPS score from original cleaned data. This seems great as it is quite a large increase in NPS Score when removing these detractors
# 21.24963-4.24378 # 17.00585% increase in total NPS score from original cleaned data.
#                   This is when removing the Blue Status (a lot of rows erased so now only 2672 left). Very helpful in increasing score but not the best 
#                  for statistical inference because we are erasing so many rows. Blue is associated with lower NPS, but is it best to remove?
# 21.83908-4.24378 # 17.5953% increase in total NPS when removing EV Partner airline
# 21.89176-4.24378 #17.64798% increase in NPS score when removing those with multiple freq flyer accounts (0nly0.052% stepwise increase)
# 24.82924-4.24378 # 20.58546% increase in NPS when removing Personal Travel type from the dataset. Change marketing/spending for this subgroup
# FINAL CALCULATION BASED on stat significance of coefficients and R^2 in relation to number of variables included
24.15051-4.243784 # 19.90673 increase in NPS compared to a cleaned version of Southwest dataset
# ****************************************************************************************************************************
MYMODEL <- lm(formula = NPSNums ~ OriginLat+AgeBins+Airline.Status+Type.of.Travel, Southwest)
summary(MYMODEL)
plot(MYMODEL)
# I like the statistical significance of the necessary coefficients. I even see stat significance of 5% in the tropic olat region.
# 2.26158 -0.70422-0.62092-0.30788 modeled for worst optimum:Seniors, HyperFrequent, LateArrival, from SATX.GVL.HOU but do not use because not including Arrival Delays: Hyperfrequent
# After I took out ArrivalDelay, I added LoyaltyBins and was disspleased because Loyal coefficient became negative and neutral was not stat significant so drop loyalty from model as well
2.339719-0.519089-0.216095-0.126000-0.205048-0.802918 # 0.470569 =  shows the worst case for each different bin

# Below is OriginLat+AgeBins+Airline.Status+Type.of.Travel (best stat significance figures overall) 
2.49848-0.56265-0.22190-0.80936 # 0.90457 =  shows the worst case for each different bin

# ****** HOW R^2 changed in stepwise multiple regression analysis *******
# OriginLat = 0.01458
# OriginLat+AgeBins = 0.1238
# OriginLat+AgeBins+YearlyTripBins = 0.1353
# OriginLat+AgeBins+YearlyTripBins+Airline.Status = 0.2021
# OriginLat+AgeBins+YearlyTripBins+Airline.Status+Partner.Code = 0.2105
# OriginLat+AgeBins+YearlyTripBins+Airline.Status+Partner.Code+FlyerAcctBins = 0.2109 # This is concerningly low so I will take out
# OriginLat+AgeBins+YearlyTripBins+Airline.Status+Partner.Code+FlyerAcctBins+Type.of.Travel = 0.3673
# OriginLat+AgeBins+YearlyTripBins+Airline.Status+Partner.Code+Type.of.Travel = 0.3677

#______ MY MODEL _______
####### OriginLat+AgeBins+Airline.Status+Type.of.Travel = 0.3566 
# I am most pleased with this model. Other metrics may be overcomplicating and unnecessarily removing rows. This also shows the best overall p-values ########
# ***************************************************************************************************************************


#__________ Association Rules ___________#
table(Southwest$OriginLat, Southwest$NPSNums)
table(Southwest$AgeBins, Southwest$NPSNums)
table(Southwest$YearlyTripBins, Southwest$NPSNums)
table(Southwest$Airline.Status, Southwest$NPSNums)
table(Southwest$Partner.Code, Southwest$NPSNums)
table(Southwest$Type.of.Travel, Southwest$NPSNums)
table(Southwest$NPSNums)
#1___________________________________________________________
# Create a dataset to test
SouthwestAs <- data.frame(Ages = Southwest$AgeBins, TravelType = as.factor(Southwest$Type.of.Travel), RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs)

AgesXtravelType <- as(SouthwestAs, "transactions")
inspect(AgesXTravelType)
itemFrequency(AgesXtravelType)

ruleset <- apriori(AgesXtravelType,
                   parameter = list(support=0.005, confidence=0.5),
                   appearance = list(default = "lhs", rhs = ("RecNPS=1")))
inspect(ruleset)
inspectDT(ruleset)
summary(ruleset)
goodrules <- ruleset[quality(ruleset)$lift > 2.2]
inspectDT(goodrules)

#2________________________________________________________________
SouthwestAs2 <- data.frame(FlightOrigin = Southwest$OriginLat, TravelType = as.factor(Southwest$Type.of.Travel), 
                           RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs2)

OlatXtravelType <- as(SouthwestAs2, "transactions")
inspect(OlatXtravelType)
itemFrequency(OlatXtravelType)

ruleset2 <- apriori(OlatXtravelType,
                   parameter = list(support=0.005, confidence=0.5),
                   appearance = list(default = "lhs", rhs = ("RecNPS=1")))
inspect(ruleset2)
inspectDT(ruleset2)
summary(ruleset2)
goodrules2 <- ruleset2[quality(ruleset2)$lift > 2.1]
inspectDT(goodrules2)

#3______________________Seniors from SATX.GVL.HOU_______________________________________
SouthwestAs3 <- data.frame(FlightOrigin = Southwest$OriginLat, Ages = Southwest$AgeBins, 
                           RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs3)

OlatXages <- as(SouthwestAs3, "transactions")
inspect(OlatXages)
itemFrequency(OlatXages)

ruleset3 <- apriori(OlatXages,
                    parameter = list(support=0.005, confidence=0.5),
                    appearance = list(default = "lhs", rhs = ("RecNPS=1")))
inspect(ruleset3)
inspectDT(ruleset3)
summary(ruleset3)
goodrules3 <- ruleset3[quality(ruleset3)$lift > 1.9]
inspectDT(goodrules3)

#4________________________________________________________________
SouthwestAs4 <- data.frame(FlightOrigin = Southwest$OriginLat, TravelType = as.factor(Southwest$Airline.Status), 
                           RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs4)

OlatXstatus <- as(SouthwestAs4, "transactions")
inspect(OlatXstatus)
itemFrequency(OlatXstatus)

ruleset4 <- apriori(OlatXstatus,
                    parameter = list(support=0.005, confidence=0.5),
                    appearance = list(default = "lhs", rhs = ("RecNPS=1")))
inspect(ruleset4)
inspectDT(ruleset4)
summary(ruleset4)
goodrules4 <- ruleset4[quality(ruleset4)$lift > 1]
inspectDT(goodrules4)

#5________________________________________________________________OriginLat+AgeBins+Airline.Status+Type.of.Travel
SouthwestAs5 <- data.frame(FlightOrigin = Southwest$OriginLat, Ages = Southwest$AgeBins, Status = Southwest$Airline.Status,
                           TravelType = as.factor(Southwest$Type.of.Travel), RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs5)

myMod <- as(SouthwestAs5, "transactions")
inspect(myMod)
itemFrequency(myMod)

ruleset5 <- apriori(myMod,
                    parameter = list(support=0.005, confidence=0.5),
                    appearance = list(default = "lhs", rhs = ("RecNPS=1")))
inspect(ruleset5)
inspectDT(ruleset5)
summary(ruleset5)
goodrules5 <- ruleset5[quality(ruleset5)$lift > 2.6]
inspectDT(goodrules5)
# This helps my regression modelling further proves a lot of the findings I showed above

#6__________________+freq flyer______________________________________________OriginLat+AgeBins+Airline.Status+Type.of.Travel
SouthwestAs6 <- data.frame(FlightOrigin = Southwest$OriginLat, Ages = Southwest$AgeBins, Status = Southwest$Airline.Status,
                           TravelType = as.factor(Southwest$Type.of.Travel),
                           FreqFlyers = Southwest$FlyerAcctBins, RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs6)

myMod2 <- as(SouthwestAs6, "transactions")
inspect(myMod2)
itemFrequency(myMod2)

ruleset6 <- apriori(myMod2,
                    parameter = list(support=0.005, confidence=0.5),
                    appearance = list(default = "lhs", rhs = ("RecNPS=1")))
inspect(ruleset6)
inspectDT(ruleset6)
summary(ruleset6)
goodrules6 <- ruleset6[quality(ruleset6)$lift > 2.8]
inspectDT(goodrules6)
# This model points away from FreqFlyers being useful because it does not have stat significance, as shown in regressions
# It occurs so frequently that it cannot be taken seriously as a variable so helps my reasoning for the MYMODEL final regression for leaving it out

#7________________________________________________________________OriginLat+AgeBins+Airline.Status+Type.of.Travel
SouthwestAs7 <- data.frame(FlightOrigin = Southwest$OriginLat, Ages = Southwest$AgeBins, Status = Southwest$Airline.Status,
                           TravelType = as.factor(Southwest$Type.of.Travel), RecNPS = as.factor(Southwest$NPSNums))
str(SouthwestAs7)

myMod3 <- as(SouthwestAs7, "transactions")
inspect(myMod3)
itemFrequency(myMod3)

ruleset7 <- apriori(myMod3,
                    parameter = list(support=0.005, confidence=0.5),
                    appearance = list(default = "lhs", rhs = ("RecNPS=3"))) # Change to view the positive recommedations and their associations
inspect(ruleset7)
inspectDT(ruleset7)
summary(ruleset7)
goodrules7 <- ruleset7[quality(ruleset7)$lift > 1.85]
inspectDT(goodrules7)
# This helps my regression modelling further proves a lot of the findings I showed above on the Promoter side


# Support Vector Machine Analysis
library(caret)
Southwest <- Southwest %>%
  mutate(DetractorNPS = case_when(Likelihood.to.recommend <= 6 ~ 'Detractor',
                             Likelihood.to.recommend >6  ~ 'Non-Detractor')) 
# Above created a new bin group to just analyze where the harmful detractors are

# SVMA ____________________________________________________________________________________________________
svm <- data.frame(Age = Southwest$AgeBins, Olat = Southwest$OriginLat, 
                  TravelType = Southwest$Type.of.Travel, Status = Southwest$Airline.Status,
                  RecNPS = as.factor(Southwest$DetractorNPS))

#create the train (2/3) and test (1/3) sets
trainList <- createDataPartition(y= svm$RecNPS, p=.7, list=FALSE)
trainData <- svm[trainList,]
testData <- svm[-trainList,]

#Run the model
svmOutput <- ksvm(RecNPS ~., data=trainData, kernel="rbfdot", kpar="automatic",
            C=10, cross=3, prob.model=TRUE) # Usinig a larger C to 
svmOutput
hist(alpha(svmOutput)[[1]],
     main = "Support Vector with C=10", 
     xlab = "Support Vector Values")
# The many numbers  to the right side of the histogram mean it's  hard for the model to predict the zone properly
# The numbers on the left had side of the histogram are too simple and easy to predict so don't offer any use in modeling
# I alter this with a different 'C=  '
# Ideally it would be more balanced but R shuts down when increasing C too much
# C=100 is a good compromise
svmPred <- predict(svmOutput, testData)
svmPred
confusionMatrix(testData$RecNPS, svmPred)

# Visualizing a .5 Threshold of the best predictive svm analysis
tp <- c(0, 0.6986, 1 ) # The Sensitivity of the model acts as the true positive rate
#1-0.8434 # 0.1566
fp <- c(0 ,0.1566, 1) # The Specificity shows the false negative rate so subtracting by 1 gives is the false positive rate
plot(fp,tp, pch = 19, col = "red", xlab = "False Positive Rate", 
     ylab =  "True Positive Rate", main = )
lines(fp,tp, col = "red")
xadj <- c(.02, 0, -.04)
yadj <- c(0.02,.03, -.05)
text(x = fp + xadj, y = tp + yadj, 
     labels = c("Threshold\n1", "Threshold\n.5", "Threshold\n0"))
abline(v = 0, lty = 2)
abline(h = 1, lty = 2)
text(.13, .96, labels = "Ideal Model")
points(0,1, pch = "O", cex = 1.5)


# Phase 4: Mapping Low Satisfaction Routes
#Southwest <- Southwest %>%
  #mutate(OriginLat = case_when(olat < 29.5 ~ 'DeepSouth.Tropics',
#                               olat >=29.5 & olat <29.9 ~ 'SATX.GVL.HOU',
#                               olat >=29.9 & olat<39 ~ 'Middle',
#                               olat >=39 & olat <45 ~ 'North',
#                               olat >=45 & olat<50 ~ 'FarNorth',
#                               olat >=50 ~ 'Alaska'))
#table(Southwest$OriginLat)
#Bad <- lm(formula = NPSNums ~ OriginLat, Southwest)
#summary(Bad)

olat3cities <- Southwest[(Southwest$OriginLat=="SATX.GVL.HOU"),]
unique(olat3cities$Origin.City) #San Antonio, Gainsville, Houston are all dissatisfied olat airports


olat3cities <- subset(olat3cities, select=-c(freeText))
View(olat3cities)
badAirports <- na.omit(olat3cities)
count(badAirports) # 550 rows of bad origin airports

table(badAirports$NPSNums)
#  1   2   3 
# 262 200  88 
(88-262)/550 # -31.6% Recommendation Scores


library(maps)
install.packages("ggrepel")
library(ggrepel)
Routes <- data.frame(Southwest$olong,Southwest$olat,Southwest$dlong,Southwest$dlat)
#View(Routes)
usMap <- borders("state", colour="grey", fill="white")
ggplot() + usMap
BadRouteMap <- ggplot() + usMap +
  geom_curve(data=badAirports,
             aes(x=olong, y=olat, xend=dlong, yend=dlat),
             col="black",
             size=.3,
             curvature=0.2) +
  geom_point(data=badAirports,
             aes(x=dlong, y=dlat), 
             colour="orange",
             size=.75) +
  geom_point(data=badAirports,
             aes(x=olong, y=olat), 
             colour="red",
             size=3.5) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=14)) +
  ggtitle("Detractor Routes: Originating in San Antonio, Houston, and Gainsville")
BadRouteMap


# Citation
# Threshold Visualization : https://www.hranalytics101.com/how-to-assess-model-accuracy-the-basics/
