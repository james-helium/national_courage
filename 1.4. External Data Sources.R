#This file is to add external control datasets according to our pre-registration
#Note: the National_Data this file imports already has the dependent measures manually added in;
#the dependent measures (apart from IDV from WVS) are all from "indexes" - they are available only as digital web-shots, which requires manual add-ins
#Pre-registered controls:
#GDP per capita from WB, UN
#Education from OECD, UN, and IPUMS
#Urbanisation from UN, WB, OECD, IPUMS


##################################
#LIBRARIES AND FILES
##################################

#GDP Data
UN_GDP_percap <- read.csv("External Data/UN/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3469501.csv")
UN_GDP_PPP_percap <- read.csv("External Data/UN/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_3469401.csv")
WB_GDP_nd_percapita <- read.csv("External Data/WB/SYB64_230_202110_GDP and GDP Per Capita.csv")
OECD_GDP_percap <- read.csv("External Data/OECD/OECD_GDP_percap.csv")

#Education Data
OECD_Adult_Edu_lvl <- read.csv("External Data/OECD/OECD_Adult_Edu_lvl.csv")
UN_Edu_PrimComplRate <- read.csv("External Data/UN/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_3469778.csv")
UN_Edu_PrimEnrolRate <- read.csv("External Data/UN/API_SE.PRM.ENRR_DS2_en_csv_v2_3469664.csv")
UN_Edu_SecnEnrolRate <- read.csv("External Data/UN/API_SE.SEC.ENRR_DS2_en_csv_v2_3471030.csv")
UN_Edu_TertEnrolRate <- read.csv("External Data/UN/API_SE.TER.ENRR_DS2_en_csv_v2_3479591.csv")
WB_Educ_enrollments <- read.csv("External Data/WB/SYB64_309_202110_Education.csv")
#missing IPUMS

#Urbanisation Data
UN_Metro_Population <- read.csv("External Data/UN/API_EN.URB.MCTY.TL.ZS_DS2_en_csv_v2_3481982.csv")
UN_Urbanisation <- read.csv("External Data/UN/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_3471498.csv")
WB_Urban_Pop_Growth <- read.csv("External Data/WB/SYB61_253_Population Growth Rates in Urban areas and Capital cities.csv")
OECD_Urban <- read.csv("External Data/OECD/OECD_Urbanisation.csv")
#Missing IPUMS

#the Edu and Urb data from IPUMS
IPUMS_Edu_Urb <- read.csv('External Data/IPUMS-I/ipumsi_00003.csv')

#also import our national data for country name references
National_Data <- read.csv("Data/National_Data.csv")[,-1]


#################################
#STANDARDIZING COUNTRY NAMES
#################################

#UN_Edu_PrimComplRate - following this we will just use the country codes
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Czech Republic'] <- 'Czechia'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Korea'] <- 'South Korea'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Slovak Republic'] <- 'Slovakia'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Egypt, Arab Rep.' ] <- 'Egypt'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Hong Kong SAR, China' ] <- 'Hong Kong'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Iran, Islamic Rep.' ] <- 'Iran'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Korea, Rep.' ] <- 'South Korea'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Russian Federation' ] <- 'Russia'
UN_Edu_PrimComplRate$Country.Name[UN_Edu_PrimComplRate$Country.Name == 'Venezuela, RB' ] <- 'Venezuela'

#add country codes into the National_Data for easier matching later
colnames(National_Data)[1] <- 'Country.Name'
UN_Edu_PrimComplRate <- UN_Edu_PrimComplRate[order(UN_Edu_PrimComplRate$Country.Name),] #have them in the standard order
Country_Code <- UN_Edu_PrimComplRate[,2][UN_Edu_PrimComplRate$Country.Name %in% National_Data$Country.Name]
Country_Code <- c(Country_Code[1:69], 'TWN', Country_Code[70:79])#add Taiwan manually, at the right place
National_Data <- data.frame(National_Data, Country.Code = Country_Code)
National_Data <- National_Data[,order(colnames(National_Data))]

#WB's data needs name changing as well
#first clean up the messy data base structure
colnames(WB_Educ_enrollments) <- WB_Educ_enrollments[1,]
colnames(WB_Educ_enrollments)[2] <- 'Region.Name'
WB_Educ_enrollments <- WB_Educ_enrollments[-1,-1]
#then change the names that don't match
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'China, Hong Kong SAR' ] <- 'Hong Kong'
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'Iran (Islamic Republic of)' ] <- 'Iran'
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'Republic of Korea' ] <- 'South Korea'
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'Russian Federation' ] <- 'Russia'
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'United States of America' ] <- 'United States'
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'Venezuela (Boliv. Rep. of)' ] <- 'Venezuela'
WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Region.Name == 'Viet Nam' ] <- 'Vietnam'

#same with the other WB file: GDP
#first clean up the messy data base structure
colnames(WB_GDP_nd_percapita) <- WB_GDP_nd_percapita[1,]
colnames(WB_GDP_nd_percapita)[2] <- 'Region.Name'
WB_GDP_nd_percapita <- WB_GDP_nd_percapita[-1,-1]
#then change the names that don't match
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'China, Hong Kong SAR' ] <- 'Hong Kong'
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'Iran (Islamic Republic of)' ] <- 'Iran'
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'Republic of Korea' ] <- 'South Korea'
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'Russian Federation' ] <- 'Russia'
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'United States of America' ] <- 'United States'
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'Venezuela (Boliv. Rep. of)' ] <- 'Venezuela'
WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Region.Name == 'Viet Nam' ] <- 'Vietnam'

#now do the same with the urbanisation data; didn't write a function because there's only these three
#first clean up the messy data base structure
colnames(WB_Urban_Pop_Growth) <- WB_Urban_Pop_Growth[1,]
colnames(WB_Urban_Pop_Growth)[2] <- 'Region.Name'
WB_Urban_Pop_Growth <- WB_Urban_Pop_Growth[-1,-1]
#then change the names that don't match
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'China, Hong Kong SAR' ] <- 'Hong Kong'
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'Iran (Islamic Republic of)' ] <- 'Iran'
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'Republic of Korea' ] <- 'South Korea'
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'Russian Federation' ] <- 'Russia'
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'United States of America' ] <- 'United States'
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'Venezuela (Boliv. Rep. of)' ] <- 'Venezuela'
WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Region.Name == 'Viet Nam' ] <- 'Vietnam'

#IPUMS country coding is through numbers and they didn't include labels in the csv, so we get it from script 1.3 "country_labels"
IPUMS_Data <- read_ipums_micro(read_ipums_ddi("External Data/IPUMS-I/ipumsi_00002.xml")) #file not use elsewhere in this script
country_labels <- data.frame(val_labels(IPUMS_Data$COUNTRY)) #get labels 
rm(IPUMS_Data) #since we don't need it anymore
country_labels <- data.frame(country_labels, labels(country_labels))[,-3]
colnames(country_labels) <- c('code', 'name')
#make the labels standardised to our country names
country_labels$name[country_labels$name == 'Slovak Republic'] <- 'Slovakia'
#replace the country coding to actual country names takes way too long, so we do it only after extraction.
#here, we write a function to help with later
country_name_ipums <- function(frame){
  frame$Country <- as.integer(frame$Country)
  for(i in 1:nrow(frame)){
    frame$Country[i] <- country_labels$name[country_labels$code == frame$Country[i]]
    print(paste(i / nrow(frame) * 100))
  }
  frame
}


##################################
#EXTRACTION
##################################

############################ Extracting economic controls (WB & UN's GDP per cap)
X.GDP.UN <- data.frame(Country = UN_GDP_percap$Country.Code,
                       X.GDP.percap.UN.16_20 = (UN_GDP_percap$X2016 + UN_GDP_percap$X2017 + UN_GDP_percap$X2018 + 
                                                               UN_GDP_percap$X2019 + UN_GDP_percap$X2020) / 5,
                       X.GDP.percap.UN.PPP.16_20 = (UN_GDP_PPP_percap$X2016 + UN_GDP_PPP_percap$X2017 + 
                                                            UN_GDP_PPP_percap$X2018 + UN_GDP_PPP_percap$X2019 + 
                                                            UN_GDP_PPP_percap$X2020) / 5)
#ofc the world bank recorded their data in strings... and added commas into them... which is why we need the as.numeric and gsub functions 
WB_GDP_percap_17 <- as.numeric(gsub(",","",WB_GDP_nd_percapita[WB_GDP_nd_percapita$Series == "GDP per capita (US dollars)" & 
                                                                 WB_GDP_nd_percapita$Year == 2017, 4]))
WB_GDP_percap_18 <- as.numeric(gsub(",","",WB_GDP_nd_percapita[WB_GDP_nd_percapita$Series == "GDP per capita (US dollars)" & 
                                                                 WB_GDP_nd_percapita$Year == 2018, 4]))
WB_GDP_percap_19 <- as.numeric(gsub(",","",WB_GDP_nd_percapita[WB_GDP_nd_percapita$Series == "GDP per capita (US dollars)" & 
                                                                 WB_GDP_nd_percapita$Year == 2019, 4]))
X.GDP.WB <- data.frame(Country = WB_GDP_nd_percapita$Region.Name[WB_GDP_nd_percapita$Series == "GDP per capita (US dollars)" & 
                                                                        WB_GDP_nd_percapita$Year == 2017],
                        X.GDP.percap.WB.17_19 = (WB_GDP_percap_17 + WB_GDP_percap_18 + WB_GDP_percap_19) / 3)


############################### Extracting educational controls from all four sources
#UN
X.EDU.UN <- data.frame(Country = UN_Edu_PrimComplRate$Country.Code,
                       X.EDU.UN.Pri.Comp.Rate.18 = UN_Edu_PrimComplRate$X2018,
                       X.EDU.UN.Pri.Enrl.Rate.18 = UN_Edu_PrimEnrolRate$X2018,
                       X.EDU.UN.Sec.Enrl.Rate.18 = UN_Edu_SecnEnrolRate$X2018,
                       X.EDU.UN.Ter.Enrl.Rate.18 = UN_Edu_TertEnrolRate$X2018)
#WB
WB_Educ_enrollments$Value <- as.numeric(gsub(",","", WB_Educ_enrollments$Value))
X.EDU.WB.Pri.Enrl.Rate.15 <- data.frame(
  Country = WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Series == "Gross enrollment ratio - Primary (male)" & WB_Educ_enrollments$Year == 2015],
  X.EDU.WB.Pri.Enrl.Rate.15 = (WB_Educ_enrollments$Value[WB_Educ_enrollments$Series == "Gross enrollment ratio - Primary (male)" & WB_Educ_enrollments$Year == 2015] + 
                                 WB_Educ_enrollments$Value[WB_Educ_enrollments$Series == "Gross enrollment ratio - Primary (female)" & WB_Educ_enrollments$Year == 2015]) / 2)
X.EDU.WB.Sec.Enrl.Rate.15 <- data.frame(
  Country = WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Series == "Gross enrollment ratio - Secondary (male)" & WB_Educ_enrollments$Year == 2015],
  X.EDU.WB.Sec.Enrl.Rate.15 = (WB_Educ_enrollments$Value[WB_Educ_enrollments$Series == "Gross enrollment ratio - Secondary (male)" & WB_Educ_enrollments$Year == 2015] + 
                                 WB_Educ_enrollments$Value[WB_Educ_enrollments$Series == "Gross enrollment ratio - Secondary (female)" & WB_Educ_enrollments$Year == 2015]) / 2)
X.EDU.WB.USc.Enrl.Rate.15 <- data.frame(
  Country = WB_Educ_enrollments$Region.Name[WB_Educ_enrollments$Series == "Gross enrollment ratio - Upper secondary level (male)" & WB_Educ_enrollments$Year == 2015],
  X.EDU.WB.USc.Enrl.Rate.15 = (WB_Educ_enrollments$Value[WB_Educ_enrollments$Series == "Gross enrollment ratio - Upper secondary level (male)" & WB_Educ_enrollments$Year == 2015] + 
                                 WB_Educ_enrollments$Value[WB_Educ_enrollments$Series == "Gross enrollment ratio - Upper secondary level (female)" & WB_Educ_enrollments$Year == 2015]) / 2)
#OECD
X.EDU.OECD <- data.frame(Country = OECD_Adult_Edu_lvl$LOCATION[OECD_Adult_Edu_lvl$SUBJECT == 'TRY' & OECD_Adult_Edu_lvl$TIME == 2017],
                         X.EDU.OECD.Ter.Attain.Rate.17 = OECD_Adult_Edu_lvl$Value[OECD_Adult_Edu_lvl$SUBJECT == 'TRY' & 
                                                                                    OECD_Adult_Edu_lvl$TIME == 2017])
#IPUMS - we just extract the percentage of uni completed as marked in education attainment (val == 4) and literacy (val == 2)
IPUMS_countries <- unique(IPUMS_Edu_Urb$COUNTRY)
X.EDU.IPUMS.Ter.Attain.Rate <- c()
X.EDU.IPUMS.Literacy <- c()
for(i in 1:length(IPUMS_countries)){
  country <- IPUMS_countries[i]
  this_country <- IPUMS_Edu_Urb[IPUMS_Edu_Urb$COUNTRY == country,]
  ter.attain.rate <- nrow(this_country[this_country$EDATTAIN == 4,]) / nrow(this_country)
  literacy <- nrow(this_country[this_country$LIT == 2,]) / nrow(this_country)
  X.EDU.IPUMS.Ter.Attain.Rate <- c(X.EDU.IPUMS.Ter.Attain.Rate, ter.attain.rate)
  X.EDU.IPUMS.Literacy <- c(X.EDU.IPUMS.Literacy, literacy)
  print(paste(i, 'out of', length(IPUMS_countries), 'literacy =', literacy))
}
X.EDU.IPUMS <- data.frame(Country = IPUMS_countries, X.EDU.IPUMS.Ter.Attain.Rate, X.EDU.IPUMS.Literacy)
#remember to change the country names to our standardised names
X.EDU.IPUMS <- country_name_ipums(X.EDU.IPUMS)


################################# Finally, extracting urbanization from all four
#UN
X.URB.UN <- data.frame(Country = UN_Urbanisation$Country.Code,
                       X.URB.UN.Urban.Pop.17_20 = (UN_Urbanisation$X2016 + UN_Urbanisation$X2017 + UN_Urbanisation$X2018 +
                                                     UN_Urbanisation$X2019 + UN_Urbanisation$X2020) / 5,
                       X.URB.UN.Metro.Pop.17_20 = (UN_Metro_Population$X2016 + UN_Metro_Population$X2017 + UN_Metro_Population$X2018 +
                                                     UN_Metro_Population$X2019 + UN_Metro_Population$X2020) / 5)
#WB
WB_Urban_Pop_Growth$Value <- as.numeric(gsub(",","", WB_Urban_Pop_Growth$Value))
X.URB.WB <- data.frame(
  Country = WB_Urban_Pop_Growth$Region.Name[WB_Urban_Pop_Growth$Year == 2018 & 
                                              WB_Urban_Pop_Growth$Series == "Urban population (percent)"],
  X.URB.WB.Urban.Pop.18 = WB_Urban_Pop_Growth$Value[WB_Urban_Pop_Growth$Year == 2018 & 
                                                      WB_Urban_Pop_Growth$Series == "Urban population (percent)"])
#OECD
X.URB.OECD <- data.frame(Country = OECD_Urban$LOCATION, X.URB.OECD.Urban.Pop.14 = OECD_Urban$Value)
#IPUMS - find the percentage of those in urban in each country (val == 2)
X.URB.IPUMS <- c()
for(i in 1:length(IPUMS_countries)){
  country <- IPUMS_countries[i]
  this_country <- IPUMS_Edu_Urb[IPUMS_Edu_Urb$COUNTRY == country,]
  urban.rate <- nrow(this_country[this_country$URBAN == 2,]) / nrow(this_country)
  X.URB.IPUMS <- c(X.URB.IPUMS, urban.rate)
  print(paste(i, 'out of', length(IPUMS_countries), 'urbanisation =', urban.rate))
}
X.URB.IPUMS <- data.frame(Country = IPUMS_countries, X.URB.IPUMS)
X.URB.IPUMS <- country_name_ipums(X.URB.IPUMS)


####################################
#ADD TO NATIONAL_DATA
###################################

#write a function to add things into national_data, since there are so many of them
add_extractions <- function(extraction){
  #create an empty frame to contain our data
  country_data <- data.frame(matrix(data = NA, nrow = nrow(National_Data), ncol = ncol(extraction) - 1))
  colnames(country_data) <- c(colnames(extraction[,-1]))
  #see if the extraction's country is code or name
  if(nchar(extraction$Country[1]) == 3){code <- T}else{code <- F}
  #look at each country in our list
  for(i in 1:nrow(country_data)){
    country <- ifelse(code, National_Data$Country.Code[i], National_Data$Country.Name[i])
    if(country %in% extraction$Country){
      country_data[i,] <- extraction[extraction$Country == country,-1]
    } else {country_data[i,-1:-2] <- NA}
    print(paste(i, 'out of', nrow(country_data),'countries done'))
  }
  #finally, add the extraction into the Main National Data 
  National_Data <- data.frame(National_Data, country_data)
  #make sure that our col name is right
  if(colnames(National_Data)[ncol(National_Data)] == 'country_data'){
    colnames(National_Data)[ncol(National_Data)] <- colnames(extraction)[ncol(extraction)]
  }
  #output
  National_Data
}

#Start! First, economic data
National_Data <- add_extractions(X.GDP.UN)
National_Data <- add_extractions(X.GDP.WB)
#education data
National_Data <- add_extractions(X.EDU.IPUMS)
National_Data <- add_extractions(X.EDU.OECD)
National_Data <- add_extractions(X.EDU.UN)
National_Data <- add_extractions(X.EDU.WB.Pri.Enrl.Rate.15)
National_Data <- add_extractions(X.EDU.WB.Sec.Enrl.Rate.15)
National_Data <- add_extractions(X.EDU.WB.USc.Enrl.Rate.15)
#urbanization data
National_Data <- add_extractions(X.URB.IPUMS)
National_Data <- add_extractions(X.URB.OECD)
National_Data <- add_extractions(X.URB.UN)
National_Data <- add_extractions(X.URB.WB)


####################################
#SAVE FILES
###################################

write.csv(National_Data, "Data/National_Data.csv")

