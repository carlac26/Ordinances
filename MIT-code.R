## Do recent urban migrants drive rural backlash against renewable energy projects? 
## Author: Inhwan Ko (University of Washington)
## Date: Jan 31, 2023

## visit https://faculty.washington.edu/cadolph/index.php?page=60 
## to download tile and simcf packages

#install.packages("pacman")
pacman::p_load(MASS, readxl, tseries, car, tile, simcf, # data refinement
               survival, spaMM, plm, coda, fields, # data analysis
               sf, sp, tmap, raster, spdep, rgdal, rgeos,
               ggplot2, ggpubr, shiny, shinyjs, tmaptools, graph4lg, # spatial 
               tidyverse)

########################### Data Refinement ##################################
# Ordinances data
data <- read_excel("county_list.xlsx")
windord <- read_excel("Wind Ordinances.xlsx", sheet=2)

## Wind ordinances data 
windord <- windord %>% select(State, County, 'Original Captured Date', 'Feature Type', 'Value Type', Value, Citation, Comment)
colnames(windord) <- c("state_name","county_name", "year", "feature","unit","value","citation","comment")

### Features refined
unique(windord$feature) %>% sort()
windord$feature[windord$feature=="Banned"] <- "Moratorium"

windord$feature[windord$feature=="Black River"] <- "River"
windord$feature[windord$feature=="Iowa River"] <- "River"
windord$feature[windord$feature=="Iroquis River"] <- "River"
windord$feature[windord$feature=="Kankakee River"] <- "River"
windord$feature[windord$feature=="Little River"] <- "River"
windord$feature[windord$feature=="Mississippi River"] <- "River"
windord$feature[windord$feature=="Missouri River"] <- "River"
windord$feature[windord$feature=="North Santee River"] <- "River"
windord$feature[windord$feature=="Pee Dee River"] <- "River"
windord$feature[windord$feature=="Rivers"] <- "River"
windord$feature[windord$feature=="Rivers/Lakes"] <- "River"
windord$feature[windord$feature=="Sampit River"] <- "River"
windord$feature[windord$feature=="Wabash River"] <- "River"
windord$feature[windord$feature=="Waccamaw River"] <- "River"

windord$feature[windord$feature=="Highway 111"] <- "Highway"
windord$feature[windord$feature=="Highways"] <- "Highway"

windord$feature[windord$feature=="Tower Density"] <- "Density"
windord$feature[windord$feature=="Tower Denisty"] <- "Density"

windord$feature[windord$feature=="Huntington Reservoir"] <- "Reservoir"
windord$feature[windord$feature=="Salamonie Reservoir"] <- "Reservoir"

windord$feature[windord$feature=="Lake Audubon"] <- "Lake"
windord$feature[windord$feature=="Lake Oahe"] <- "Lake"
windord$feature[windord$feature=="Lake Sakakawea"] <- "Lake"
windord$feature[windord$feature=="Lakes"] <- "Lake"
windord$feature[windord$feature=="Lake Audubon"] <- "Lake"
windord$feature[windord$feature=="Lake Audubon"] <- "Lake"

windord$feature[windord$feature=="Maximum Instillation Size"] <- "Maximum Installation Size"

windord$feature[windord$feature=="Minimum lot size"] <- "Minimum Lot Size"

windord$feature[windord$feature=="Oil & Gas Pipelines"] <- "Oil & Gas Pipeline"

windord$feature[windord$feature=="Property"] <- "Property Line"

windord$feature[windord$feature=="Railroads"] <- "Railroad"

windord$feature[windord$feature=="Roads"] <- "Road"

windord$feature[windord$feature=="Structures"] <- "Structure"

windord$feature[windord$feature=="Noise"] <- "Sound"

unique(windord$feature) %>% sort()


### Unit refined
unique(windord$unit) %>% sort()
windord$unit[windord$unit=="dBa"] <- "dBA"

windord$unit[windord$unit=="Hub-height Multiplier"] <- "Max Tip-height Multiplier"
windord$unit[windord$unit=="Max tip-height"] <- "Max Tip-height Multiplier"
windord$unit[windord$unit=="Max-tip height Multiplier"] <- "Max Tip-height Multiplier"

windord$unit[windord$unit=="Acres"] <- "Acre"
windord$unit[windord$unit=="Minimum Lot Size"] <- "Acre"

windord$unit[windord$unit=="Meters"] <- "Meter"

windord$unit[windord$unit=="Rotor Diameter"] <- "Rotor-diameter Multiplier"
windord$unit[windord$unit=="Rotor Diameter Multiplier"] <- "Rotor-diameter Multiplier"
windord$unit[windord$unit=="Rotor Diameter Mutliplier"] <- "Rotor-diameter Multiplier"
windord$unit[windord$unit=="Rotor-Diameter Multiplier"] <- "Rotor-diameter Multiplier"
windord$unit[windord$unit=="Rotor-radius"] <- "Rotor-diameter Multiplier"

windord$unit[windord$unit=="Turbine Count"] <- "Acre"
windord$value[windord$feature=="Density" & windord$county_name=="Botetourt"] <- 5

windord$unit[windord$unit=="Turbines"] <- "Total Turbine Cap"

unique(windord$unit) %>% sort()

windord <- windord[!is.na(windord$county_name),]

write.csv(windord, "windord_long.csv")

## 1.2. solar ordinances data 
solarord <- read_excel("Solar Ordinances.xlsx", sheet=2)
solarord <- solarord %>% select(State, County, 'Original Captured Date', 'Feature Type', 'Value Type', Value, Citation, Comment)
colnames(solarord) <- c("state_name","county_name", "year", "feature","unit","value","citation","comment")

### Features refined
unique(solarord$feature) %>% sort()

solarord$feature[solarord$feature=="Banned"] <- "Moratorium"

solarord$feature[solarord$feature=="Coverage"] <- "Maximum Lot Coverage"

solarord$feature[solarord$feature=="Lakes"] <- "Lake"

solarord$feature[solarord$feature=="Lankford Highway"] <- "Highway"

solarord$feature[solarord$feature=="M.D. Route 413"] <- "Road"

solarord$feature[solarord$feature=="maximum lot size"] <- "Maximum Lot Size"
solarord$feature[solarord$feature=="Maximum lot size"] <- "Maximum Lot Size"
solarord$feature[solarord$feature=="Maximum Project Size"] <- "Maximum Lot Size"

solarord$feature[solarord$feature=="Mimimum Lot Size"] <- "Minimum Lot Size"
solarord$feature[solarord$feature=="Minimum lot size"] <- "Minimum Lot Size"
solarord$feature[solarord$feature=="Minimum lot Size"] <- "Minimum Lot Size"
solarord$feature[solarord$feature=="MInimum Lot Size"] <- "Minimum Lot Size"

solarord$feature[solarord$feature=="Property line"] <- "Property Line"
solarord$feature[solarord$feature=="Property Lines"] <- "Property Line"

solarord$feature[solarord$feature=="Railroads"] <- "Railroad"
solarord$feature[solarord$feature=="Rivers"] <- "River"
solarord$feature[solarord$feature=="Roads"] <- "Road"

solarord$feature[solarord$feature=="Sounds"] <- "Sound"
solarord$feature[solarord$feature=="Noise"] <- "Sound"

solarord$feature[solarord$feature=="Structures"] <- "Structure"

solarord$feature[solarord$feature=="Total Installation Size"] <- "Total Installation"

solarord$feature[solarord$feature=="U.S. Route 13"] <- "Road"

solarord$feature[solarord$feature=="Waters"] <- "Water"
solarord$feature[solarord$feature=="Wetlands"] <- "Wetland"

unique(solarord$feature) %>% sort()


### Unit refined
unique(solarord$unit) %>% sort()

solarord$unit[solarord$unit=="Acres"] <- "Acre"

solarord$unit[solarord$unit=="dBa"] <- "dBA"

solarord$unit[solarord$unit=="Maximum Structure Height"] <- "Maximum Structure Height Multiplier"

solarord$unit[solarord$unit=="Meters"] <- "Meter"

solarord$unit[solarord$unit=="n/a"] <- "N/A"
solarord$unit[solarord$unit=="N/a"] <- "N/A"

solarord$unit[solarord$unit=="Megawatt"] <- "MW"


solarord <- solarord[!is.na(solarord$county_name),]

write.csv(solarord, "solarord_long.csv")
# manually put the years of ordinance

# 2. County-level covariates, 2010-2021

solarord <- read.csv("solarord_long_revised.csv")[,-1]

year <- c(rep(2010,3142), rep(2011,3142), rep(2012,3142),
          rep(2013,3142), rep(2014,3142), rep(2015,3142),
          rep(2016,3142), rep(2017,3142), rep(2018,3142),
          rep(2019,3142), rep(2020,3142), rep(2021,3142),
          rep(2022,3142))

fulldata <- rbind(data,data,data,data,data,
                  data,data,data,data,data,
                  data,data,data)

fulldata$year <- year

windord_unique <- windord %>% select(state_name, county_name, year) %>% unique()
windord_unique$windord <- 1 # 560 counties with wind ordinances 
solarord_unique <- solarord %>% select(state_name, county_name, year) %>% unique()
solarord_unique$solarord <- 1 # 315 counties with solar ordinances (311)

#fulldata <- left_join(fulldata, windord_unique, by=c("state_name","county_name","year"))
#sum(windord_unique$windord); sum(fulldata$windord, na.rm=T) # 7 not transferred
# due to spelling differences

windord_unique$county_name[windord_unique$county_name=="Dekalb"] <- "DeKalb" 
windord_unique$county_name[windord_unique$county_name=="DeWitt"] <- "De Witt" 
windord_unique$county_name[windord_unique$county_name=="Koscuisko"] <- "Kosciusko"
windord_unique$county_name[windord_unique$county_name=="Chautaqua"] <- "Chautauqua"
windord_unique$county_name[windord_unique$county_name=="Lac Qui Parle"] <- "Lac qui Parle"
windord_unique$county_name[windord_unique$county_name=="FIllmore"] <- "Fillmore"
windord_unique$county_name[windord_unique$county_name=="Notrona"] <- "Natrona"

fulldata <- left_join(fulldata, windord_unique, by=c("state_name","county_name","year"))

sum(windord_unique$windord); sum(fulldata$windord, na.rm=T) # perfect match

#fulldata <- left_join(fulldata, solarord_unique, by=c("state_name","county_name","year"))

#sum(solarord_unique$solarord); sum(fulldata$solarord, na.rm=T) # 15 not transferred
# also due to spelling differences

solarord_unique$county_name[solarord_unique$county_name=="Gadsen"] <- "Gadsden"
# Oglethorpe, GA (2023) -> right-censored
solarord_unique$county_name[solarord_unique$county_name=="DeWitt"] <- "De Witt" 
solarord_unique$county_name[solarord_unique$county_name=="Dearborne"] <- "Dearborn" 
solarord_unique$county_name[solarord_unique$county_name=="Koscuisko"] <- "Kosciusko"
solarord_unique$county_name[solarord_unique$county_name=="Saint Joseph"] <- "St. Joseph"
solarord_unique$county_name[solarord_unique$county_name=="Fredrick"] <- "Frederick"
solarord_unique$county_name[solarord_unique$county_name=="Queen Annes"] <- "Queen Anne's"
solarord_unique$county_name[solarord_unique$county_name=="Gladwind"] <- "Gladwin"
# Granville, NC (2009) -> excluded from the analysis (left the risk set)
# Linclon, NC (2009) -> excluded from the analysis (left the risk set)
solarord_unique$county_name[solarord_unique$county_name=="Dinwiddle"] <- "Dinwiddie"
solarord_unique$county_name[solarord_unique$county_name=="Dinwiddle"] <- "Dinwiddie"
solarord_unique$county_name[solarord_unique$county_name=="King Williams"] <- "King William"
# Richmond, VI (2007) -> excluded from the analysis (left the risk set)
solarord_unique$state_name[solarord_unique$state_name=="Forida"] <- "Florida"

fulldata <- left_join(fulldata, solarord_unique, by=c("state_name","county_name","year"))
sum(solarord_unique$solarord); sum(fulldata$solarord, na.rm=T) # 4 not transferred
# due to right-censor or risk set left

fulldata$windord[is.na(fulldata$windord)] <- 0
fulldata$solarord[is.na(fulldata$solarord)] <- 0

# make every observation leave the risk set once they have adopted the ordinance
fulldata$windord_status <- fulldata$windord
fulldata$solarord_status <- fulldata$solarord

fulldata <- fulldata %>% arrange(state_name, county_name, year)

for (i in 2:nrow(fulldata)){
  fulldata$windord_status[i][fulldata$county_fips[i]==fulldata$county_fips[i-1] 
                             & fulldata$windord_status[i-1]==1] <- 1
  fulldata$solarord_status[i][fulldata$county_fips[i]==fulldata$county_fips[i-1] 
                              & fulldata$solarord_status[i-1]==1] <- 1
}

for (i in 2:nrow(fulldata)){
  fulldata$windord[i][fulldata$county_fips[i]==fulldata$county_fips[i-1] 
                      & fulldata$windord[i-1]==1] <- NA
  fulldata$solarord[i][fulldata$county_fips[i]==fulldata$county_fips[i-1] 
                       & fulldata$solarord[i-1]==1] <- NA
}

for (i in 2:nrow(fulldata)){
  fulldata$windord[i][fulldata$county_fips[i]==fulldata$county_fips[i-1] 
                      & is.na(fulldata$windord[i-1])] <- NA
  fulldata$solarord[i][fulldata$county_fips[i]==fulldata$county_fips[i-1] 
                       & is.na(fulldata$solarord[i-1])] <- NA
}

# population and domestic migration
# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html (10-19)
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html (20-21)

pop1019 <- read.csv("population10-19.csv")
pop2021 <- read.csv("population20-21.csv")

demo <- left_join(pop1019, pop2021, by=c("COUNTY","STNAME","CTYNAME"))
demo <- demo %>% 
  select(STNAME, CTYNAME, CENSUS2010POP, POPESTIMATE2011, POPESTIMATE2012,
         POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016,
         POPESTIMATE2017, POPESTIMATE2018, POPESTIMATE2019, ESTIMATESBASE2020,
         POPESTIMATE2021, DOMESTICMIG2010, DOMESTICMIG2011, DOMESTICMIG2012, 
         DOMESTICMIG2013, DOMESTICMIG2014, DOMESTICMIG2015, DOMESTICMIG2016,
         DOMESTICMIG2017, DOMESTICMIG2018, DOMESTICMIG2019, DOMESTICMIG2020,
         DOMESTICMIG2021)

colnames(demo) <- c("state_name","county_name","pop2010","pop2011","pop2012",
                    "pop2013","pop2014","pop2015","pop2016","pop2017","pop2018",
                    "pop2019","pop2020","pop2021","mig2010","mig2011","mig2012",
                    "mig2013","mig2014","mig2015","mig2016","mig2017","mig2018",
                    "mig2019","mig2020","mig2021")

pop <- demo %>% select(state_name, county_name, pop2010, pop2011, pop2012,
                       pop2013, pop2014, pop2015, pop2016, pop2017, pop2018, 
                       pop2019, pop2020,pop2021)
pop <- pop[pop$state_name!=pop$county_name,]

mig <- demo %>% select(state_name, county_name, mig2010, mig2011, mig2012,
                       mig2013, mig2014, mig2015, mig2016, mig2017, mig2018, 
                       mig2019, mig2020,mig2021)
mig <- mig[mig$state_name!=mig$county_name,]

popdata <- pop %>% gather(year, population, pop2010:pop2021, factor_key=T)
popdata$year <- gsub("pop", "", popdata$year); popdata$year <- as.numeric(popdata$year)

migdata <- mig %>% gather(year, migration, mig2010:mig2021, factor_key=T)
migdata$year <- gsub("mig", "", migdata$year); migdata$year <- as.numeric(migdata$year)

popmig <- left_join(popdata, migdata, by=c("state_name","county_name","year"))
popmig$county_name <- encodeString(popmig$county_name)
popmig$county_name <- gsub(" County", "", popmig$county_name)

fulldata <- left_join(fulldata, popmig, by=c("state_name","county_name","year"))
# except for 2022, all data have been successfully merged


# generator-level wind and solar energy capacity, by county by year
# https://www.eia.gov/electricity/data/eia860/

gen2010 <- read_excel("gen2010.xls") %>% filter(ENERGY_SOURCE_1=="SUN" | ENERGY_SOURCE_1=="WND")
gen2011 <- read_excel("gen2011.xlsx", skip=1) %>% filter(ENERGY_SOURCE_1=="SUN" | ENERGY_SOURCE_1=="WND")
gen2012 <- read_excel("gen2012.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2013 <- read_excel("gen2013.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2014 <- read_excel("gen2014.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2015 <- read_excel("gen2015.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2016 <- read_excel("gen2016.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2017 <- read_excel("gen2017.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2018 <- read_excel("gen2018.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2019 <- read_excel("gen2019.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2020 <- read_excel("gen2020.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2021 <- read_excel("gen2021.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")

gen2010$year <- 2010; gen2011$year <- 2011; gen2012$year <- 2012; gen2013$year <- 2013
gen2014$year <- 2014; gen2015$year <- 2015; gen2016$year <- 2016; gen2017$year <- 2017
gen2018$year <- 2018; gen2019$year <- 2019; gen2020$year <- 2020; gen2021$year <- 2021

gen2010 <- gen2010 %>% 
  select("UTILITY_ID","UTILITY_NAME","PLANT_CODE","PLANT_NAME","GENERATOR_ID","STATE","COUNTY","year","NAMEPLATE","ENERGY_SOURCE_1")
gen2011 <- gen2011 %>% 
  select("UTILITY_ID","UTILITY_NAME","PLANT_CODE","PLANT_NAME","GENERATOR_ID","STATE","COUNTY","year","NAMEPLATE","ENERGY_SOURCE_1")
gen2012 <- gen2012 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2013 <- gen2013 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2014 <- gen2014 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2015 <- gen2015 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2016 <- gen2016 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2017 <- gen2017 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2018 <- gen2018 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2019 <- gen2019 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2020 <- gen2020 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2021 <- gen2021 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")

varlist <- c("utility_ID", "utility_name", "plant_code", "plant_name", "generator_ID", "state_abbv","county_name", "year","capacity", "source")
colnames(gen2010) <- colnames(gen2011) <- colnames(gen2012) <- colnames(gen2013) <- 
  colnames(gen2014) <- colnames(gen2015) <- colnames(gen2016) <- colnames(gen2017) <- 
  colnames(gen2018) <- colnames(gen2019) <- colnames(gen2020) <- colnames(gen2021) <- varlist

gentotal <- rbind(gen2010, gen2011, gen2012, gen2013, gen2014, gen2015, gen2016,
                  gen2017, gen2018, gen2019, gen2020, gen2021)

gentotal$county_name <- tolower(gentotal$county_name); fulldata$county_name <- tolower(fulldata$county_name)
# gentotal data has some upper case county names; just use lower case for all datasets for easy merging

mean(gentotal$capacity[gentotal$source=="WND"])
mean(gentotal$capacity[gentotal$source=="SUN"])

windcapa <- gentotal %>% filter(source=="WND") %>% 
  group_by(state_abbv, county_name, year) %>% 
  summarize(capacity=sum(capacity))

solarcapa <- gentotal %>% filter(source=="SUN") %>% 
  group_by(state_abbv, county_name, year) %>% 
  summarize(capacity=sum(capacity))

#rawdata <- fulldata
fulldata <- left_join(fulldata, windcapa, by=c("state_abbv","county_name","year"))
colnames(fulldata)[12] <- "windcapa"

fulldata <- left_join(fulldata, solarcapa, by=c("state_abbv","county_name","year"))
colnames(fulldata)[13] <- "solarcapa"

fulldata$windcapa[is.na(fulldata$windcapa)] <- 0
fulldata$solarcapa[is.na(fulldata$solarcapa)] <- 0

# coal mines (time-unvarying)
# https://atlas.eia.gov/datasets/eia::coal-mines/explore?location=36.555263%2C-97.721358%2C5.02
# unit: short tons
coalmine <- read.csv("Coal_Mines.csv") %>% 
  select(year_, name, state, county, tot_prod)
colnames(coalmine) <- c("year","name","state_name","county_name","tot_prod")
coalmine$tot_prod <- as.numeric(gsub(",", "", coalmine$tot_prod))
coalmine <- coalmine %>% 
  group_by(year, state_name, county_name) %>% 
  summarize(tot_prod=sum(tot_prod, na.rm=T))
coalmine$county_name <- tolower(coalmine$county_name)

fulldata <- left_join(fulldata, coalmine, by=c("state_name","county_name"))
fulldata$tot_prod[is.na(fulldata$tot_prod)] <- 0
fulldata <- fulldata[,-14]
colnames(fulldata)[5] <- "year"

# county-level partisanship
# county-level presidential republican vote share, 2008, 2012, 2016, 2020
# https://electionlab.mit.edu/data, https://dataverse.harvard.edu/file.xhtml?fileId=6689930&version=11.0 

county_partisan <- read.table("countypres_2000-2020.tab", fill=NA, header=T)
for (i in 1:nrow(county_partisan)){
  county_partisan$county_fips[i][unlist(str_split(county_partisan$county_fips[i], ""))[1]=="0"] <- paste(unlist(str_split(county_partisan$county_fips[i], ""))[2:5], collapse="")
}

county_partisan <- county_partisan %>% 
  select(county_fips, year, party, candidatevotes, totalvotes) %>% 
  filter(party=="REPUBLICAN") %>% 
  group_by(county_fips, year) %>% 
  summarize(repvote=sum(candidatevotes), totalvotes=totalvotes) %>% unique() %>% 
  summarize(repshare=100*repvote/totalvotes)

county_partisan$year[county_partisan$year==2008] <- 2010

#rawdata <- fulldata
fulldata$county_fips <- as.character(fulldata$county_fips)
fulldata <- left_join(fulldata, county_partisan, by=c("county_fips","year"))
fips_list <- unique(fulldata$county_fips)

for (i in 1:length(fips_list)) { # the most recent presidential election year
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2011] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2010] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2013] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2012] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2014] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2012] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2015] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2012] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2017] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2016] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2018] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2016] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2019] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2016] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2021] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2020] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2022] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2020] 
}

# median age
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/cc-est2020-agesex.pdf
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/cc-est2021-agesex.pdf
medianage1020 <- read.csv("CC-EST2020-AGESEX-ALL.csv") %>% filter(YEAR!=2 & YEAR!=3 & YEAR!=13 & YEAR!=14)
medianage1020$YEAR[medianage1020$YEAR==1] <- 2010
medianage1020$YEAR[medianage1020$YEAR==4] <- 2011
medianage1020$YEAR[medianage1020$YEAR==5] <- 2012
medianage1020$YEAR[medianage1020$YEAR==6] <- 2013
medianage1020$YEAR[medianage1020$YEAR==7] <- 2014
medianage1020$YEAR[medianage1020$YEAR==8] <- 2015
medianage1020$YEAR[medianage1020$YEAR==9] <- 2016
medianage1020$YEAR[medianage1020$YEAR==10] <- 2017
medianage1020$YEAR[medianage1020$YEAR==11] <- 2018
medianage1020$YEAR[medianage1020$YEAR==12] <- 2019

medianage1020 <- medianage1020 %>% 
  select(STNAME, CTYNAME, YEAR, MEDIAN_AGE_TOT)
colnames(medianage1020) <- c("state_name", "county_name", "year", "medianage")

medianage2021 <- read.csv("cc-est2021-agesex-all.csv") %>% filter(YEAR!=2)
medianage2021$YEAR[medianage2021$YEAR==1] <- 2020
medianage2021$YEAR[medianage2021$YEAR==3] <- 2021

medianage2021 <- medianage2021 %>% 
  select(STNAME, CTYNAME, YEAR, MEDIAN_AGE_TOT)
colnames(medianage2021) <- c("state_name", "county_name", "year", "medianage")

medianage <- rbind(medianage1020, medianage2021)
medianage <- medianage %>% arrange(state_name, county_name, year)
for (i in 21637:21648){medianage$county_name[i] <- "dona ana"} # encoding error
medianage$county_name <- tolower(medianage$county_name)

#rawdata <- fulldata
fulldata <- left_join(fulldata, medianage, by=c("state_name", "county_name", "year"))

# per capita income and employment
# https://apps.bea.gov/regional/histdata/
# https://www.bea.gov/system/files/methodologies/LAPI-Methodology.pdf 

econ <- read.csv("CAINC4__ALL_AREAS_1969_2020.csv")
pcincome <- econ[,c(1,7, 50:60)] %>% filter(Description=="Per capita personal income (dollars) 4/") %>% select(-Description)
colnames(pcincome)[1] <- c("county_fips")
pcincome <- pcincome %>% gather(year, pcincome, X2010:X2020, factor_key=T)
pcincome$year <- gsub("X","",pcincome$year) %>% as.numeric()


#rawdata <- fulldata
fulldata <- left_join(fulldata, pcincome, by=c("county_fips","year"))

# per capita income (2021)
# https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas
pcincome21 <- read_excel("income2021.xlsx")[-c(1:5),c(1,2,5)]
colnames(pcincome21) <- c("state_name","county_name","pcincome")
pcincome21$year <- 2021
pcincome21$county_name <- tolower(pcincome21$county_name)

#rawdata <- fulldata
fulldata <- left_join(fulldata, pcincome21, by=c("state_name","county_name","year"))
fulldata$pcincome.x[fulldata$year==2021] <- fulldata$pcincome.y[fulldata$year==2021]
colnames(fulldata)[17] <- "pcincome"; fulldata <- fulldata %>% select(-pcincome.y)

# unemployment rate
# https://www.bls.gov/lau/tables.htm#cntyaa
unemp <- data.frame(NULL)

for (k in 1:12){
  iter_data <- read_excel(paste0("laucnty",k+9,".xlsx"))[-c(1:5),c(2,3,5,10)]
  colnames(iter_data) <- c("state_code","county_code","year","unemprate")
  iter_data$county_fips <- NA
  for (i in 1:nrow(iter_data)) {
    iter_data$county_fips[i] <- paste0(iter_data$state_code[i], iter_data$county_code[i])
    iter_data$county_fips[i][unlist(str_split(iter_data$county_fips[i], ""))[1]=="0"] <- paste(unlist(str_split(iter_data$county_fips[i], ""))[2:5], collapse="")
  }
  unemp <- rbind(unemp, iter_data)
}
unemp <- unemp[,-c(1:2)]
unemp$year <- as.numeric(unemp$year)
unemp$unemprate <- as.numeric(unemp$unemprate)

#rawdata <- fulldata
fulldata <- left_join(fulldata, unemp, by=c("county_fips","year"))

# geospatial component: adjacency matrix
geo <- st_read("Population_By_County_US_Census_2010.shp")
E <- nb2mat(poly2nb(geo), style = "B", zero.policy = T)

# save centroids for potential use
geo$id <- 1:nrow(geo)
geo_sp <- as(geo, "Spatial")
centr <- gCentroid(geo_sp, byid=TRUE)
coord <- as.data.frame(centr@coords)
coord$county_fips <- geo$GEOID10
coord$id <- geo$id
for (i in 1:nrow(coord)) {
  coord$county_fips[i][unlist(str_split(coord$county_fips[i], ""))[1]=="0"] <- paste(unlist(str_split(coord$county_fips[i], ""))[2:5], collapse="")
}

rawdata <- fulldata
fulldata <- left_join(fulldata, coord, by="county_fips")

# index of relative ruralness
irr <- read_excel("IRR_2000_2010.xlsx", sheet=2)

irr <- irr[,-c(4,5)]
colnames(irr) <- c("county_fips","county","irr")
irr$county_fips <- as.character(irr$county_fips)
fulldata <- left_join(fulldata, irr, by="county_fips")

# state-level covariates
# RPS source: https://www.ncsl.org/energy/state-renewable-portfolio-standards-and-goals
unique(fulldata$state_name)
fulldata$state_rps <- 0
fulldata$state_rps[fulldata$state_name=="Arizona"] <- 1 #since 2006
fulldata$state_rps[fulldata$state_name=="California"] <- 1 #since 2002
fulldata$state_rps[fulldata$state_name=="Colorado"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="Connecticut"] <- 1 #since 1998
fulldata$state_rps[fulldata$state_name=="Delaware"] <- 1 #since 2005
fulldata$state_rps[fulldata$state_name=="Hawaii"] <- 1 #since 2001
fulldata$state_rps[fulldata$state_name=="Illinois"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Indiana" & fulldata$year>=2011] <- 1 #since 2011
fulldata$state_rps[fulldata$state_name=="Iowa"] <- 1 #since 1983
fulldata$state_rps[fulldata$state_name=="Kansas"] <- 1 #since 2009
fulldata$state_rps[fulldata$state_name=="Maine"] <- 1 #since 1999
fulldata$state_rps[fulldata$state_name=="Maryland"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="Massachusetts"] <- 1 #since 1997
fulldata$state_rps[fulldata$state_name=="Michigan"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Minnesota"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Missouri"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Montana"] <- 1 #since 2005
fulldata$state_rps[fulldata$state_name=="Nevada"] <- 1 #since 1997
fulldata$state_rps[fulldata$state_name=="New Hampshire"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="New Jersey"] <- 1 #since 1991
fulldata$state_rps[fulldata$state_name=="New Mexico"] <- 1 #since 2002
fulldata$state_rps[fulldata$state_name=="New York"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="North Carolina"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="North Dakota"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Ohio"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Oklahoma"] <- 1 #since 2010
fulldata$state_rps[fulldata$state_name=="Oregon"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Pennsylvania"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="Rhode Island"] <- 1 #since 2004
#fulldata$state_rps[fulldata$state_name=="South Carolina" & fulldata$year>=2014] <- 1 #since 2014 (but voluntary)
fulldata$state_rps[fulldata$state_name=="South Dakota"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Texas"] <- 1 #since 1999
fulldata$state_rps[fulldata$state_name=="Utah"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Vermont" & fulldata$year>=2015] <- 1 #since 2015
fulldata$state_rps[fulldata$state_name=="Virginia" & fulldata$year>=2020] <- 1 #since 2020
fulldata$state_rps[fulldata$state_name=="Washington"] <- 1 #since 2006
fulldata$state_rps[fulldata$state_name=="West Virginia" & fulldata$year<=2015] <- 1 #2009-2015
fulldata$state_rps[fulldata$state_name=="Wisconsin"] <- 1 #since 1998
fulldata$state_rps[fulldata$state_name=="District of Columbia"] <- 1 #since 2005

# state-level ordinance
fulldata$state_windord <- 0
fulldata$state_windord[fulldata$state_name=="California" & fulldata$year>=2018] <- 1 #since 2018
fulldata$state_windord[fulldata$state_name=="Connecticut" & fulldata$year>=2018] <- 1 #since 2018
fulldata$state_windord[fulldata$state_name=="Kentucky" & fulldata$year>=2018] <- 1 #since 2018
fulldata$state_windord[fulldata$state_name=="Minnesota" & fulldata$year>=2018] <- 1 #since 2018
fulldata$state_windord[fulldata$state_name=="Ohio" & fulldata$year>=2014] <- 1 #since 2014
fulldata$state_windord[fulldata$state_name=="Oklahoma" & fulldata$year>=2018] <- 1 #since 2018
fulldata$state_windord[fulldata$state_name=="South Dakota" & fulldata$year>=2009] <- 1 #since 2009
fulldata$state_windord[fulldata$state_name=="Tennessee" & fulldata$year>=2022] <- 1 #since 2022
fulldata$state_windord[fulldata$state_name=="Vermont" & fulldata$year>=2017] <- 1 #since 2017
fulldata$state_windord[fulldata$state_name=="Wisconsin" & fulldata$year>=2018] <- 1 #since 2018
fulldata$state_windord[fulldata$state_name=="Wyoming" & fulldata$year>=2018] <- 1 #since 2018

write.csv(fulldata, "finaldata.csv")
#rawdata <- fulldata

####################### Event History Analysis ###############################
gc()
rm(list=ls())
mdata <- read.csv("finaldata.csv")
mdata <- mdata %>% 
  filter(county_fips!="2158") %>% filter(county_fips!="46102")
mdata$X <- 1:nrow(mdata)
# these two counties do not have geo information

mdata$medianage <- as.numeric(mdata$medianage)
mdata$pcincome <- as.numeric(mdata$pcincome)
mdata$unemprate <- round(as.numeric(mdata$unemprate),3)
mdata$tot_prod <- as.numeric(mdata$tot_prod)
mdata$coal <- ifelse(mdata$tot_prod!=0, 1, 0)

# some duplicates due to the same name between state and county
# retrive the observation with correct covariate values
mdata <- mdata[-c(1443,4668,7099,7477,10871,21884,24160,28411,36407,38008),]
mdata <- pdata.frame(mdata, index=c("county_fips","year"))

mdata$windcapa_l <- lag(mdata$windcapa, 1)
mdata$solarcapa_l <- lag(mdata$solarcapa, 1)
mdata$medianage_l <- lag(mdata$medianage, 1)
mdata$pcincome_l <- lag(mdata$pcincome, 1)
mdata$unemprate_l <- lag(mdata$unemprate, 1)
mdata$ptmig_l <- lag(1000*mdata$migration/mdata$population, 1) # per thousands migration
mdata$windord_status_l <- lag(mdata$windord_status, 1)
mdata$solarord_status_l <- lag(mdata$solarord_status, 1)

# Contingent counties' ordinances
geo <- st_read("Population_By_County_US_Census_2010.shp")
E <- nb2mat(poly2nb(geo), style = "B", zero.policy = T); rawE <- E
dim(E)
id_list <- mdata$id %>% unique() %>% sort()
colnames(E) <- 1:dim(E)[2]
mE <- E[paste(id_list,sep=","),paste(id_list,sep=","),drop=F]
dim(mE) # numbers are IDs

windord_ct_df <- solarord_ct_df <- windord_ct <- solarord_ct <- as.data.frame(NULL)

# wind ordinances
# make sure ids are arranged 
for (i in 1:12) {
  # last year's ordinances
  windord_ct <- mdata %>% filter(year==paste(2010+i)) %>% select(id, windord_status_l) %>% arrange(id) 
  windord_ct <- mE %*% windord_ct$windord_status_l # contingency
  windord_ct <- as.data.frame(windord_ct)
  windord_ct$year <- paste(2010+i)
  windord_ct$id <- id_list
  windord_ct_df <- rbind(windord_ct_df, windord_ct)
}

# solar ordinances
for (i in 1:12) {
  # last year's ordinances
  solarord_ct <- mdata %>% filter(year==paste(2010+i)) %>% select(id, solarord_status_l) %>% arrange(id) 
  solarord_ct <- mE %*% solarord_ct$solarord_status_l # contingency
  solarord_ct <- as.data.frame(solarord_ct)
  solarord_ct$year <- paste(2010+i)
  solarord_ct$id <- id_list
  solarord_ct_df <- rbind(solarord_ct_df, solarord_ct)
}

colnames(windord_ct_df) <-  c("windord_ct","year","id")
colnames(solarord_ct_df) <-  c("solarord_ct","year","id")

mdata <- left_join(mdata, windord_ct_df, by=c("year","id")) # match by id, not county_fips
mdata <- left_join(mdata, solarord_ct_df, by=c("year","id"))

## Contiguous counties' existing capacity
windcapa_ct_df <- solarcapa_ct_df <- as.data.frame(NULL)
cttotal <- rowSums(mE)

for (i in 1:12) {
  # last year's wind capacity
  windcapa_ct <- mdata %>% filter(year==paste(2010+i)) %>% select(id, windcapa) %>% arrange(id) 
  windcapa_ct <- mE %*% windcapa_ct$windcapa # contingency
  windcapa_ct <- as.data.frame(windcapa_ct)
  windcapa_ct$year <- paste(2010+i)
  windcapa_ct$id <- id_list
  windcapa_ct$cttotal <- cttotal
  windcapa_ct_df <- rbind(windcapa_ct_df, windcapa_ct)
}

for (i in 1:12) {
  # last year's solar capacity
  solarcapa_ct <- mdata %>% filter(year==paste(2010+i)) %>% select(id, solarcapa) %>% arrange(id) 
  solarcapa_ct <- mE %*% solarcapa_ct$solarcapa # contingency
  solarcapa_ct <- as.data.frame(solarcapa_ct)
  solarcapa_ct$year <- paste(2010+i)
  solarcapa_ct$id <- id_list
  solarcapa_ct$cttotal <- cttotal
  solarcapa_ct_df <- rbind(solarcapa_ct_df, solarcapa_ct)
}

colnames(windcapa_ct_df) <-  c("windcapa_ct","year","id", "cttotal")
colnames(solarcapa_ct_df) <-  c("solarcapa_ct","year","id", "cttotal")

windcapa_ct_df$windcapa_ct_mean <- windcapa_ct_df$windcapa_ct / windcapa_ct_df$cttotal
solarcapa_ct_df$solarcapa_ct_mean <- solarcapa_ct_df$solarcapa_ct / solarcapa_ct_df$cttotal

windcapa_ct_df <- windcapa_ct_df %>% select(id,year,windcapa_ct_mean,cttotal)
solarcapa_ct_df <- solarcapa_ct_df %>% select(id,year,solarcapa_ct_mean)

#rawmdata <- mdata
mdata <- left_join(mdata, windcapa_ct_df, by=c("year","id")) # match by id, not county_fips
mdata <- left_join(mdata, solarcapa_ct_df, by=c("year","id"))

mdata$irr <- mdata$irr*100

# omit Hawai'i and Alaska
#rawmdata <- mdata
mdata <- mdata %>% filter(state_abbv!="HI") %>% filter(state_abbv!="AK")
mdata <- as.data.frame(mdata)

## share of contiguous
mdata$windord_ct_share_l <- 100*mdata$windord_ct / mdata$cttotal
mdata$solarord_ct_share_l <- 100*mdata$solarord_ct / mdata$cttotal

#rawmdata <- mdata
############################# wind ordinances ###################################
mdata$year <- as.numeric(mdata$year)
mdata$id <- as.factor(mdata$id)
wind_surv <- Surv(mdata$year-1, mdata$year, mdata$windord)

wind_formula <- wind_surv ~ irr + log(windcapa_l+1) + 
  repshare + unemprate_l + log(pcincome_l+1) + ptmig_l + medianage_l +
  coal + windord_ct_share_l + log(windcapa_ct_mean+1) + state_rps + state_windord + 
  frailty(id) 

wind_model1 <- coxph(wind_formula, mdata); 
wind_result1 <- summary(wind_model1)
wind_result1
round(wind_result1$coefficients,3)
round(wind_result1$conf.int,3)

####################### solar ordinances ######################
solar_surv <- Surv(mdata$year-1, mdata$year, mdata$solarord)

solar_formula <- solar_surv  ~ irr + log(solarcapa_l+1) + 
  repshare + unemprate_l + log(pcincome_l+1) + ptmig_l + medianage_l +
  coal + solarord_ct_share_l + log(solarcapa_ct_mean+1) + state_rps  + 
  frailty(id)

solar_model1 <- coxph(solar_formula, mdata); 
solar_result1 <- summary(solar_model1)
solar_result1
round(solar_result1$coefficients,3)
round(solar_result1$conf.int,3)

#### Counterfactual plots: ## Adapted from Holtmaat et al. (2020)

simCoxPH <- function(sims, before, after, baseline=0) {
  crossRR <- sims^(after - before)
  res <- c(mean(crossRR), quantile(crossRR, probs=c(0.025, 0.975)))
  res
} 

## Figure 3
sims <- 10000
simbetas1 <- exp(mvrnorm(sims, coef(wind_model1), vcov(wind_model1)))
CFnames1 <- CFs1 <- NULL

CFnames1 <- c(CFnames1, "Higher relative rurality (25 % vs 75 %)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,1],
                       quantile(mdata$irr, probs=0.25),
                       quantile(mdata$irr, probs=0.75),
                       0)
)

CFnames1 <- c(CFnames1, "Higher total wind capacity (+1 sd)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,2],
                       log(mean(mdata$windcapa_l+1,na.rm=T)),
                       log(mean(mdata$windcapa_l+1,na.rm=T)+
                             sd(mdata$windcapa_l+1,na.rm=T)),
                       0)
)

CFnames1 <- c(CFnames1, "Higher Republican vote share (+10 %p)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,3],
                       mean(mdata$repshare, na.rm=T),
                       mean(mdata$repshare, na.rm=T)+10,
                       0)
)

CFnames1 <- c(CFnames1, "Higher unemployment rate (+1 %p)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,4],
                       mean(mdata$unemprate_l, na.rm=T),
                       mean(mdata$unemprate_l, na.rm=T)+1,
                       0)
)

CFnames1 <- c(CFnames1, "Higher per capita income (+1 sd)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,5],
                       log(mean(mdata$pcincome_l+1,na.rm=T)),
                       log(mean(mdata$pcincome_l+1,na.rm=T)+
                             sd(mdata$pcincome_l+1,na.rm=T)),
                       0)
)

CFnames1 <- c(CFnames1, "Higher per thousand migration (+1)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,6],
                       mean(mdata$ptmig_l, na.rm=T),
                       mean(mdata$ptmig_l, na.rm=T)+1,
                       0)
)

CFnames1 <- c(CFnames1, "Higher median age by (+1)")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,7],
                       mean(mdata$medianage_l, na.rm=T),
                       mean(mdata$medianage_l, na.rm=T)+1,
                       0)
)

CFnames1 <- c(CFnames1, "Coal producing county")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,8],
                       0,
                       1,
                       0)
)

CFnames1 <- c(CFnames1, "9") # due to margin size error, just 9
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,9],
                       mean(mdata$windord_ct_share_l, na.rm=T),
                       mean(mdata$windord_ct_share_l, na.rm=T) +
                         sd(mdata$windord_ct_share_l, na.rm=T),
                       0)
)

CFnames1 <- c(CFnames1, "10") # due to margin size error, just 10
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,10],
                       log(mean(mdata$windcapa_ct_mean+1,na.rm=T)),
                       log(mean(mdata$windcapa_ct_mean+1,na.rm=T)+
                             sd(mdata$windcapa_ct_mean+1,na.rm=T)),
                       0)
)

CFnames1 <- c(CFnames1, "State-level RPS")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,11],
                       0,
                       1,
                       0)
)

CFnames1 <- c(CFnames1, "State-level wind restriction")
CFs1 <- rbind(CFs1, 
              simCoxPH(simbetas1[,12],
                       0,
                       1,
                       0)
)

sortedCFnames1 <- CFnames1[rev(order(CFs1[,1]))]
sortedCFs1 <- CFs1[rev(order(CFs1[,1])),]

trace1 <- ropeladder(x=sortedCFs1[,1],
                     lower=sortedCFs1[,2],
                     upper=sortedCFs1[,3],
                     labels=sortedCFnames1,
                     entryheight=0.2,
                     pch=c(15, rep(16, length(sortedCFs1)-1)),
                     col="#367089",
                     size=0.725, lex=1.5,
                     lineend="square",
                     plot=1
)

sigMark1 <- sortedCFs1[,1]
is.na(sigMark1) <- ((sortedCFs1[,2]-1)*(sortedCFs1[,3]-1)) > 0
traceSig1 <- ropeladder(x=sigMark1,
                        col="white",
                        pch=c(15, rep(16, length(sortedCFs1)-1)),
                        group=1,
                        plot=1)

vertmark <- linesTile(x=c(1,1), y=c(0,1), layer=12, plot=1)

file <- "Figure3"
tile(trace1, vertmark, traceSig1,
     limits=c(0.5, 3.0),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, log=TRUE, at=c(0.5, 1, 2, 3), 
                  labels=c("0.5x", "1x", "2x", "3x")),
     xaxis=list(log=TRUE, at=c(0.5, 1,  2, 3),
                labels=c("0.5x", "1x", "2x", "3x")),
     topaxistitle=list(labels="Relative risk of adopting wind restriction", y=0.8),
     width=list(plot=2),
     height=list(topaxistitle=2),
     output=list(outfile=file,width=7.0)
) # change the font later

## Figure 4

simbetas2 <- exp(mvrnorm(sims, coef(solar_model1), vcov(solar_model1)))
CFnames2 <- CFs2 <- NULL

CFnames2 <- c(CFnames2, "Higher relative rurality (25 % vs 75 %)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,1],
                       quantile(mdata$irr, probs=0.25),
                       quantile(mdata$irr, probs=0.75),
                       0)
)

CFnames2 <- c(CFnames2, "Higher total solar capacity (+1 sd)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,2],
                       log(mean(mdata$solarcapa_l+1,na.rm=T)),
                       log(mean(mdata$solarcapa_l+1,na.rm=T)+
                             sd(mdata$solarcapa_l+1,na.rm=T)),
                       0)
)

CFnames2 <- c(CFnames2, "Higher Republican vote share (+10 %p)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,3],
                       mean(mdata$repshare, na.rm=T),
                       mean(mdata$repshare, na.rm=T)+10,
                       0)
)

CFnames2 <- c(CFnames2, "Higher unemployment rate (+1 %p)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,4],
                       mean(mdata$unemprate_l, na.rm=T),
                       mean(mdata$unemprate_l, na.rm=T)+1,
                       0)
)

CFnames2 <- c(CFnames2, "Higher per capita income (+1 sd)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,5],
                       log(mean(mdata$pcincome_l+1,na.rm=T)),
                       log(mean(mdata$pcincome_l+1,na.rm=T)+
                             sd(mdata$pcincome_l+1,na.rm=T)),
                       0)
)

CFnames2 <- c(CFnames2, "Higher per thousand migration (+1)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,6],
                       mean(mdata$ptmig_l, na.rm=T),
                       mean(mdata$ptmig_l, na.rm=T)+1,
                       0)
)

CFnames2 <- c(CFnames2, "Higher median age (+1)")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,7],
                       mean(mdata$medianage_l, na.rm=T),
                       mean(mdata$medianage_l, na.rm=T)+1,
                       0)
)

CFnames2 <- c(CFnames2, "Coal producing county")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,8],
                       0,
                       1,
                       0)
)

CFnames2 <- c(CFnames2, "9") # due to margin size error, just 9
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,9],
                       mean(mdata$solarord_ct_share_l, na.rm=T),
                       mean(mdata$solarord_ct_share_l, na.rm=T) +
                         sd(mdata$solarord_ct_share_l, na.rm=T),
                       0)
)

CFnames2 <- c(CFnames2, "10") # due to margin size error, just 10
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,10],
                       log(mean(mdata$solarcapa_ct_mean+1,na.rm=T)),
                       log(mean(mdata$solarcapa_ct_mean+1,na.rm=T)+
                             sd(mdata$solarcapa_ct_mean+1,na.rm=T)),
                       0)
)

CFnames2 <- c(CFnames2, "State-level RPS")
CFs2 <- rbind(CFs2, 
              simCoxPH(simbetas2[,11],
                       0,
                       1,
                       0)
)


sortedCFnames2 <- CFnames2[rev(order(CFs2[,1]))]
sortedCFs2 <- CFs2[rev(order(CFs2[,1])),]

trace2 <- ropeladder(x=sortedCFs2[,1],
                     lower=sortedCFs2[,2],
                     upper=sortedCFs2[,3],
                     labels=sortedCFnames2,
                     entryheight=0.2,
                     pch=c(15, rep(16, length(sortedCFs2)-1)),
                     col="#BF882E",
                     size=0.725, lex=1.5,
                     lineend="square",
                     plot=1
)

sigMark2 <- sortedCFs2[,1]
is.na(sigMark2) <- ((sortedCFs2[,2]-1)*(sortedCFs2[,3]-1)) > 0
traceSig2 <- ropeladder(x=sigMark2,
                        col="white",
                        pch=c(15, rep(16, length(sortedCFs2)-1)),
                        group=1,
                        plot=1)

vertmark <- linesTile(x=c(1,1), y=c(0,1), layer=12, plot=1)

file <- "Figure4"
tile(trace2, vertmark, traceSig2,
     limits=c(0.5, 3.0),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, log=TRUE, at=c(0.5, 1, 2, 3), 
                  labels=c("0.5x", "1x", "2x", "3x")),
     xaxis=list(log=TRUE, at=c(0.5, 1,  2, 3),
                labels=c("0.5x", "1x", "2x", "3x")),
     topaxistitle=list(labels="Relative risk of adopting solar restriction", y=0.8),
     width=list(plot=2),
     height=list(topaxistitle=2),
     output=list(outfile=file,width=7.0)
) # change the font later

## model without lowest IRR counties (most urban)
mdata %>% filter(year==2022) %>% 
  select(county_name, irr) %>% 
  arrange(desc(-irr))
# top 10 -> 10 >

mdata2 <- mdata %>% filter(irr>10)

wind_surv2 <- Surv(mdata2$year-1, mdata2$year, mdata2$windord)
wind_formula2 <- wind_surv2  ~  irr + log(windcapa_l+1) + 
  repshare + unemprate_l + log(pcincome_l+1) + ptmig_l + medianage_l +
  coal + windord_ct_share_l + log(windcapa_ct_mean+1) + state_rps + state_windord + 
  frailty(id) 
wind_model2 <- coxph(wind_formula2, mdata2)  
wind_result2 <- summary(wind_model2)

solar_surv2 <- Surv(mdata2$year-1, mdata2$year, mdata2$solarord)
solar_formula2 <- solar_surv2  ~ irr + log(solarcapa_l+1) + 
  repshare + unemprate_l + log(pcincome_l+1) + ptmig_l + medianage_l +
  coal + solarord_ct_share_l + log(solarcapa_ct_mean+1) + state_rps  + 
  frailty(id)

solar_model2 <- coxph(solar_formula2, mdata2)
solar_result2 <- summary(solar_model2)

round(wind_result2$coefficients,3)
round(solar_result2$coefficients,3)


##### Descriptive statistics

wind_formula <- wind_surv ~ irr + log(windcapa_l+1) + 
  repshare + unemprate_l + log(pcincome_l+1) + ptmig_l + medianage_l +
  coal + windord_ct_share_l + log(windcapa_ct_mean+1) + state_rps + state_windord + 
  frailty(id) 

mean(fulldata$windcapa, na.rm=T); sd(fulldata$windcapa,na.rm=T)
mean(fulldata$solarcapa, na.rm=T); sd(fulldata$solarcapa,na.rm=T)
mean(fulldata$irr, na.rm=T); sd(fulldata$irr,na.rm=T)
mean(fulldata$repshare, na.rm=T); sd(fulldata$repshare,na.rm=T)
mean(fulldata$pcincome, na.rm=T); sd(fulldata$pcincome,na.rm=T)
mean(fulldata$unemprate, na.rm=T); sd(fulldata$unemprate,na.rm=T)
mean(mdata$ptmig_l, na.rm=T); sd(mdata$ptmig_l,na.rm=T)
mean(fulldata$medianage, na.rm=T); sd(fulldata$medianage,na.rm=T)
mean(mdata$coal, na.rm=T); sd(mdata$coal,na.rm=T)
mean(fulldata$state_rps, na.rm=T); sd(fulldata$state_rps,na.rm=T)
mean(fulldata$state_windord, na.rm=T); sd(fulldata$state_windord,na.rm=T)
mean(mdata$windord_ct_share, na.rm=T); sd(mdata$windord_ct_share,na.rm=T)
mean(mdata$windcapa_ct_mean, na.rm=T); sd(mdata$windcapa_ct_mean,na.rm=T)
mean(mdata$solarcapa_ct_mean, na.rm=T); sd(mdata$solarcapa_ct_mean,na.rm=T)







