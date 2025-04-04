### For taking in Penn World Table data
## Can be used for graphing log rgdp per capita of countries against
## against each other, and for assigning "types" described in README

## NOTE: You NEED to set a working directory below thi imported packages

library(haven) # for importing Stata data
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

# NEED TO SET YOUR WORKING DIRECTORY
setwd()


# Penn world table data saved in "d"
d <- read_stata("pwt1001.dta")

# Create Necessary variables
# Use d_small to avoid messing with general data and to
# Keep the program from doing too much work
d_small <- d[c('countrycode')]
d_small <- d_small %>% group_by(countrycode) %>% summarize()
d_small <- d_small %>% mutate(type = 0)
d <- d %>% mutate(lrgdppc = log(rgdpo/pop))

#for Flags
d_small <- d_small %>% mutate(fmrUSSR = 0)
d_small <- d_small %>% mutate(fmrYugo = 0)
d_small <- d_small %>% mutate(flag = 0)
d_small <- d_small %>% mutate(interesting = 0)


# Function to give a certain type to a country
give_type <- function(data,code,input_type){
  i = 1
  while (i <= nrow(data)){
    if (toString(data[i,'countrycode']) == code){
      data[i,'type'] = input_type
    }
    i = i + 1
  }
  return(data)
}

#fxn to give fmrUSSR
give_fmrUSSR <- function(data,code,input_type){
  i = 1
  while (i <= nrow(data)){
    if (toString(data[i,'countrycode']) == code){
      data[i,'fmrUSSR'] = input_type
    }
    i = i + 1
  }
  return(data)
}

#fxn to give fmrYugo 
give_fmrYugo <- function(data,code,input_type){
  i = 1
  while (i <= nrow(data)){
    if (toString(data[i,'countrycode']) == code){
      data[i,'fmrYugo'] = input_type
    }
    i = i + 1
  }
  return(data)
}

#fxn to give flags 
give_flag <- function(data,code,input_type){
  i = 1
  while (i <= nrow(data)){
    if (toString(data[i,'countrycode']) == code){
      data[i,'flag'] = input_type
    }
    i = i + 1
  }
  return(data)
}

### Graph 2 countries against each other
graph2 <- function(country1, country2){
  d1 <- d %>% filter(countrycode == country1 | countrycode == country2)
  ggplot(d1, aes(x= year, y = lrgdppc, colour = countrycode)) +
    geom_line(linewidth = 1) + 
    geom_hline(yintercept=8,linetype=2) + 
    geom_hline(yintercept = 10, linetype = 2) +
    ylim(5,13.2) + xlim(1950, 2020) + ggtitle("Log rGDP per capita") + 
    scale_color_manual(values=c('blue', "black"))
}

### Graph 3 countries function
graph3 <- function(c1, c2, c3){
  d1 <- d %>% filter(countrycode==c1 | countrycode==c2 | countrycode==c3)
  ggplot(d1, aes(x= year, y = lrgdppc, colour = countrycode)) +
    geom_line(linewidth = 1) + 
    geom_hline(yintercept=8,linetype=2) + 
    geom_hline(yintercept = 10, linetype = 2) +
    ylim(5,13.2) + xlim(1950, 2020) + ggtitle("Log rGDP per capita") + 
    scale_color_manual(values=c('red', "blue", 'black'))
}

#graph based on type, same functionality as graph2
graphbytype <- function(data, input_type){
  d1 <- data() %>% filter(type == input_type)
  ggplot(d1, aes(x= year, y = lrgdppc, colour = countrycode)) +
    geom_line(size = 1) + 
    geom_hline(yintercept=8,linetype=2) + 
    geom_hline(yintercept = 10, linetype = 2) +
    ylim(5,13.5) + xlim(1950, 2020) + ggtitle("Log rGDP per capita") + 
    theme(legend.position = "none")
}


### Pre-assigned type 1's by Professor
d_small <- give_type(d_small, "USA", 1) # United States
d_small <- give_type(d_small, "CAN", 1) # Canada
d_small <- give_type(d_small, "GBR", 1) # UK
d_small <- give_type(d_small, "DEU", 1) # Germany
d_small <- give_type(d_small, "FRA", 1) # France
d_small <- give_type(d_small, "ESP", 1) # Spain
d_small <- give_type(d_small, "ITA", 1) # Italy
d_small <- give_type(d_small, "SWE", 1) # Sweden
d_small <- give_type(d_small, "NOR", 1) # Norway

### Pre-assigned type 2's by Professor
d_small <- give_type(d_small, "JPN", 2) # Japan
d_small <- give_type(d_small, "KOR", 2) # South Korea (Republic of Korea)
d_small <- give_type(d_small, "SGP", 2) # Singapore
d_small <- give_type(d_small, "TWN", 2) # Taiwan
d_small <- give_type(d_small, "HKG", 2) # Hong Kong

### Pre-assigned type 3's by Professor
d_small <- give_type(d_small, "BRA", 3) # Brazil
d_small <- give_type(d_small, "MEX", 3) # Mexico
d_small <- give_type(d_small, "ARG", 3) # Argentina
d_small <- give_type(d_small, "COL", 3) # Columbia
d_small <- give_type(d_small, "TUR", 3) # Turkey
d_small <- give_type(d_small, "ZAF", 3) # South Africa

### Pre-assigned type 4's by Professor
d_small <- give_type(d_small, "IND", 4) # India
d_small <- give_type(d_small, "CHN", 4) # China
d_small <- give_type(d_small, "EGY", 4) # Egypt
d_small <- give_type(d_small, "IDN", 4) # Indonesia
d_small <- give_type(d_small, "THA", 4) # Thailand
d_small <- give_type(d_small, "VNM", 4) # Vietnam

### Pre-assigned type 5's by Professor
d_small <- give_type(d_small, "KEN", 5) # Kenya
d_small <- give_type(d_small, "UGA", 5) # Uganda
d_small <- give_type(d_small, "HTI", 5) # Haiti
d_small <- give_type(d_small, "NPL", 5) # Nepal
d_small <- give_type(d_small, "ETH", 5) # Ethiopia
d_small <- give_type(d_small, "GMB", 5) # Gambia

### Manual Assignments
#Slide 1
d_small <- give_type(d_small, "ABW", 2) # Aruba
d_small <- give_type(d_small, "AGO", 4) # Angola
d_small <- give_type(d_small, "AIA", 0) # Anguilla * flag

#Slide 2
d_small <- give_type(d_small, "ARE", 7) # United Arab Emirates
d_small <- give_type(d_small, "ALB", 4) # Albania
d_small <- give_type(d_small, "ARM", 6) # Armenia * USSR

#Slide 3
d_small <- give_type(d_small, "ATG", 3) # Antigua and Barbuda
d_small <- give_type(d_small, "AUS", 1) # Australia
d_small <- give_type(d_small, "AUT", 1) # Austria

#Slide 4
d_small <- give_type(d_small, "BEL", 1) # Belgium
d_small <- give_type(d_small, "BDI", 5) # Burundi
d_small <- give_type(d_small, "AZE", 6) # Azerbaijan * USSR

#Slide 5
d_small <- give_type(d_small, "BEN", 5) # Benin
d_small <- give_type(d_small, "BFA", 5) # Burkina Faso
d_small <- give_type(d_small, "BGD", 5) # Bangladesh

#Slide 6
d_small <- give_type(d_small, "BGR", 3) # Bulgaria
d_small <- give_type(d_small, "BHR", 1) # Bahrain 
d_small <- give_type(d_small, "BHS", 1) # Bahamas

#Slide 7
d_small <- give_type(d_small, "BIH", 6) # Bosnia and Herzegovina * Yugoslavia
d_small <- give_type(d_small, "BLR", 6) # Belarus * USSR
d_small <- give_type(d_small, "BLZ", 3) # Belize

#Slide 8
d_small <- give_type(d_small, "BMU", 7) # Bermuda
d_small <- give_type(d_small, "BOL", 3) # Bolivia
d_small <- give_type(d_small, "BRB", 3) # Barbados * maybe 4

#Slide 9
d_small <- give_type(d_small, "BRN", 7) # Brunei Darussalam
d_small <- give_type(d_small, "BTN", 3) # Bhutan 
d_small <- give_type(d_small, "BWA", 4) # Botswana

#Slide 10
d_small <- give_type(d_small, "CAF", 5) # Central African Republic
d_small <- give_type(d_small, "CHE", 1) # Switzerland
d_small <- give_type(d_small, "CHL", 3) # Chile

#Slide 11
d_small <- give_type(d_small, "CIV", 5) # Côte d'Ivoire
d_small <- give_type(d_small, "CMR", 5) # Cameroon
d_small <- give_type(d_small, "COD", 5) # D.R. of the Congo

#Slide 12
d_small <- give_type(d_small, "COG", 5) # Congo
d_small <- give_type(d_small, "COM", 5) # Comoros
d_small <- give_type(d_small, "CPV", 3) # Cabo Verde 

#Slide 13
d_small <- give_type(d_small, "CRI", 3) # Costa Rica
d_small <- give_type(d_small, "CUW", 0) # Curaçao
d_small <- give_type(d_small, "CYM", 7) # Cayman Islands 

#Slide 14
d_small <- give_type(d_small, "CYP", 2) # Cyprus
d_small <- give_type(d_small, "CZE", 0) # Czech Republic
d_small <- give_type(d_small, "DJI", 5) # Djibouti

#Slide 15
d_small <- give_type(d_small, "DMA", 3) # Dominica
d_small <- give_type(d_small, "DNK", 1) # Denmark 
d_small <- give_type(d_small, "DOM", 3) # Dominican Republic

#Slide 16
d_small <- give_type(d_small, "DZA", 3) # Algeria
d_small <- give_type(d_small, "ECU", 3) # Ecuador
d_small <- give_type(d_small, "EST", 6) # Estonia * USSR

#Slide 17
d_small <- give_type(d_small, "FIN", 1) # Finland
d_small <- give_type(d_small, "FJI", 3) # Fiji
d_small <- give_type(d_small, "GAB", 3) # Gabon

#Slide 18
d_small <- give_type(d_small, "GEO", 6) # Georgia * USSR
d_small <- give_type(d_small, "GHA", 5) # Ghana
d_small <- give_type(d_small, "GIN", 5) # Guinea

#Slide 19
d_small <- give_type(d_small, "GNB", 5) # Guinea-Bissau 
d_small <- give_type(d_small, "GNQ", 2) # Equatorial Guinea * flag as good case
d_small <- give_type(d_small, "GRC", 2) # Greece

#Slide 20
d_small <- give_type(d_small, "GRD", 3) # Grenada
d_small <- give_type(d_small, "GTM", 3) # Guatemala
d_small <- give_type(d_small, "GUY", 3) # Guyana

#Slide 21
d_small <- give_type(d_small, "HND", 5) # Honduras
d_small <- give_type(d_small, "HRV", 6) # Croatia * Yugoslavia
d_small <- give_type(d_small, "HUN", 3) # Hungary

#Slide 22
d_small <- give_type(d_small, "IDN", 3) # Indonesia
d_small <- give_type(d_small, "IRL", 2) # Ireland
d_small <- give_type(d_small, "IRN", 3) # Iran * maybe 0 interesting case

#Slide 23
d_small <- give_type(d_small, "IRQ", 3) # Iraq * flag
d_small <- give_type(d_small, "ISL", 1) # Iceland
d_small <- give_type(d_small, "ISR", 1) # Israel

#Slide 24
d_small <- give_type(d_small, "JAM", 3) # Jamaica
d_small <- give_type(d_small, "JOR", 3) # Jordan 
d_small <- give_type(d_small, "KAZ", 6) # Kazakhstan * USSR

#Slide 25
d_small <- give_type(d_small, "KGZ", 6) # Kyrgyzstan * USSR
d_small <- give_type(d_small, "KHM", 5) # Cambodia
d_small <- give_type(d_small, "KNA", 3) # Saint Kitts and Nevis

#Slide 26
d_small <- give_type(d_small, "KWT", 7) # Kuwait
d_small <- give_type(d_small, "LAO", 4) # Lao People's DR
d_small <- give_type(d_small, "LBN", 3) # Lebanon * flag

#Slide 27
d_small <- give_type(d_small, "LBR", 5) # Liberia
d_small <- give_type(d_small, "LCA", 3) # Saint Lucia
d_small <- give_type(d_small, "LKA", 3) # Sri Lanka

#Slide 28
d_small <- give_type(d_small, "LSO", 5) # Lesotho
d_small <- give_type(d_small, "LTU", 6) # Lithuania * USSR
d_small <- give_type(d_small, "LUX", 1) # Luxembourg

#Slide 29
d_small <- give_type(d_small, "LVA", 6) # Latvia * USSR
d_small <- give_type(d_small, "MAC", 2) # China, Macao SAR * flag
d_small <- give_type(d_small, "MAR", 3) # Morocco 

#Slide 30
d_small <- give_type(d_small, "MDA", 6) # Moldova * USSR 
d_small <- give_type(d_small, "MDG", 5) # Madagascar
d_small <- give_type(d_small, "MDV", 3) # Maldives 

#Slide 31
d_small <- give_type(d_small, "MKD", 6) # North Macedonia * Yugoslavia
d_small <- give_type(d_small, "MLI", 5) # Mali
d_small <- give_type(d_small, "MLT", 2) # Malta * flag

#Slide 32
d_small <- give_type(d_small, "MMR", 4) # Myanmar
d_small <- give_type(d_small, "MNE", 6) # Montenegro * Yugoslavia
d_small <- give_type(d_small, "MNG", 4) # Mongolia

#Slide 33
d_small <- give_type(d_small, "MOZ", 5) # Mozambique
d_small <- give_type(d_small, "MRT", 5) # Mauritania
d_small <- give_type(d_small, "MSR", 7) # Montserrat 

#Slide 34
d_small <- give_type(d_small, "MUS", 3) # Mauritius
d_small <- give_type(d_small, "MWI", 5) # Malawi
d_small <- give_type(d_small, "MYS", 3) # Malaysia

#Slide 35
d_small <- give_type(d_small, "NAM", 3) # Namibia
d_small <- give_type(d_small, "NER", 5) # Niger
d_small <- give_type(d_small, "NGA", 5) # Nigeria * flag

#Slide 36
d_small <- give_type(d_small, "NIC", 5) # Nicaragua
d_small <- give_type(d_small, "NLD", 1) # Netherlands
d_small <- give_type(d_small, "NPL", 5) # Nepal * flag

#Slide 37
d_small <- give_type(d_small, "NZL", 1) # New Zealand
d_small <- give_type(d_small, "OMN", 2) # Oman * flag
d_small <- give_type(d_small, "PAK", 3) # Pakistan

#Slide 38
d_small <- give_type(d_small, "PAN", 2) # Panama
d_small <- give_type(d_small, "PER", 3) # Peru
d_small <- give_type(d_small, "PHL", 3) # Philippines

#Slide 39
d_small <- give_type(d_small, "POL", 2) # Poland
d_small <- give_type(d_small, "PRT", 2) # Portugal
d_small <- give_type(d_small, "PRY", 3) # Paraguay

#Slide 40
d_small <- give_type(d_small, "PSE", 3) # State of Palestine
d_small <- give_type(d_small, "QAT", 7) # Qatar 
d_small <- give_type(d_small, "ROU", 2) # Romania

#Slide 41
d_small <- give_type(d_small, "RWA", 5) # Rwanda
d_small <- give_type(d_small, "SAU", 7) # Saudi Arabia
d_small <- give_type(d_small, "SDN", 5) # Sudan

#Slide 42
d_small <- give_type(d_small, "SEN", 5) # Senegal
d_small <- give_type(d_small, "SLE", 5) # Sierra Leone
d_small <- give_type(d_small, "SLV", 4) # El Salvador

#Slide 43
d_small <- give_type(d_small, "SRB", 6) # Serbia * Yugoslavia
d_small <- give_type(d_small, "STP", 5) # Sao Tome and Principe
d_small <- give_type(d_small, "SUR", 3) # Suriname

#Slide 44
d_small <- give_type(d_small, "SVK", 3) # Slovakia * flag
d_small <- give_type(d_small, "SVN", 6) # Slovenia * Yugoslavia
d_small <- give_type(d_small, "SWZ", 3) # Eswatini * flag

#Slide 45
d_small <- give_type(d_small, "SXM", 0) # Sint Maarten
d_small <- give_type(d_small, "SYC", 3) # Seychelles * flag
d_small <- give_type(d_small, "SYR", 5) # Syrian Arab Republic

#Slide 46
d_small <- give_type(d_small, "TCA", 7) # Turks and Caicos Islands
d_small <- give_type(d_small, "TCD", 5) # Chad
d_small <- give_type(d_small, "TGO", 5) # Togo

#Slide 47
d_small <- give_type(d_small, "TJK", 6) # Tajikistan * USSR
d_small <- give_type(d_small, "TKM", 6) # Turkmenistan * USSR
d_small <- give_type(d_small, "TTO", 3) # Trinidad and Tobago * flag

#Slide 48
d_small <- give_type(d_small, "TUN", 3) # Tunisia
d_small <- give_type(d_small, "TZA", 5) # Tanzania
d_small <- give_type(d_small, "UGA", 5) # Uganda

#Slide 49
d_small <- give_type(d_small, "UKR", 6) # Ukraine * USSR
d_small <- give_type(d_small, "URY", 3) # Uruguay
d_small <- give_type(d_small, "UZB", 6) # Uzbekistan * USSR 

#Slide 50
d_small <- give_type(d_small, "VCT", 3) # St. Vincent and the Grenadines
d_small <- give_type(d_small, "VEN", 3) # Venezuela * flag 
d_small <- give_type(d_small, "VGB", 2) # British Virgin Islands * flag

#Slide 51
d_small <- give_type(d_small, "YEM", 5) # Yemen * flag 
d_small <- give_type(d_small, "ZMB", 5) # Zambia
d_small <- give_type(d_small, "ZWE", 5) # Zimbabwe

#Other
d_small <- give_type(d_small, "RUS", 6) # Russia * USSR


#### Giving fmrUSSR and fmrYugo 
d_small <- give_fmrUSSR(d_small, "ARM", 1) # Armenia 
d_small <- give_fmrUSSR(d_small, "AZE", 1) # Azerbaijan
d_small <- give_fmrUSSR(d_small, "BLR", 1) # Belarus
d_small <- give_fmrUSSR(d_small, "EST", 1) # Estonia 
d_small <- give_fmrUSSR(d_small, "GEO", 1) # Georgia
d_small <- give_fmrUSSR(d_small, "KAZ", 1) # Kazakhstan
d_small <- give_fmrUSSR(d_small, "KGZ", 1) # Kyrgyzstan 
d_small <- give_fmrUSSR(d_small, "LVA", 1) # Latvia
d_small <- give_fmrUSSR(d_small, "LTU", 1) # Lithuania
d_small <- give_fmrUSSR(d_small, "MDA", 1) # Moldova
d_small <- give_fmrUSSR(d_small, "RUS", 1) # Russia
d_small <- give_fmrUSSR(d_small, "TJK", 1) # Tajikistan
d_small <- give_fmrUSSR(d_small, "TKM", 1) # Turkmenistan 
d_small <- give_fmrUSSR(d_small, "UKR", 1) # Ukraine
d_small <- give_fmrUSSR(d_small, "UZB", 1) # Uzbekistan

# Yugoslavia
d_small <- give_fmrYugo(d_small, "HRV", 1) # Croatia 
d_small <- give_fmrYugo(d_small, "SVN", 1) # Slovenia
d_small <- give_fmrYugo(d_small, "BIH", 1) # Bosnia & Herzegovina
d_small <- give_fmrYugo(d_small, "MKD", 1) # Macedonia 
d_small <- give_fmrYugo(d_small, "MNE", 1) # Montenegro
d_small <- give_fmrYugo(d_small, "SRB", 1) # Serbia

# Flags



d_small <- give_flag(d_small, "CYM", 1) # Cayman Islands * flag, maybe 6
d_small <- give_flag(d_small, "GNQ", 1) # Equatorial Guinea * flag as good case
d_small <- give_flag(d_small, "IRN", 1) # Iran * maybe 0 interesting case
d_small <- give_flag(d_small, "IRQ", 1) # Iraq * flag
d_small <- give_flag(d_small, "LBN", 1) # Lebanon * flag
d_small <- give_flag(d_small, "MAC", 1) # China, Macao SAR * flag
d_small <- give_flag(d_small, "NGA", 1) # Nigeria * flag
d_small <- give_flag(d_small, "OMN", 1) # Oman * flag
d_small <- give_flag(d_small, "SVK", 1) # Slovakia * flag
d_small <- give_flag(d_small, "SWZ", 1) # Eswatini * flag





# Merge Format
final_data <- merge(d,d_small, by = 'countrycode')
write_dta(final_data, "pwt1001_typed.dta")



