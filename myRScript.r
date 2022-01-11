getwd()

setwd("C:/Users/Filip/Desktop/DataCA1")

install.packages("tidyverse")
library(tidyverse)
#property file for Dublin have been downloaded from https://data.smartdublin.ie/dataset/dublin-residential-property-price-register
#school data has been downloaded from https://data.smartdublin.ie/dataset/schools-in-dublin-region

#Property dataset
DublinYear2019 = read.csv("ppr-2019-dublin.csv", header = TRUE, sep =",")

#Schools dataset
SecondarySchools = read.csv("post-primary-schools-2019-2020-1.csv", header = TRUE, sep =",")
SpecialSchools = read.csv("special-schools_dublin_2019-2020.csv", header = TRUE, sep =",")
MainSchools = read.csv("mainstream-schools_dublin_2019-2020.csv", header = TRUE, sep =",")

# I don't need some columns so I will remove them from each file.

DublinYear2019 = DublinYear2019[,-which(names(DublinYear2019) %in% c("Property.Size.Description","VAT.Exclusive","Not.Full.Market.Price","County","Date.of.Sale..dd.mm.yyyy."))]
SpecialSchools = SpecialSchools[,-which(names(SpecialSchools) %in% c("County.Description","Local.Authority.Description","Phone.No.","Email","School.Type.Description","Ethos.Description","Island..Y.N.","DEIS..Y.N.","Irish.Classification.Description","Gaeltacht.Indicator..Y.N.","Enrolment.per.Return","Address..Line.4."))]
SecondarySchools = SecondarySchools[,-which(names(SecondarySchools) %in% c("Roll.Number","Local.Authority","Principal.Name","Email","Phone","DEIS..Y.N.","Gaeltacht.Area.Location..Y.N.","Island.Location..Y.N.","Fee.Paying.School..Y.N.","Pupil.Attendance.Type","School.Gender...Post.Primary","Irish.Classification...Post.Primary","Post.Primary.School.Type","Ethos.Religion","FEMALE","MALE","TOTAL..2019.20.","County","Address.4","Address.3"))]
MainSchools = MainSchools[,-which(names(MainSchools) %in% c("Roll.Number","County.Description","Local.Authority.Description","Principal.Name","Email","Phone.No.","DEIS..Y.N.","Gaeltacht.Indicator..Y.N.","Island..Y.N.","Irish.Classification.Description","Ethos.Description","Female","Male","Total","Address..Line.4.","Address..Line.3."))]

# Some columns have bad names so lets change them
#Renaming Columns 

DublinYear2019 = rename(DublinYear2019, Price = Price....)
DublinYear2019 = rename(DublinYear2019, PostCode = Postal.Code)
DublinYear2019 = rename(DublinYear2019, PropertyType = Description.of.Property)
# The data for the prices was inputted wrong , it got put in with symbols and commas.

DublinYear2019$Price = sub("€", "", DublinYear2019$Price)
DublinYear2019$Price = gsub(",", "", DublinYear2019$Price)
DublinYear2019$Price = as.numeric(DublinYear2019$Price)
  


# I don't want a long post code string , I will change to letter and numbers instead
DublinYear2019$PostCode = sub("Dublin 1", "D1", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 2", "D2", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 3", "D3", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 4", "D4", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 5", "D5", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 6", "D6", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 7", "D7", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 8", "D8", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 9", "D9", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 10", "D10", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 11", "D11", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 12", "D12", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 13", "D13", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 14", "D14", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 15", "D15", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 16", "D16", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 17", "D17", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 18", "D18", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 19", "D19", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 20", "D20", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 21", "D21", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 22", "D22", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 23", "D23", DublinYear2019$PostCode)
DublinYear2019$PostCode = sub("Dublin 24", "D24", DublinYear2019$PostCode)
# Will merge D6w into just D6 
DublinYear2019$PostCode = sub("D6w", "D6", DublinYear2019$PostCode)

#I will simply rename the values for Property Type into New Property and Second Hand Property
DublinYear2019$PropertyType = sub("New Dwelling house /Apartment", "NewProperty", DublinYear2019$PropertyType)
DublinYear2019$PropertyType = sub("Second-Hand Dwelling house /Apartment", "SecondHand Property", DublinYear2019$PropertyType)

unique(DublinYear2019$PropertyType)
#rename irish value Teach/Árasán Cónaithe Atháimhe to SecondHand Property
DublinYear2019$PropertyType = sub("Teach/Árasán Cónaithe Atháimhe", "SecondHand Property", DublinYear2019$PropertyType)

#Dublin postal codes only go from 1-24 so newer areas like Balbriggan , Swords etc wont have a postal code
#Some Post code column values are empty so I will fill these with County Dublin . 
DublinYear2019$PostCode[DublinYear2019$PostCode==""] = "CoDublin"

# some Outlier removal
max(DublinYear2019$Price) # this is 129500000 and if you read the " address " column it will tell you its 266 Apts & 18 Commercial Units so lets get rid of it.

# I will remove the top 3 outliers 
DublinYear2019 = DublinYear2019[-which(DublinYear2019$Price == "129500000"), ]
DublinYear2019 = DublinYear2019[-which(DublinYear2019$Price == "84420800"), ]
DublinYear2019 = DublinYear2019[-which(DublinYear2019$Price == "48324000"), ]



#Since now all of the Post code fields have a value , I will clean up the address fields a little bit so there is no repetition
DublinYear2019$Address = sub("DUBLIN 1", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 2", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 3", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 4", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 5", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 6", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 7", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 8", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 9", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 10", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 11", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 12", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 13", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 14", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 15", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 16", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 17", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 18", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 19", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 20", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 21", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 22", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 23", "", DublinYear2019$Address)
DublinYear2019$Address = sub("DUBLIN 24", "", DublinYear2019$Address)
###################################################################### for the lowercase Dublins 
DublinYear2019$Address = sub("Dublin 1", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 2", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 3", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 4", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 5", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 6", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 7", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 8", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 9", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 10", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 11", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 12", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 13", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 14", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 15", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 16", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 17", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 18", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 19", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 20", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 21", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 22", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 23", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Dublin 24", "", DublinYear2019$Address)
## little more cleaning
DublinYear2019$Address = sub(", DUBLIN", "", DublinYear2019$Address)
DublinYear2019$Address = sub(", dublin", "", DublinYear2019$Address)
DublinYear2019$Address = sub("County Dublin", "", DublinYear2019$Address)
DublinYear2019$Address = sub("Co Dublin", "", DublinYear2019$Address)
DublinYear2019$Address = sub("CoDublin", "", DublinYear2019$Address)
DublinYear2019$Address = sub("CO DUBLIN", "", DublinYear2019$Address)


# School files underneath

MainSchools = rename(MainSchools, School = Official.Name)
MainSchools = rename(MainSchools, Address1 = Address..Line.1.)
MainSchools = rename(MainSchools, Address2 = Address..Line.2.)
MainSchools = rename(MainSchools, PostCode = Eircode)
# cleaning the eircodes. first will target the D01 format

MainSchools$PostCode <- sub("^(D(?:0[1-9]|1[0-9]|2[0-4])).*$", "\\1", MainSchools$PostCode)

# now the d1 format
MainSchools$PostCode <- sub("^(D(?:[0-9]|0[1-9]|1[0-9]|2[0-4])).*$", "\\1", MainSchools$PostCode)
##sub the K eircodes with nothing inside
MainSchools$PostCode <- sub("^(K).*$", "", MainSchools$PostCode)
#same with A eircodes.
MainSchools$PostCode <- sub("^(A).*$", "", MainSchools$PostCode)



# now Ill make all empty eircode values to County Dublin
MainSchools$PostCode[MainSchools$PostCode==""] = "CoDublin"

# Remove duplicate rows
MainSchools = MainSchools[!duplicated(MainSchools$School), ]

MainSchools$SchoolPostCode = MainSchools$PostCode

#Secondary schools file now .
SecondarySchools = rename(SecondarySchools, School = Official.School.Name)
SecondarySchools = rename(SecondarySchools, Address1 = Address.1)
SecondarySchools = rename(SecondarySchools, Address2 = Address.2)
SecondarySchools = rename(SecondarySchools, PostCode = Eircode)

SecondarySchools$PostCode <- sub("^(D(?:0[1-9]|1[0-9]|2[0-4])).*$", "\\1", SecondarySchools$PostCode)

# now the d1 format
SecondarySchools$PostCode <- sub("^(D(?:[0-9]|0[1-9]|1[0-9]|2[0-4])).*$", "\\1", SecondarySchools$PostCode)


# filling in the empty values.
SecondarySchools[113,4] = "D04"
SecondarySchools[114,4] = "D15"
SecondarySchools[115,4] = "D24"
SecondarySchools[117,4] = "D13"
SecondarySchools[182,4] = "D09"

#removing all the eircodes now so only post codes.
SecondarySchools$PostCode = sub("^(K).*$", "", SecondarySchools$PostCode)
SecondarySchools$PostCode = sub("^(A).*$", "", SecondarySchools$PostCode)
SecondarySchools$PostCode[SecondarySchools$PostCode==""] = "CoDublin"


# Remove duplicate rows
SecondarySchools = SecondarySchools[!duplicated(SecondarySchools$School), ]

SecondarySchools$SchoolPostCode = SecondarySchools$PostCode


# special schools file
SpecialSchools = rename(SpecialSchools, School = Address..Line.1.)
SpecialSchools = rename(SpecialSchools, Address1 = Address..Line.2.)
SpecialSchools = rename(SpecialSchools, Address2 = Address..Line.3.)
SpecialSchools = rename(SpecialSchools, PostCode = Eircode)

SpecialSchools$PostCode <- sub("^(D(?:0[1-9]|1[0-9]|2[0-4])).*$", "\\1", SpecialSchools$PostCode)
SpecialSchools$PostCode <- sub("^(D(?:[0-9]|0[1-9]|1[0-9]|2[0-4])).*$", "\\1", SpecialSchools$PostCode)
SpecialSchools$PostCode = sub("^(K).*$", "", SpecialSchools$PostCode)
SpecialSchools$PostCode = sub("^(A).*$", "", SpecialSchools$PostCode)
SpecialSchools$PostCode[SpecialSchools$PostCode==""] = "CoDublin"

SpecialSchools = SpecialSchools[!duplicated(SpecialSchools$School), ]

SpecialSchools$SchoolPostCode = SpecialSchools$PostCode

# combing all 3 schools files into one 
AllSchools = rbind(MainSchools, SecondarySchools, SpecialSchools)
#just in case there is any duplicates .
AllSchools = AllSchools[!duplicated(AllSchools$School), ]
#sort the row numbers
rownames(AllSchools) = NULL     

library(ggplot2)
# find mean for each postcode
tapply(DublinYear2019$Price, DublinYear2019$PostCode, mean)
#create a new data frame 
Postcode = c("CoDublin", "D1", "D2", "D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D20","D22","D24")
Price = c(458379.8, 613516.4, 586021.3, 616726.1,705740.4,404012.4,860760.0,399754.1,462002.3,397750.8,212025.3,281736.7,323489.1,411529.6,587230.7,332335.5,469138.6,906712.0,546944.0,1377512.3,263896.9,289640.7)
Dublin_Average = data.frame(Postcode,Price)



#plot 1 of Dublin average property prices per Post code.
AveragePlot = ggplot(Dublin_Average, 
       aes(x = Postcode, y=Price)) +
  geom_col(fill = "Red", col = "Black", alpha=I(.2)) +
  scale_y_continuous(name="Price €", labels = scales::comma)

AveragePlot + labs(title = "Figure 3: Average Dublin Property Prices per Postcode")

#plot 2 of Dublin property post code frequency 
FrequencyPlot = ggplot(DublinYear2019, 
       aes(x = PostCode)) +
  geom_bar(fill = "Red", col = "Black", alpha=I(.2))

FrequencyPlot + labs(title = "Figure 4: Frequency of Dublin Property Post Codes")

#plot 3 Dublin schools post code frequency 
FrequencyPlotforSchools = ggplot(AllSchools, 
                       aes(x = SchoolPostCode)) +
  geom_bar(fill = "Red", col = "Black", alpha=I(.2))

FrequencyPlotforSchools + labs(title = "Figure 5: Frequency of Dublin School Post Codes")

#plot 4 distribution of Property prices limited to 1.5million.

qplot(DublinYear2019$Price,
      geom="histogram",
      binwidth = 50000,  
      main = "Figure 6: Histogram for Property Prices", 
      ylab = "Count",
      xlab = "Prices",  
      fill=I("Red"), 
      col=I("Black"), 
      alpha=I(.2),
      xlim=c(50000,1500000))


     
                                