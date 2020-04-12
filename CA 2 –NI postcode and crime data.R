# Section 1 - NIPostcode dataset related tasks and solutions using R coding

# A) To display total number of rows, the structure of the data frame,and first 10 rows of the data frame containing all of the NIPostcode data

my_data <- read.csv("/Users/Roshini/Desktop/Data Science/CA2/NIPostcodes.csv", header = FALSE) #Reading the dataset and loading it into dataFrame
#header = FALSE as read.csv file as it is not reading 1st column
my_data
#Displaying the total number of rows
nrow(my_data)  #943033


str(my_data) # Displaying structure of the dataframe 


head(my_data, 10) #First 10 rows are displayed

## b) Add a suitable title for coloumn of the data
new_col_names <- c("organisation Name",
                   "Sub-building Name",
                   "Building Name",
                   "Number",
                   "Primary Thorfare",
                   "Alt Thorfare",
                   "Secondary Thorfare",
                   "Locality",
                   "Townland",
                   "Town",
                   "County",
                   "Postcode",
                   "x-coordinates",
                   "y-coordinates",
                   "Primary Key") 

colnames(my_data) <- new_col_names # assigning all new columns names to the data 

head(my_data,10) # Displaying top 10 data with there titles assigned.


## Finding the length of each column
length(my_data$`organisation Name`[my_data$`organisation Name`==""])
length(my_data$`Sub-building Name`[my_data$`Sub-building Name`==""])
length(my_data$`Building Name`[my_data$`Building Name`==""])
length(my_data$Number[my_data$Number==""])
length(my_data$`Primary Thorfare`[my_data$`Primary Thorfare`==""])
length(my_data$`Alt Thorfare`[my_data$`Alt Thorfare`==""])
length(my_data$`Secondary Thorfare`[my_data$`Secondary Thorfare`==""])
length(my_data$Locality[my_data$Locality==""])
length(my_data$Townland[my_data$Townland==""])
length(my_data$Town[my_data$Town==""])
length(my_data$County[my_data$County==""])
length(my_data$Postcode[my_data$Postcode==""])
length(my_data$`x-coordinates`[my_data$`x-coordinates`==""])
length(my_data$`y-coordinates`[my_data$`y-coordinates`==""])
length(my_data$`Primary Key`[my_data$`Primary Key`==""])

## c)Replace and recode all missing entries with a suitable identifier

my_data$`organisation Name`[my_data$`organisation Name`==""] <- NA
my_data$`Sub-building Name`[my_data$`Sub-building Name`==""] <- NA
my_data$`Building Name`[my_data$`Building Name`==""] <- NA
my_data$Number[my_data$Number==""] <- NA
my_data$`Primary Thorfare`[my_data$`Primary Thorfare`==""] <- NA
my_data$`Alt Thorfare`[my_data$`Alt Thorfare`==""] <- NA
my_data$`Secondary Thorfare`[my_data$`Secondary Thorfare`==""] <- NA
my_data$Locality[my_data$Locality==""] <- NA
my_data$Townland[my_data$Townland==""] <- NA
my_data$Town[my_data$Town ==""] <- NA
my_data$County[my_data$County==""] <- NA
my_data$Postcode[my_data$Postcode==""] <- NA
my_data$`x-coordinates`[my_data$`x-coordinates`==""] <- NA
my_data$`y-coordinates`[my_data$`y-coordinates`==""] <- NA
my_data$`Primary Key`[my_data$`Primary Key`==""] <- NA


View(my_data)

## d) Total number of missing values for each column in the postcode data 

Missing_count <- data.frame(sapply(my_data, function(x)sum(length(which(is.na(x))))))
Missing_count <- data.frame(Missing_count)
Missing_count

## c) the missing data displaying via suitable graphical output 
library(mice)
md.pattern(my_data)


library(VIM)

missing_values <- aggr(my_data, prop= FALSE, numbers = TRUE) # Here it is calculating missing data like the amout of missing and propostion of missing
summary(missing_values)

##e) Move the primary key identifier to the start of the dataset
my_data <-my_data[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]

head(my_data)

## f) Create a new dataset called Limavady_data. Store within it only information where locality, townland and town contain the name Limavady. Count and display the number of rows. 
##Store this information in a csv file called Limavady
attach(my_data)
limavady_data <- my_data[which( my_data$Locality =="LIMAVADY"| my_data$Townland == "LIMAVADY"| my_data$Town=="LIMAVADY"),] 
limavady_data
nrow(limavady_data)
write.csv(limavady_data,"Limavady.csv")

## g) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.

write.csv(my_data,"CleanNIPostcodeData.csv")


####SECTION2####

##a) Using R, amalgamate all of the crime data from each csv file into one dataset. Save this dataset into a csv file called AllNICrimeData. Count and show the number of rows in the AllNICrimeData dataset. 
#Do not hard code the dataset path into your R code.

rm(list = ls(all=TRUE))
getwd()
setwd("/Users/Roshini/Desktop/Data Science/CA2/NI Crime Data")

##Question a

#look for pattern in the particular path annd get filenames
file_names = list.files(pattern = "[.]csv$", recursive = TRUE)

#assuming tab separated values with a header    
AllNICrimeData_list = lapply(file_names, function(x)read.csv(x, header=T)) 

#assuming the same header/columns for all files
AllNICrimeData = do.call("rbind", AllNICrimeData_list) 
write.csv(AllNICrimeData,"AllNICrimeData.csv",row.names = FALSE)

##Question b

AllNICrimeData_new = subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category) )

##Question c
unique(AllNICrimeData$Crime.type)

library(dplyr)
AllNICrimeData_new=
  AllNICrimeData_new %>% mutate(Crime.type=recode_factor(Crime.type, 
                                                         'Anti-social behaviour' = 'ASBO', 
                                                         'Bicycle theft' = 'BITH',
                                                         'Burglary' = 'BURG',
                                                         'Criminal damage and arson' = 'CDAR',
                                                         'Drugs' = 'DRUG', 
                                                         'Other theft' = 'OTTH',
                                                         'Possession of weapons' = 'POFW',
                                                         'Public order' = 'PUBO',
                                                         'Robbery' = 'ROBY', 
                                                         'Shoplifting' = 'SHOP',
                                                         'Theft from the person' = 'THPR',
                                                         'Vehicle crime' = 'VECR',
                                                         'Violence and sexual offences'='VECO',
                                                         'Other crime' = 'OTCR'))
######

##Question d
counts <- table(AllNICrimeData_new$Crime.type)
my_plot = barplot(counts,main="Distribution of Crime Type"
                  ,ylab = 'freq',
                  col=c( rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.9,0.9,0.4,0.6) ,  rgb(0.3,0.3,0.4,0.6),rgb(0.7,0.1,0.4,0.6) , rgb(0.1,0.5,0.4,0.6) , rgb(0.3,0.9,0.9,0.6) ,  rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.1,0.9,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.9) ,  rgb(0.3,0.9,0.4,0.9)),
                  xlab = 'Crime Type',las=2)
text(my_plot, counts/2 , paste("", counts, sep="") ,cex=1) 

#######

## Question e

AllNICrimeData_new$Location <- gsub('On or near ', '', AllNICrimeData_new$Location)

#####

##Question f

AllNICrimeData_new=AllNICrimeData_new[!( AllNICrimeData_new$Location==""), ]

rows=seq(1,nrow(AllNICrimeData_new),1)
set.seed(100)

random_crime_sample=AllNICrimeData_new[sample(rows, 5000), ]
getwd()
CleanNIPostcodeData = read.csv('CleanNIPostcodeData.csv',header = TRUE)

head(CleanNIPostcodeData)

find_a_town <- function(i) {
  random_crime_sample$City_Town_Village<-CleanNIPostcodeData[match(toupper(random_crime_sample$Location),CleanNIPostcodeData$Primary.Thorfare),11]
}

find_a_town(toupper(random_crime_sample$Location))

########


## Question g

random_crime_sample$Context = NULL
VillageList = read.csv('VillageList.csv',header = TRUE)

add_town_data <- function(i) {
  random_crime_sample$POPULATION <- VillageList[match(random_crime_sample$City_Town_Village,toupper(VillageList$CITY.TOWN.VILLAGE)),2]
}

add_town_data(random_crime_sample$City_Town_Village)

write.csv(random_crime_sample,'random_crime_sample.csv',row.names=FALSE)








