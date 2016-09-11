#require(xlsx)

# > print(system.time(source(paste(filePath, "R/data.R", sep = ""))))
# user  system elapsed 
# 9.912   0.066  10.022 
#travelCases = read.xlsx(paste(filePath, "travelcase/newDataSet.xls", sep = ""), sheetName = "newDataSet")

# > print(system.time(source(paste(filePath, "R/data.R", sep = ""))))
# user  system elapsed 
# 0.127   0.002   0.128 
travelCases = read.csv(paste(filePath, "travelcase/newDataSet.csv", sep = ""))

# shuffle the data set
#
#set.seed(9850)
#gp = runif(nrow(travelCases))
#travelCases = travelCases[order(gp), ]
# convert items into numeric values
new_data = data.frame(convert_holiday_to_value(travelCases$HolidayType),
                      convert_price_to_value(travelCases$Price),
                      convert_transportation_to_value(travelCases$Transportation),
                      travelCases$NumberOfPersons,
                      convert_region_to_value(travelCases$Region),
                      convert_season_to_value(travelCases$Season),
                      travelCases$Duration,
                      convert_accommodation_to_value(travelCases$Accommodation))
names(new_data) = c("nHolidayType", "nPrice", "nTransportation", "nNumberOfPersons", "nRegion", "nSeasons", "nDuration", "nAccommodation")

sim_table = as.data.frame(matrix(0, nrow = nrow(new_data), ncol = ncol(new_data)))
names(sim_table) = c("nHolidayType", "nPrice", "nTransportation", "nNumberOfPersons", "nRegion", "nSeasons", "nDuration", "nAccommodation")