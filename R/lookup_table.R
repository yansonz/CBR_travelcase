seasons = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
lookup_seasons = data.frame(season = seasons,
                           value = 1:length(seasons))#value = rep(1:4, each = 3))

holidayType = c("Active", "Bathing", "City", "Language", "Education", "Recreation", "Skiing", "Wandering")
lookup_holiday = data.frame(holidayType = holidayType,
                            value = 1:length(holidayType),
                            groupID = c(4, 3, 1, 2, 2, 3, 4, 1))

price = c("<= 500", "> 500 and <= 1000", "> 1000 and <= 1500", "> 1500 and <= 2000", "> 2000 and <= 2500", "> 2500 and <= 3000", "> 3000 and <= 3500", "> 3500 and <= 4000", "> 4000")
lookup_price = data.frame( price = price,
                           value = 1:length(price))

transportation = c("Car", "Coach", "Plane", "Train")
lookup_transportation = data.frame(transportation = transportation,
                                   value = 1:length(transportation))

accommodation = c("HolidayFlat", "OneStar", "TwoStars", "ThreeStars", "FourStars", "FiveStars")
lookup_accommodation = data.frame(accommodation = accommodation,
                                  value = 1:length(accommodation))

region = c("AdriaticSea", "Algarve", "Allgaeu", "Alps", "Atlantic", "Attica", "Balaton", "BalticSea", "Bavaria", "Belgium"
           , "BlackForest", "Bornholm", "Brittany", "Bulgaria", "Cairo", "Carinthia", "Chalkidiki", "Corfu", "Corsica", "CostaBlanca"
           , "CostaBrava", "CotedAzur", "Crete", "Czechia", "Denmark" , "Egypt", "England", "ErzGebirge", "Fano", "France"
           , "Fuerteventura", "GiantMountains", "GranCanaria", "Harz", "Holland", "Ibiza", "Ireland", "LakeGarda", "Lolland", "Madeira"
           , "Mallorca", "Malta", "Normandy", "NorthSea", "Poland", "Rhodes", "Riviera", "SalzbergerLand", "Scotland", "Slowakei"  
           , "Styria", "Sweden", "Teneriffe", "Thuringia", "Tunisia", "TurkishAegeanSea", "TurkishRiviera", "Tyrol", "Wales"
           , "South Korea", "Japan", "China", "Singapore", "Indonesia", "New Zealand")
lookup_region = data.frame(region = region,
                           value = 1:length(region),
                           lon = c(17.29028, -7.930834, 14.84378, -84.60326, -95.01388, -87.2489, 17.73404, 19.86328, 11.49789, 4.469936
                                   , -104.7008, 14.86688, -2.932644, 25.48583, 31.23571, 14.18059, 23.28709, 19.92235, 9.012893, -0.2643455
                                   , -77.13571, -97.89565, 24.80927, 15.47296, 9.501785, 30.8025, -1.17432, 13, 13.01942, 2.213749
                                   , -14.05368, 15.62222, -15.54744, 10.23836, 5.291266, 1.420598, -8.24389, 10.63514, 11.46493, -16.95947
                                   , 3.017571, 14.37542, -90.29734, 3.515625, 19.14514, 28.21748, -97.81489, 13.05501, -4.202646, 19.69902
                                   , 14.46998, 18.6435, 153.0486, 10.84535, 9.537499, 23.42998, -80.62598, 11.60149, -3.783712
                                   , 127.7669, 138.2529, 104.1954, 103.8198, 113.9213, 174.886),
                           lat = c(41.8550, 37.01795, 46.84286, 33.14818, 41.4036, 40.2942, 46.83027, 58.48795, 48.79045, 50.50389
                                   , 39.01305, 55.16043, 48.20205, 42.73388, 30.04442, 46.7222, 40.3695, 39.62498, 42.0396, 38.50438
                                   , -12.04578, 33.24449, 35.24012, 49.81749, 56.26392, 26.82055, 52.35552, 50.58, 43.83982, 46.22764
                                   , 28.35874, 50.76722, 27.92022, 51.80952, 52.13263, 38.90673, 53.41291, 45.60494, 54.72754, 32.76071
                                   , 39.69526, 35.9375, 38.72088, 56.51102, 51.91944, 36.43496, 27.29865, 47.80949, 56.49067, 48.66903
                                   , 47.35934, 60.12816, -27.45639, 51.01099, 33.88692, 39.05043, 28.03313, 47.25374, 52.13066
                                   , 35.90776, 36.20482, 35.86166, 1.352083, -0.789275, -40.90056)
)

convert_season_to_value = function(seasons) {
  index = 1
  retVal = numeric(length(seasons))
  
  for(i in seasons) {
    #print(lookup_seasons$value[lookup_seasons$season %in% i])
    retVal[index] = lookup_seasons$value[lookup_seasons$season %in% i]
    index = index + 1
  }
  retVal
}

convert_holiday_to_value = function(holiday) {
  index = 1
  retVal = numeric(length(holiday))
  
  for(i in holiday) {
    #print(lookup_months$value[lookup_months$month %in% i])
    retVal[index] = lookup_holiday$value[lookup_holiday$holidayType %in% i]
    index = index + 1
  }
  retVal
}

parse_user_input = function(str) {
  which(price == str)
}

convert_price_to_value = function(price) {
  index = 1
  retVal = numeric(length(price))
  
  price_to_index = function(price) {
    if(price <= 500) (1)
    else if(price > 500 & price <= 1000 ) (2)
    else if(price > 1000 & price <= 1500 ) (3)
    else if(price > 1500 & price <= 2000 ) (4)
    else if(price > 2000 & price <= 2500 ) (5)
    else if(price > 2500 & price <= 3000 ) (6)
    else if(price > 3000 & price <= 3500 ) (7)
    else if(price > 3500 & price <= 4000 ) (8)
    else (9)
  }
  
  for(i in price) {
    #print(lookup_months$value[lookup_months$month %in% i])
    retVal[index] = price_to_index(i)
    index = index + 1
  }
  retVal
}

convert_transportation_to_value = function(transportation) {
  index = 1
  retVal = numeric(length(transportation))
  
  for(i in transportation) {
    #print(lookup_months$value[lookup_months$month %in% i])
    retVal[index] = lookup_transportation$value[lookup_transportation$transportation %in% i]
    index = index + 1
  }
  retVal
}

convert_accommodation_to_value = function(accommodation) {
  index = 1
  retVal = numeric(length(accommodation))
  
  for(i in accommodation) {
    #print(lookup_months$value[lookup_months$month %in% i])
    retVal[index] = lookup_accommodation$value[lookup_accommodation$accommodation %in% i]
    index = index + 1
  }
  retVal
}

convert_region_to_value = function(region) {
  index = 1
  retVal = numeric(length(region))
  
  for(i in region) {
    #print(lookup_months$value[lookup_months$month %in% i])
    retVal[index] = lookup_region$value[lookup_region$region %in% i]
    index = index + 1
  }
  retVal
}

convert_user_input = function(user_input) {
  converted_user_input = data.frame(convert_holiday_to_value(user_input["HolidayType"]),
                              as.numeric(parse_user_input(user_input["Price"])),
                              convert_transportation_to_value(user_input["Transportation"]),
                              as.numeric(user_input["NumberOfPersons"]),
                              convert_region_to_value(user_input["Region"]),
                              convert_season_to_value(user_input["Season"]),
                              as.numeric(user_input["Duration"]),
                              convert_accommodation_to_value(user_input["Accommodation"]))
  names(converted_user_input) = c("nHolidayType", "nPrice", "nTransportation", "nNumberOfPersons", "nRegion", "nSeasons", "nDuration", "nAccommodation")

  converted_user_input
}

#
# local similarity table - holidayType
holiday_class1 = 1
holiday_class2 = .8
holiday_class3 = .2
# local similarity table - price : Less is perfect
price_class1 = 1
price_class2 = 0
table_sim_price = diag(length(price))
table_sim_price[lower.tri(table_sim_price, diag = TRUE)] = price_class1
#table_sim_price[upper.tri(table_sim_price, diag = FALSE)] = price_class2
dimnames(table_sim_price) = list(price, price)
# local similarity table - transportation
transportation_class1 = 1
transportation_class2 = .7
transportation_class3 = .5
transportation_class4 = .3
transportation_class5 = 0
table_sim_transportation = rbind(
  c(transportation_class1, transportation_class3, transportation_class5, transportation_class3),
  c(transportation_class3, transportation_class1, transportation_class5, transportation_class2),
  c(transportation_class4, transportation_class3, transportation_class1, transportation_class3),
  c(transportation_class3, transportation_class2, transportation_class5, transportation_class1)
)
dimnames(table_sim_transportation) = list(transportation, transportation)
# local similarity table - num of person
numOfPerson_tolerance = 5
numOfPerson_class1 = 1
numOfPerson_class2 = 0.4
# local similarity table - region
region_class1 = 1
region_class2 = .7
region_class3 = .4
region_class4 = .2
region_class5 = .1
# local similarity table - season
season_class1 = 1
season_class2 = .8
season_class3 = .4
season_class4 = 0
table_sim_season = rbind(
  c(season_class1, season_class2, season_class3, season_class3, season_class3, season_class4, season_class4, season_class4, season_class3, season_class3, season_class3, season_class2),
  c(season_class2, season_class1, season_class3, season_class3, season_class3, season_class4, season_class4, season_class4, season_class3, season_class3, season_class3, season_class2),
  
  c(season_class3, season_class3, season_class1, season_class2, season_class2, season_class3, season_class3, season_class3, season_class4, season_class4, season_class4, season_class3),
  c(season_class3, season_class3, season_class2, season_class1, season_class2, season_class3, season_class3, season_class3, season_class4, season_class4, season_class4, season_class3),
  c(season_class3, season_class3, season_class2, season_class2, season_class1, season_class3, season_class3, season_class3, season_class4, season_class4, season_class4, season_class3),
  
  c(season_class4, season_class4, season_class3, season_class3, season_class3, season_class1, season_class2, season_class2, season_class3, season_class3, season_class3, season_class4),
  c(season_class4, season_class4, season_class3, season_class3, season_class3, season_class2, season_class1, season_class2, season_class3, season_class3, season_class3, season_class4),
  c(season_class4, season_class4, season_class3, season_class3, season_class3, season_class2, season_class2, season_class1, season_class3, season_class3, season_class3, season_class4),
  
  c(season_class3, season_class3, season_class4, season_class4, season_class4, season_class3, season_class3, season_class3, season_class1, season_class2, season_class2, season_class3),
  c(season_class3, season_class3, season_class4, season_class4, season_class4, season_class3, season_class3, season_class3, season_class2, season_class1, season_class2, season_class3),
  c(season_class3, season_class3, season_class4, season_class4, season_class4, season_class3, season_class3, season_class3, season_class2, season_class2, season_class1, season_class3),
  
  c(season_class2, season_class2, season_class3, season_class3, season_class3, season_class4, season_class4, season_class4, season_class3, season_class3, season_class3, season_class1)
)
dimnames(table_sim_season) = list(seasons, seasons)
# local similarity table - duration - Greater is perfect
duration_class1 = 1
duration_class2 = .3
# local similarity table - accommodation - Greater is perfect
accommodation_class1 = 1
accommodation_class2 = .8
accommodation_class3 = .5
accommodation_class4 = .2
accommodation_class4 = 0
table_sim_accommodation = diag(length(price))
table_sim_accommodation[upper.tri(table_sim_accommodation, diag = TRUE)] = accommodation_class1
# table_sim_accommodation = rbind(
#   c(accommodation_class1, accommodation_class2, accommodation_class3, accommodation_class4, accommodation_class4, accommodation_class4),
#   c(accommodation_class2, accommodation_class1, accommodation_class2, accommodation_class4, accommodation_class4, accommodation_class4),
#   c(accommodation_class3, accommodation_class2, accommodation_class1, accommodation_class4, accommodation_class4, accommodation_class4),
#   c(accommodation_class4, accommodation_class4, accommodation_class4, accommodation_class1, accommodation_class2, accommodation_class3),
#   c(accommodation_class4, accommodation_class4, accommodation_class4, accommodation_class2, accommodation_class1, accommodation_class2),
#   c(accommodation_class4, accommodation_class4, accommodation_class4, accommodation_class3, accommodation_class2, accommodation_class1)
# )
# dimnames(table_sim_accommodation) = list(accommodation, accommodation)