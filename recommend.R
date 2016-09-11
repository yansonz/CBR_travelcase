setSimilarityValue = function(converted_user_input) {
  #setSimSeason - asymetric
  print(class(converted_user_input))
  print(converted_user_input)
  for(i in 1:nrow(new_data)) {
    sim_table$nSeasons[i] = table_sim_season[as.numeric(converted_user_input["nSeasons"]), new_data$nSeasons[i]]
  }
  print("##")
  #setSimHolidayType
  for(i in 1:nrow(new_data)) {
    if(new_data$nHolidayType[i] == converted_user_input["nHolidayType"]) {
      value = holiday_class1
    }
    # Compare group id
    else if(lookup_holiday$groupID[which(as.numeric(converted_user_input["nHolidayType"]) == lookup_holiday$value)]
            == lookup_holiday$groupID[which(new_data$nHolidayType[i] == lookup_holiday$value)]) {
      value = holiday_class2
    }
    else {
      value = holiday_class3
    }
    
    sim_table$nHolidayType[i] = value
  }
  print("###")
  #setSimPriceType
  for(i in 1:nrow(new_data)) {
    sim_table$nPrice[i] = ifelse(converted_user_input["nPrice"] >= new_data$nPrice[i], price_class1, price_class2)
  }
  
  #setSimTransportation - asymetric
  for(i in 1:nrow(new_data)) {
    sim_table$nTransportation[i] = table_sim_transportation[as.numeric(converted_user_input["nTransportation"]), new_data$nTransportation[i]]
  }
  
  #setSimNumOfPerson
  for(i in 1:nrow(new_data)) {
    sim_table$nNumberOfPersons[i] = ifelse(abs(converted_user_input["nNumberOfPersons"] - new_data$nNumberOfPersons[i]) < numOfPerson_tolerance, numOfPerson_class1, numOfPerson_class2)
  }
  
  print("!!!")
  #setSimRegion
  # get Euclidian Distance
  getDistance = function(x, y) {
    print(c(x, y))
    retVal = dist(rbind(c(lookup_region$lon[x], lookup_region$lat[x]),
                        c(lookup_region$lon[y], lookup_region$lat[y])))

    retVal    
  }
  
  user_index = lookup_region$value[lookup_region$value %in% converted_user_input["nRegion"]]

  for(i in 1:nrow(new_data)) {
    table_index = lookup_region$value[lookup_region$value %in% new_data$nRegion[i]]
    sim_table$nRegion[i] = getDistance(user_index, table_index)
  }
  
  #setSimDuration
  for(i in 1:nrow(new_data)) {
    sim_table$nDuration[i] = ifelse(converted_user_input["nDuration"] <= new_data$nDuration[i], duration_class1, duration_class2)
  }
  
  #setAccommodation
  for(i in 1:nrow(new_data)) {
    sim_table$nAccommodation[i] = table_sim_accommodation[as.numeric(converted_user_input["nAccommodation"]), new_data$nAccommodation[i]]
  }
  
  print("####")
  print(head(sim_table))
  print("####")
  sim_table
}

#normalise datas
normalise = function(x) {
  if(max(x) - min(x) == 0){
    x
  }
  else {
    (x-min(x)) / (max(x) - min(x))
  }
}

getTop5Case = function(user_input) {
  # convert user input
  converted_user_input = convert_user_input(user_input)
  print(converted_user_input)
  print(class(converted_user_input))

  # test
  #converted_user_input = c(2, 2, 1, 12, 20, 2, 7, 7, 1)
  #names(converted_user_input) = c("nHolidayType", "nPrice", "nTransportation", "nNumberOfPersons", "nRegion", "nSeasons", "nDuration", "nAccommodation")

  #Similarity
  local_sim_table = setSimilarityValue(converted_user_input)
  sim_table <<- local_sim_table
  
  # normalisation
  local_sim_table = as.data.frame(apply(local_sim_table, 2, normalise))
  local_sim_table["nRegion"] = apply(sweep(local_sim_table["nRegion"] , 1, 1), 1, abs)

  #print(head(local_sim_table))

  # weight
  #c("nHolidayType", "nPrice", "nTransportation", "nNumberOfPersons", "nRegion", "nSeasons", "nDuration", "nAccommodation")
  weight = c(2,    # HolidayType
             1.5,  # Price
             1.2,  # Transportation
             1,    # NumberOfPersons
             3,    # Region
             1.5,  # Seasons
             0.8,  # Duration
             1.2   # Accommodation
             )
  
  # calculate weighted mean
  similarity = apply(local_sim_table, 1, weighted.mean, w=weight/sum(weight))
  
  # sim_table2 is for displaying the result on Shiny
  sim_table2 <<- data.frame(JourneyCode = travelCases$Case, Total = round(similarity, 3), round(local_sim_table, 3))
  names(sim_table2) <<- c("JourneyCode", "Total", names(travelCases)[3:10])
  
  #print(head(similarity))
  
  similarity = data.frame(caseID = travelCases$JourneyCode, Total = similarity)
  
  # find similarity
  #similarity = apply(new_data, 1, getCosineDistance)
  #similarity = apply(new_data$nHolidayType, 1, getDistance)
  
  # sort
  sorted_sim = similarity[order(similarity["Total"], decreasing = TRUE), ]
  print(head(sorted_sim))
  
  # kmeans(cbind(sim_table$nRegion, sim_table$nHolidayType), 4)
  # top 5
  travelCases[sorted_sim$caseID[1:5], -2]
}
