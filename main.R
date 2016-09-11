#deploy_status = "EVENT"
deploy_status = "RELEASE"
filePath = ifelse(deploy_status == "RELEASE", "./", "./UI/")

#print(filePath)
print(system.time(source(paste(filePath, "R/lookup_table.R", sep = ""))))
print(system.time(source(paste(filePath, "R/data.R", sep = ""))))
print(system.time(source(paste(filePath, "R/recommend.R", sep = ""))))
