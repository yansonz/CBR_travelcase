
numOftestcase = length(travelCases[, "Case"])

f = function(param) {
  index = which(travelCases[, "Case"] == param)
  print(paste("checking.....", index, "out of", numOftestcase))
  param %in% getTop5Case(new_data[index, ])$Case[1:5]
} 
result = apply(as.matrix(travelCases[, "Case"]), 1, f)

precision = sum(result) / numOftestcase * 100
print(precision)








