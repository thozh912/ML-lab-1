library(XLConnect)
wb = loadWorkbook("data/creditscoring.xls")
data = readWorksheet(wb, sheet = "credit", header = TRUE)
head(data)
