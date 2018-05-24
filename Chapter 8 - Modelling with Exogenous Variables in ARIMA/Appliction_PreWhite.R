library(readxl)
library(tseries)

#Read Data
Prognosis17 <- read_excel("Data/Prognosis17.xlsx")
Deseasoned <- read.csv("Deseasoned.txt", sep="")

source("Pre-whitening.R")

#Remove unused indices and transform to daily
Prognosis17 = Prognosis17[-c(1,2)]
Prognosis17 = HoursToDaysTransformer(Prognosis17)

#Perform ADF test
adf.test(ts(Prognosis17$ProductionPrognosis))
adf.test(ts(Prognosis17$ConsumptionPrognosis))
adf.test(ts(Prognosis17$WindPowerPrognosis))
adf.test(ts(Deseasoned))

Model_Specifier_ekso(Prognosis17$WindPowerPrognosis)

prewhite(x = Prognosis17$WindPowerPrognosis, y = Deseasoned, arima_order = c(1,0,3), titl = "" )

diff_prod = diff(ts(Prognosis17$ProductionPrognosis))
diff_cons = diff(ts(Prognosis17$ConsumptionPrognosis))
diff_dese = diff(ts(Deseasoned))

#Retest ADF
adf.test(diff_prod)
adf.test(diff_cons)


#Perform Prewhitening
Model_Specifier_ekso(diff_prod)

prewhite(x = diff_prod, y = diff_dese, arima_order = c(4,0,4), titl = "")

Model_Specifier_ekso(diff_cons)

prewhite(x = diff_cons, y = diff_dese, arima_order = c(4,0,5), titl = "")

Model_Specifier_ekso(diff_wind)

prewhite(x = Prognosis17$WindPowerPrognosis, y = Deseasoned, arima_order = c(0,0,3), titl = "Series: Diff wind prognosis & Diff demeaned")



