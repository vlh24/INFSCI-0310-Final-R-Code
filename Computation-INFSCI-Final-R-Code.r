#INFSCI 0310 Final Project R Code
#Contributors: Valerie Hosler, Jerry Huang, and Michael Pacifico

# # # # # # # #

#External libraries used

#formattable is a function for formatting data frames
install.packages("formattable")

#Load external libraries
library(formattable)

# # # # # # # # #

#Covid cases from January 2020 - September 2020

#Allegheny County
AlleghenyCovid_perMonth = c(0, 0, 374, 1006, 554, 964, 5311, 2172, 2082)

#Philadelphia County
PhiladelphiaCovid_perMonth = c(0, 0, 1573, 11062, 5692, 3243, 4243, 3646, 3023)

#Table of cases
COVID_table = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Allegheny_County_Covid_Cases = c(AlleghenyCovid_perMonth),
  Philadelphia_County_Covid_Cases = c(PhiladelphiaCovid_perMonth))

formattable(COVID_table, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "purple",
  font.weight = "bold"))))

# # # # # # # # #

#Correlation between amount of homes sold and COVID cases
#January 2020 - September 2020

# # #

#Allegheny County
Allegheny_Amount_HomeSales = c(901, 934, 1204, 958, 644, 1132, 1972, 1762, 1602)

#Scatter plot
plot(AlleghenyCovid_perMonth, Allegheny_Amount_HomeSales, main = "New Covid Cases vs Amount of Home Sales", xlab = "New Cases per Month", ylab = "Amount of Home Sales", pch = 19)

#Adds the linear regression to the scatter plot
abline(lm(Allegheny_Amount_HomeSales~AlleghenyCovid_perMonth), col = "red")

#Correlation test between two sets
cor(Allegheny_Amount_HomeSales, AlleghenyCovid_perMonth, method = c("pearson"))
#0.8557511

# # #

#Philadelphia County
Philadelphia_Amount_HomeSales = c(1275, 1283, 1349, 1057, 771, 1158, 1775, 1758, 1849)

#Scatter plot
plot(PhiladelphiaCovid_perMonth, Philadelphia_Amount_HomeSales, main = "New Covid Cases vs Amount of Home Sales", xlab = "New Cases per Month", ylab = "Amount of Home Sales", pch = 19)

#Adds the linear regression to the scatter plot
abline(lm(Philadelphia_Amount_HomeSales~PhiladelphiaCovid_perMonth), col = "red")

#Correlation test between two variables
cor(Philadelphia_Amount_HomeSales, PhiladelphiaCovid_perMonth, method = c("pearson"))
#-0.2861365

# # # # # # # # #

#Amount of home sales January 2019 - September 2019 vs January 2020 - September 2020

# # #

#Allegheny County
Allegheny_Amount_HomeSales2019 = c(841, 836, 1154, 1272, 1616, 1582, 1609, 1544, 1255)

#Table to compare data
AC_table_2019_vs_2020_1 = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Amount_Home_Sales_2019 = c(Allegheny_Amount_HomeSales2019),
  Amount_Home_Sales_2020 = c(Allegheny_Amount_HomeSales))

formattable(AC_table_2019_vs_2020_1, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "purple",
  font.weight = "bold")), "Amount_Home_Sales_2020" = formatter("span", style = x~style(color =
  ifelse(x > c(Allegheny_Amount_HomeSales2019), "green", "red")))))

# # #

#Philadelphia County
Philadelphia_Amount_HomeSales2019 = c(1120, 1074, 1387, 1567, 1882, 1752, 1776, 1671, 1468)

#Table to compare data
PC_table_2019_vs_2020_1 = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Amount_Home_Sales_2019 = c(Philadelphia_Amount_HomeSales2019),
  Amount_Home_Sales_2020 = c(Philadelphia_Amount_HomeSales))

formattable(PC_table_2019_vs_2020_1, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "purple",
  font.weight = "bold")), "Amount_Home_Sales_2020" = formatter("span", style = x~style(color =
  ifelse(x > c(Philadelphia_Amount_HomeSales2019), "green", "red")))))

# # # # # # # # #

#Correlation between median days on the market and COVID cases
#January 2020 - September 2020

# # #

#Allegheny County
Allegheny_medianDays_Market = c(85, 86, 70, 65, 76, 72, 53, 54, 56)

#Scatter plot
plot(AlleghenyCovid_perMonth, Allegheny_medianDays_Market, main = "New Covid Cases vs Median Days on the Market", xlab = "New Cases per Month", ylab = "Median Days on the Market", pch = 19)

#Adds the linear regression to the scatter plot
abline(lm(Allegheny_medianDays_Market~AlleghenyCovid_perMonth), col = "red")

#Correlation test between two variables
cor(Allegheny_medianDays_Market, AlleghenyCovid_perMonth, method = c("pearson"))
#-0.8068793

# # #

#Philadelphia County
Philadelphia_medianDays_Market = c(51, 59, 48, 39, 45, 49, 42, 37, 35)

#Scatter plot
plot(PhiladelphiaCovid_perMonth, Philadelphia_medianDays_Market, main = "New Covid Cases vs Median Days on the Market", xlab = "New Cases per Month", ylab = "Median Days on the Market", pch = 19)

#Adds the linear regression to the scatter plot
abline(lm(Philadelphia_medianDays_Market~PhiladelphiaCovid_perMonth), col = "red")

#Correlation test between two variables
cor(Philadelphia_medianDays_Market, PhiladelphiaCovid_perMonth, method = c("pearson"))
#-0.5867771

# # # # # # # # #

#Median days on the market January 2019 - September 2019 vs January 2020 - September 2020

# # #

#Allegheny County
Allegheny_medianDays_Market2019 = c(81, 88, 78, 62, 57, 56, 57, 58, 65)

#Table to compare data
AC_table_2019_vs_2020_2 = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Median_Days_Market_2019 = c(Allegheny_medianDays_Market2019),
  Median_Days_Market_2020 = c(Allegheny_medianDays_Market))

formattable(AC_table_2019_vs_2020_2, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "purple",
  font.weight = "bold")), "Median_Days_Market_2020" = formatter("span", style = x~style(color =
  ifelse(x > c(Allegheny_medianDays_Market2019), "green", "red")))))

# # #

#Philadelphia County
Philadelphia_medianDays_Market2019 = c(46, 50, 51, 41, 37, 41, 39, 41, 39)

#Table to compare data
PC_table_2019_vs_2020_2 = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Median_Days_Market_2019 = c(Philadelphia_medianDays_Market2019),
  Median_Days_Market_2020 = c(Philadelphia_medianDays_Market))

formattable(PC_table_2019_vs_2020_2, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "purple",
  font.weight = "bold")), "Median_Days_Market_2020" = formatter("span", style = x~style(color =
  ifelse(x > c(Philadelphia_medianDays_Market2019), "green", "red")))))

# # # # # # # # #

#Correlation between amount of new listings and COVID cases
#January 2020 - September 2020

# # #

#Allegheny County
Allegheny_newListings = c(1096, 1277, 1267, 443, 1701, 1969, 2056, 1815, 1720)

#Scatter plot
plot(AlleghenyCovid_perMonth, Allegheny_newListings, main = "New Covid Cases vs Number of New Listings", xlab = "New Cases per Month", ylab = "Number of New Listings", pch = 19)

#Adds the linear regression to the scatter plot
abline(lm(Allegheny_newListings~AlleghenyCovid_perMonth), col = "red")

#Correlation test between two variables
cor(Allegheny_newListings, AlleghenyCovid_perMonth, method = c("pearson"))
#0.5432712

# # #

#Philadelphia County
Philadelphia_newListings = c(1834, 2021, 1957, 756, 1706, 2395, 2588, 2434, 2462)

#Scatter plot
plot(PhiladelphiaCovid_perMonth, Philadelphia_newListings, main = "New Covid Cases vs Number of New Listings", xlab = "New Cases Per Month", ylab = "Number of New Listings", pch = 19)

#Adds the linear regression to the scatter plot
abline(lm(Philadelphia_newListings~PhiladelphiaCovid_perMonth), col = "red")

#Correlation test between two variables
cor(Philadelphia_newListings, PhiladelphiaCovid_perMonth, method = c("pearson"))
#-0.608845

# # # # # # # # #

#Number of new listings January 2019 - September 2019 vs January 2020 - September 2020

# # #

#Allegheny County
Allegheny_newListings2019 = c(1139, 1120, 1668, 1960, 2024, 1843, 1670, 1614, 1566)

#Table to compare data
AC_table_2019_vs_2020_3 = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Number_New_Listings_2019 = c(Allegheny_newListings2019),
  Number_New_Listings_2020 = c(Allegheny_newListings))

formattable(AC_table_2019_vs_2020_3, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "grey",
  font.weight = "bold")), "Number_New_Listings_2020" = formatter("span", style = x~style(color =
  ifelse(x > c(Allegheny_newListings2019), "green", "red")))))

# # #

#Philadelphia County
Philadelphia_newListings2019 = c(1788, 1865, 2333, 2534, 2529, 2226, 2060, 1988, 2220)

#Table to compare data
PC_table_2019_vs_2020_3 = data.frame(
  Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
  Number_New_Listings_2019 = c(Philadelphia_newListings2019),
  Number_New_Listings_2020 = c(Philadelphia_newListings))

formattable(PC_table_2019_vs_2020_3, align = c("c", "c", "c"), list("Month" = formatter("span", style = ~style(color = "purple",
  font.weight = "bold")), "Number_New_Listings_2020" = formatter("span", style = x~style(color =
  ifelse(x > c(Philadelphia_newListings2019), "green", "red")))))