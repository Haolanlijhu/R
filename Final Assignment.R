#Set working directory and libraries (you'll need dplyr, tidyr, stringr, and ggplot2)
setwd("C:/Users/12468/Desktop/R") 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Read in the WDIData and WDICountry datasets, which can be downloaded at:
# http://databank.worldbank.org/data/download/WDI_csv.zip
# The zip file will need to be extracted, and you will need to read the files from the destination folder
# Note that the WDIData file is big (nearly 400,000 observations), so it will take a second to load into memory!
WDIData <- read.csv("/Users/12468/Desktop/R/WDI_csv/WDIData.csv", stringsAsFactors = TRUE )
WDICountry <- read.csv("/Users/12468/Desktop/R/WDI_csv/WDICountry.csv", stringsAsFactors = TRUE)

# CLEANING ------------------------------------------------------------

# Take a look at the data, using any of the commands we've discussed to inspect.
head(WDIData)
head(WDICountry)
str(WDIData)
str(WDICountry)

# Note that, in the WDICountry file, we have country codes, and some other indicators
# that might be useful for subsetting. Pull the "Country.Code", "Region", and "Income.Group"
# variables from this data. Note the way these names are formatted is a little odd.
wdigroupmap <- WDICountry[, c("Country.Code", "Region", "Income.Group")]

#Considering these names, in their raw format, are a little tough to work with, rename
#them to something easier to work with that you'll remember. Use whatever convention you like.
names(wdigroupmap) <- c("CountryCode", "Region", "IncomeGroup")
head(wdigroupmap)

# Inspect the WDIData - doing some initial cleaning:
#
#     1) Rename the Country.Code variable so it matches with your name above - we'll need this to
#     match exactly to use as a key for merging!

#     2) Remove the extraneous "X" column that appears at the end of the dataset - we don't have
#     any need for it, and it's just cluttering things for us. (*Hint*: you can *remove* a column
#     in `select` by placing a `-` in front of the column you'd like to remove!)

#     3) Rename any of the other identifier variables you'd like to make them easier to work with
#     for you. Be sure to leave the "year" variables alone - we'll work with them later.
#

head(WDIData)
names(WDIData)[names(WDIData) == "Country.Code"] <- "CountryCode"
wdifull <- select(WDIData,-X)
names(wdifull)[names(wdifull) %in% c("Country.Name", "Indicator.Name","Indicator.Code")]  <- c("CountryName", "IndicatorName", "IndicatorCode")

# Since the WDI dataset is so big, it can be unwieldy to work with and can certainly make
# debugging harder! Before we proceed with reshaping our data so we can perform some useful
# analysis, let's trim things down a bit. Create a subset of the WDI data with:
#
#     1) The country names and codes, as well as indicator codes (you may want to keep track of the
#        human-readable names of these codes for later, but do NOT leave this column in the data set
#        - the codes aren't very descriptive, and I will refer to them by name in the analysis portion 
#        of the project)

wdisubset <- wdifull %>% select("CountryCode", "CountryName", "IndicatorCode",starts_with("X"))

#     2) The following five 'variable' indicators:
#        c("SE.PRM.NENR", "SP.DYN.IMRT.IN", "NY.GDP.PCAP.CD", "GB.XPD.RSDV.GD.ZS", "IQ.CPA.BREG.XQ")
#        (*Hint*: the `%in%` function checks to see which elements of a vector are *in* another, e.g.
#        c(1,2,3) %in% c(3,3,3) would return c(FALSE, FALSE, TRUE), implying that 3 is in the second
#        vector!)
indicatorsofinterest <- c("SE.PRM.NENR", "SP.DYN.IMRT.IN", "NY.GDP.PCAP.CD", "GB.XPD.RSDV.GD.ZS", "IQ.CPA.BREG.XQ")

wdisubset <- wdisubset %>% filter(IndicatorCode %in% indicatorsofinterest)
head(wdisubset)
#     3) Only the observations from 2000 to 2019.
wdisubset<- wdisubset %>% select("CountryCode", "CountryName", "IndicatorCode", matches("^X(2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019)$"))


# Recall that `dplyr` pipes work by passing data from one command to the next! So, in your `select` and
# `filter` statements below, you need only include the selections and filter logic, *not* the data.
#
# If your data has been filtered and selected correctly with the specifications above, your resulting
# dataframe should be 23 variables with 1,330 observations.

indics <- c("SE.PRM.NENR", "SP.DYN.IMRT.IN", "NY.GDP.PCAP.CD", "GB.XPD.RSDV.GD.ZS", "IQ.CPA.BREG.XQ")

# Let's clean things up. Since this data is "wide" it's not very helpful for our analysis.
# Remember, tidy data should have an observation in each row, and a variable in each column. 
# We need to "gather" our data to make it workable. Feel free to do this with whatever approach you'd
# like. To help you get going:
#
#    1) Gather is an incredibly useful tool here; fortunately, the structure of the WDI data here is 
#       remarkably similar to the WEO data we worked with together in part 3 of the class. Using the code from
#       part 3 as a guide, 'pivot' the year variables into a single column. 
long_df <- pivot_longer(
  wdisubset,
  cols=X2000:X2019,
  names_prefix = "X",
  names_to = "year"
)
#    2) Once our data is gathered and in 'long' format, we can 'spread' our indicators out again, so we
#       have a time series for each individual variable, for each country, rather than one single 'value.'
#       Again, this data is very similar in structure to the WEO data, so adapt the same code here.
wide_df <- pivot_wider(
  data=long_df,
  id_cols=c('CountryCode','CountryName', 'year'),
  names_from='IndicatorCode',
  values_from='value'
) 

# If your data has been filtered and selected correctly with the specifications above, your resulting
# dataframe should be 8 variables with 5,320 observations.


# Merge the WDICountry data set that we cleaned at the beginning of the script to our reformatted WDI data
# Remember your keys! The resulting data set should now have 10 variables, with regions and income groups
# defined for each country.

wdiallvars <-  left_join(wide_df, wdigroupmap, by='CountryCode')

# With everything we need in the dataset, let's clean and rearrange things just a bit:
#
#    1) Clean the year variable - in the current character format, the variable will be very unhelpful for
#       plotting. Remove the "X" portion of the year variable using 'gsub' as we did in part 3 of the class, then, 
#       paste '-01-01' to the values and convert the values to date using 'as.Date', to ensure we have
#       dates that correspond to the first day of the calender year. You can also check out the `floor_date`
#       function from the `lubridate` package (see documentation).
#
#       For example:
#       > as.Date(paste0(YEAR_VARIABLE, '-01-01'))
#       [1] "2000-01-01"
#
#       Note: You can remove this 'X' portion in `pivot_longer` as well! Check the code from part 3,
#       and adjust accordingly, if so!

wdiallvars$year <- as.Date(paste0(gsub("X", "", wdiallvars$year), "-01-01"))

#    2) Remove observations where income group and region are either missing or empty (i.e. NA and "")
wdiallvars <- wdiallvars %>%filter(!(is.na(IncomeGroup) | IncomeGroup == "") & !(is.na(Region) | Region == ""))
#    2) Select columns in such an order that your identifiers come before the observational variables.
wdiallvars <- wdiallvars %>%select(CountryCode, CountryName, Region, IncomeGroup, everything())
#    3) Arrange the data by country, by year.
wdiall <- wdiallvars %>%
  arrange(CountryCode, year)

# If your data has been filtered and selected correctly with the specifications above, your final
# data frame should be 10 variables with 4,320 observations.

# Export your data to a .csv file
write.csv(wdiall, "wdiall.csv", row.names=FALSE)

# ANALYSIS ------------------------------------------------------------
#
# Using your cleaned and formatted excerpt of the WDI data set, answer all of the questions below. Remember to save the code you use for each question in this...
# ...R script so that your analysis can be run for grading. 
#
#     Basics ---
#
#     1) How many unique countries are in this data set? Regions? Income groups? (6 points)
wdiall %>% 
  distinct(CountryCode) %>%
  nrow()

wdiall %>% 
  distinct(Region) %>%
  nrow()

wdiall %>% 
  distinct(IncomeGroup) %>%
  nrow()
#     2) Which income group has the largest number of observations? Which region? (4 points)
incomegroupcounts <- wdiall %>%
  group_by(IncomeGroup) %>%
  summarise(count = n())

incomegroupcounts %>%
  filter(count == max(count))

regioncounts <- wdiall %>%
  group_by(Region) %>%
  summarise(count = n())

regioncounts %>%
  filter(count == max(count))

#     3) Which region has the highest incidence of observations in the "Upper middle income" income group? (4 points)
#        (*Hint*: Pass two different vectors to the 'table' function to get cross-tabulations, or use 
#                 `group_by` and the `n()` summary function in `dplyr`)

uppermiddleincomedata <- wdiall %>%
  filter(IncomeGroup == "Upper middle income")

regioncountsuppermiddleincome <- uppermiddleincomedata %>%
  group_by(Region) %>%
  summarise(count = n())

regioncountsuppermiddleincome %>%
  filter(count == max(count))

#     4) Report the average infant mortality rate for the time horizon in each country. Which country has
#        the highest? The lowest? (6 points)

infantmortastats <- wdiall %>%
  group_by(CountryCode) %>%
  summarise(avginfantmortality = mean(SP.DYN.IMRT.IN, na.rm = TRUE))

infantmortastats %>% filter(avginfantmortality == max(avginfantmortality, na.rm = TRUE))

infantmortastats %>% filter(avginfantmortality == min(avginfantmortality, na.rm = TRUE))

#     5) Which countries are missing the most observations for the GDP per Capita variable? How many observations
#         are missing? (4 points)
missinggdp <- wdiall %>%
  group_by(CountryCode) %>%
  summarise(missing_count = sum(is.na(NY.GDP.PCAP.CD)))

missinggdp %>%filter(missing_count == max(missing_count))

#     6) Which indicator variable is missing the most observations? What percent of this variable is missing? (4 points)
missingindicatorsvars <- wdiall %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Indicator", value = "MissingCount") %>%
  arrange(desc(MissingCount))

# Finding the indicator variable with the most missing observations
mostmissingindicator <- missingindicatorsvars %>%filter(MissingCount == max(MissingCount))

# Calculating the percentage of missing values for the most missing indicator
total_rows <- nrow(wdiallvars)
(mostmissingindicator$MissingCount / total_rows) * 100

#     7) Using the `wdi_full` version of this data set (not fully cleaned up), report a vector of the
#        human-readable indicator names of the indicator codes that are in your final `wdiall` data set (4 points)
indicatornames <- wdi_full %>%
  filter(IndicatorCode %in% names(wdiall)) %>%
  distinct(IndicatorCode, IndicatorName)
head(indicatornames)

#     Base R Plotting ---
#
#     8) Filter out NA values for the R&D expenditure variable - store this as a new dataframe (2 points)
RDdf <- wdiall %>%filter(!(is.na(GB.XPD.RSDV.GD.ZS) | GB.XPD.RSDV.GD.ZS == "") )

#     9) Using base R (not ggplot) create a time series plot of the average R&D expenditure variable 
#        from each year; format the labels and title appropriately. (6 points)
avgrddf<-RDdf %>% group_by(year)%>% summarise(avg_rd=mean(GB.XPD.RSDV.GD.ZS))
plot(avgrddf$year,avgrddf$avg_rd, xlab = "year", ylab="average_R&D", main ="time series plot of the average R&D expenditure")
#     Cross-sectional analysis ---
#
#     10) Filter your data to include only observations from 2018 - store it as a new dataframe (2 points)
data2018 <- wdiall %>%
  filter(year == "2018-01-01")
#     11) Using only this cross-section, using base R (not ggplot) create histograms of the GDP per Capita 
#        variable for each region; format the labels and title appropriately. (8 points)
unique(data2018$Region)[1]
regiongdp1 <- data2018 %>% filter(Region==unique(data2018$Region)[1])
hist(regiongdp1$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For Latin America & Caribbean")

unique(data2018$Region)[2]
regiongdp2 <- data2018 %>% filter(Region==unique(data2018$Region)[2])
hist(regiongdp2$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For South Asia")

unique(data2018$Region)[3]
regiongdp3 <- data2018 %>% filter(Region==unique(data2018$Region)[3])
hist(regiongdp3$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For Sub-Saharan Africa")

unique(data2018$Region)[4]
regiongdp4 <- data2018 %>% filter(Region==unique(data2018$Region)[4])
hist(regiongdp4$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For Europe & Central Asia")

unique(data2018$Region)[5]
regiongdp5 <- data2018 %>% filter(Region==unique(data2018$Region)[5])
hist(regiongdp5$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For Middle East & North Africa")

unique(data2018$Region)[6]
regiongdp6 <- data2018 %>% filter(Region==unique(data2018$Region)[6])
hist(regiongdp6$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For East Asia & Pacific")

unique(data2018$Region)[7]
regiongdp7 <- data2018 %>% filter(Region==unique(data2018$Region)[7])
hist(regiongdp7$NY.GDP.PCAP.CD, xlab = "GDP per cap", main = "Historgam of the GDP per Capita For North America")

#     12) Repeat 11), but instead of doing so manually, create all 7 histograms by using a loop (if you used
#        a loop already, you may count this in your total as well!) (10 points)
uniqueregions <- unique(data2018$Region)
for (region in uniqueregions) {
  region_gdp <- data2018 %>% filter(Region == region)
  
  hist(region_gdp$NY.GDP.PCAP.CD, main = paste("Histogram of GDP per Capita in", region, "(2018)"), xlab = "GDP per Capita")
}
#     13) Using this cross-section, report the average GDP per Capita for each region in your data. (4 points)
avggdppercapita <- data2018 %>%
  group_by(Region) %>%
  summarise(avggdppercapita = mean(NY.GDP.PCAP.CD, na.rm = TRUE))
#     14) Using this cross-section, run a regression of GDP per Capita on both the infant mortality
#        and education variables. Report the output using the summary function. (6 points)
regression1 <- lm(NY.GDP.PCAP.CD ~ SP.DYN.IMRT.IN + SE.PRM.NENR, data = data2018)
summary(regression1)
#     15) Using this cross-section and ggplot, create a histogram of primary school enrollment; format the 
#        labels and title appropriately (6 points).
ggplot(data = data2018, aes(x = SE.PRM.NENR)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(
    title = "Histogram of Primary School Enrollment (2018)",
    x = "Primary School Enrollment",
    y = "Frequency"
  ) +
  theme_minimal()
#     16) Building off of the previous plot, alias the 'fill' argument of the aes layer (see part 3 of the course) 
#        to the 'region' variable to add separate histograms for each region as different colors.(6 points)
ggplot(data = data2018, aes(x = SE.PRM.NENR, fill = Region)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(
    title = "Histogram of Primary School Enrollment by Region (2018)",
    x = "Primary School Enrollment",
    y = "Frequency",
    fill = "Region"
  ) +
  theme_minimal()
#     Time-series plotting ---
#
#     17) Create two new dataframes: One which is summarised up to the 'region'-, 'year'-level, and one
#        which is summarised up to the 'income_group'-, 'year'-level, using the averages of each of the 
#        the indicator variables as the summary statistic/function (*Hint*: Recall that we can summarise
#        multiple variables in one `summarise` command, just as we can create multiple variables in mutate!) (6 points)
regionyearsummary <- wdiall %>%
  group_by(Region, year) %>%
  summarise(mean_IQ = mean(IQ.CPA.BREG.XQ, na.rm = TRUE),
            mean_IMRT = mean(SP.DYN.IMRT.IN, na.rm = TRUE),
            mean_GD_ZS = mean(GB.XPD.RSDV.GD.ZS, na.rm = TRUE),
            mean_NY=mean(NY.GDP.PCAP.CD, na.rm=TRUE),
            mean_SE=mean(SE.PRM.NENR,na.rm=TRUE)
)

incomeyearsummary <- wdiall %>%
  group_by(IncomeGroup, year) %>%
  summarise(mean_IQ = mean(IQ.CPA.BREG.XQ, na.rm = TRUE),
            mean_IMRT = mean(SP.DYN.IMRT.IN, na.rm = TRUE),
            mean_GD_ZS = mean(GB.XPD.RSDV.GD.ZS, na.rm = TRUE),
            mean_NY=mean(NY.GDP.PCAP.CD, na.rm=TRUE),
            mean_SE=mean(SE.PRM.NENR,na.rm=TRUE))
#     18) Using the region-level dataframe, create a time series plot of GDP per capita, using the `col`
#        argument in the aes layer (see class part 3) so each region is plotted as a different color; format
#        the labels and title appropriately. (10 points)
ggplot(data = region_year_summary, aes(x = year, y = mean_NY, col = Region)) +
  geom_line() +
  labs(
    title = "Time Series of GDP per Capita by Region",
    x = "Year",
    y = "GDP per Capita",
    col = "Region"
  ) +
  theme_minimal()
#     19) Using the income-group-level dataframe, create a time series plot of infant mortality, using the `col`
#        argument in the aes layer (see class part 3) so each region is plotted as a different color; format
#        the labels and title appropriately. (10 points)
ggplot(data = income_year_summary, aes(x = year, y = mean_IMRT, col = IncomeGroup)) +
  geom_line() +
  labs(
    title = "Time Series of infant mortality by Region",
    x = "Year",
    y = "Infant mortality",
    col = "Region"
  ) +
  theme_minimal()
# ---------------------------------------------------------------------
