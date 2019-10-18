# calling Packages ####
library(readr)                                                                       # Reading data set
library(caret)                                                                       # Prediction
library(dplyr)                                                                       # Data manipulation
library(ggplot2)                                                                     # Plotting
library(plotly)                                                                      # Plotting
library(lubridate)                                                                   # Working with Date and time

# Calling data ####
rawdata2556 <- read.csv("Data2556.csv", TRUE, sep =",")
DATA2556 <- rawdata2556

# Convert Posix in date
DATA2556$visitStartTime <- as_datetime(DATA2556$visitStartTime)

# Creating attribute for time
DATA2556$year <- year(DATA2556$visitStartTime)
DATA2556$month <- month(DATA2556$visitStartTime)
DATA2556$day <- day(DATA2556$visitStartTime)
DATA2556$hour <- hour(DATA2556$visitStartTime)
DATA2556$minute <- minute(DATA2556$visitStartTime)
DATA2556$second <- second(DATA2556$visitStartTime)

# Counting the visits for each hour
table(DATA2556$hour)

# The bar plot number of visis VS hour
ggplot(DATA2556, aes(x = DATA2556$hour)) + geom_bar(fill = "#0073C2FF") + 
                                           ylab("count") +
                                           xlab("hour") +
                                           ggtitle("Number of visit") +
                                           theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0)) +
                                           scale_x_continuous(breaks=seq(0, 23, 1))


# Hits and Time #### 
# Excluding unimportant attributes
DATA2556$timeOnScreen <- NULL

# Excluding NA values
NoNaDATA2556 <- na.exclude(DATA2556) 


# Creating data set only for the US
NoNaDATA2556US <- NoNaDATA2556
USData <- NoNaDATA2556US %>%
          filter(NoNaDATA2556US$Country %in% c("United States"))

# Creating graphs for the US
gUSA <- ggplot(data = USData) + 
        geom_point(mapping = aes(y = TotalNumberHits, x = timeOnSite)) +
        scale_y_continuous(breaks=seq(0, 350, 20)) + 
        scale_x_continuous(breaks=seq(0, 8000, 500)) +
        ylab("Total Number of Hits") +
        xlab("TimeSpent(s)") +
        ggtitle("Number of Hits In the US")


# Creating data set only for India
NoNaDATA2556India <- NoNaDATA2556
IndiaData <- NoNaDATA2556India %>%
            filter(NoNaDATA2556India$Country %in% c("India"))

# Creating graphs for India
gIndia <- ggplot(data = IndiaData) + 
          geom_point(mapping = aes(y = TotalNumberHits, x = timeOnSite, color = "#00AFBB"), fill = "#00AFBB") +
          scale_y_continuous(breaks=seq(0, 350, 5)) + 
          scale_x_continuous(breaks=seq(0, 8000, 500)) +
          ylab("Total Number of Hits") +
          xlab("TimeSpent(s)") +
          ggtitle("Number of Hits In India")


# Creating data set for the US and India
NoNaDATA2556IndiaUS <- NoNaDATA2556
USAndIndiaData <- NoNaDATA2556IndiaUS %>%
                  filter(NoNaDATA2556IndiaUS$Country %in% c("India", "United States"))

# Creating graph for the US and India
gIndiaAndUS <- ggplot(data = USAndIndiaData) + 
               geom_point(mapping = aes(x = timeOnSite, y = TotalNumberHits, fill = Country, color = Country)) +
               scale_y_continuous(breaks=seq(0, 350, 20)) + 
               scale_x_continuous(breaks=seq(0, 8000, 1000)) +
               ylab("Total Number of Hits") +
               xlab("TimeSpent(s)") +
               ggtitle("Number of Hits In India and US")