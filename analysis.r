# Title: Data Mining Project
#     Analysis on BeautifulPeople.com
#
# Authors: Vlad Burca
#          Zach Freedman
# Date: 30 March 2013
# 
# Filename: analysis.r
#

# entry_rating will take one specific entry's ratings for "Absolutely Not,"
# "No," "Hmmm OK", and "Beautiful" and calculate an average rating for the
# applicant. The function designed to calculate average applicant rating
# assigns negative values to negative ratings and positive values to positive
# ratings.
#
# @param p1, the quantity of "Absolutely Not" ratings
# @param p2, the quantitiy of "No" ratings
# @param p3, the quantity of "Hmmmm OK" ratings
# @param p4, the quantity of "Beautiful" ratings
# @return entry rate, the average rating for the user
#
entry_rating <- function(p1, p2, p3, p4) {
  # Weights of the entry rating formula
  weight_1 <- -1
  weight_2 <- -0.5
  weight_3 <- 0.5
  weight_4 <- 1

  # Maximum rate based on the HTML bars
  max_rate <- 274

  # Entry rating formula
  entry_rate <- weight_1 * p1 + 
                weight_2 * p2 +
                weight_3 * p3 +
                weight_4 * p4
  entry_rate <- entry_rate / ( weight_4 * max_rate)

  return(entry_rate)
}

# average_rating will take any attribute in the data set and any
# corresponding attribute value to the passed attribute. Upon
# acceptance of these paramters, the function calls entry_rating
# on each entry with the attribute_value specified, and iteratively
# computes the average rating, for the given data set, of applicants
# with the specified attribute value
#
# @param column, an attribute such as "Country" or "Hair Color"
# @param column_value, the attribute value of the specified attribute
# @return avg_rating, the average rating in the data set for entries with
#   the specified attribute value
# @return 0, if there are no entries in the dataset with the specified
#   attribute value
#
average_rating <- function(column, column_value) {
  avg_rating <- 0.0

  # Maximum rating from the HTML bars
  max_rating <- 274

  # Get number of entries that have the given column_value
  n <- sum(girls.valid.data[,column] == column_value)
  if (n != 0) {
    # Get the entries that have the given column_name
    filtered <- girls.valid.data[girls.valid.data[,column] == column_value, ]

    for (i in 1:n)
      # Compute the average rating
      avg_rating <- avg_rating + entry_rating(filtered[i, 2], 
                                              filtered[i, 3],
                                              filtered[i, 4],
                                              filtered[i, 5])
    avg_rating <- avg_rating / n

    return(avg_rating)
  }
  # No entries found
  return(0)
}

# show_histogram will take any attribute in the given data set
# and create a histogram
#
# @param column, an attribute such as "Country" or "Hair Color"
# @return none
# @display a hisogram whose horizontal axis lists all possible attribute value
#   and whose vertical axis lists the average rating corresponding to that
#   attribute value
#
show_histogram <- function(column) {
  # Get the unique values of a given column
  column_values <- sort(unique(girls.valid.data[,column]))

  # Collection of averages that we will plot
  averages <- c()
  # Collection of the labels for each average that we compute
  labels_ <- c()

  # Flag that checks for negative values
  plot_negatives <- FALSE
  for (value in column_values) {
    # Compute average rating for a specific column value
    avg = average_rating(column, value)
    if (avg < 0) 
      # Flag the negative values - we will want to display these
      plot_negatives <- TRUE

    # If there is an average, construct the collections that we want to plot
    if (avg != 0) {
      labels_ <- append(labels_, value)
      averages <- append(averages, avg)
    }
  }

  # Set the y axis to start from 0
  ylims <- c(0, 1.00001)
  if (plot_negatives == TRUE)
    # Make it start from -1 if we have negative values to display
    ylims <- c(-1, 1.00001)

  # Install 'plotrix' package if it is not on the current system
  #install.packages('plotrix')

  # Plot the histogram
  plotrix::barp(averages, col=rainbow(length(labels_) + 10), xlab=column, ylab="Average Ratings", ylim=ylims,
              names.arg=labels_, staxx=TRUE, srt=90, cex=0.5)
}

girls.data <- read.delim("data3.txt", header=F, sep=" ")

# remove the unknown last column
girls.data$V17 = NULL

#Setting the column names of the girls.data dataframe (the data table)
columns <- c("ID", "Rating Absolutely Not", "Rating No", "Rating Hmmmm OK", 
            "Rating Beautiful", "Age", "Hair Color", "Field of Work", "Eye Color", 
            "Smoking", "Body Type", "Car Owner", "Zodiac Sign", "Home Owner", 
            "Relationship Status", "Country")  
colnames(girls.data)[] <- columns

#Creating a new data frame that will exclude bad data points after preprocessing
girls.valid.data <- data.frame()

# Ratings -> is.real
# Age -> is.integer

#A collection of all possible hair types
hair.types <- c("White", "Blonde", "DarkBlonde", "Brown", "DarkBrown", 
              "Black", "Grey", "Red", "Auburn", "Bald", "EverChanging", "Other")

#A collection of all possible work fields
field_of_work.types <- c("Administration", "Architecture", "Artist",
              "Computers", "Construction", "Design", "Education",
              "Engineering", "Entrepreneur", "Fashion", "Financial",
              "Government", "Hospitality", "LawEnforcement", "Law",
              "Management", "Marketing", "Medicine", "Military",
              "Nonprofit", "PerformingArts", "Restaurant", "Retail",
              "Retired", "Student", "Teacher")

#A collection of all possible eye colors
eye.types <- c("Green", "Hazel", "Blue", "DarkBlue", "Grey", 
              "Brown", "DarkBrown", "LightBrown", "Other")

#A collection of all possible values for smoking
smoking.types <- c("Yes", "No", "Sometimes")

#A collection of all possible values for body type
body.types <- c("Slim", "Average", "Toned", "Athletic", "Muscular", "Cuddly",
              "Ample")

#A collection of all possible values for car owner
car_owner.types <- c("Yes", "No")

#A collection of all possible values for zodiac sign
zodiac.types <- c("Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo",
              "Libra", "Scorpio", "Sagittarius", "Capricorn", "Aquarius", "Pisces")

#A collection of all possible values for home owner
home_owner.types <- c("Yes", "No")

#A collection of all possible values for relationship status
relationship.types <- c("Single", "Married", "InArelationship", "Divorced", 
              "It'sComplicated")

# Country -> is.string  We can not do this now


#Data preprocessing algorithm
#NOTE: POSSIBLY, this should be in an algorithm called preprocess()

valid.index = 1

#Getting the total number of data entries in our original unprocessed dataframe
n.rows = nrow(girls.data)

for (row in 1:n.rows) {

  # Check ID as Integer
  if (is.integer(girls.data[row, "ID"]) == FALSE)
    next

  # Check Rating as Float and not NA
  for (rating in c("Rating Absolutely Not", "Rating No", "Rating Hmmmm OK", 
                "Rating Beautiful")) {
    go_next = FALSE
    if (is.na(girls.data[row, rating]) == TRUE || 
        is.real(girls.data[row, rating]) == FALSE) {
      go_next = TRUE
      break
    }
  }

  # One of the Rating values is invalid
  if (go_next == TRUE)
    next

  # Check Age as Integer
  if ((is.na(girls.data[row, "Age"]) == TRUE) || 
       is.integer(girls.data[row, "Age"]) == FALSE)
    next

  # Check Hair Color as hair.types
  if (girls.data[row, "Hair Color"] %in% hair.types == FALSE)
    next

  # Check Field of Work as field_of_work.types
  if (girls.data[row, "Field of Work"] %in% field_of_work.types == FALSE)
    next

  # Check Eye Color as eye.types
  if (girls.data[row, "Eye Color"] %in% eye.types == FALSE)
    next

  # Check Smoking as smoking.types
  if (girls.data[row, "Smoking"] %in% smoking.types == FALSE)
    next

  # Check Body Type as body.types
  if (girls.data[row, "Body Type"] %in% body.types == FALSE)
    next

  # Check Car Owner as car_owner.types
  if (girls.data[row, "Car Owner"] %in% car_owner.types == FALSE)
    next

  # Check Zodiac Sign as zodiac.types
  if (girls.data[row, "Zodiac Sign"] %in% zodiac.types == FALSE)
    next

  # Check Home Owner as home_owner.types
  if (girls.data[row, "Home Owner"] %in% home_owner.types == FALSE)
    next

  # Check Relationship Status as relationship.types
  if (girls.data[row, "Relationship Status"] %in% relationship.types == FALSE)
    next

  # Check Country as not ''
  if (girls.data[row, "Country"] == '')
    next

  # Save valid entries in girls.valid.data
  girls.valid.data <- rbind(girls.valid.data, girls.data[row,])
  row.names(girls.valid.data) <- 1:nrow(girls.valid.data)
}
