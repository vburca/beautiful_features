# Title: Data Mining Project
#     Analysis on BeautifulPeople.com
#
# Authors: Vlad Burca
#          Zach Freedman
# Date: 30 March 2013
# Updated: 13 April 2013
# 
# Filename: analysis.r
#

# entry_rating will take one specific entry's ratings for "Absolutely Not,"
# "No," "Hmmmm OK", and "Beautiful" and calculate an average rating for the
# applicant. The function designed to calculate average applicant rating
# assigns negative values to negative ratings and positive values to positive
# ratings.
#
# @param entry, the entry from the data
# @return entry rate, the average rating for the user
#
entry_rating <- function(entry) {
  # Weights of the entry rating formula
  weight_1 <- -1
  weight_2 <- -0.5
  weight_3 <- 0.5
  weight_4 <- 1

  # Maximum rate based on the HTML bars
  max_rate <- 274

  # Get the ratings
  p1 <- entry["Rating Absolutely Not"]
  p2 <- entry["Rating No"]
  p3 <- entry["Rating Hmmmm OK"]
  p4 <- entry["Rating Beautiful"]

  # Entry rating formula
  entry_rate <- weight_1 * p1 + 
                weight_2 * p2 +
                weight_3 * p3 +
                weight_4 * p4
  entry_rate <- entry_rate / (weight_4 * max_rate)

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
      avg_rating <- avg_rating + entry_rating(filtered[i,])
    avg_rating <- avg_rating / n

    return(avg_rating[1,])
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

average_attribute <- function(column) {
  column_values <- sort(unique(girls.valid.data[,column]))

  average_attr <- 0.0

  for (column_value in column_values)
    average_attr <- average_attr + average_rating(column, column_value)

  average_attr <- average_attr / length(column_values)

  return(average_attr)
}

good_attribute_values <- function(attribute) {
  good_values <- c()

  # Compute the average value for the given attribute
  average <- average_attribute(attribute)

  # Get the unique possible values of the current attribute
  attribute_values <- sort(unique(girls.valid.data[,attribute]))

  # Computing good attribute values
  for (attribute_value in attribute_values) {
    if (average_rating(attribute, attribute_value) >= average)
      good_values <- append(good_values, attribute_value)
  }

  return(good_values)
}

generate_errors <- function() {
  columns <- c("Age", "Hair Color", "Field of Work", "Eye Color", 
            "Smoking", "Body Type", "Car Owner", "Zodiac Sign", "Home Owner", 
            "Relationship Status", "Country")

  attribute_errors <- c()

  # Get number of entries
  n <- nrow(girls.valid.data)

  # For each attribute in the data set
  for (attribute in columns) {
    print(sprintf("Attribute= %s", attribute))

    good_values <- c()
    good_guys <- c()

    false <- 0
    error_rate <- 0.0

    # Computing good attribute values
    good_values <- good_attribute_values(attribute)

    # print(sprintf("Good values= %s", good_values))

    # For each entry in the data set
    for (i in 1:n) {

      # print(sprintf("Attribute= %s, iteration= %i", attribute, i))

      # Save the entry
      entry <- girls.valid.data[i,]
      # Compute the average rating of the entry, based on users' ratings
      entry_rate <- entry_rating(entry)

      # Check if the entry's attribute value is a "good" one
      if (entry[,attribute] %in% good_values) {
        good_guys <- append(good_guys, i)

        # Check if the entry's rating is good
        if (entry_rate < 0)
          false <- false + 1
      # else if the entry's attribute value is a "bad" one
      }
      else 
        # Check if the entry's rating is bad
        if (entry_rate >= 0)
          false <- false + 1
    }

    # Compute the error rate
    error_rate <- false / n

    # print(sprintf("Error rate= %s", error_rate))

    # Append the error rate to the collection of errors per attribute
    attribute_errors <- append(attribute_errors, error_rate)

  }

  # Write it to a file
  write(attribute_errors, file="attributes.err", ncolumns = length(attribute_errors))

}

pick_top_splits <- function(n) {
  # Grab the attribute errors from the previously generated file
  attribute.errors <- read.delim("attributes.err", header=F, sep=" ")
  # Set the columns
  columns <- c("Age", "Hair Color", "Field of Work", "Eye Color", 
            "Smoking", "Body Type", "Car Owner", "Zodiac Sign", "Home Owner", 
            "Relationship Status", "Country")  
  colnames(attribute.errors)[] <- columns

  # Sort by the attribute error
  attribute.errors <- sort(attribute.errors)

  # Check if n is too large and set it to max in that case
  if (n > length(columns))
    n = length(columns)

  # Return the best n attribute errors (minimum ones)
  return(attribute.errors[1:n])
}

find_best_depth <- function() {
  columns <- c("Age", "Hair Color", "Field of Work", "Eye Color", 
            "Smoking", "Body Type", "Car Owner", "Zodiac Sign", "Home Owner", 
            "Relationship Status", "Country")  
  n <- nrow(girls.valid.data)

  # Get the top split points for n = # of attributes
  top_splits <- pick_top_splits(n)
  max <- length(top_splits)

  good_hash <- new.env()

  # Generate good attribute values for each of the attribute
  print("Generating good attribute values...")
  for (attribute in columns) {
    cat("\tAttribute= ", attribute, "\n")
    good_hash[[attribute]] <- good_attribute_values(attribute)
  }

  error_rates <- c()

  # Start
  for (i in 1:max) {
    print(sprintf("Calculating for depth= %s", i))
    false <- 0
    error <- 0

    for (j in 1:n) {
      cat("\tEntry= ", i, "\n")
      entry <- girls.valid.data[j,]

      for (k in 1:i) {
        node_attribute <- colnames(top_splits[k])

        cat("\tNode attr= ", node_attribute, "  k= ", k, "\n")

        if (!(entry[,node_attribute] %in% good_hash[[node_attribute]])) {
          if (entry_rating(entry) >= 0) {
            false <- false + 1
            break
          }
        }
        else
        if (entry[,node_attribute] %in% good_hash[[node_attribute]]) {
          if (entry_rating(entry) < 0) {
            false <- false + 1
            break
          }
        }
      }
    }

    error <- false / n
    cat("\tError[", i, "]= ", error, "\n")
    error_rates <- append(error_rates, error)
  }

  write(error_rates, file = "tree.err", ncolumns = length(error_rates))
}

test_entry <- function(depth, id) {
  columns <- c("Age", "Hair Color", "Field of Work", "Eye Color", 
            "Smoking", "Body Type", "Car Owner", "Zodiac Sign", "Home Owner", 
            "Relationship Status", "Country")  
  n <- nrow(girls.valid.data)

  if (depth > length(columns))
    depth <- length(columns)

  # Get the top split points for n = # of attributes
  top_splits <- pick_top_splits(depth)

  good_hash <- new.env()

  interesting_columns <- colnames(top_splits)

  # Generate good attribute values for each of the attribute
  print("Generating good attribute values...")
  for (attribute in interesting_columns) {
    cat("\tAttribute= ", attribute, "\n")
    good_hash[[attribute]] <- good_attribute_values(attribute)
  }

  entry <- girls.valid.data[id,]
  for (i in 1:depth) {
    node_attribute <- colnames(top_splits[i])

    if (!(entry[,node_attribute] %in% good_hash[[node_attribute]]))
      return(0)
  }

  return(1)
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

  write.table(girls.valid.data, file = "valid_data.txt", sep = " ", col.names = FALSE, quote = FALSE)
}



