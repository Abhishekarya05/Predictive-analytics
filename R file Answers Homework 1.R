# 1 
# a. Examine the raw data file SwineFlu2009.csv using Excel.
# Ans Open the file in Excel and review its contents to understand its structure and the data it contains.
#b. Read the data into R using fread() from the data.table package.
install.packages("data.table")
library(data.table)

flu_data <- fread("C:/Users/hardi/OneDrive/Desktop/SwinFlu2009.csv")

flu_data


# c. Assign proper variable names and types to each column.


col_names <- c("observation_id", "firstcase_date_id", "firstcase_continent_id", "country", "firstcasereport_date", 
               paste0("cum_case_", c("Apr09", "May09", "Jun09", "Jul09", "Aug09")),
               "cum_case_Aug09", "firstdeath_date_id", "firstdeath_continent_id", "firstdeath_date",
               paste0("cum_death_", c("May09", "Jun09", "Jul09", "Aug09", "Sep09", "Oct09", "Nov09", "Dec09")))
flu_data <- fread("C:/Users/hardi/OneDrive/Desktop/SwinFlu2009.csv",  col.names = col_names)
flu_data

#d. In R, dates can be stored as a special type of numeric data. Modify the DATA step to make sure that the dates are read in the correct R date format (not as character). (HINT: Use the correct date type format statements in as.Date(), e.g., format = “%m/%d/%Y”)
flu_data$firstcasereport_date <- as.Date(flu_data$firstcasereport_date, format = "%m/%d/%Y")
flu_data$firstdeath_date <- as.Date(flu_data$firstdeath_date, format = "%m/%d/%Y")
flu_data

# e.Calculate the date difference of the firstcasereport_date variable from the first
# case report date across the world, which is Apr 24, 2009
# Define the world's first case report date
world_first_case_date <- as.Date("2009-04-24")

# Calculate the date difference for each observation
flu_data$days_from_first_incidence <- flu_data$firstcasereport_date - world_first_case_date
flu_data
# f .Subset the columns (“firstcase_date_id”, “country”) and the answer from the above question 1.e, and save it as the file “SwineFlu2009_days_from_first_incidence.csv”) using fwrite(). (HINT: the new csv file should have three columns)

# Subset the desired columns
subset_data <- flu_data[, c("firstcase_date_id", "country", "days_from_first_incidence")]

# Save the subset data as a new CSV file
fwrite(subset_data, "SwineFlu2009_days_from_first_incidence.csv")

file_path <- "C:/Users/hardi/OneDrive/Desktop/SwineFlu2009_days_from_first_incidence.csv"

# Save the subset data as a new CSV file
fwrite(subset_data, file_path)



# 2.
# a. Examine the raw data file Pizza.csv and read it into R using fread().
pizza_data <- fread("C:/Users/hardi/OneDrive/Desktop/Pizza.csv")
#b. Print the data set (on the Console).
print(pizza_data)

# c. Print the class of each column
sapply(pizza_data, class)

# d. Print the summary statistics of the data using describe() in “psych” package.
install.packages("psych")
library(psych)

# Print summary statistics
describe(pizza_data)

# e.Open the raw data file in a simple editor like WordPad and compare the data values to the output from part b) to make sure that they were read correctly into R. In a comment in your report, identify any problems with the R data set that cannot be resolved using the fread(). Explain what is causing the problem. (Hint: You need to make sure the type of each variable is read correctly.)
# There may be issues with the column types not being read correctly, leading to incorrect data representation.
# For example, if a numeric column is read as character, it may affect summary statistics calculations.
#f. Read the same raw data file, Pizza.csv, again. This time, make sure the issues you’ve identified in the previous step ls resolved.
 # No problem arised first time.

# g.Create a column that contains the average ratings for each topping. (Hint: You need to make sure “NA” entries are not included in the average. They should not be treated as zeros. See the documentation for rowMeans().)
pizza_data$average_rating <- rowMeans(pizza_data[, -1], na.rm = TRUE)
pizza_data


# 3.
# a. Examine the raw data file Hotel.csv and read it into R using fread(). Is there any “problem” with this data read? Explain.
hotel_data <- fread("C:/Users/hardi/OneDrive/Desktop/Hotel.csv")
hotel_data <- fread("C:/Users/hardi/OneDrive/Desktop/Hotel.csv", fill= TRUE, verbose = TRUE)
hotel_data

# b. Assign the column names for room number and number of guests first. For other column names, you should assign them as you answer the remaining questions.
# Assign column names for room number and number of guests
# Assign column names for specific columns
names(hotel_data)[1] <- "RoomNumber"
names(hotel_data)[2] <- "NumberOfGuests"
hotel_data
# c.Create date variables for the check-in and check-out dates, and format them to display as readable dates.
# Read the data without specifying column names
hotel_data
names(hotel_data)[3] <- "monthin"
names(hotel_data)[4]<-"dayin"
names(hotel_data)[5]<-"yearin"
names(hotel_data)[6]<-"monthout"
names(hotel_data)[7]<-"dayout"
names(hotel_data)[8]<-"yearout"

# Combine check-in date components into a single date variable

hotel_data

hotel_data$checkindate <- as.Date(with(hotel_data, paste(monthin, dayin, yearin,sep="-")), "%m-%d-%Y")
hotel_data$checkoutdate <- as.Date(with(hotel_data, paste(monthout, dayout, yearout,sep="-")),"%m-%d-%Y")

hotel_data

#d.Using the data.table syntax, create a column of days of internet use. If the guest did not use the internet, assign “0”. Check the class of the column you created and coerce the variable type to “numeric” as necessary. (Hint. Days of internet use is recorded only when the use of wireless internet service is YES. See the documentation for as.numeric() and as.character())

names(hotel_data)[9] <- "InternetUse" 

hotel_data[, DaysOfInternetUse := 0]

# Assuming the column names for internet use and days of internet use are 'InternetUse' and 'DaysOfInternetUse' respectively
hotel_data[, DaysOfInternetUse := ifelse(InternetUse == "YES", as.numeric(as.character(V10)), 0)]

# Check the class of the column
print(class(hotel_data$DaysOfInternetUse))

# Coerce to numeric if necessary
hotel_data$DaysOfInternetUse <- as.numeric(hotel_data$DaysOfInternetUse)
hotel_data
# e.Using the data.table syntax, create a column of room type. (Again, use the hint from the above)
hotel_data[, roomtype := ""]

hotel_data[, roomtype := gsub("[0-9]", "", paste(V10, V11))]
hotel_data
# f. Using the data.table syntax, create a column of room rate. Check the class of the column you created and coerce the variable type to “numeric” as necessary. (Again, use the hint from the above)
hotel_data[, roomrate := 0]

hotel_data[, roomrate := as.numeric(gsub("[^0-9.]", "", paste(V11, V12)))]

names(hotel_data)[15] <- "number_of_days_of_Internet_use"
names(hotel_data)[9] <-"use_of_wireless_Internet_service"
hotel_data


# g.Subset the cleaned variables only and create a new data.table: room number, number of guests, check-in date, check-out date, use of wireless Internet service, number of days of Internet use, room type, and room rate.
# Create a new data.table with selected columns
cleaned_hotel_data <- hotel_data[, .(RoomNumber, NumberOfGuests, checkindate, checkoutdate, use_of_wireless_Internet_service, number_of_days_of_Internet_use, roomtype, roomrate)]
cleaned_hotel_data



# Assuming cleaned_hotel_data is your data.table and you have the necessary cleaned columns: RoomRate, CheckInDate, CheckOutDate, NumberOfGuests, and DaysOfInternetUse
# h. Create a variable that calculates the subtotal as the room rate times the number of days in the stay, plus a per person rate ($10 per day for each person beyond one guest), plus an Internet service fee ($9.95 for a one-time activation and $5.95 per day of use).
# Calculate the number of days in the stay
cleaned_hotel_data[, DaysOfStay := as.numeric(difftime(checkoutdate, checkindate, units = "days"))]

# Calculate the total room rate for the stay
cleaned_hotel_data[, TotalRoomRate := roomrate * DaysOfStay]

# Calculate the additional charges for extra guests
cleaned_hotel_data[, ExtraGuestCharges := 10 * (NumberOfGuests - 1) * DaysOfStay]

# Calculate the internet service fee
cleaned_hotel_data[, InternetServiceFee := 9.95 + 5.95 * number_of_days_of_Internet_use]

# Calculate the subtotal
cleaned_hotel_data[, Subtotal := TotalRoomRate + ExtraGuestCharges + InternetServiceFee]
cleaned_hotel_data


# Assuming cleaned_hotel_data is your data.table and you have the Subtotal column
# i. Create a variable that calculates the grand total as the subtotal plus sales tax at 8.75%. The result should be rounded to two decimal places.
# Calculate the sales tax
cleaned_hotel_data[, SalesTax := Subtotal * 0.0875]

# Calculate the grand total
cleaned_hotel_data[, GrandTotal := Subtotal + SalesTax]

# Round the grand total to two decimal places
cleaned_hotel_data[, GrandTotal := round(GrandTotal, 2)]
cleaned_hotel_data


# j. View the resulting data set. In a comment in your report, state the value for the grand total for room 247, checked in on Feb. 7th, 2014.
# Calculate the grand total for room 247, checked in on Feb. 7th, 2014
grand_total_247 <- cleaned_hotel_data[RoomNumber == 247 & checkindate == "2014-02-07"]$GrandTotal

# Print the grand total for room 247
cat("Grand total for room 247, checked in on Feb. 7th, 2014:", grand_total_247, "\n")

