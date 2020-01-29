# the csv file and R script are under the same directory, so I import data with a relative path
# strings in this csv file are not categorical variables, so I set the parameter "stringsAsFactors" as FALSE
roster <- read.table(file="./Lab2data.csv", 
                     header=TRUE, 
                     stringsAsFactors=FALSE, 
                     nrows=50, 
                     encoding="utf-8", 
                     sep=",")
# check data type
class(roster)

# take a peek
head(roster)

# rows and columns of this data frame
dim(roster)

# check data type of each column
str(roster)

print(paste("Mean of age is", mean(roster$Age)))
# adopt round to keep two decimal places
print(paste("Standard deviation of age is", format(round(sd(roster$Age), digits=2), nsmall=2)))

print(paste("Mean of income is", mean(roster$Income)))
print(paste("Standard deviation of income is", format(round(sd(roster$Income), digits=2), nsmall=2)))

# adopt Pearson Correlation for these two columns containing numerical values
format(round(cor(x=roster[, "Age"], y=roster[, "EduYears"], method="pearson"), digits=2), nsmall=2)

# Names of all people have an income level of 7
# the parameter 2 is short for the index of Name column
roster[roster$Income==7, 2]

# their average age
mean(roster[roster$Income==7, 3])

# the new numeric column is named "Added"
roster["Added"] <- rep(1, 50)
# take a peek at the new data frame
head(roster)

# the new column is named "ID"
roster["ID"] <- 1:50
# take a peek at the new data frame
head(roster)

# the new column is named "AgeLimit"
roster["AgeLimit"] <- roster$Age < 40
# take a peek at the new data frame
head(roster)

# use as.Date for the conversion
# in the format parameter, %m denotes month, %d denotes day, %Y denotes year (in four digits form)
roster["SignupDate"] <- as.Date(roster$SignupDate, format="%m/%d/%Y")
# perform the logical judgment (before May 1st 2008 as TRUE, otherwise FALSE)
# the new column is named "DateLimit"
roster["DateLimit"] <- roster$SignupDate < "2008-05-01"
# take a peek
head(roster)

# display the complete data
roster

# the new data frame is named "rosterSubset"
# Name column is the second column, Income is the fourth
rosterSubset <- roster[, c(2, 4)]
# take a peek
head(rosterSubset)

# check the dimensions
dim(rosterSubset)

# the new data file is named "Lab2data_subset.csv"
write.csv(rosterSubset, 
          file="./Lab2data_subset.csv", 
          quote=FALSE, 
          row.names=FALSE)
