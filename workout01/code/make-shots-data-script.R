#Title: Code of Aggregating Player Data
#Description: Codes to combine five player's individual data frame into one and summarize it.
#Input(s): stephen-curry.csv, andre-iguodala.csv, kevin-durant.csv, draymond-green.csv, klay-thompson.csv
#Output(s): shots-data.csv, shots-data-summary.txt, stephen-curry-summary.txt, andre-iguodala-summary.txt...

data_types = c("team_name"="character", "game_date"="character", "season" = "integer", "period"="integer",
               "minutes_remaining"="integer", "seconds_remaining"="integer", "shot_made_flag"="character",
               "action_type"="factor", "shot_type"="factor", "shot_distance"="integer", "opponent"="character",
               "x"="integer", "y"="integer")

#read in five data sets, using relative file paths
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE, colClasses = data_types)
andre <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE, colClasses = data_types)
kevin <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE, colClasses = data_types)
draymond <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE, colClasses = data_types)
klay <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE, colClasses = data_types)

#add column name
curry$name <- "Stephen Curry"
andre$name <- "Andre Iguodala"
kevin$name <- "Kevin Durant"
draymond$name <- "Draymond Green"
klay$name <- "Klay Thompson"

#change the original values of shot_made_flag to more descriptive values
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
andre$shot_made_flag[andre$shot_made_flag == "n"] <- "shot_no"
kevin$shot_made_flag[kevin$shot_made_flag == "n"] <- "shot_no"
draymond$shot_made_flag[draymond$shot_made_flag == "n"] <- "shot_no"
klay$shot_made_flag[klay$shot_made_flag == "n"] <- "shot_no"

curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
andre$shot_made_flag[andre$shot_made_flag == "y"] <- "shot_yes"
kevin$shot_made_flag[kevin$shot_made_flag == "y"] <- "shot_yes"
draymond$shot_made_flag[draymond$shot_made_flag == "y"] <- "shot_yes"
klay$shot_made_flag[klay$shot_made_flag == "y"] <- "shot_yes"

#add a column minute that contains the minute number where a shot occured
curry$minute <- (curry$period - 1) * 12 + (12 - curry$minutes_remaining)
andre$minute <- (andre$period - 1) * 12 + (12 - andre$minutes_remaining)
kevin$minute <- (kevin$period - 1) * 12 + (12 - kevin$minutes_remaining)
draymond$minute <- (draymond$period - 1) * 12 + (12 - draymond$minutes_remaining)
klay$minute <- (klay$period - 1) * 12 + (12 - klay$minutes_remaining)

#use sink() to send the summary() output of each imported data frame into individuals text files
sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()
sink(file = '../output/andre-iguodala-summary.txt')
summary(andre)
sink()
sink(file = '../output/draymond-green-summary.txt')
summary(draymond)
sink()
sink(file = '../output/kevin-durant-summary.txt')
summary(kevin)
sink()
sink(file = '../output/klay-thompson-summary.txt')
summary(klay)
sink()

#use the row binding function rbind() to stack the tables into one single data frame
combined <- rbind(curry, andre, draymond, kevin, klay)

#export the assembled table
write.csv(combined, file = '../data/shots-data.csv')

#use sink() to send the summary() output of the assembled table
sink(file = '../output/shots-data-summary.txt')
summary(combined)
sink()

