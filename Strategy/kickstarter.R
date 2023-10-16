week <- 19

#load in data
info <- readRDS(sprintf("Documents/Data Science in Business and Entrepreneurship/Year 1/S1/Strategy and Business Models/data/Kickstarter/2019_week%s/info.rds",week)) 

# Create a new column 'Successful' based on the condition
info$Successful <- ifelse(info$Pledge_USD > info$Goal_USD, 1, 0)

# Dates are in Unix timestamp format
head(info$Launched_at, 2)
head(info$Deadline, 2)

# Convert 'Launched_at' and 'Deadline' columns to Date type because dates are in Unix timestamp format
info$Launched_at <- as.POSIXct(as.numeric(info$Launched_at), tz = "America/New_York")
info$Deadline <- as.POSIXct(as.numeric(info$Deadline), tz = "America/New_York")

# Print the updated dataset
head(info$Launched_at,2)
head(info$Deadline,2)

# Calculate the duration in days
info$Duration <- round(as.numeric(difftime(info$Deadline, info$Launched_at, units = "days")))
head(info$Duration,2)

# Plot
library(ggplot2)
# Calculate the success rate
success_rate <- mean(info$Successful)
# Bar plot of success rate by duration
ggplot(info, aes(x = Duration, y = Successful)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(x = "Duration (days)", y = "Success Rate", title = "Success Rate vs. Duration") +
  geom_hline(yintercept = success_rate, color = "red", linetype = "dashed") +
  theme_minimal()

# Function to combine data from week 19 to week 32
library(dplyr)
combineKickstarterData <- function() {
  combined_data <- data.frame()
  # Loop through weeks 19 to 32
  for (week in 19:32) {
    # Load data for the current week
    current_data <- readRDS(sprintf("Documents/Data Science in Business and Entrepreneurship/Year 1/S1/Strategy and Business Models/data/Kickstarter/2019_week%s/info.rds", week))
    
    # Append the current week's data to the combined data
    combined_data <- bind_rows(combined_data, current_data)
  }
  
  # Return the combined data
  return(combined_data)
}

# Call the function to combine the data
combined_data <- combineKickstarterData()

# Create a new column 'Successful' based on the condition
combined_data$Successful <- ifelse(combined_data$Pledge_USD > combined_data$Goal_USD, 1, 0)

# Convert 'Launched_at' and 'Deadline' columns to Date type because dates are in Unix timestamp format
combined_data$Launched_at <- as.POSIXct(as.numeric(combined_data$Launched_at), tz = "America/New_York")
combined_data$Deadline <- as.POSIXct(as.numeric(combined_data$Deadline), tz = "America/New_York")

# Calculate the duration in days
combined_data$Duration <- round(as.numeric(difftime(combined_data$Deadline, combined_data$Launched_at, units = "days")))
head(combined_data$Duration,2)

# Plot
library(ggplot2)
# Calculate the success rate
success_rate <- mean(combined_data$Successful)
# Bar plot of success rate by duration
ggplot(combined_data, aes(x = Duration, y = Successful)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(x = "Duration (days)", y = "Success Rate", title = "Success Rate vs. Duration") +
  geom_hline(yintercept = success_rate, color = "red", linetype = "dashed") +
  theme_minimal()


# Calculate success rates for different campaign durations
success_rates <- combined_data %>%
  group_by(Duration) %>%
  summarize(Success_Rate = mean(Successful))

# Plot success rates over different durations
ggplot(success_rates, aes(x = Duration, y = Success_Rate)) +
  geom_line(color = "blue") +
  labs(x = "Duration (days)", y = "Success Rate", title = "Success Rate vs. Duration") +
  theme_minimal()

