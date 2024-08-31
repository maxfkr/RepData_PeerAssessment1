# Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
activity_data <- read.csv("activity.csv")
activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")

# 1. Mean total number of steps per day
total_steps_per_day <- activity_data %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE))

# Calculate mean and median
mean_steps <- mean(total_steps_per_day$total_steps)
median_steps <- median(total_steps_per_day$total_steps)

# Print mean and median
cat("Mean steps per day:", mean_steps, "\n")
cat("Median steps per day:", median_steps, "\n")

# Histogram of total steps per day
ggplot(total_steps_per_day, aes(x = total_steps)) +
        geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
        labs(title = "Histogram of Total Steps per Day", x = "Total Steps", y = "Frequency") +
        ggsave("histogram_total_steps.png")

# 2. Average daily activity pattern
avg_steps_per_interval <- activity_data %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm = TRUE))

# Time series plot
ggplot(avg_steps_per_interval, aes(x = interval, y = avg_steps)) +
        geom_line() +
        labs(title = "Average Number of Steps per 5-Minute Interval", x = "5-Minute Interval", y = "Average Steps") +
        ggsave("avg_daily_activity.png")

# Interval with maximum average steps
max_interval <- avg_steps_per_interval %>%
        filter(avg_steps == max(avg_steps)) %>%
        pull(interval)

cat("Interval with maximum average steps:", max_interval, "\n")

# 3. Imputing missing values
# Count missing values
total_missing <- sum(is.na(activity_data$steps))
cat("Total number of missing values:", total_missing, "\n")

# Impute missing values with mean steps per interval
imputed_data <- activity_data %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
        ungroup()

# Total steps per day after imputation
total_steps_per_day_imputed <- imputed_data %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps))

# Calculate mean and median after imputation
mean_steps_imputed <- mean(total_steps_per_day_imputed$total_steps)
median_steps_imputed <- median(total_steps_per_day_imputed$total_steps)

cat("Mean steps per day after imputation:", mean_steps_imputed, "\n")
cat("Median steps per day after imputation:", median_steps_imputed, "\n")

# Histogram of total steps per day (imputed data)
ggplot(total_steps_per_day_imputed, aes(x = total_steps)) +
        geom_histogram(binwidth = 1000, fill = "green", color = "black") +
        labs(title = "Histogram of Total Steps per Day (Imputed Data)", x = "Total Steps", y = "Frequency") +
        ggsave("histogram_total_steps_imputed.png")

# 4. Differences in activity patterns between weekdays and weekends
# Create a new variable for weekday or weekend
imputed_data$day_type <- ifelse(weekdays(imputed_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Average steps per 5-minute interval by day type
avg_steps_by_daytype <- imputed_data %>%
        group_by(day_type, interval) %>%
        summarize(avg_steps = mean(steps))

# Panel plot
ggplot(avg_steps_by_daytype, aes(x = interval, y = avg_steps, color = day_type)) +
        geom_line() +
        facet_wrap(~day_type, ncol = 1) +
        labs(title = "Average Steps per 5-Minute Interval: Weekdays vs. Weekends", x = "5-Minute Interval", y = "Average Steps") +
        ggsave("avg_steps_by_daytype.png")

# Final message
cat("Analysis complete. Plots have been saved.")