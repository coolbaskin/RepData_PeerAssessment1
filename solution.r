data_dir <- 'data';
data_file <- 'activity.csv'
zipped_data_file <- 'activity.zip'

data_file_path <- file.path(data_dir, data_file);

# If the final data file does not exist, check for zipped data file. 
#   If it exists, unzip it and continue
#   If it does not exist, exit with error

if(!file.exists(data_file_path)) {
    cat('Data file <', data_file_path, '> is not found. Searching for ziped data file <', zipped_data_file, '>' );

    if(file.exists(zipped_data_file)) {
        cat('Zipped data file found. Unzipping data...');
        unzip(zipped_data_file, files = c(data_file), exdir = data_dir); 
    }
    else {
		stop(paste0("Cannot find either zipped or unzipped data file. Cannot continue without data!"));
    }
}

########################################################################################################################
# Read the data
########################################################################################################################
d <- read.csv(data_file_path);

# Only steps variable in this dataset takes NA values
# > colSums(is.na(d))
#   steps     date interval 
#    2304        0        0 

########################################################################################################################
# Clean the data
########################################################################################################################
# Lets completely exclude the dates for which there only datapoints with NAs are available. 
# There is too little information (none!) available for them to make any meaningful imputations
library(dplyr)

d_agg_perday <- d %>% 
    group_by(date) %>% 
    summarize(
        any_steps_not_NA = any(!is.na(steps))          # Flag indicating if there is any non-NA data available for the
                                                        # date
    )

na_dates <- d_agg_perday %>% filter(any_steps_not_NA == FALSE) %>% select(date)
d.no_na_dates <- filter(d, !(date %in% na_dates$date))


########################################################################################################################
# PROBLEM: What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
#   1. Make a histogram of the total number of steps taken each day
#   2. Calculate and report the mean and median total number of steps taken per day
########################################################################################################################
# SOLUTION
# We can go about it using standard *apply* functions, e.g.
#   tapply(d$steps, d$date, sum, na.rm = TRUE)
# Or we can use newly learned dplyr functions. Let's practice dplyr
########################################################################################################################

# Aggregate the data ignoring the NAs
d_agg_perday <- d.no_na_dates %>% 
    group_by(date) %>% 
    summarize(
        sum_steps = sum(steps, na.rm = TRUE)
        ,mean_steps = mean(steps, na.rm = TRUE)
        ,median_steps = median(steps, na.rm = TRUE)
        ,cnt_datapoints = n()
        ,any_steps_not_NA = any(!is.na(steps))          # Flag indicating if there is any non-NA data available for the
                                                        # date (should be all TRUE after cleaning)
    )

# Histogram of the total number of steps taken each day
hist(
    d_agg_perday$sum_steps
    ,breaks = 25
    ,col = "gold"
    ,main = 'Total nubmer of steps taken per day'
    ,xlab = 'Total number of steps per day'
    ,ylab = 'Number of days'
)

# Calculate and report the mean and median total number of steps taken per day
cat('Summary for the total number of steps taken per day:', "\n");
cat(paste0('Mean = ', mean(d_agg_perday$sum_steps), ', median = ', median(d_agg_perday$sum_steps), "\n"));

########################################################################################################################
# PROBLEM: What is the average daily activity pattern?
#   1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps
#   taken, averaged across all days (y-axis)
#   2 . Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
########################################################################################################################
# SOLUTION
# Similar to previous only aggregation is performed per 5-minute interval, not per day
########################################################################################################################

# Aggregate the data ignoring the NAs
d_agg_perintl <- d.no_na_dates %>% 
    group_by(interval) %>% 
    summarize(
        sum_steps = sum(steps, na.rm = TRUE)
        ,mean_steps = mean(steps, na.rm = TRUE)
        ,median_steps = median(steps, na.rm = TRUE)
        ,cnt_datapoints = n()
        ,any_steps_not_NA = any(!is.na(steps))          # Flag indicating if there is any non-NA data available for the
                                                        # date (should be all TRUE after cleaning)
    )

# Since the number of datapoints per day and interval is the same (only full days were removed during cleaning, so these
# numbers are intact), the order on sum and mean is the same (mean = sum/n and n's are all equal)
ind_intl_max_sum <- which.max(d_agg_perintl$sum_steps)
cat(paste0('Interval with the maximum number of steps (on average across all days in the dataset): ',
    d_agg_perintl$interval[ind_intl_max_sum], 
    '. Its mean number of steps is ', round(d_agg_perintl$mean_steps[ind_intl_max_sum], 2), "\n"));

########################################################################################################################
# PROBLEM: Imputing missing values
# 
# 1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of
# missing days may introduce bias into some calculations or summaries of the data.
# 
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# 
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be
# sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 
# 4 .Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total
# number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What
# is the impact of imputing missing data on the estimates of the total daily number of steps?
########################################################################################################################
# SOLUTION
# Similar to previous only aggregation is performed per 5-minute interval, not per day
########################################################################################################################

# Total number of rows with NAs
cnt_incomplete_entries <- sum(!complete.cases(d));
cnt_entries <- dim(d)[1];
if( cnt_incomplete_entries > 0 ) {
    cat('Achtung! The data contains incomplete entries (where at least one variable has value of NA.', "\n");
    cat(paste0('Count = ', cnt_incomplete_entries, ', percentage = ', 
        round(cnt_incomplete_entries/cnt_entries*100, 2), "%\n"));
}

# Impute the values for the missing days
# TODO: better imputation strategy
d.imputed <- d;
d.imputed$steps[d.imputed$date %in% na_dates$date] <- 0

# Aggregate the data NOT ignoring the NAs (there should be none)
d_agg_perday <- d.imputed %>% 
    group_by(date) %>% 
    summarize(
        sum_steps = sum(steps)
        ,mean_steps = mean(steps)
        ,median_steps = median(steps)
        ,cnt_datapoints = n()
        ,any_steps_not_NA = any(!is.na(steps))          # Flag indicating if there is any non-NA data available for the
                                                        # date (should be all TRUE after imputation)
    )

# Histogram of the total number of steps taken each day
hist(
    d_agg_perday$sum_steps
    ,breaks = 25
    ,col = "gold"
    ,main = 'Total nubmer of steps taken per day'
    ,xlab = 'Total number of steps per day'
    ,ylab = 'Number of days'
)

# Calculate and report the mean and median total number of steps taken per day
cat('Summary for the total number of steps taken per day:', "\n");
cat(paste0('Mean = ', mean(d_agg_perday$sum_steps), ', median = ', median(d_agg_perday$sum_steps), "\n"));

########################################################################################################################
# PROBLEM: Are there differences in activity patterns between weekdays and weekends?
# 
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for
# this part.
# 
# 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given
# date is a weekday or weekend day.
# 
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average
# number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like
# the following, which was created using simulated data:
########################################################################################################################
# SOLUTION
# Similar to previous only aggregation is performed per 5-minute interval, not per day
########################################################################################################################

# Use lubridate for operations with dates
library(lubridate)

# wday returns the day of the week as decimal. 
#   (Sun = 1, Sat = 7) => weekend
#   (everything else) => weekday
d.imputed %<>% mutate(
    week_period = as.factor(ifelse(wday(ymd(date)) %in% c(1,7), 'weekend', 'weekday'))
);

# Note: assume no NAs in imputed data
d_agg_per_weekperiod_intl <- d.imputed %>% 
    group_by(week_period, interval) %>% 
    summarize(
        sum_steps = sum(steps)
        ,mean_steps = mean(steps)
        ,median_steps = median(steps)
        ,cnt_datapoints = n()
    )

p <- ggplot(d_agg_per_weekperiod_intl, aes(interval, mean_steps)) + 
    geom_line(colour = 'blue') +
    facet_grid(week_period ~ .) +
    ggtitle('Average number of steps taken per time interval') + 
    xlab('Time interval') +
    ylab('Average number of steps taken')

print(p)
