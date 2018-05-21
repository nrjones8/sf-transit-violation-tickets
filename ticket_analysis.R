library(broom)
library(dplyr)
library(ggdag)
library(ggplot2)
library(lubridate)
library(plotly)

# 03/01/2017
ticket_date_format <- '%m/%d/%Y'
weather_date_format <- '%Y-%m-%d'

prep_violation_date <- function() {
  all_tickets <- read.csv('data/P000340_042418_Transit_Violation_Tickets_March_2017_through_March_2018.csv')
  all_tickets <- all_tickets %>% 
    mutate(
      ticket_date = as.Date(as.character(Tick.Issue.Date), ticket_date_format),
      # Strip the "$", make the money numeric
      total_paid = as.numeric(gsub('\\$', '', Total.Paid)),
      amount_due = as.numeric(gsub('\\$', '', Total.Amt.Due)),
      # Note that this can be _greater_ than a normal fee of $120 if that fee was not paid in a timely manner:
      # "Failure to pay a citation penalty by the first due date affixed to the notice of violation shall be $30;... etc"
      fee_amount = total_paid + ifelse(is.na(amount_due), 0, amount_due),
      is_unpaid = fee_amount > total_paid,
      day_of_week = wday(ticket_date, label=TRUE),
      is_weekday = ifelse(weekdays(ticket_date) %in% c('Saturday', 'Sunday'), FALSE, TRUE)
    ) %>%
    # Exactly one year - starting March 1 2017, ending at end of February, 2018
    filter(ticket_date > as.Date('02/28/2017', ticket_date_format)) %>%
    filter(ticket_date < as.Date('03/01/2018', ticket_date_format))

  return(all_tickets)
}

prep_weather_data <- function() {
  daily_weather <- read.csv('data/raw/order_1327523_noaa_2017_01_01_to_2018_04_23.csv')
  daily_weather <- daily_weather %>%
    mutate(
      weather_date = as.Date(as.character(DATE), weather_date_format),
      did_it_rain = ifelse(PRCP > 0, TRUE, FALSE)
    )
  return(daily_weather)
}

prep_monthly_ridership_data <- function() {
  # https://www.sfmta.com/reports/muni-average-weekday-boardings
  riders <- read.csv('data/raw/muni_average_weekday_boardings.csv')
  parse_month_num <- function(raw_month) {
    cleaned_month <- trimws(as.character(raw_month))
    
    return(which(cleaned_month == month.name))
  }
  # I don't understand why this works, but mutate(month_num = parse_month_num(Month.of.Month)) does not!!!
  riders$month_num <- sapply(riders$Month.of.Month, parse_month_num)
  riders$fiscal_year <- as.numeric(trimws(gsub('FY', '', riders$Year.of.Month)))
  
  # Fiscal year starts in July - so e.g. "July" of Fiscal year 2018 is really July of 2017.
  riders$true_year <- ifelse(riders$month_num > 6, riders$fiscal_year - 1, riders$fiscal_year)
  riders$estimated_boardings <- as.numeric(gsub(',', '', riders$Adjusted.Avg.Daily.Boardings))
  riders <- riders %>% arrange(true_year, month_num)
  
  return(riders)
}

combine_violation_weather_data <- function(violation_df, weather_df) {
  together <- merge(weather_df, violation_df, by.x = 'weather_date', by.y = 'ticket_date')
  
  return(together)
}

daily_violation_data <- function(df_with_weather) {
  by_day <- df_with_weather %>%
    group_by(weather_date) %>%
    summarise(
      total_tickets = n(),
      total_fees = sum(fee_amount),
      # If the fee has already been paid, then we have an NA for the amount still due. Ignore that in summing here.
      total_still_due = sum(amount_due, na.rm = TRUE),
      total_num_tickets_paid = sum(ifelse(is_unpaid, 0, 1)),
      total_paid = sum(total_paid),
      pct_paid = total_paid / total_fees,
      
      day_of_week = unique(day_of_week)[[1]],
      is_weekday = unique(is_weekday)[[1]],
      precipitation = unique(PRCP)[[1]],
      did_it_rain = unique(did_it_rain)[[1]]
    ) %>% mutate(
      # These fields are here just for convenience - will make it easier to join this table to others later on
      year = year(weather_date),
      month_num = month(weather_date)
    )

  return(by_day)
}

summarize_ticket_data <- function(df) {
  by_violation <- df %>%
    group_by(Violation.Desc) %>%
    summarise(n = n(), pct = n / nrow(df)) %>%
    arrange(desc(n))
  num_violations <- nrow(df)
  print(sprintf('Total violations: %s', num_violations))
  print('Most common violation types')
  print(head(by_violation))
  
  by_violation_code <- df %>%
    group_by(Violation) %>%
    summarise(n = n(), pct = n / nrow(df)) %>%
    arrange(desc(n))
  print('Most common violation codes')
  print(head(by_violation_code))
  
  print('-----------')
  avg_fee <- mean(df$fee_amount)
  print(sprintf('Avg violaton cost (whether it has been paid or not): %s', avg_fee))
  
  print('---------')
  by_day_of_week <- df %>%
    group_by(day_of_week) %>%
    summarise(n = n(), pct = n / num_violations)
  
  print('By day of week')
  print(by_day_of_week)
  
  
  daily_tickets <- df %>%
    group_by(ticket_date) %>%
    summarise(
      num_tickets = n()
    )
}

summarize_weather_citations_data <- function(df) {
  # df should be the daily summary data
  overall_summary <- df %>%
    group_by(did_it_rain) %>%
    summarise(
      num_days = n(),
      num_tickets = sum(total_tickets),
      avg_tickets = mean(total_tickets)
    )
  print('Overall summary of tickets based on whether it rained')
  print(overall_summary)
  
  weekday_weekend_summary <- overall_summary <- df %>%
    group_by(did_it_rain, is_weekday) %>%
    summarise(
      num_days = n(),
      num_tickets = sum(total_tickets),
      avg_tickets = mean(total_tickets)
    ) %>% arrange(is_weekday)
  print('Summary based on weekday vs. weekend, and rain')
  print(weekday_weekend_summary)
  
  print('t test for all data')
  rainy_subset <- df %>% filter(did_it_rain)
  non_rainy_subset <- df %>% filter(!did_it_rain)
  t_test <- t.test(rainy_subset$total_tickets, non_rainy_subset$total_tickets)
  print(t_test)
  
  print('t test for just weekdays')
  rainy_weekday_subset <- rainy_subset %>% filter(is_weekday)
  non_rainy_weekday_subset <- non_rainy_subset %>% filter(is_weekday)
  weekday_t_test <- t.test(rainy_weekday_subset$total_tickets, non_rainy_weekday_subset$total_tickets)
  print(weekday_t_test)
}

summarize_daily_data <- function(df) {
  by_rain_days <- df %>% 
    group_by(did_it_rain, day_of_week) %>% 
    summarise(
      n = n(),
      pct = n / nrow(df),
      
      avg_num_tix = mean(total_tickets), 
      median_num_tix = median(total_tickets),
      
      avg_fee_amount = mean(total_fees),
      median_fee_amount = median(total_fees),
      
      avg_fee_paid = mean(total_paid),
      median_fee_paid = median(total_paid)
    ) %>%
    arrange(day_of_week, did_it_rain)
  
  return(by_rain_days)
}

# Did it rain more or less on weekends?
summarize_weather_by_part_of_week <- function(df) {
  by_day <- df %>%
    group_by(day_of_week, did_it_rain) %>%
    summarise(n = n(), pct = n / nrow(df))
  print('Day of week weather summary')
  print(by_day)
  
  by_weekday_or_not <- df %>%
    group_by(is_weekday, did_it_rain) %>%
    summarise(n = n(), pct = n / nrow(df))
  print('Weekday vs. weekend weather summary')
  print(by_weekday_or_not)
}

boxplot_by_weather_and_day <- function(df_daily, save_plot=FALSE) {
  # Just create a new column with readable names, they'll get used for the legend
  df_daily$weather_description <- ifelse(df_daily$did_it_rain, 'Rain', 'No Rain')

  day_of_week_weather_bp <- ggplot(df_daily, aes(x = day_of_week, y = total_tickets, fill = weather_description)) +
    # include `varwidth` to indicate sample size by each group?
    geom_boxplot() +
    scale_x_discrete('Day of week') +
    scale_y_continuous('Number of Citations Issued') +
    scale_fill_discrete('Weather') +
    ggtitle('Daily MUNI Citations, by Weather and Day of Week', subtitle =  'Data from March 2017 - March 2018') +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  print(day_of_week_weather_bp)
  if (save_plot) {
    ggsave('graphics/boxplot_by_weather_and_day_of_week', device='png')
  }
}

plotly_boxplot <- function(df_daily) {
  # Just create a new column with readable names, they'll get used for the legend
  df_daily$weather_description <- ifelse(df_daily$did_it_rain, 'Rain', 'No Rain')

  p <- plot_ly(
    df_daily,
    x = ~day_of_week,
    y = ~total_tickets,
    color = ~weather_description,
    type = 'box',
    boxpoint = 'all',
    hoverinfo = ~paste(
      'some text'
    )
  ) %>% layout(boxmode = "group")
  p
}

barplot_weather_by_day <- function(daily_data_df, save_plot=FALSE) {
  rain_by_day <- daily_data_df %>%
    filter(did_it_rain) %>%
    group_by(day_of_week) %>%
    summarise(num_days = n())
  print(rain_by_day)
  g <- ggplot(data = rain_by_day, aes(x = day_of_week, y = num_days)) +
    geom_bar(stat = 'identity') +
    scale_x_discrete('Day of Week') +
    scale_y_continuous('Number of days it rained') + 
    ggtitle('Number of days it rained, by day of week', subtitle =  'Data from March 2017 - March 2018') +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  print(g)
  if (save_plot) {
    ggsave('graphics/rain_by_day_of_week.png', device='png')
  }
}



scatterplot_monthly_tickets_vs_ridership <- function(monthly_data, save_plot = FALSE) {
  g <- ggplot(data = monthly_data, aes(x = estimated_boardings, y = num_tickets)) +
    geom_point() +
    geom_text(aes(label = Month.of.Month), vjust = 0, hjust = 0) +
    scale_x_continuous('Estimated Daily Boardings', expand = expand_scale(.5)) +
    scale_y_continuous('Number of Monthly Citations') +
    ggtitle('Citation Volume vs. Ridership', subtitle = 'Data from March 2017 - March 2018') +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  print(g)
  
  if (save_plot) {
    ggsave('graphics/scatter_citation_vs_ridership', device='png')
  }
}

violation_data <- prep_violation_date()
weather_data <- prep_weather_data()
monthly_ridership_data <- prep_monthly_ridership_data()

combined_data <- combine_violation_weather_data(violation_data, weather_data)
daily_data <- daily_violation_data(combined_data)
daily_data_with_monthly_riders <- daily_with_ridership <- merge(
  daily_data, monthly_ridership_data, by.x = c('year', 'month_num'), by.y = c('true_year', 'month_num'), all.x = TRUE
)

summarize_weather_by_part_of_week(daily_data)
monthly_violation_data <- daily_data %>% 
  group_by(year, month_num) %>%
  summarise(
    num_tickets = sum(total_tickets),
    total_paid = sum(total_paid),
    total_fees = sum(total_fees)
  ) %>% left_join(monthly_ridership_data, by = c('year' = 'true_year', 'month_num' = 'month_num'))

summarize_ticket_data(violation_data)
summarize_daily_data(daily_data)

# ok, so the interesting thing is probably to look at:
# 1. Rain correlated with more tickets
# 2. Rain correlated with greater total fees
# 3. Rain correlated, but less so, with total _paid_ fees
# 4. If that's all true, then supports hypothesis that rainy days may mean more tickets, but those tickets are
# less likely to be paid (presumably b/c the extra people riding transit on rainy days can't afford / are just trying
# to get out of the rain).
# That does _not_ seem to be true - here we look at what % of fees have actually been paid. They're the same (~25%) across
# rain vs. not rain
rain_vs_no_rain_summary <- daily_data %>% 
  group_by(did_it_rain) %>% 
  summarise(
    num_days = n(),
    total_tickets = sum(total_tickets),
    total_paid_tickets = sum(total_num_tickets_paid),
    pct_tickets_paid = total_paid_tickets / total_tickets,
    # Amount actually paid so far
    total_paid_fees = sum(total_paid),
    # Amount paid + amount still due
    total_fees_paid_and_still_due = sum(total_fees),
    
    pct_paid = sum(total_paid_fees) / sum(total_fees_paid_and_still_due),
    paid_per_ticket = total_paid_fees / total_tickets,
    fee_per_ticket = total_fees_paid_and_still_due / total_tickets
  )

### plots on plots

boxplot_by_weather_and_day(daily_data)


linear_model <- function(ticket_data_by_day) {
  ticket_data_by_day$char_day_of_week <- as.character(ticket_data_by_day$day_of_week)
  day_of_week_model <- lm(total_tickets ~ did_it_rain + char_day_of_week, data = ticket_data_by_day)
  print(summary(day_of_week_model))
  return(day_of_week_model)
}
model <- linear_model(daily_data)