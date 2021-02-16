library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

df <- read_csv("fcc-forum-pageviews.csv")

# Clean the data (drop the days that had page views below the 2.5th percentile or above the 97.5th percentile)
df <- df %>% filter(
    (value >= quantile(df$value, 0.025)) &
        (value <= quantile(df$value, 0.975))
)

# Create a line plot
df %>% ggplot(aes(x = date, y = value)) +
    geom_line(color = "#445BC1", size = 1) +
    labs(
        x = "Date",
        y = "Page Views",
        title = "Daily freeCodeCamp Forum Page Views 5/2016-12/2019"
    ) +
    theme(plot.title = element_text(hjust = 0.5))

# Create a bar plot showing average daily page views for each month grouped by year
df_bar <- df

df_bar$month <- month(df_bar$date, label = TRUE, abbr = FALSE)
df_bar$year <- year(df_bar$date)

color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(12)

df_bar %>%
    group_by(year, month) %>%
    summarise(avg = mean(value)) %>%
    ggplot(aes(x = year, y = avg)) +
    geom_bar(
        aes(fill = month),
        color = "white",
        position = "dodge",
        stat = "identity"
    ) +
    scale_fill_manual(values = color_palette) +
    labs(
        x = "Years",
        y = "Average Page Views",
        fill = "Month",
        title = "Average page views by month (2016-2019)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))


# Create two box plots showing how the page views are distributed within a given year or month and how it compares over time
df_box <- df_bar
df_box$month <- month(df_bar$date, label = TRUE)

p1 <- df_box %>%
    ggplot(aes(x = factor(year), y = value, fill = factor(year))) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Set2") +
    labs(
        x = "Year",
        y = "Page Views",
        title = "Year-wise Box Plot (Trend)"
    ) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p2 <- df_box %>%
    ggplot(aes(x = month, y = value, fill = month)) +
    geom_boxplot() +
    scale_fill_manual(values = col_palette2) +
    labs(
        x = "Month",
        y = "Page Views",
        title = "Month-wise Box Plot (Seasonality)"
    ) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(p1, p2, ncol = 2)