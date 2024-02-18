"Unemployment is measured by the unemployment rate which is the
number of people who are unemployed as a percentage of the total labour
force.
We have seen a sharp increase in the unemployment rate during Covid-19, this is why 
we think it is a appropriate idea to analyse the unemployemnt."

"The following report provides an analysis of unemployment rates across different states of India, with a focus on the impact of the COVID-19 
pandemic. By exploring various dimensions of the dataset, including state-wise
unemployment, urban vs. rural disparities, and labor participation rates, we
aim to uncover patterns and insights that could inform policy decisions."


"The dataset contains monthly unemployment data from Kaggle spanning from 31st May, 2019 to 30th june 2020."

"DATASET OVERVIEW"

"The provided dataset delves into the unemployment landscape across diverse states in India:

States: Various states constituting the Indian subcontinent.
Date: The specific dates of unemployment rate recordings.
Measuring Frequency: The regularity of measurement collection (Monthly).
Estimated Unemployment Rate (%): The proportion of unemployed individuals in each Indian state.
Estimated Employed Individuals: The tally of presently engaged individuals.
Estimated Labour Participation Rate (%): The percentage of the working-age populace 
  (16-64 years) actively involved in the job market, including both employed individuals and 
  those actively seeking jobs."

"Step 1 : Loading the data"

"Install and load the readxl package"
install.packages("readxl")
library(readxl)

# Load the data from the Excel file
unemp_data <- read.csv("Unemployment in India.csv")
unemp_data


"Step 2 : Cleaning and preprocessing the data "

"Duplicate entries were removed, and rows with missing values were omitted to
ensure the accuracy of the analysis. Date formats were standardized, and 
extraneous whitespace was trimmed from the 'Frequency' column."

installed.packages("dplyr")
library(dplyr)
# Remove duplicate rows
unemp_data <- distinct(unemp_data)

# Remove rows with all NAs
unemp_data <- na.omit(unemp_data)
unemp_data

# Convert the Date column to Date objects
unemp_data$Date <- as.Date(unemp_data$Date, format = "%d-%m-%Y")

# Strip leading/trailing whitespace from the Frequency column
unemp_data$Frequency <- trimws(unemp_data$Frequency)
unemp_data


"Step 3 : Exploratory data analysis"

# Summary statistics
summary(unemp_data$`Estimated Unemployment Rate (%)`)
sd(unemp_data$`Estimated Unemployment Rate (%)`)

"Unemployment Rate
The unemployment rate has a wide range, from 0% to 76.74%, with a mean around 11.79%.
The standard deviation is quite high at 10.72%, indicating significant variability.
50% of the data (the median) falls below 8.35%, which suggests that the distribution
   might be right-skewed with some very high unemployment rates pulling the mean upwards.
"
# Check data type of the column
class(unemp_data$`Estimated Unemployment Rate (%)`)

summary(unemp_data$`Estimated Employed`)
sd(unemp_data$`Estimated Employed`)

"Employed

The number of employed individuals also varies widely across the dataset.
The standard deviation is nearly as large as the mean, indicating a substantial 
    spread in the number of employed people across different regions or times.
The median is less than half of the maximum value, again suggesting a right-skewed 
   distribution."

colnames(unemp_data)
summary(unemp_data$`Estimated Unemployment Rate (%)`)
sd(unemp_data$`Estimated Labour Participation Rate (%)`)

"Labour Participation Rate

The labor participation rate has a mean of 42.63% with a standard deviation of 8.11%, 
    suggesting moderate variability.
The median is very close to the mean, which may indicate a more symmetric distribution
    of labor participation rates."


"Next, we should visualize these distributions and look at how the unemployment rate
changed over time, especially in light of the COVID-19 pandemic, as well as
differences between regions and urban vs. rural areas.

Let's start with visualizations for the distribution of the unemployment rate,
employed population, and labor participation rate. We'll use histograms
for this purpose."



# Install and load necessary packages
install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

# Histogram of Estimated Unemployment Rate (%)

ggplot(unemp_data, aes(x = "Estimated Unemployment Rate (%)")) +
  geom_histogram(binwidth = 2.5, fill = "red", colour = "blue", alpha = 0.7, stat = "count") + 
  labs(title = "Distribution of Estimated Unemployment Rate (%)", x = "Estimated Unemployment Rate (%)", y = "Frequency")


ggplot(unemp_data, aes(x = "Estimated Unemployment Rate (%)")) +
  geom_histogram(binwidth = 2.5, fill = "pink",colour = "grey", alpha = 0.7,stat = "count") + 
  labs(title = "Distribution of Estimated Unemployment Rate (%)", x = "Unemployment Rate (%)", y = "Frequency")


"The distribution shows a peak at lower percentages, indicating that most data points 
    have a relatively low unemployment rate.
There is a long tail toward higher percentages, confirming the right-skewness
    suggested by the summary statistics.
The presence of very high unemployment rates may indicate extreme cases or 
    specific times of distress (such as the COVID-19 pandemic)."


ggplot(unemp_data, aes(x = "Estimated Labour Participation Rate (%)")) +
  geom_histogram(binwidth = 3, fill = "green",colour = "black", alpha = 0.7, stat = "count") + 
  labs(title = "Distribution of Estimated Labour Participation Rate(%) ", 
       x = "Labour Participation Rate(%)", y = "Frequency")

"The distribution appears more symmetric than the other two, with a clear central
     peak and tails on both sides.
This suggests that the labor participation rate varies less dramatically across
    different regions or over time than the unemployment rate or the number 
    of employed individuals."

" Step 4"

"Next, let's analyze how the unemployment rate has changed over time. We'll 
    create a time series plot to visualize this trend. This will help us 
    understand if there were any significant changes that might be associated
    with the COVID-19 pandemic or other events. We'll also look at the 
    unemployment rate's trend in rural versus urban areas."

# Line plot for Unemployment Rate over time

ggplot(unemp_data, aes(x = Date, y = "Estimated Unemployment Rate (%)", 
                 group = Area, color = Area)) +
  geom_line() 
labs(title = "Unemployment Rate Over Time by Area", x = "Date",
     y = "Unemployment Rate (%)")

"The time series plots provide the following insights:

Overall Trend:

The unemployment rate fluctuates over time, with some peaks and troughs indicating
  periods of higher and lower unemployment.
There is a noticeable spike around mid-2020, which aligns with the onset of the 
  COVID-19 pandemic and the subsequent lockdowns and economic disruptions.
  
By Area (Rural vs. Urban):

The plot shows the overall trend without specifically focusing on the 
COVID-19 period. The unemployment rate in urban areas seems to be generally higher
  than in rural areas.
The plot, highlights the impact of the COVID-19 pandemic more clearly. Both rural and urban areas 
experienced a significant increase in unemployment rates during the pandemic, 
with urban areas being more affected.
After the spike, there is a downward trend indicating a recovery phase,
but the rates have not returned to the pre-pandemic levels, especially in 
urban areas.
"
"Step 5"

# State-wise analysis
# Recalculate the mean

state_unemployment <- unemp_data %>%
  group_by(Region) %>%
  summarize(Average_Unemployment = mean("Estimated Unemployment Rate (%)", na.rm = TRUE)) %>%
  filter(!is.na(Average_Unemployment)) %>%
  arrange(Average_Unemployment)


ggplot(state_unemployment, aes(x = Average_Unemployment, y = reorder(Region, Average_Unemployment))) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Unemployment Rate by State", x = "Average Unemployment Rate (%)", y = "State")


"The bar chart shows the average unemployment rate for each state over the time
period covered by the dataset. Here are some observations:

There is a wide range in the average unemployment rates across different states,
indicating regional disparities in employment conditions.
Some states have notably higher average unemployment rates, while others have
managed to maintain lower averages.
This state-wise analysis is valuable for policymakers to identify which regions
may need more attention and resources to combat unemployment."

# Urban vs. Rural analysis
urban_rural_unemployment <- unemp_data %>%
  group_by(Area, Date) %>%
  summarize(Average_Unemployment = mean("Estimated Unemployment Rate (%)"))

ggplot(urban_rural_unemployment, aes(x = Date, y = Average_Unemployment, group = Area, color = Area)) +
  geom_line() +
  labs(title = "Urban vs. Rural Unemployment Rate Over Time", x = "Date", y = "Average Unemployment Rate (%)")

"The line plot illustrates the urban versus rural unemployment rate trends over time:

Both urban and rural areas show fluctuations in unemployment rates, with urban 
areas generally experiencing higher rates than rural areas throughout the dataset's timeframe.
The spike around mid-2020, likely corresponding to the COVID-19 pandemic's impact,
is clearly visible in both urban and rural areas, with urban areas showing a sharper increase.
Post the spike, there appears to be a gradual decline, suggesting a recovery from
the peak unemployment rates experienced during the pandemic."


"Step 6"

# Pre and Post COVID-19 analysis
pre_pandemic <- filter(unemp_data, Date < as.Date("2020-03-01"))
post_pandemic <- filter(unemp_data, Date >= as.Date("2020-03-01"))

mean(pre_pandemic$`Estimated Unemployment Rate (%)`)
mean(post_pandemic$`Estimated Unemployment Rate (%)`)

"The comparison of average unemployment rates before and after the onset 
of the COVID-19 pandemic reveals:

Pre-pandemic average unemployment rate: approximately 9.51%
Post-pandemic average unemployment rate: approximately 17.77%
This indicates a significant increase in the unemployment rate following the onset of the
pandemic. The data suggests that the economic impact of COVID-19 was considerable,
nearly doubling the unemployment rate on average across the regions in the dataset."