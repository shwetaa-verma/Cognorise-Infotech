"80 CEREALS"
"If you like to eat cereal, do yourself a favor and avoid this dataset at all
costs. After seeing these data it will never be the same for me to eat Fruity
Pebbles again."

#Installing package
install.packages("readxl")
library(readxl)

# Load the data from Excel file
cereal_data <- read.csv("C:/Users/hp/OneDrive/Desktop/projects/TASK 3/cereal.csv")
cereal_data

# View the first few rows of the data
head(cereal_data)

# Install and load necessary packages
install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# Display the structure of the data
str(cereal_data)

# Summary statistics
summary(cereal_data)

# Check for missing values
sum(is.na(cereal_data))

# Remove duplicates
data1 <- distinct(cereal_data)
data1

##Plots
###Bar charts
"The number of cereals by manufacturer"
ggplot(cereal_data, aes(x = mfr, fill = mfr)) +
  geom_bar() +
  labs(title = "Number of Cereals by Manufacturer", x = "Manufacturer", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Cereal Distribution by Manufacturer and Type
ggplot(cereal_data, aes(x = mfr, fill = type)) +
  geom_bar() +
  labs(title = "Cereal Distribution by Manufacturer and Type", x = "Manufacturer", y = "Count")

# Shelf distribution
ggplot(cereal_data, aes(x = shelf, fill = factor(shelf))) +
  geom_bar() +
  labs(title = "Cereal Distribution by Shelf", x = "Shelf", y = "Count")

#MFR wrt carbo, calories, rating
# Carbo with respect to mfr
ggplot(cereal_data, aes(x = mfr, y = carbo, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Carbohydrate Content by Manufacturer",
       x = "Manufacturer",
       y = "Carbohydrate",
       fill = "Manufacturer") +
  theme_minimal()

# Calorieswith respect to mfr
ggplot(cereal_data, aes(x = mfr, y = calories, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Calories Content by Manufacturer",
       x = "Manufacturer",
       y = "Calories",
       fill = "Manufacturer") +
  theme_minimal()

# Carbo with respect to mfr
ggplot(cereal_data, aes(x = mfr, y = rating, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Rating of Manufacturer",
       x = "Manufacturer",
       y = "rating",
       fill = "Manufacturer") +
  theme_minimal()

##Pie charts
library(dplyr)
install.packages("plotly")
library(plotly)

# Manufacturer distribution
manufacturer_distribution <- cereal_data %>%
  group_by(mfr) %>%
  summarize(count = n())

plot_ly(labels = manufacturer_distribution$mfr, values = manufacturer_distribution$count, type = "pie")

#Share of each mfr
# Calculate the share of each manufacturer
manufacturer_share <- table(cereal_data$mfr)

# Create a data frame for plotting
share_data <- data.frame(manufacturer = names(manufacturer_share),
                         share = manufacturer_share)
share = manufacturer_share

# Creating pie chart
ggplot(share_data, aes(x = "", y = share, fill = manufacturer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Manufacturer Share", fill = "Manufacturer") +
  theme_minimal()

# Scatter plot for protein vs. calories
ggplot(cereal_data, aes(x = protein, y = calories, color = type)) +
  geom_point() +
  labs(title = "Scatter Plot of Protein vs. Calories", x = "Protein", y = "Calories")


# Box plot for calories, protein, and fat
library(ggplot2)
ggplot(cereal_data, aes(x = type, y = calories, fill = type)) +
  geom_boxplot() +
  labs(title = "Distribution of Calories by Cereal Type", x = "Cereal Type", y = "Calories")

# Correlation heatmap
install.packages("corrplot")
library(corrplot)
correlation_matrix <- cor(cereal_data[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", 
                                   "potass", "vitamins", "rating")])
corrplot(correlation_matrix, method = "color")

