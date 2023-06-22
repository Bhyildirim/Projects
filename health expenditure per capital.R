# Load the required packages
library(ggplot2)
library(dplyr)

# Sample data
data <- data.frame(
  Year = c(2010:2021, 2010:2021, 2010:2021),
  Country = rep(c("United States", "Turkiye", "European Union"), each = 12),
  Health_Expenditure_Per_Capita = c(
    4000, 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 5000, 5100,
    1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600,
    3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000, 4100
  )
)

# Convert Year column to factor for better visualization
data$Year <- as.factor(data$Year)

# Create the timeline plot
plot <- ggplot(data, aes(x = Year, y = Health_Expenditure_Per_Capita, group = Country, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "Health Expenditure per Capita", title = "Timeline Data") +
  scale_x_discrete(breaks = seq(2010, 2021, by = 2)) +
  scale_color_manual(values = c("United States" = "red", "Turkiye" = "blue", "European Union" = "green"))

# Display the plot
print(plot)
