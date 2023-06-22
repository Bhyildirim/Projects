View(Ireland_Primary_Care)
df <- data.frame(
  year = rep(2010:2021, each = 7), # changed from 5 to 7
  type = rep(c(
    'Asthma hospital admission',
    'Chronic obstructive pulmonary disease hospital admission',
    'Asthma and chronic obstructive pulmonary disease hospital admission',
    'Congestive heart failure hospital admission',
    'Hypertension hospital admission',
    'Congestive heart failure and hypertension hospital admission',
    'Diabetes hospital admission'
  ), times = 12),
  value = runif(7 * 12 * 12) # updated to match the new row count
)
# Plot the data
ggplot(df, aes(x = year, y = value)) +
  geom_line() +
  labs(x = "Year", y = "Value", title = "Primary Care Over Time for Ireland")


# Basic line plot with ggplot2
ggplot(data = df, aes(x = year, y = value, color = type)) + 
  geom_line() +
  labs(x = "Year", y = "Value", color = "Type", 
       title = "Primary Care in Ireland over time") +
  theme_minimal()