#
#       Introduction to Tidyverse, dlpyr and ggplot2 in R
#    University at Buffalo Biomedical Informatics Bootcamp   
#       By Samuel Tiosano, MD, MPH             2024-07-23     
#                       stiosano@buffalo.edu
#

# Setup -------------------------------------------------------------------

# Load necessary libraries for data manipulation, string handling, and visualization
library(tidyverse) # https://www.tidyverse.org/packages/ 
##                   https://en.wikipedia.org/wiki/Tidyverse
library(english) # for manipulating numbers to words, see later
(.packages()) # view loaded packages

# Data loading ------------------------------------------------------------

# Load the built-in 'mtcars' dataset
data("mtcars")
?mtcars  # Get help/documentation for the 'mtcars' dataset

# Data exploration --------------------------------------------------------

# Display the first few rows of the dataset
head(mtcars)
# Open the dataset in a spreadsheet-like viewer
View(mtcars)

# Row names as a column, create 'dat' object-------------------------------

# Get the row names of the 'mtcars' dataset
rownames(mtcars)
# Combine row names as a column and create a new dataframe 'my_dat'
my_dat = cbind(car_type = rownames(mtcars), mtcars)
# Remove row names from the new dataframe
rownames(my_dat) = NULL
# View the modified dataframe
View(my_dat)
# Note: Can also use tibble::rownames_to_column() for this purpose

# Add car make to the dataframe -------------------------------------------

# Extract the first word from the 'car_type' column to represent the car make
word(my_dat$car_type, 1)
# Add a new column 'make' to the dataframe with the car make
my_dat = my_dat %>%
  mutate(make = word(my_dat$car_type, 1))
# Display the structure of the dataframe
str(my_dat)
# Create a frequency table of the 'make' column
table(my_dat$make)

# Sort --------------------------------------------------------------------

# Sort the dataframe by 'make' in ascending order
arrange(my_dat, make)
# Sort the dataframe by 'make' in descending order
arrange(my_dat, desc(make))

# Sort by 'cyl', then by 'mpg' --------------------------------------------

# Sort the dataframe first by 'cyl' and then by 'mpg'
arrange(my_dat, cyl, mpg)

# Filter, keep only rows with mpg < 20 ------------------------------------

# Filter the dataframe to keep only rows where 'mpg' is less than 20
filtered_dat = my_dat %>%
  filter(mpg < 20)

# Select columns ----------------------------------------------------------

# Select all columns except 'disp'
filtered_dat = filtered_dat %>%
  select(-disp) # Be careful with deleting column number instead of name

# Add logical vector ------------------------------------------------------

# Add a new column 'weight_over_4k' indicating if weight is over 4000 lbs
my_dat = my_dat %>%
  mutate(weight_over_4k = wt > 4)

# Convert 'vs' to string vector -------------------------------------------

# Convert the 'vs' column to a string vector with labels
my_dat = my_dat %>%
  mutate(my_vs = if_else(vs == 0, 'V-shaped', 'straight'))
# Display the structure of the dataframe
str(my_dat)

# Convert 'am' to factor --------------------------------------------------

# Check the class of the 'am' column
class(my_dat$am)
# Convert the 'am' column to a factor with labels 'automatic' and 'manual'
my_dat = my_dat %>%
  mutate(my_am = factor(am, labels = c('automatic', 'manual'))) # Transmission (0 = automatic, 1 = manual)
# Display the structure of the dataframe
str(my_dat)

# Add gear in words (character), carb in words (factor) -------------------

# Convert 'gear' to character and 'carb' to factor with English words
my_dat = my_dat %>%
  mutate(gear_eng = as.character(as.english(gear))) %>%
  mutate(carb_eng = as.factor(as.character(as.english(carb))))
# Display the structure of the dataframe
str(my_dat)
# Create frequency tables for 'gear' and 'gear_eng'
table(my_dat$gear)
table(my_dat$gear_eng)
# Display the first few rows of 'carb' and 'carb_eng'
head(my_dat$carb)
head(my_dat$carb_eng)

# Group by make -----------------------------------------------------------

# Group the dataframe by 'make' and add summary statistics
grouped_dat = my_dat %>%
  group_by(make) %>%
  mutate(mpg_mean = mean(mpg)) %>%
  mutate(gear_max = max(gear)) %>%
  mutate(wt_cumsum = cumsum(wt))  # cumulative sum

# View selected columns of the grouped dataframe
View(grouped_dat %>%
       select(make, mpg, mpg_mean, gear, gear_max, wt, wt_cumsum))

# View grouped dataframe where the group size is greater than 1
View(
  grouped_dat %>%
    filter(n() > 1) %>%
    select(make, mpg, mpg_mean, gear, gear_max, wt, wt_cumsum)
)

# Plots -------------------------------------------------------------------

# Simple histogram
hist(my_dat$wt)
# Histogram with increased granularity
hist(my_dat$wt, breaks = 16)

## ggplot2!

# Weight histogram with density plot
p1 = my_dat %>%
  ggplot(aes(x = wt)) +
  geom_histogram(binwidth = .125, color = "black", fill = "white")
p1

# Add density plot to the histogram
p2 = p1 + geom_density(alpha = 0.2, fill = "#FF6666")
p2

# Add title and axis labels to the plot
p3 = p2 + labs(title = "Histogram with Density Plot of weight", x = "Weight", y = "")
p3

# Apply dark theme to the plot
p4 = p3 + theme_dark()
p4

# Scatterplot by make, car type is annotated, number of cylinders is size of point, color is transmission type
p5 = my_dat %>%
  ggplot(aes(
    x = make,
    y = mpg,
    size = cyl,
    label = car_type,
    color = my_am)) +
  geom_point(alpha = 0.5) +
  geom_text(size = 3.5, show_guide  = FALSE, vjust = 1.2) +
  labs(
    title = "Scatterplot by make, car type is annotated, number of cylinders is size of point",
    x = "Make",
    y = "Miles per Gallon (mpg)",
    size = "Cylinders",
    color = 'Transmission type',
    label = "Car Type"
  ) +
  theme_minimal()
p5

# Scatterplot of qsec against weight, faceted by 'am', color by 'vs'
p6 = my_dat %>%
  ggplot(aes(
    x = wt,
    y = qsec,
    size = hp,
    color = my_vs
  )) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Scatterplot of qsec by weight, hp, engine type",
    x = "Weight",
    y = "1/4 mile time",
    size = "HP",
    color = 'Engine',
    label = "Car Type"
  ) +
  theme_minimal()
p6

# Add facet wrap by 'am' and apply dark theme
p6 + facet_wrap(~ my_am) + theme_dark()

# Long format, create dat_long object -------------------------------------

## Tidy data!
## https://en.wikipedia.org/wiki/Glossary_of_probability_and_statistics#tidy_data
## Standard for structuring data such that "each variable is a column, 
## each observation is a row, and each type of observational unit is a table"

# Remove columns containing specific strings
filtered_dat = my_dat %>%
  select(-contains(c('my', 'eng', 'weight')))
# Display the structure of the dataframe
str(filtered_dat)

# Pivot the dataframe to long format
dat_long = pivot_longer(
  filtered_dat,
  cols = -c(car_type, make),
  names_to = 'characteristic',
  values_to = 'value'
)
# View the long format dataframe
View(dat_long)

# Find distinct number of carburetors by make -----------------------------

# Filter the long format dataframe for 'carb' characteristic and get distinct values
dat_long %>%
  filter(characteristic == 'carb') %>%
  select(make, characteristic, value) %>%
  distinct() %>%
  arrange(make, value) %>%
  View()

# End of file, thank you! -------------------------------------------------