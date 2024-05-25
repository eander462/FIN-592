here::i_am("HW5/code/HW5.R")

# Load packages
pacman::p_load(tidyverse, ggplot2, magrittr)


########### Load data ####################

portfolios_df = read_csv(here::here("HW5", "data", "25_Portfolios_5x5.CSV"),
                         skip = 15) |> # This is just trial and error to get rid of extra lines at the top
  mutate(date = ym(`...1`)) |> 
  # Rearrange and drop badly formatted date column
  select(date, everything(), -1) |> 
  # The last row is just NAs so remove it
  filter(row_number() < 8723) |> 
  # We only need after January 1963 and before December 2023
  filter(year(date) >= 1963 & year(date) <= 2023) |> 
  # This data set has annualized returns and several other equal rated returns and several other sepecifications. This just takes the first of those which is value weighted returns
  filter(row_number() <= 732) |> 
  # Clean the names
  janitor::clean_names()

ff_df = read_csv(here::here("HW5", "data", "F-F_Research_Data_Factors.CSV"),
                 skip = 3) |>
  # Same process
  mutate(date = ym(...1)) |>
  select(date, everything(), -1) |>
  # There are annual factors at the bottom we don't need
  filter(row_number() <= 1173) |>
  # Select same interval
  filter(year(date) >= 1963 & year(date) <= 2023) |> 
  # Clean names
  janitor::clean_names()

# Merge data sets
returns_df = left_join(ff_df, portfolios_df)

# Create excess returns for each portfolio
returns_df %<>%
  # 6:length(returns_df) are the columns of interest
  # All the returns are characters for some reason so make them numeric
  mutate(across(6:length(returns_df), ~ as.numeric(.x)),
         # Subtract the risk free rate from each of the return columns
         across(6:length(returns_df), ~ .x - rf))





########## Question 1 ####################

# Extract the relevant names of the columns to loop over
name = names(returns_df)
# We only want the names of the different sortings, so select those
name = name[6:length(name)]


# Run the regression for each of the relevant columns
coefficients = map(name, function(x){
  
  # Run regression for each column
  estimates = lm(unlist(returns_df[,x]) ~ returns_df$mkt_rf + returns_df$smb + returns_df$hml) |> 
            broom::tidy() |> 
            select(estimate)
  
  
  return(estimates)
})

# Get a vector for each different coefficient
# Constant
a = map_dbl(1:length(coefficients), function(i){
  coefficients[[i]] |> nth(1) |> unlist()
}) |> 
  # Organize
  matrix(nrow = 5, byrow = T)

# Set the names
rownames(a) = c("Small","2","3","4","Big")
colnames(a) = c("Low", "2", "3", "4", "High")


# Excess market risk
b = map_dbl(1:length(coefficients), function(i){
  coefficients[[i]] |> nth(2) |> unlist()
}) |> 
  # Organize
  matrix(nrow = 5, byrow = T)

# Set the names
rownames(b) = c("Small","2","3","4","Big")
colnames(b) = c("Low", "2", "3", "4", "High")


# Small minus big
s = map_dbl(1:length(coefficients), function(i){
  coefficients[[i]] |> nth(3) |> unlist()
}) |> 
  # Organize
  matrix(nrow = 5, byrow = T)

# Set the names
rownames(s) = c("Small","2","3","4","Big")
colnames(s) = c("Low", "2", "3", "4", "High")


# Value minus growth
h = map_dbl(1:length(coefficients), function(i){
  coefficients[[i]] |> nth(4) |> unlist()
}) |> 
  # Organize
  matrix(nrow = 5, byrow = T)

# Set the names
rownames(h) = c("Small","2","3","4","Big")
colnames(h) = c("Low", "2", "3", "4", "High")

