######### Setup ############

# Here setup
here::i_am("HW1/code/HW1.Rmd")

# Load packages
pacman::p_load(haven, tidyverse, magrittr, ggplot2, gridExtra, stargazer, kableExtra)

# Load data
brets_df = read_dta(here::here("HW1", "data", "brets.dta"))
trets_df = read_dta(here::here("HW1", "data", "trets.dta"))

##############################

#### Functions ################

# Source functions
source(here::here("HW1/code/functions.R"))

################################

######  Question 1 #############

# Calculate mean returns
trets_mean = mean_returns(trets_df)

# Calculate the relevant statistics for each event window
means = sapply(c(0,1,2,5), function(x){
  test_stat(trets_mean, 0-x, 0+x)
})

colnames(means) = c(0,1,2,5)

means_plot = plot_CAR(trets_mean) + ggtitle("Mean Returns Model")


# Add capm predictions to data set
trets_capm = CAPM_returns(trets_df)


# CAPM results for 0, 1, 2, and 5 day windows
capm = sapply(c(0,1,2,5), function(x){
  test_stat(trets_capm, 0-x, 0+x)
})

colnames(capm) = c(0,1,2,5)

# Plot
capm_plot = plot_CAR(trets_capm) + ggtitle("CAPM Model")



one_table = kbl(
  cbind(means, capm),
  format = "latex",
  longtable = F,
  booktabs = T,
  caption = "Target Firms 30 Day Event Window",
  digits = 2
) |> 
  add_header_above(c(" ", "Mean Returns" = 4, "CAPM" = 4)) |>  
  kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"))




######################################

##### Question 2 ####################

# Estimate the capm for the bidding firms
brets_capm = CAPM_returns(brets_df)

# CAPM results for 0, 1, 2, and 5 day windows
capm_B = sapply(c(0,1,2,5), function(x){
  test_stat(brets_capm, 0-x, 0+x)
})

# Plot results
capm_B_plot = plot_CAR(brets_capm) + ggtitle("CAPM Model for Bidding Firms")


two_table = kbl(
  capm_B,
  format =  'latex',
  longtable = F,
  booktabs = T,
  caption = "Bidding Firms 30 Day Event Window. CAPM Model",
  digits = 2
) |> 
  add_header_above(c(" ", "CAPM" = 4)) |>  
  kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"))

######################################

##### Question 3 #####################


# Calculate abnormal returns for three CAR series: regular, and plus or minus one se
# I have to make three different data frames because of how my functions work and I don't want to change them at this point

long_capm_point_df = CAPM_returns(brets_df, error = 0)
long_capm_plus_df = CAPM_returns(brets_df, error = 1)
long_capm_minus_df = CAPM_returns(brets_df, error = -1)



# Calculate average of beta standard errors
beta_se = long_capm_point_df$se |> unique() |> mean()


# CAPM results for 0, 1, 2, and 5 day windows for point estimate
capm_point = sapply(c(0,1,2,5), function(x){
  test_stat(long_capm_point_df, 0-x, 0+x)
})
colnames(capm_point) = c(0,1,2,5)


capm_plus = sapply(c(0,1,2,5), function(x){
  test_stat(long_capm_plus_df, 0-x, 0+x)
})
colnames(capm_plus) = c(0,1,2,5)


capm_minus = sapply(c(0,1,2,5), function(x){
  test_stat(long_capm_minus_df, 0-x, 0+x)
})
colnames(capm_minus) = c(0,1,2,5)



three_table = kbl(
  cbind(capm_minus, capm_point, capm_plus),
  format = 'latex',
  longtable = F,
  booktabs = T,
  caption = "Target Firms 30 Day Event Window",
  digits = 2
) |> 
  add_header_above(c(" ", "Minus" = 4, "Point" = 4, "Plus" = 4)) |>  
  kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"))


# Calculate cumulative returns
ret_point = cumulative_returns(long_capm_point_df, -30, 30)
ret_plus = cumulative_returns(long_capm_plus_df,-30, 30)
ret_minus = cumulative_returns(long_capm_minus_df,-30, 30)

# Make into a dataframe to plot
cum = tibble(
  abnorm_point = ret_point,
  abnorm_plus = ret_plus,
  abnorm_minus = ret_minus,
  date = -30:30
)

long_plot1 =   cum |>
  pivot_longer(cols = starts_with("abnorm")) |>
  drop_na() |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_point() +
  geom_line() +
  #geom_vline(xintercept = 0,
  #linewidth = 1.5,
  #alpha = 0.5) +
  scale_x_continuous(breaks = seq(-30, 30, by = 10)) +
  scale_color_discrete(
    limits = c("abnorm_plus", "abnorm_point", "abnorm_minus"),
    labels = c("+1 SE", "Point", "-1 SE")
  ) +
  cowplot::theme_cowplot() +
  labs(x = "Month Relative to Split", y = "Cumulative Abnormal Return", color = NULL)


# Calculate cumulative returns for the relevant period
ret_point = cumulative_returns(long_capm_point_df,-2, 250)
ret_plus = cumulative_returns(long_capm_plus_df,-2, 250)
ret_minus = cumulative_returns(long_capm_minus_df,-2, 250)

# Make into one data frame
cum = tibble(
  abnorm_point = ret_point,
  abnorm_plus = ret_plus,
  abnorm_minus = ret_minus,
  date = -2:250
)

long_plot2 =   cum |>
  pivot_longer(cols = starts_with("abnorm")) |>
  drop_na() |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_point() +
  geom_line() +
  #geom_vline(xintercept = 0,
  #linewidth = 1.5,
  #alpha = 0.5) +
  scale_x_continuous(breaks = seq(-2, 250, by = 50)) +
  scale_color_discrete(
    limits = c("abnorm_plus", "abnorm_point", "abnorm_minus"),
    labels = c("-1 SE", "Point", "+1 SE")
  ) +
  cowplot::theme_cowplot() +
  labs(x = "Month Relative to Split", y = "Cumulative Abnormal Return", color = NULL)


