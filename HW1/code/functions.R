# This file contains all the functions for this assignment

###### Setup ##################

here::i_am("HW1/code/functions.R")



########### Functions #########

# Function to Calculate SE for each firm
firm_var = function(df){
  # Define interator
  firms = unique(unlist((df["permno"])))
  
  lapply(firms, function(x){
    # Get the abnormal returns in the estimation period for each firm
    epsilon = df |> 
      # Estimation window is 3 months before 0 date
      filter(rd <= -61) |> 
      filter(permno == {x}) |> 
      select(abnorm) |> 
      drop_na() |> 
      as.matrix() # Make a matrix for matrix algebra in next step
    
    # Calculate the variance. We want to keep the variances rather than calculate SE because we need to sum these up for the aggregate variance
    sigma2 = t(epsilon) %*% epsilon / (nrow(epsilon) - 2) 
    return(sigma2)
  }) |> unlist()
}

# Now calculate the SE for each firm within the range of interest
cum_var = function(df, from, to, firm_var){
  # Define iterator
  firms = unique(unlist((df["permno"])))
  
  var = lapply(firms, function(x){
    # Calculate the length of the series for each firm
    N = df |> 
      filter(permno == {x}) |> 
      drop_na() |> 
      nrow()
    
    # Make diagonal variance matrix
    V = diag(firm_var[which(unique(df$permno) == x)], nrow = N)
    
    # Create vector of 1's and 0 to indicate the event days. Since the matrix is symmetric, we don't actually have to put the 1's in the middle where they event happens. Its equivalent to just put the same number of 1's at the beginning which I do for simplicity
    gamma = c(rep(1, length(from:to)), rep(0, N - length(from:to)))
    
    # Calculate firm variance within the window
    firm_cum_var = t(gamma) %*% V %*% gamma
    
    return(firm_cum_var)
  }) |> unlist()
  
  # Now calculate variance aggregating over firms
  agg_var = sum(var) / length(var)^2
  agg_se = sqrt(agg_var)
  
  return(agg_se)
}

# Calculate the cumulative abnormal returns 
cumulative_returns = function(df, from, to){
  # Note: event_window should be a two long vector with the first value the lower bound of the event window and the second value the upper bound
  # Define iterator
  firms = unique(unlist((df["permno"])))
  
  # Define event window for each firm and get cumulative returns over the window
  car_firms = sapply(firms, function(x){
    df |> 
      # Select firms
      filter(permno == {x}) |> 
      # Add a column of 1's and 0's where 1's indicate value is in the event window
      mutate(event = if_else(rd >= from & rd <= to, 1, 0)) |> 
      filter(event == 1) |> 
      drop_na() |> 
      select(abnorm) |> 
      cumsum()
  }) 
  
  # Remove vector names
  sapply(1:length(car_firms), function(i){
    names(car_firms[[i]]) <<- NULL
  })
  
  # Unlist
  car_firms = unlist(car_firms)
  
  
  # Aggregate over firms
  car = sapply(seq(1, length(from:to)), function(i){
    # Select returns for each date across all firms
    if(length(from:to) == 1)
      date = car_firms
    else
      date = car_firms[names(car_firms) == paste("abnorm", i, sep = "")]
    
    # Aggregate cumulative abnormal returns at each date in the event window
    sum(date) / length(date)
  })
  
  return(car)
}


# Calculate the test statistic and return t stat and p value
test_stat = function(df, from, to){
  # Calculate SE
  se = cum_var(df, from, to, firm_var = firm_var(df))
  
  # Calculate cumulative returns. We just need the final one for the test stat
  car = cumulative_returns(df, from, to)
  car = car[length(car)]
  
  # Calculate test stat which is car/se
  j = car / se
  
  # Calculate p value
  p = 2*(1 - pnorm(abs(j)))
  
  return(list(`Cumulative Abnormal Return` = car, `Standard Error` = se, `T-stat` = j, `P Value` = p))
}


# Plot the CAR
plot_CAR = function(df){
  
  se = cum_var(df, -30,30, firm_var(df))
  
  # Calculate cumulative returns
  ret = cumulative_returns(df, -30, 30)
  
  # Make into a dataframe to plot
  cum = tibble(abnorm = ret, date = -30:30, se = se) 
  
  cum |> 
    drop_na() |> 
    ggplot(aes(x = date, y = abnorm)) + 
    geom_point() + 
    geom_ribbon(aes(ymin = abnorm - 2*se, ymax = abnorm + 2*se), alpha = 0.3) +
    geom_vline(xintercept = 0, linewidth = 1.5, alpha = 0.5) +
    scale_x_continuous(breaks = seq(-30,30, by = 10)) +
    cowplot::theme_cowplot() + 
    labs(x = "Month Relative to Merger", y = "Cumulative Abnormal Return")
}


mean_returns = function(df, error = 0){
  # for convenience define a vector of all the firm ids
  firms = unique(unlist((df["permno"])))
  
  # Calculate mean returns
  returns = lapply(firms, function(x){
    df |> 
      filter(rd <= -61) |> 
      filter(permno == {x}) |> 
      reframe(permno = permno, mean_return = mean(ret, na.rm = T)) |> 
      unique()
  }) |> bind_rows()
  
  df = df |> 
    left_join(returns) |> 
    mutate(abnorm = ret - mean_return)
}


# Estimate CAPM returns model for target firms
CAPM_returns = function(df, error = 0){
  # Define iterator
  firms = unique(unlist((df["permno"])))
  
  returns = lapply(firms, function(x) {
    # Calculate beta for each firm
    df |>
      filter(rd <= -61) |> 
      #drop_na() |> 
      filter(permno == {x}) %>%
      reframe(permno = permno, constant = coef(lm(ret ~ vwretd, .))[1], beta = coef(lm(ret ~ vwretd, .))[2], se =  summary(lm(ret ~ vwretd + -1, .))$coefficients[2]) |>  # We're not using an intercept here because the variables already include the risk free rate
      unique() 
  }) |> bind_rows()
  
  # Merge capm coefficients back
  df = left_join(df, returns)
  
  # Get predictions and abnormal returns
  # For latter use this if else statement lets me calculate the abnormal returns for either the point estimate, or the point estimate plus or minus one standard error
  df = df |> 
    mutate(capm = ifelse(error == 0, vwretd * beta + constant,
                         ifelse(error == 1, constant + vwretd * (beta + se), constant + vwretd * (beta - se))),
           abnorm = ret - capm)
  
  return(df)
}
