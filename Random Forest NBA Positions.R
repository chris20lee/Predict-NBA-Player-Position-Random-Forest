# Install packages if not already installed
if(!require(janitor)) install.packages("janitor")
if(!require(randomForest)) install.packages("randomForest")
if(!require(rvest)) install.packages("rvest")
if(!require(tidyverse)) install.packages("tidyverse")

# Variables to declare
setwd("C:/Users/Chris/Desktop")
data_dir = paste("C:/Users/Chris/Desktop")
lag_sec = 5
start_year = 1982
end_year = 2019

#FUNCTIONS

# Scrape NBA player stats
scrape_player_stats = function(season){
  # Total stats
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  stats_tot = url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_tot = stats_tot %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player == "Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    select(-rk)
  
  Sys.sleep(lag_sec) # Lag to avoid being blocked for web scraping
  
  # Per game stats
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_game.html")
  stats_pg = url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_pg = stats_pg %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player == "Player") %>%
    mutate_at(vars(-c(player,pos,tm)),as.numeric) %>% 
    mutate_at(vars(-c(player,pos,tm)), funs(replace(., is.na(.), 0))) %>% 
    rename_at(vars(8:30),funs(paste0(.,"_pg"))) %>% 
    select(-rk, -fg_percent_pg, -x3p_percent_pg, -x2p_percent_pg, -e_fg_percent_pg, -ft_percent_pg)
  
  Sys.sleep(lag_sec) # Lag to avoid being blocked for web scraping
  
  # Per minute stats
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html")
  stats_pm = url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_pm = stats_pm %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player == "Player") %>%
    mutate_at(vars(-c(player,pos,tm)),as.numeric) %>% 
    mutate_at(vars(-c(player,pos,tm)), funs(replace(., is.na(.), 0))) %>%
    rename_at(vars(9:29),funs(paste0(.,"_pm"))) %>% 
    select(-rk, -fg_percent_pm, -x3p_percent_pm, -x2p_percent_pm, -ft_percent_pm)
  
  # Per possesion stats
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_poss.html")
  stats_pp = url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_pp = stats_pp %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player == "Player") %>%
    mutate_at(vars(-c(player,pos,tm)),as.numeric) %>% 
    mutate_at(vars(-c(player,pos,tm)), funs(replace(., is.na(.), 0))) %>%
    rename_at(vars(9:31),funs(paste0(.,"_pp"))) %>% 
    select(-rk, -fg_percent_pp, -x3p_percent_pp, -x2p_percent_pp, -ft_percent_pp)
  
  Sys.sleep(lag_sec) # Lag to avoid being blocked for web scraping
  
  # Advanced stats
  url = paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html")
  stats_adv = url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_adv = stats_adv %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player == "Player") %>%
    mutate_at(vars(-c(player,pos,tm)),as.numeric) %>% 
    mutate_at(vars(-c(player,pos,tm)), funs(replace(., is.na(.), 0))) %>%
    select(-rk)
  
  Sys.sleep(lag_sec) # Lag to avoid being blocked for web scraping
  
  player_stats = full_join(player_stats_tot,player_stats_pg, by = c("player", "pos", "age", "tm", "g", "gs")) %>% 
    full_join(player_stats_pm, by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>%
    full_join(player_stats_pp, by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>%
    full_join(player_stats_adv, by = c("player", "pos", "age", "tm", "g", "mp"))
  
  return(player_stats)
}

# CALL TO SCRAPE THE DATA

# Scrape player stats data
datalist = list()
for (i in start_year:end_year) {
  x = scrape_player_stats(i)
  x$season = i
  datalist[[i]] = x
  print(i)
}

player_data = do.call(rbind, datalist)
sapply(player_data, class)
save(player_data, file = paste(data_dir, "player_data.Rdata", sep = "/"))

# Load in dataset
load(paste(data_dir, "player_data.Rdata", sep = "/"))

# Select players with a single position in a year
pos_player_data = subset(player_data, pos == "PG" | pos == "SG" | pos == "SF" | pos == "PF" | pos == "C", select = player:season)
# Set position column as a factor for the Random Forest
pos_player_data$pos = as.factor(pos_player_data$pos)
# Drop columns
pos_player_data = subset(pos_player_data, select = -c(player, tm))

# VISUALIZE THE DISTRIBUTION OF STATISTICS BY POSITION

for (i in 2:ncol(pos_player_data)) { # 102 columns to run through
  # Mean of column, split by position
  col_mean = aggregate(pos_player_data[,i], list(pos_player_data$pos), mean)
  # Standard deviation of column, split by position
  col_sd = aggregate(pos_player_data[,i], list(pos_player_data$pos), sd)
  # Merge 2 columns together
  col_sum_stats =merge(col_mean, col_sd, by = "Group.1", all = TRUE)
  # Create column names
  colnames(col_sum_stats) = c(colnames(pos_player_data[1]),paste0(colnames(pos_player_data[i]),"_mean"),
                              paste0(colnames(pos_player_data[i]),"_sd"))
  # Display statistics by position
  print(col_sum_stats)
  
  # Create statistics by position
  vis = data.frame(pos_player_data[,1],pos_player_data[,i])
  # Plot the statistics by position
  boxplot(split(vis[,2],vis[,1]), xlab = "position", ylab = colnames(pos_player_data[i]), 
          main = paste0("Boxplot of position and ", colnames(pos_player_data[i]))) 
}

# RUN RANDOM FOREST TO FIND MOST IMPORTANT VARIABLES

# Run default Random Forest model on all variables
model = randomForest(pos ~ ., data = pos_player_data, replace = TRUE, importance = TRUE)
# View model results
model
# Plot model results
plot(model)
# Plot of important variables
varImpPlot(model)
# Place important variables into a dataframe
imp_vars = data.frame(importance(model))
# Subset top half of important variables
imp_vars = imp_vars[imp_vars$MeanDecreaseGini %in% tail(sort(imp_vars$MeanDecreaseGini),floor((ncol(pos_player_data) - 1)/2)),]
# Subset original player statistics table based on these important variables
final_data = cbind(pos_player_data$pos,pos_player_data[rownames(imp_vars)])
# Set first column name to pos
colnames(final_data)[1] = "pos"

# CREATE TRAINING AND TEST SETS
set.seed(123)
# Create training sample with replacement
train = sample(nrow(final_data), 0.7 * nrow(final_data), replace = FALSE)
# Create training set
train_set = final_data[train,]
# Create test set
test_set = final_data[-train,]

# TUNING PARAMETERS OF THE MODEL

# Create empty list
low_mtry = c()
# Use for loop to identify the right mtry for the model
for (i in 1:20) {
  model_1 = randomForest(pos ~ ., data = train_set, ntree = 1000, mtry = i, replace = TRUE, importance = TRUE)
  # Store OOB estimates of errors
  low_mtry[i] = model_1$err.rate[nrow(model_1$err.rate),1]
}
# Plot OOB estimates of errors
plot(1:20, low_mtry, pch = 20, xlab = "Variable Splits", ylab = "OOB Errors", main = "OOB Errors by # of Variable Splits")

# Find number of variables split associated with lowest OOB errors
best_mtry = which(low_mtry == min(low_mtry))

# Run Random Forest model on best number of variable splits
model_2 = randomForest(pos ~ ., data = final_data, ntree= 1000, mtry = best_mtry, replace = TRUE, importance = TRUE)
# Store OOB estimates of error
x = model_2$err.rate
# Plot model results
plot(model_2)
# Find number of trees associated with lowest OOB errors
best_ntree = min(which(x == min(x[,1])))

# RUN TUNED RANDOM FOREST MODEL
final_model = randomForest(pos ~ ., data = train_set, ntree = best_ntree, mtry = best_mtry, replace = TRUE, importance = TRUE)
# View model results
final_model
# Plot model results
plot(final_model)
# Predict on training set
pred_train = predict(final_model, train_set, type = "class")
# Check classification accuracy
table(Predicted = pred_train, Actual = train_set$pos) 
# Predict on test set
pred_test = predict(final_model, test_set, type = "class")
# Check classification accuracy
table(Predicted = pred_test, Actual = test_set$pos)
# Prediction accuracy
mean(pred_test == test_set$pos) 

graphics.off()
