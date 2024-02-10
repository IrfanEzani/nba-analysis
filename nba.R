# Install packages
install.packages("dendextend")
install.packages("cluster")

# Import packages
library(readr)
library(dendextend)
library(tibble)
library(dplyr)
library(purrr)
library(cluster)
library(ggplot2)

# The following lines before the players assignment are not necessary for the solution, but can improve the dendrogram visualization

# Optional: Change the width of the plots by default to better visualize dendrogram
options(repr.plot.width = 15)

# Optional: Adjust plot margins: c(bottom, left, top, right)
par(mar = c(10, 0, 0, 0))  # Increase the bottom margin

# Load the data and create a new column and rownames
players <- read_csv(paste0("nba_players_2023.csv")) %>% 
  mutate(pra_per_game = points_per_game + assists_per_game +
           rebounds_per_game) %>% 
  column_to_rownames("name")

# Normalize or scale your data
players_scaled <- scale(players)

# Create function to calculate total within-cluster sum of squares
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = players_scaled, centers = k)
  model$tot.withinss
})

# Design a data frame to hold the total within-cluster sum of squares
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Make a plot of the total within-cluster sum of squares
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

# Look for the 'elbow' in the plot to decide on the optimal number of clusters
# In this case, it looks like 2 clusters is a good choice
num_clusters <- 2

# Compute distances and perform hierarchical clustering
dist_players <- dist(players_scaled, method = 'euclidean')
hc_players <- hclust(dist_players, method = 'average')

# Create and visualize dendrogram with colored branches
dend_colored <- color_branches(as.dendrogram(hc_players), 
                               k = num_clusters)
plot(dend_colored)

# Assign clusters to players and prepare data for analysis
# Decided by looking at the dendrogram
cut_players <- cutree(hc_players, h = 3.5)
clust_players <- mutate(players, cluster = cut_players)

# By age
clust_players %>% 
  group_by(factor(cluster)) %>% 
  summarize(mean = mean(age), 
            sd = sd(age), 
            median = median(age), 
            min = min(age), 
            max = max(age))

# By minutes played per game
clust_players %>% 
  group_by(factor(cluster)) %>% 
  summarize(mean = mean(minutes_played_per_game), 
            sd = sd(minutes_played_per_game), 
            median = median(minutes_played_per_game), 
            min = min(minutes_played_per_game), 
            max = max(minutes_played_per_game))

# By rebounds per game
clust_players %>% 
  group_by(factor(cluster)) %>% 
  summarize(mean = mean(rebounds_per_game), 
            sd = sd(rebounds_per_game), 
            median = median(rebounds_per_game), 
            min = min(rebounds_per_game), 
            max = max(rebounds_per_game))

# By assists per game
clust_players %>% 
  group_by(factor(cluster)) %>% 
  summarize(mean = mean(assists_per_game), 
            sd = sd(assists_per_game), 
            median = median(assists_per_game), 
            min = min(assists_per_game), 
            max = max(assists_per_game))

# By points per game
clust_players %>% 
  group_by(factor(cluster)) %>% 
  summarize(mean = mean(points_per_game), 
            sd = sd(points_per_game), 
            median = median(points_per_game), 
            min = min(points_per_game), 
            max = max(points_per_game))

# By points, rebounds, and assists per game
clust_players %>% 
  group_by(factor(cluster)) %>% 
  summarize(mean = mean(pra_per_game), 
            sd = sd(pra_per_game), 
            median = median(pra_per_game), 
            min = min(pra_per_game), 
            max = max(pra_per_game))

# Identify most influential statistics for clustering
# Those that do not an overlap between cluster 1 max and cluster 2 min (in no particular order)
strongest_influence <- c(
  "minutes_played_per_game", 
  "points_per_game", 
  "pra_per_game"
)