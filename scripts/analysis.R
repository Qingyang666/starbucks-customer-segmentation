library(tidyverse)
library(ggplot2)
library(NbClust)
library(factoextra)
library(cluster)
# import final data set
starbucks_customer <- read_csv('data/profile_offer_transaction.csv')[,-1]
starbucks_customer <- starbucks_customer %>%
  mutate(gender_male = as.factor(gender_male),
         gender_female = as.factor(gender_female),
         gender_other = as.factor(gender_other))
# examining data 
glimpse(starbucks_customer)
summary(starbucks_customer)
# split dataset into customer_id_df and predictors_df
## remove categorical columns since they cannot be scaled.
customer_id_df <- starbucks_customer %>% 
  select(customer_id)
predictors_df <- starbucks_customer %>% 
  select(-customer_id, -gender_male, -gender_female, -gender_other)

#############################PRINCIPAL COMPONENT ANALYSIS#######################

# check standard deviations of each predictor variables
round(apply(predictors_df, 2, sd), 4)
# Predictor variables have very different standard deviations.
# Therefore, we need to standardize each variable before performing principal component analysis.
predictors_scale_df <- predictors_df %>%
  mutate_all(scale)
# reexamine standard deviations of each predictor variables after standardization
round(apply(predictors_scale_df, MARGIN = 2, FUN = sd), 4)

# perform principal component analysis
predictors_scale_pca <- prcomp(predictors_scale_df)
# extract the importance of each component
summary(predictors_scale_pca)

# create a tibble that includes principal component id, variance explained, and cumulative proportion
importance_of_components <- tibble(
  principal_component = c(1:24),
  variance_explained = round(predictors_scale_pca$sdev^2 / sum(predictors_scale_pca$sdev^2),4),
  cumulative_proportion = cumsum(round(predictors_scale_pca$sdev^2 / sum(predictors_scale_pca$sdev^2),4)))

# scree plot1: proportion of variance explained
theme_set(theme_minimal())
ggplot(data = importance_of_components, 
       aes(x = principal_component, 
           y = variance_explained, 
           group = 1)) +
  geom_point(size = 0.8, color = 'steelblue')+
  geom_line(color = 'steelblue')+
  scale_x_continuous(name = 'principal component id', breaks = c(1:24), limits = c(1,24))+
  scale_y_continuous(name = 'proportion of variance explained', labels = scales::percent)+
  labs(title = 'Scree Plot',
       subtitle = 'Variance Explained By Each Principal Component')+
  theme(text = element_text(family = "serif", size = 10))

# scree plot2: cumulative proportion of variance explained
ggplot(data = importance_of_components, 
       aes(x = principal_component, 
           y = cumulative_proportion, 
           group = 1)) +
  geom_point(size = 0.8, color = 'steelblue')+
  geom_line(color = 'steelblue')+
  scale_x_continuous(name = 'principal component id', breaks = c(1:24), limits = c(1,24))+
  scale_y_continuous(name = 'cumulative proportion of variance explained', labels = scales::percent)+
  labs(title = 'Scree Plot',
       subtitle = 'Cumulative Proportion of Variance Explained')+
  theme(text = element_text(family = "serif", size = 10))

# I want to choose 11 principal components, which explains 95% of the variance.
model_df <- as_tibble(predictors_scale_pca$x[,1:14])

##############################CLUSTER ANALYSIS (PART1)##########################

# distance matrix
dist.mat <- dist(x = model_df, method = 'euclidean')

# single linkage
clust.nn <- hclust(dist.mat, method = 'single')
# visualization
plot(clust.nn, hang = -1, main = "Single Linkage", labels = FALSE, xlab = "", sub = "")

# complete linkage
clust.fa <- hclust(dist.mat, method = 'complete')
# visualization
plot(clust.fa, hang = -1, main = "Complete Linkage", labels = FALSE, xlab = "", sub = "")

# average linkage
clust.avg <- hclust(dist.mat, method = 'average')
# visualization
plot(clust.avg, hang = -1, main = "Average Linkage", labels = FALSE, xlab = "", sub = "")

# centroid linkage
clust.centroid <- hclust(dist.mat, method = 'centroid')
# visualization
plot(clust.centroid, hang = -1, main = "Centroid Linkage", labels = FALSE, xlab = "", sub = "")

# ward linkage
clust.ward <- hclust(dist.mat, method = 'ward.D2')
# visualization
plot(clust.ward, hang = -1, main = "Ward Linkage", labels = FALSE, xlab = "", sub = "")

# Ward method derive the best separation between each cluster. 
# Then we need to determine the optimal number of clusters using.
ward.silhouette <- NbClust(data = model_df, 
                    distance = "euclidean",
                    min.nc = 2, 
                    max.nc = 10, 
                    method = 'ward.D2',
                    index = "silhouette")
# visualization 
ggplot(data = data.frame(num_clusters = as.numeric(names(ward.silhouette$All.index)),
                         avg_silhouette = ward.silhouette$All.index),
       aes(x = num_clusters,
           y = avg_silhouette,
           group = 1)) +
  geom_point(size = 0.8, color = 'steelblue')+
  geom_line(color = 'steelblue') +
  geom_hline(yintercept = 0.0774,linetype="dashed", color = "grey") +
  geom_vline(xintercept = 3, linetype="dashed", color = "grey") +
  scale_x_continuous(name = 'number of clusters', breaks = c(2:10))+
  scale_y_continuous(name = 'average silhouette width')+
  labs(title = 'Optimal Number of Clusters',
       subtitle = 'silhouette method')+
  theme(text = element_text(family = "serif", size = 10))

# I would like to choosing local maximum value as my optimal number of clusters, which is 4 clusters.
ward_cluster_3 <- cutree(clust.ward, k = 3)

# visualize clusters in first two principal component space
theme_set(theme_minimal())
ward_pc <- model_df %>% mutate(cluster = as.factor(ward_cluster_3))
ggplot(data = ward_pc,
       aes(x = PC1,
           y = PC2, 
           color = cluster,
           shape = cluster))+
  geom_point(size = 0.7, alpha = 0.6)+ 
  geom_hline(yintercept=0,color="grey", linetype="dashed")+
  geom_vline(xintercept=0,color="grey", linetype="dashed")+
  labs(title="Ward (D2) Linkage In Pricnipal Components Space",
       x="PC1", y = "PC2")+
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

##############################CLUSTER ANALYSIS (PART2)##########################

# using elbow method
set.seed(2021)
tot_wss <- c()
k.levels <- 1:10
for(k in k.levels){
  clust.km <- kmeans(x = model_df, centers = k, nstart = 100, iter.max = 100)
  tot_wss[k]<-clust.km$tot.withinss
}
# visualization
as_tibble(tot_wss) %>%
  mutate(num_clusters = as.factor(k.levels)) %>%
  ggplot(aes(x = num_clusters, 
             y = tot_wss, 
             group = 1)) +
  geom_line(color = "steelblue")+
  geom_point(color = "steelblue")+
  labs(title = "Optimal Number of Clusters",
       subtitle = "Elbow Method",
       x = "Number of clusters",
       y = "Total Within Sum of Squares")

# not very clear, maybe 5 clusters 

# using silhouetee score
km.silhouette <- NbClust(data = model_df, 
                           distance = "euclidean",
                           min.nc = 2, 
                           max.nc = 10, 
                           method = 'kmeans',
                           index = "silhouette")

# visualization
ggplot(data = data.frame(num_clusters = as.numeric(names(km.silhouette$All.index)),
                         avg_silhouette = km.silhouette$All.index),
       aes(x = num_clusters,
           y = avg_silhouette,
           group = 1)) +
  geom_point(size = 0.8, color = 'steelblue')+
  geom_line(color = 'steelblue') +
  geom_hline(yintercept = 0.1150,linetype="dashed", color = "grey") +
  geom_vline(xintercept = 4, linetype="dashed", color = "grey") +
  scale_x_continuous(name = 'number of clusters', breaks = c(2:10))+
  scale_y_continuous(name = 'average silhouette width')+
  labs(title = 'Optimal Number of Clusters',
       subtitle = 'silhouette method')+
  theme(text = element_text(family = "serif", size = 10))

set.seed(2021)
cluster_km_3 <- kmeans(x = model_df, centers = 3, nstart = 100, iter.max = 100)
plot(cluster::silhouette(cluster_km_3$cluster, dist.mat), col=1:3, border=NA)

kmeans_pc <- model_df %>% 
  mutate(cluster = as.factor(cluster_km_3$cluster))
ggplot(data = kmeans_pc,
       aes(x = PC1,
           y = PC2, 
           color = cluster,
           shape = cluster))+
  geom_point(size = 0.7, alpha = 0.6)+ 
  geom_hline(yintercept=0,color="grey", linetype="dashed")+
  geom_vline(xintercept=0,color="grey", linetype="dashed")+
  labs(title="Kmeans In Pricnipal Components Space",
       x="PC1", y = "PC2")+
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# add back clusters to our original data set
starbucks_customer_cluster <-  customer_id_df %>%
  mutate(cluster = as.factor(cluster_km_3$cluster)) %>%
  left_join(starbucks_customer, by = 'customer_id')

write.csv(starbucks_customer_cluster, 'data/starbucks_customer_cluster.csv')
