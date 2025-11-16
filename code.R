setwd("/home/agricolamz/work/articles/2026_a_with_sv_and_vera/analysis/")
library(tidyverse)
df <- read_csv("data.csv")

df |> 
  select(speaker, t1:t5) |> 
  na.omit() |> 
  mutate(id = 1:n()) |> 
  pivot_longer(names_to = "type", values_to = "value", t1:t5) |> 
  mutate(type = factor(type, levels = c("t1", "t2", "t3", "t4", "t5"))) |> 
  ggplot(aes(type, value))+
  geom_point()

library(dtwclust)

n_clust <- 7

data(uciCT)

series <- reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))
series <- series[1:20]
labels <- CharTrajLabels[1:20]


df |> 
  filter(corpus != "ШМП без мужчин((") |> 
  select(corpus, speaker, t1:t5) |> 
  na.omit() |> 
  mutate(id = 1:n(),
         label = str_c(corpus, "_", speaker, "_", id)) ->
  for_analysis

set.seed(42)
for_analysis |> 
  distinct(corpus) |> 
  mutate(color = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 
                        28)) ->
  colors

for_analysis |> 
  left_join(colors) ->
  for_analysis

for_analysis |> 
  select(-corpus, -speaker, -colors) |> 
  column_to_rownames("label") |> 
  tsclust(k = n_clust,
          distance = "L2", 
          centroid = "pam",
          seed = 3247, 
          trace = TRUE,
          control = partitional_control(nrep = 1L)) ->
  clustering

distance_matrix <- clustering@distmat

save(distance_matrix, file = "distance_matrix.Rdata")
load("distance_matrix.Rdata")

set.seed(42)

distance_matrix |> 
  hclust() |> 
  ape::as.phylo() %>%
  plot(tip.color = colors$color[match(.$tip.label, colors$language)],
       direction = "downwards",
       cex = 1.5,
       font = 2)
