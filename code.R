setwd("/home/agricolamz/work/articles/2026_a_with_sv_and_vera/analysis/")
library(tidyverse)
df <- read_csv("data.csv")

df |> 
  select(speaker, t0:t5) |> 
  na.omit() |> 
  mutate(id = 1:n()) |> 
  pivot_longer(names_to = "type", values_to = "value", t0:t5) |> 
  mutate(type = factor(type, levels = c("t0", "t1", "t2", "t3", "t4", "t5"))) |> 
  ggplot(aes(type, value))+
  geom_point()

library(dtwclust)

n_clust <- 7

df |> 
  add_count(corpus) |> 
  filter(corpus != "ШМП без мужчин((",
         n >= 15) |> 
  select(corpus, speaker, t0:t5, f0:f5) |> 
  na.omit() |> 
  mutate(across(t0:t5, as.double),
         across(f0:f5, as.double),
         t1 = if_else(t1 == 0, 1, t1),
         merge0 = (t0+1)*f0,
         merge1 = t1*f1,
         merge2 = t2*f2,
         merge3 = t3*f3,
         merge4 = t4*f4,
         merge5 = t5*f5,
         id = 1:n(),
         label = str_c(corpus, "_", speaker, "_", id)) |> 
  select(corpus, speaker, label, merge0:merge5) ->
  for_analysis

set.seed(42)
for_analysis |> 
  distinct(corpus) |> 
  mutate(color = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 
                        23)) ->
  colors

for_analysis |> 
  left_join(colors) ->
  for_analysis

for_analysis |> 
  select(-corpus, -speaker, -color) |> 
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

distance_matrix |> 
  hclust() |> 
  ape::as.phylo() %>%
  plot(tip.color = for_analysis$color[match(.$tip.label, for_analysis$label)],
       direction = "downwards",
       cex = 0.5,
       font = 2)

distance_matrix |> 
  hclust() |> 
  ape::as.phylo() %>%
  plot(tip.color = for_analysis$color[match(.$tip.label, for_analysis$label)],
       direction = "downwards",
       type = "fan",
       cex = 0.3,
       font = 2)

distance_matrix |> 
  hclust() |> 
  as.dendrogram() |> 
  cut(h = 70000) ->
  r

length(r$lower)

library(dendextend)
par(mar=c(5.1,4.1,4.1,2.1)) 
r$upper |> 
  plot()

par(mar=c(3,1,1,12)) 
dend <- r$lower[[1]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[2]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[3]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[4]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[5]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[6]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[7]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

dend <- r$lower[[8]]
labels_colors(dend) <- for_analysis$color[order.dendrogram(dend)]
plot(dend, horiz = TRUE)

library(tidytext)
map(1:8, function(i){
  r$lower[[i]] |> 
    labels() |> 
    tibble(corpus = _,
           cluster = i) |> 
    mutate(corpus = str_extract(corpus, ".*?\\_"),
           corpus = str_remove(corpus, "\\_"))
}) |> 
  list_rbind() |> 
  count(corpus, cluster) |> 
  mutate(cluster = as.character(cluster),
         corpus = reorder_within(corpus, by = n, within = cluster)) |> 
  ggplot(aes(n, corpus, label = n))+
  geom_col()+
  geom_label()+
  facet_wrap(~cluster, scales = "free")+
  theme_minimal()+
  labs(y = NULL)+
  scale_y_reordered()



# averaged ----------------------------------------------------------------

library(dtwclust)

n_clust <- 7

df |> 
  add_count(corpus) |> 
  filter(corpus != "ШМП без мужчин((",
         n >= 15) |> 
  select(corpus, speaker, t0:t5, f0:f5) |> 
  na.omit() |> 
  mutate(across(t0:t5, as.double),
         across(f0:f5, as.double),
         t0 = if_else(t0 == 0, 1, t0),
         t1 = if_else(t1 == 0, 1, t1)) |> 
  group_by(corpus, speaker) |>
  mutate(t1 = scale(t1+1)[,1],
         t2 = scale(t2+1)[,1],
         t3 = scale(t3+1)[,1],
         t4 = scale(t4+1)[,1],
         t5 = scale(t5+1)[,1],
         f0 = scale(f0+1)[,1],
         f1 = scale(f1+1)[,1],
         f2 = scale(f2+1)[,1],
         f3 = scale(f3+1)[,1],
         f4 = scale(f4+1)[,1],
         f5 = scale(f5+1)[,1],
         merge0 = t0*f0,
         merge1 = t1*f1,
         merge2 = t2*f2,
         merge3 = t3*f3,
         merge4 = t4*f4,
         merge5 = t5*f5) |> 
  na.omit() |> 
  ungroup() |> 
  mutate(id = 1:n(),
          label = str_c(corpus, "_", speaker, "_", id)) ->
  
  
  select(label, merge0:merge5) |> 
  column_to_rownames("label") |> 
  tsclust(k = n_clust,
          distance = "L2", 
          centroid = "pam",
          seed = 3247, 
          trace = TRUE,
          control = partitional_control(nrep = 1L)) ->
  clustering

distance_matrix <- clustering@distmat

set.seed(42)
for_analysis |> 
  distinct(corpus) |> 
  mutate(color = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 
                        23)) ->
  colors

for_analysis |> 
  left_join(colors) ->
  for_analysis


distance_matrix |> 
  hclust() |> 
  ape::as.phylo() %>%
  plot(tip.color = for_analysis$color[match(.$tip.label, for_analysis$label)],
       direction = "rightwards",
       font = 2)

distance_matrix |> 
  hclust() |> 
  as.dendrogram() |> 
  cut(h = 5) ->
  r

length(r)
