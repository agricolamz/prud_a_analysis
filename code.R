setwd("/home/agricolamz/work/articles/2026_a_with_sv_and_vera/analysis/")
library(tidyverse)
df <- read_csv("data.csv")

df |> 
  add_count(corpus) |> 
  filter(corpus != "ШМП без мужчин((",
         n >= 15) |> 
  select(speaker, corpus, t0:t5, f0:f5) |> 
  na.omit() |> 
  mutate(across(t0:f5, as.double),
         utterance_id = 1:n()) |> 
  pivot_longer(names_to = "type", values_to = "value", t0:f5) |> 
  mutate(obs_id = str_extract(type, "\\d"),
         type = str_remove(type, "\\d")) |> 
  pivot_wider(names_from = type, values_from = value) |> 
  group_by(speaker, corpus) |> 
  mutate(t_scaled = scale(t)[,1],
         f_scaled = scale(f)[,1],
         t_f_scaled = t*f_scaled,
         label = str_c(corpus, "_", speaker, "_", utterance_id)) |> 
  ungroup() ->
  df_transformed

set.seed(42)
df_transformed |> 
  distinct(corpus) |> 
  mutate(color = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 
                        24)) ->
  colors

df_transformed |> 
  left_join(colors) ->
  df_transformed
  
df_transformed |> 
  ggplot(aes(t, f, group = utterance_id))+
  geom_line(data = df_transformed |> select(-corpus),
            color = "grey70",
            linewidth = 0.2)+
  geom_line(linewidth = 0.2)+
  facet_wrap(~corpus)+
  theme_minimal()

df_transformed |>
  ggplot(aes(t, f_scaled, group = utterance_id))+
  geom_line(data = df_transformed |> select(-corpus),
            color = "grey80",
            linewidth = 0.2)+
  geom_line(linewidth = 0.2)+
  facet_wrap(~corpus)+
  theme_minimal()

df_transformed |>
  ggplot(aes(obs_id, f_scaled, group = utterance_id))+
  geom_line(data = df_transformed |> select(-corpus),
            color = "grey80",
            linewidth = 0.2)+
  geom_line(linewidth = 0.2)+
  facet_wrap(~corpus)+
  theme_minimal()

lapply(unique(df_transformed$utterance_id), FUN = function(i){
    df_transformed |> 
      filter(utterance_id == i) |> 
      select(t, f) |> 
      as.matrix()
  }) ->
  for_multivariate_clustering

df_transformed |> 
  distinct(label) |> 
  pull(label) ->
  names(for_multivariate_clustering)

start.time <- Sys.time()
mvc <- tsclust(for_multivariate_clustering, 
               k = 7L, 
               distance = "gak", 
               seed = 4242)
end.time <- Sys.time()
end.time - start.time

save(mvc, file = "mvc.RData")
load("mvc.RData")

df_transformed |>
  left_join(tibble(cluster = mvc@cluster,
                   label = names(mvc@datalist))) |> 
  count(corpus, cluster) |> 
  mutate(clust_dist = str_c("c", cluster, ": ", n)) |> 
  group_by(corpus) |> 
  summarise(clust_dist = str_c(clust_dist, collapse = "; ")) ->
  clust_dist_df

df_transformed |>
  left_join(tibble(cluster = mvc@cluster,
                   label = names(mvc@datalist))) |> 
  count(corpus, cluster) |> 
  ggplot(aes(cluster, corpus, fill = n))+
  geom_tile()+
  geom_text(aes(label = n), color = "white")+
  theme_minimal()+
  labs(y = NULL)

df_transformed |>
  left_join(tibble(cluster = mvc@cluster,
                   label = names(mvc@datalist))) |> 
  left_join(clust_dist_df) |> 
  mutate(cluster = as.factor(cluster),
         corpus = str_c(corpus, "\n", clust_dist)) |> 
  ggplot(aes(t, f_scaled, group = utterance_id, color = cluster))+
  geom_line(data = df_transformed |> select(-corpus),
            color = "grey80",
            linewidth = 0.2)+
  geom_line(linewidth = 0.2)+
  facet_wrap(~corpus)+
  theme_minimal()

# average model -----------------------------------------------------------


