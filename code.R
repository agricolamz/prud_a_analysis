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
