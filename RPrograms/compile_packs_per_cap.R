library(vroom)
library(tidyverse)
library(janitor)

data_files <-dir("Data/smoking")
all_data <- list()
for (i in 1:length(data_files)){
  IN <- 
    read_delim(file.path("Data","smoking",data_files[i]), delim = " ", na = "—") |> 
    pivot_longer(-State, names_to = "Year", values_to = "packs_percap") |> 
    clean_names()
  all_data[[i]] <- IN
}

smoking <- bind_rows(all_data) |> 
  mutate(year = as.integer(year))


smoking |> 
  ggplot(aes(x = year, y = packs_percap, group = state)) +
  geom_line(color = gray(.1), alpha = .8) +
  theme_minimal()

smoking |> 
  arrange(state, year) |> 
  group_by(state) |> 
  mutate(dpacks = lead(packs_percap) - packs_percap) |> 
  ggplot(aes(x = year, y = dpacks, group = state)) +
  geom_line(color = gray(.1), alpha = .8) +
  theme_minimal()

write_csv(smoking, file = "Data/smoking/packs_percap.csv")
