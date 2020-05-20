library(tidyverse)

groups <- read_csv(here::here("project", "project-groups.csv"), 
                   col_names = FALSE) %>% 
  rename(group = X1) %>% 
  pivot_longer(cols = -group, 
               names_to = "room", 
               values_to = "email_address") %>% 
  filter(!is.na(email_address)) %>% 
  mutate(room = as.integer(factor(group))) 

# create zoom rooms
zoom_col_names <- c("Pre-assign Room Name",
                    "Email Address")

zoom_rooms <- groups %>%
  select(room, email_address) %>%
  mutate(room = paste0("room ", room)) %>%
  setNames(zoom_col_names)

write_csv(zoom_rooms, here::here("project", "zoom-groups.csv"))
