## Compile and finalise dashboard scores
library(googlesheets4)
library(tidyverse)
sheet_url <- "https://docs.google.com/spreadsheets/d/1C9RZRbD5sCzT6c4OhELdyTaBPCgtKQzX66CuFzUvUxg/edit#gid=547318697"

group_scores <- read_sheet(sheet_url, sheet = "project")


peval_csvs <- list.files(here::here("project", "peer-evals"), 
                         pattern = ".csv$", 
                         full.names = TRUE) 
names(peval_csvs) <- str_remove(basename(peval_csvs), pattern = "\\.csv")

peer_scores <- map_dfr(peval_csvs, read_csv, .id = "to_group")

lookup_q1 <- tibble(Q1 = c("A", "B", "C", "D"),
                    mark = c(1, .75, .5, 0.25))


peer_scores <- left_join(peer_scores, lookup_q1, by = "Q1")

group_pevals <- peer_scores %>% 
  group_by(to_group) %>% 
  summarise(mark = mean(mark, na.rm = TRUE))




group_scores_final <- group_scores %>% 
  left_join(group_pevals %>% rename(pe_mark_milestone_05 = mark),
            by = c("group_name" = "to_group")) %>% 
  mutate(
    instructor_mark_milestone_05 = presentation_mark_instructor / 10,
    mark_milestone_04 = mark_milestone_04 / 100 * 40,
    mark_milestone_05 = 20 * instructor_mark_milestone_05 + 10 * pe_mark_milestone_05,
    mark_total_project = round(mark_milestone_01 + mark_milestone_02 + mark_milestone_03 + mark_milestone_04 + mark_milestone_05, 2),
    mark_score_project = mark_total_project / 90  * 100 # final score out of 90 
  )

indvidual_marks <- read_csv(here::here("project/project-groups.csv"), 
                            col_names = FALSE) %>% 
  rename(group = X1) %>% 
  mutate(group = str_to_lower(str_trim(group)),
         group = case_when(
           group == "hungry_verse" ~ "parking-verse",
           group == "la vita" ~ "la-vita",
           group == "pompous assets" ~ "pompous-assets",
           TRUE ~ group
         ),
         group = str_replace_all(group, " ", "")
  ) %>% 
  pivot_longer(cols = -group, values_to = "email") %>% 
  select(-name) %>% 
  filter(!is.na(email))



group_scores_student <- group_scores_final %>% 
  select(group = group_name,
         mark_score_project,
         mark_total_project,
         presentation_feedback,
         dashboard_feedback = feedback_milestone_04,
         starts_with("mark_milestone")
  )

final_marks <- indvidual_marks %>% 
  left_join(group_scores_student) %>% 
  select(email, group, everything())

# prepare sheet
# sheet_delete(sheet_url, sheet = "project-individual )
# sheet_add(sheet_url, sheet = "project-individual", .after = "project")
# header 
# final_marks %>% 
#   colnames() %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   sheet_append(sheet_url, ., sheet = "project-individual")
# 
# results
# final_marks %>% 
#   sheet_append(sheet_url, ., sheet = "project-individual")

