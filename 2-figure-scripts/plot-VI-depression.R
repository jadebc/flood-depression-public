#########################################
# CRADLE depression and flooding analysis

# plot variable importance analysis
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))

# -------------------------------------------------------
# random forest plot 
# -------------------------------------------------------
rf_depression <- readRDS(paste0(data_dir, "rf_depression.RDS"))

# models with importance scores that are non-zero and greater
# than the absolute value of the largest negative importance value

imp_dep <- rf_depression$var_importance_sorted

imp_dep <- imp_dep %>%
  filter(importance > abs(min(importance)))

palette <- c(
  "#4d4c4b",
  "#4363D8",
  "#3CB44B"
)

imp_dep <- imp_dep %>% mutate(var_cat = ifelse(variable=="hygienic_latrine", "Housing", var_cat))
imp_dep <- imp_dep %>% mutate(var_cat = ifelse(variable=="father_work", "Economic", var_cat))

imp_dep <- imp_dep %>% 
  mutate(label_final = case_when(
    variable == "latrine_flooded" ~ "Latrine flooded",
    variable == "flood_compound" ~ "Compound flooded",
    variable ==  "mother_age" ~ "Age",
    variable ==  "hhsize" ~ "Household size",
    variable ==  "mother_edu" ~ "Education",
    variable ==  "income" ~ "Household income",
    variable ==  "flood_prepared" ~ "Prepared for next flood",
    variable ==  "father_work" ~ "Spouse works in agriculture",
    variable ==  "wealth_index" ~ "Household wealth",
    variable ==  "own_house" ~ "Owns house"
  )) %>% mutate(var_cat = case_when(
    variable ==  "latrine_flooded" ~ "Flooding",
    variable ==  "flood_compound" ~ "Flooding",
    variable ==  "mother_age" ~ "Demographics",
    variable ==  "hhsize" ~ "Demographics",
    variable ==  "mother_edu" ~ "Demographics",
    variable ==  "income" ~ "Socioeconomic status",
    variable ==  "flood_prepared" ~ "Flooding",
    variable ==  "father_work" ~ "Socioeconomic status",
    variable ==  "wealth_index" ~ "Socioeconomic status",
    variable ==  "own_house" ~ "Socioeconomic status"
  ))

# Plot variable importance
depression_plot <- ggplot(imp_dep,
                          aes(x = reorder(label_final, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill=var_cat)) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Variables", y = "Importance") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid.minor = element_line(size=0.1),
        panel.grid.major = element_line(size=0.25),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = -0.75, margin = margin(b = 10)))+
  ggtitle("a)")

depression_plot

# -------------------------------------------------------
# prevalence ratios plot 
# -------------------------------------------------------
depression_PRs <- readRDS(paste0(results_path, "depression_PRs.RDS"))

depression_PRs <- depression_PRs %>% 
  mutate(label_final = case_when(
    label == "latrine_flooded" ~ "Latrine flooded",
    label == "flood_compound" ~ "Compound flooded",
    label == "age_over_23" ~ "Age > 23 years",
    label == "hhsize_over_4" ~ "Household size > 4",
    label == "edu_over_6" ~ "Education > 6 years",
    label == "income_over_5" ~ "Household income > 5",
    label == "flood_prepared" ~ "Prepared for next flood",
    label == "father_work_agr" ~ "Spouse works in agriculture",
    label == "wealth_above_med" ~ "Above median wealth",
    label == "own_house" ~ "Owns house"
  )) %>% mutate(var_cat = case_when(
  label == "latrine_flooded" ~ "Flooding",
  label == "flood_compound" ~ "Flooding",
  label == "age_over_23" ~ "Demographics",
  label == "hhsize_over_4" ~ "Demographics",
  label == "edu_over_6" ~ "Demographics",
  label == "income_over_5" ~ "Socioeconomic status",
  label == "flood_prepared" ~ "Flooding",
  label == "father_work_agr" ~ "Socioeconomic status",
  label == "wealth_above_med" ~ "Socioeconomic status",
  label == "own_house" ~ "Socioeconomic status"
))


# sort label by PR value
depression_PRs$label_final <- factor(depression_PRs$label_final,
                                     levels = depression_PRs$label_final[order(depression_PRs$pt_estimate)])

dep_PR_plot <- ggplot(depression_PRs, aes(x = label_final, y = pt_estimate, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = 1, color = "grey") +
  geom_pointrange(aes(color = var_cat), size=0.25) +
  scale_color_manual(values= palette) +
  coord_flip() +
  labs(y = "Prevalence Ratio") +
  scale_y_continuous(trans= "log", breaks=c(0.5, 0.75, 1, 1.5, 2, 3, 4, 5),
                     labels =c(0.5, 0.75, 1, 1.5, 2, 3, 4, 5)) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_line(size=0.1),
        panel.grid.major = element_line(size=0.25),
        legend.position = c(1, 0),  # x = 1, y = 0 for lower right
        legend.justification = c(1, 0),  # anchor point
        legend.box.just = "right",
        legend.margin = margin(2, 3, 3, 3),  # Reduced margins
        legend.background = element_rect(fill = "white", color = "black",size=0.1),  # White background with black border
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.5, "lines"),
        legend.text = element_text(size = 6),  # Reduce text size
        legend.spacing.y = unit(0.2, "lines"),
        plot.title = element_text(hjust = -0.75, margin = margin(b = 10))) +
  guides(color = guide_legend(override.aes = list(shape = 16, size = .6)))+
  ggtitle("b)")

dep_PR_plot

combined_plot <- grid.arrange(depression_plot, dep_PR_plot, nrow=1)

ggsave(
  filename = paste0(figure_path, "plot_variable_importance.pdf"),
  plot = combined_plot,
  width = 8, height = 3, units = "in"
)
