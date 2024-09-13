########################################################
# CRADLE depression and flooding analysis

# monthly interview dates and flooding events figures
########################################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))
library(lubridate)

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))

#----------------------------------------
# hist of interview months
#----------------------------------------

# Convert numeric month to factor to display month names, ensuring all months are included as levels
baseline$month <- factor(baseline$month, 
                         levels = 1:12, 
                         labels = c("January", "February", "March", "April", "May", "June", 
                                    "July", "August", "September", "October", "November", "December"))

# Create the bar chart with grayscale colors and font size 12
plot <- ggplot(baseline, aes(x = month)) +
        geom_bar(fill = "gray70", color = "black") +  # Use geom_bar() for categorical data
        labs(x = "Month", y = "Count", title = "") +
        theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12), # Set x-axis text size
        axis.text.y = element_text(size = 12),                                     # Set y-axis text size
        axis.title.x = element_text(size = 12),                                    # Set x-axis title size
        axis.title.y = element_text(size = 12),                                    # Set y-axis title size
        plot.title = element_text(size = 12)                                       # Set plot title size
  )

ggsave(filename = paste0(figure_path, "interview_month_histogram.png"), 
       plot = plot, 
       width = 8, height = 6, dpi = 300)

#----------------------------------------
# table of compound flooding months
#----------------------------------------

baseline <- baseline %>%
  # create a new variable that subtracts the number of months from the interview date
  mutate(flooding_c_date = date %m-% months(q21_2),
         # extract the full name of the month from the flooding_date as a factor
         flooding_c_month = factor(month(flooding_c_date, label = TRUE, abbr = FALSE)))

# check logic
# test_c = baseline %>% dplyr::select(date, q21_2, flooding_c_month)

compound_flooding_months <- baseline %>%
  count(flooding_c_month) %>%
  arrange(flooding_c_month) %>% 
  rename(N=n, Month= flooding_c_month)

write.csv(compound_flooding_months, file = paste0(table_path, "compound_flooding_months.csv"), row.names = FALSE)

#----------------------------------------
# table of compound union months
#----------------------------------------

baseline <- baseline %>%
  mutate(flooding_u_date = date %m-% months(q21_11),
         flooding_u_month = factor(month(flooding_u_date, label = TRUE, abbr = FALSE)))

# check logic
# test_u = baseline %>% dplyr::select(date, q21_11, flooding_u_month)

union_flooding_months <- baseline %>%
  count(flooding_u_month) %>%
  arrange(flooding_u_month) %>% 
  rename(N=n, Month= flooding_u_month)

write.csv(union_flooding_months, file = paste0(table_path, "union_flooding_months.csv"), row.names = FALSE)



