#########################################
# CRADLE depression and flooding analysis

# plot water level and rainfall
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))

d = readRDS(paste0(data_dir, "/baseline_clean.RDS"))

gov_data_raw = read.csv("/Users/jadebc/Library/CloudStorage/Box-Box/Jade Benjamin-Chung's Externally Shareable Files/CRADLE-Data/Climate Data Sirajganj/climate_data_sirajganj.csv")

gov_data = filter(gov_data_raw, !is.na(Sirajganj_WL)) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date >= as.Date("2023-04-01"))

baseline <- readRDS(paste0(data_dir, "baseline_clean.RDS")) %>% 
  dplyr::select(dataid, flood_compound, date_compound_flood, 
                flood_union, date_union_flood,
                latrine_flooded)

flood_data <-   d %>% filter(flood_compound == 1 | flood_union == 1) %>% 
  group_by(date_union_flood) %>%
  summarise(n_union_flood = sum(as.numeric(as.character(flood_union), na.rm=T)),
            n_compound_flood = sum(as.numeric(as.character(flood_compound), na.rm=T)),
            n_latrine_flooded = sum(as.numeric(as.character(latrine_flooded)), na.rm=T))

# merge baseline data with government data
WL_flood <- left_join(gov_data, flood_data,
                      by = c("date" = "date_union_flood")) %>% 
  mutate(n_union_flood = ifelse(is.na(n_union_flood), 0, n_union_flood),
         n_compound_flood = ifelse(is.na(n_compound_flood), 0, n_compound_flood),
         n_latrine_flooded = ifelse(is.na(n_latrine_flooded), 0, n_latrine_flooded)) %>% 
  mutate(any_flood = ifelse(n_union_flood>0 | n_compound_flood>0|n_latrine_flooded>0, 1, 0))

# make a plot of the water level over time 
plot_data = WL_flood %>% filter(date >as.Date('2023-01-01') &
                                  date <as.Date('2024-03-01'))


water_plot <- ggplot(plot_data, aes(x=date, y = Sirajganj_WL)) + 
  geom_line(data=WL_flood %>% filter(date >as.Date('2023-01-01') &
                                       date <as.Date('2024-03-01')), aes(x=date, y=Sirajganj_WL)) + 
  geom_area(fill = "#498fd1") +
  geom_point(data = plot_data %>% filter(any_flood==1 ),
             aes(x = date, y = Sirajganj_WL),size=1) +
  scale_y_continuous(breaks=seq(0,15,2),
                     labels =seq(0,15,2)) +
  xlab("Date") + ylab("Water Level (m)") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.2),
        panel.grid.minor.y = element_line(size=0.1)) +
  ggtitle("B)")


# make a plot of the rainfall over time 
rain_plot <- ggplot(plot_data, aes(x=date, y = Rainfall)) + 
  geom_line(size=0.25) + 
  scale_y_continuous(breaks=seq(0,150,25),
                     labels =seq(0,150,25)) +
  xlab("Date") + ylab("Rainfall (mm)") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.2),
        panel.grid.minor.y = element_line(size=0.1),
        axis.title.x = element_blank()) +
  ggtitle("A)")

# make a plot of % of seasonal water by month 
colors <- c(

  "#E0A17E", # April
  "#E8916C", # May
  "#C1A7A7", # June
  "#45B7D1", # July
  "#5AAFC5", # August
  "#6FA7B9", # September
  "#849FAD", # October
  "#9997A1", # November
  "#AEAEB6",  # December
  "#E0E0E0", # January
  "#D4D7E5", # February
  "#F0D6A3" # March
)

d = d %>% mutate(month_f = factor(month, levels = c(
  4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3
)))

surface_water_plot <- ggplot(d, aes(x=month_f, y = prop_seasonal_water_1000))+
  geom_violin(aes(fill = month_f)) +
  scale_fill_manual(values = colors) + 
  xlab("Month") + 
  ylab("Proportion of area within 1km\nof compound with seasonal water") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank()) +
  ggtitle("C)")

combined_plot <- grid.arrange(rain_plot, water_plot,surface_water_plot, ncol=1)


ggsave(combined_plot, filename = paste0(figure_path, "plot_rainfall_water_level.pdf"),
       width = 5, height = 7, units = "in")

