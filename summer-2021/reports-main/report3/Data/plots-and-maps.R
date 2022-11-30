## plots and maps - AQ

# load packages here
library(tidyverse)
library(ggthemes)
library(gghighlight)
library(sf)
library(urbnmapr)
library(maps)
library(stringr)

# load datasets
s5 <- read_csv("stage5.csv")
lags_data <- read_csv("lagSlopesOIS.csv")
OIS_data <- read_csv("california_OIS_2013-2021(aug3).csv") %>% 
  filter(date < '2020-1-1', ori %in% unique(ori))

# State name and abreviation
state_full <- "California"
state_abv <- "CA"

# albers projection with curvature string
crs_use <- "+proj=laea +lat_0=30 +lon_0=-95"

# load cities and capital city for chosen state
us_cities <- us.cities %>% 
  filter(country.etc == state_abv, pop >= 400000) %>% 
  mutate(name = substring(name, 1, nchar(name)-3)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = crs_use)
capital_city = us_cities %>% 
  filter(capital == 2) %>% 
  mutate(name_n = str_remove_all(name, state_abv)) %>%
  mutate(name_n = str_remove_all(name_n, " "))

# wrangle the locations of OIS data
ois_locations <- OIS_data %>%
  select(latitude,longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = crs_use)

# load the states shapefiles using the urbnmapr package
states_sf <- get_urbn_map("states", sf = TRUE) 

# filter the chosen state
states_sf_one <- states_sf %>% 
  filter(state_name == state_full)

# load the all counties shapefiles using the urbnmapr package
counties_sf <- get_urbn_map("counties", sf = TRUE)

# filter the chosen state shapefile using the urbnmapr package
counties_sf_state <- counties_sf %>% 
  filter(state_name == state_full)

### OIS mapping

# draw state outline
p1 <- ggplot(data = counties_sf_state, aes(geometry = geometry, colour = race)) +
  geom_sf(color = "black", size = 0.15) + 
  theme(legend.position="bottom",
        panel.background=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"))

# draw cities
p2 <- p1 +
  geom_sf(data = ois_locations, aes(geometry = geometry, color = "one OIS"), size = 0.50, fill = "red", shape = 20) +
  scale_color_manual(values = c("one OIS" = "red")) +
  geom_sf_text(data = us_cities, aes(label=name), size=4.25, color = "black") +
  labs(title = "Officer Involved Shootings (OIS) in CA (2013-2019)",
       color = "", shape = "")

# save plot
ggsave("ca-OIS.png",p2, height = 7, width = 5, unit = "in")

# apply transparent background
p3 <- p2 +
  theme(
    legend.position="bottom",
    legend.background = element_rect(fill = "transparent", colour='transparent'),
    legend.box.background = element_rect(fill = "transparent", colour='transparent'),
    legend.key = element_rect(fill = "transparent", colour='transparent'),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# save plot
ggsave("ca-OIS-transparent.png",p3, height = 7, width = 5, unit = "in")

### Spatial crime rates mapping
mean_cr <- lags_data %>% 
  group_by(fips_state_county_code) %>%
  summarise(average_cr = median(m_index_crime_rate)) %>%
  mutate(average_cr_cat = case_when(average_cr >= 0 & average_cr < 100 ~ "[1,100)",
                                    average_cr >= 100 & average_cr < 150 ~ "[100,150)",
                                    average_cr >= 150 & average_cr < 200 ~ "[150,200)",
                                    average_cr >= 200 & average_cr < 250 ~ "[200,250)",
                                    average_cr >= 250 & average_cr < 300 ~ "[250,300)",
                                    average_cr >= 300 & average_cr < 350 ~ "[300,350)",
                                    average_cr >= 350 ~ "[350,)"))

mean_cr_sf <- counties_sf_state %>%
  left_join(mean_cr, by = c("county_fips" = "fips_state_county_code"))

# draw state outline
p6 <- ggplot(data = mean_cr_sf, aes(geometry = geometry, fill = average_cr_cat)) +
  geom_sf(color = "black", size = 0.15) + 
  scale_fill_manual(values = c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84')) +
  theme(legend.position="bottom",
        panel.background=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE,
                           title.position="bottom",title.hjust=0.5)) +
  labs(fill = "",
       title = "Median Crime per 100K (2013-2019)")

# save plot
ggsave("median-crime-rate.png",p6, height = 7, width = 5, unit = "in")

# apply transparent background
p7 <- p6 +
  theme(
    legend.position="bottom",
    legend.background = element_rect(fill = "transparent", colour='transparent'),
    legend.box.background = element_rect(fill = "transparent", colour='transparent'),
    legend.key = element_rect(fill = "transparent", colour='transparent'),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# save plot
ggsave("median-crime-rate-transparent.png",p7, height = 7, width = 5, unit = "in")

# ---

### Poverty proportions
pov_proportions <- s5 %>% 
  group_by(fips_state_county_code) %>%
  summarise(med_estimate_Below_Poverty = median(estimate_Below_Poverty),
         med_estimate_Total = median(estimate_Total),
         prop_under_pov = med_estimate_Below_Poverty/med_estimate_Total) %>%
  mutate(prop_under_pov_cat = case_when(prop_under_pov >= 0 & prop_under_pov < 0.02 ~ "[0,2%)",
                                    prop_under_pov >= 0.02 & prop_under_pov < 0.04 ~ "[2%,4%)",
                                    prop_under_pov >= 0.04 & prop_under_pov < 0.06 ~ "[4%,6%)",
                                    prop_under_pov >= 0.06 & prop_under_pov < 0.08 ~ "[6%,8%)",
                                    prop_under_pov >= 0.08 ~ "[8%,)"))

pov_prop_sf <- counties_sf_state %>%
  left_join(pov_proportions, by = c("county_fips" = "fips_state_county_code"))

# draw state outline
p8 <- ggplot(data = pov_prop_sf, aes(geometry = geometry, fill = prop_under_pov_cat)) +
  geom_sf(color = "black", size = 0.15) + 
  scale_fill_manual(values = c('#ffffcc','#c2e699','#78c679','#31a354','#006837')) +
  theme(legend.position="bottom",
        panel.background=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE,
                           title.position="bottom",title.hjust=0.5)) +
  labs(fill = "",
       title = "Proportion Under Poverty")

# save plot
ggsave("prop-pov.png",p8, height = 7, width = 5, unit = "in")

# apply transparent background
p9 <- p8 +
  theme(
    legend.position="bottom",
    legend.background = element_rect(fill = "transparent", colour='transparent'),
    legend.box.background = element_rect(fill = "transparent", colour='transparent'),
    legend.key = element_rect(fill = "transparent", colour='transparent'),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# save plot
ggsave("prop-pov-transparent.png",p9, height = 7, width = 5, unit = "in")

# ---

### Black proportions
af_proportions <- s5 %>% 
  group_by(fips_state_county_code) %>%
  summarise(med_estimate_af = median(estimate_Af_American),
            med_estimate_Total = median(estimate_Total),
            prop_af = med_estimate_af/med_estimate_Total) %>%
  mutate(prop_af_cat = case_when(prop_af >= 0 & prop_af < 0.02 ~ "[0,2%)",
                                 prop_af >= 0.02 & prop_af < 0.04 ~ "[2%,4%)",
                                 prop_af >= 0.04 & prop_af < 0.06 ~ "[4%,6%)",
                                 prop_af >= 0.06 & prop_af < 0.08 ~ "[6%,8%)",
                                 prop_af >= 0.08 ~ "[8%,)"))

af_prop_sf <- counties_sf_state %>%
  left_join(af_proportions, by = c("county_fips" = "fips_state_county_code"))

# draw state outline
p10 <- ggplot(data = af_prop_sf, aes(geometry = geometry, fill = prop_af_cat)) +
  geom_sf(color = "black", size = 0.15) + 
  scale_fill_manual(values = c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494')) +
  theme(legend.position="bottom",
        panel.background=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE,
                           title.position="bottom",title.hjust=0.5)) +
  labs(fill = "",
       title = "Proportion of Black Population")

# save plot
ggsave("prop-af.png",p10, height = 7, width = 5, unit = "in")

# apply transparent background
p11 <- p10 +
  theme(
    legend.position="bottom",
    legend.background = element_rect(fill = "transparent", colour='transparent'),
    legend.box.background = element_rect(fill = "transparent", colour='transparent'),
    legend.key = element_rect(fill = "transparent", colour='transparent'),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# save plot
ggsave("prop-af-transparent.png",p11, height = 7, width = 5, unit = "in")

# ---

### temporal crime rates mapping while getting rid of outliers (for now)

sub_lags_data <- lags_data %>%
  filter(!GEOID %in% c("0682422","0665112")) %>%
  mutate(crosswalk_agency_name = str_replace(crosswalk_agency_name," police department","")) %>%
  group_by(crosswalk_agency_name) %>%
  filter(any(OISperMonth > 1)) %>%	
  ungroup()

top_departments <- c((sub_lags_data  %>%
  group_by(crosswalk_agency_name) %>%
  summarise(total_OIS = sum(OISperMonth, na.rm = TRUE)) %>% 
  arrange(desc(total_OIS)) %>%
  head(8))$crosswalk_agency_name)

ois_only <- sub_lags_data %>% filter(OISperMonth != 0)
  
p4 <- ggplot(data = sub_lags_data, aes(x = Rdate, y = slope_15, colour = crosswalk_agency_name)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("2013-01-01", "2019-12-31")),
               breaks = seq(as.Date("2013-01-01"), as.Date("2019-12-31"), by="2 years"),
               date_labels = "%Y") +
  geom_point(data = ois_only, aes(x = Rdate, y = slope_15, colour = crosswalk_agency_name,
                                  size = OISperMonth), alpha = 0.50) +
  geom_hline(yintercept=0, linetype='dashed', col = 'black') +
  gghighlight(crosswalk_agency_name %in% top_departments,
              use_direct_label = FALSE) + 
  scale_colour_colorblind(name = "Police Department") +
  labs(title = "Monthly change in crime rates",
       subtitle = "Each line represents a police department with at least 1 OIS",
       x = "time",
       y = "15-month prior slope",
       size = "Number of OIS") +
  facet_wrap(~ crosswalk_agency_name, nrow = 4, ncol = 4) +
  theme(text = element_text(size = 13))

# save plot
ggsave("change-crime-rates.png",p4, height = 6, width = 12, unit = "in")

# apply transparent background
p5 <- p4 +
  theme(
    #legend.background = element_rect(fill = "transparent", colour='transparent'),
    #legend.box.background = element_rect(fill = "transparent", colour='transparent'),
    #legend.key = element_rect(fill = "transparent", colour='transparent'),
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  )

# save plot
ggsave("change-crime-rates-transparent.png",p5, height = 6, width = 12, unit = "in")

