library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(sf)
library(urbnmapr) # for sf data
library(glue)
library(ggrepel)
## Theme

post_theme <- function(...) {
  theme(
    text = 
      element_text(
        color = 'black',
        family = 'serif'),
    axis.text = 
      element_text(
        color = 'black'),
    axis.text.x = 
      element_text(angle = 45, 
                   hjust = 1),
    panel.background = 
      element_blank(),
    axis.line.x = 
      element_line(
        color = 'black'),
    axis.ticks = element_blank(),
    plot.margin = 
      margin(.75, .75, .75, .75, 'cm'),
    plot.caption = 
      element_text(hjust = 0,
                   face = "italic"),
    plot.title = 
      element_text(
        face = 'bold'),
    plot.subtitle = 
      element_text(face = 'bold'),
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    strip.background = 
      element_blank(), 
    strip.text = 
      element_text(
        face = 'bold')
  ) +
    theme(...)} # this bit allows us to make changes using this same function instead of calling two theme functions.

# I created a new R-project to house the Covid-19 data in my Documents directory.
covid_county <-
  read_csv('~/Documents/R/covid-19-data/us-counties.csv')



# Pull in the USDA data from a directory I created in my cloud storage.
rural_urban <-
  read_csv('~/OneDrive - SRUC/Data/usda/ruralurbancodes2013.csv') %>%
  select(fips,
         rucc_2013,
         desc = description)


state_region <-
  tibble(state = state.name,
         region = state.region)

covid_region <-
  covid_county %>% 
  arrange(fips, date) %>% 
  group_by(fips) %>% 
  mutate(cum_cases = cumsum(cases), 
   cum_deaths = cumsum(deaths)) %>% 
  select(-cases, -deaths) %>% 
  gather(key, value, -date, -county, -state, -fips) %>% 
  ungroup() %>%
  left_join(rural_urban,
            by = 'fips') %>%
  left_join(state_region,
            by = 'state')




# Look at rural 



covid_county_rural <- covid_county %>%
  left_join(rural_urban,
            by = 'fips') %>% 
  filter(str_detect(desc, 'rural')) %>% 
  select(-date, -county, -rucc_2013) %>% 
  group_by(fips) %>% 
  mutate(tot_deaths = sum(deaths, na.rm = T), 
         tot_cases = sum(cases, na.rm = T)) %>%   ungroup() %>% 
  filter(tot_cases > 10, 
         tot_deaths > 1) %>% # must have at least 10 cases
  select(-cases, -deaths) %>% 
  distinct(fips, .keep_all = T)
  

top_10_rural_deaths <- 
  covid_county_rural %>% 
  group_by(state) %>%
  filter(tot_deaths == max(tot_deaths)) %>% 
  ungroup() %>% 
  arrange(-tot_deaths) %>% 
  slice(1:10)



Dark2 <- 
  colorRampPalette(brewer_pal(palette = 'Dark2')(8))


plot_states <- covid_county_rural %>% 
  distinct(state) %>% 
  pull()

state_cols <- 
  sample(viridis_pal()(length(plot_states)))


names(state_cols) <-  sample(plot_states)


update_date_anno <- paste('Data updated on',
      format(max(covid_county$date), '%d-%b-%Y'))

rural_point_plot <- covid_county_rural %>% 
  left_join(tibble(state = state.name, 
                   abb = state.abb)) %>% 
  ggplot(aes(tot_cases, 
             tot_deaths))+
  geom_text(aes(label = abb, 
                color = state), 
            size = 5,
            show.legend = F)+
  scale_x_log10(labels = comma)+
  scale_y_log10(labels = comma)+
  scale_fill_manual(values = state_cols)+
  scale_color_manual(values = state_cols)+
  post_theme()+
  labs(title = 'Comparing Covid-19 Cases and Deaths Across Rural Counties in the U.S.', 
       subtitle = 'SOURCE: The New York Times, based on reports from state and local health agencies &\nThe USDA Rural-Urban Continuum Codes (2013).', 
       x = 'Total Cases', 
       y = 'Total Deaths', 
       caption = paste0('Only counties shown with more than 10 cases and at least 1 death recorded.\nNOTE:Horizontal and vertical axes are on log10 scales.\n',update_date_anno))



rural_point_plot_ii <- rural_point_plot +
  theme(text = element_text(size = 15))+
  annotate('label', 
           x = 5000, 
           y = 5, 
    label = str_wrap('Letters are state abbreviations. Each pair represents a different county within the listed state. State abbreviations are consistently colored.', 40), 
    size = 3.5, 
    family = 'serif', 
    hjust = 0.5)



ggsave(rural_point_plot_ii, 
       filename = 'rural_plot_point.png', 
       width = 11, 
       height = 8)




