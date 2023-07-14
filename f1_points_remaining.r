
library(tidyverse)
library(readxl)
library(scales)

this_file_path <- 'C:/Users/Dell/OneDrive/Documents/f1_2023'
setwd(this_file_path)

#### read in data ####

wdc_points_remaining <- read_xlsx(path = 'f1_points_remaining_20230712.xlsx',
                                  sheet = 'wdc')

const_points_remaining <- read_xlsx(path = 'f1_points_remaining_20230712.xlsx',
                                    sheet = 'constructors')

prev_points <- read_xlsx(path = 'f1_points_remaining_20230712.xlsx',
                         sheet = 'previous_points')


wdc_standings <- read_xlsx(path = 'f1_points_remaining_20230712.xlsx',
                           sheet = 'wdc_standings')


constructors_standings <- read_xlsx(path = 'f1_points_remaining_20230712.xlsx',
                                    sheet = 'constructors_standings')


#### firstly for wdc ####

### Earliest possible wins

# all drivers still in with a chance so lets focus on top 4
top_four_drivers <- wdc_standings[1:4,]

ver <- as.numeric(top_four_drivers[top_four_drivers$Driver == 'VER', 'Points'])
per <- as.numeric(top_four_drivers[top_four_drivers$Driver == 'PER', 'Points'])
alo <- as.numeric(top_four_drivers[top_four_drivers$Driver == 'ALO', 'Points'])
ham <- as.numeric(top_four_drivers[top_four_drivers$Driver == 'HAM', 'Points'])

wdc_earliest <- wdc_points_remaining %>% 
  mutate(ver_max_points_after_race = ver + cumsum(maximum_points_available),
         
         per_min_points_after_race = per,
         alo_min_points_after_race = alo,
         ham_min_points_after_race = ham,
         
         ver_dif_to_per = ver_max_points_after_race - per_min_points_after_race,
         ver_dif_to_alo = ver_max_points_after_race - alo_min_points_after_race,
         ver_dif_to_ham = ver_max_points_after_race - ham_min_points_after_race,
         
         per_in_contention = ver_dif_to_per <= maximum_points_remaining_after_race,
         alo_in_contention = ver_dif_to_alo <= maximum_points_remaining_after_race,
         ham_in_contention = ver_dif_to_ham <= maximum_points_remaining_after_race,
         )


### Most likely scenario

## calculate conversion rates

# driver points avaible so far
wdc_maximum_now <- sum(prev_points$driver_maximum_points_available)
wdc_maximum_togo <- sum(wdc_points_remaining$maximum_points_available)
wdc_total_maximum <- sum(wdc_maximum_now + wdc_maximum_togo)

# driver conv rates
ver_conv <- ver/wdc_maximum_now
per_conv <- per/wdc_maximum_now
alo_conv <- alo/wdc_maximum_now
ham_conv <- ham/wdc_maximum_now


## wdc won under most likely results

wdc_most_likely <- wdc_points_remaining %>% 
  mutate(ver_likely_points_after_race = ver + cumsum(maximum_points_available*ver_conv),
         per_likely_points_after_race = per + cumsum(maximum_points_available*per_conv),
         alo_likely_points_after_race = alo + cumsum(maximum_points_available*alo_conv),
         ham_likely_points_after_race = ham + cumsum(maximum_points_available*ham_conv),
         
         ver_dif_to_per = ver_likely_points_after_race - per_likely_points_after_race,
         ver_dif_to_alo = ver_likely_points_after_race - alo_likely_points_after_race,
         ver_dif_to_ham = ver_likely_points_after_race - ham_likely_points_after_race,
         
         per_in_contention = ver_dif_to_per <= maximum_points_remaining_after_race,
         alo_in_contention = ver_dif_to_alo <= maximum_points_remaining_after_race,
         ham_in_contention = ver_dif_to_ham <= maximum_points_remaining_after_race,
  )





#### Now for constructors ####

### Earliest possible wins

# all teams still in with a chance so lets focus on top 4
top_four_teams <- constructors_standings[1:4,]

rb <- as.numeric(top_four_teams[top_four_teams$Constructor == 'RED BULL', 'Points'])
merc <- as.numeric(top_four_teams[top_four_teams$Constructor == 'MERCEDES', 'Points'])
ast <- as.numeric(top_four_teams[top_four_teams$Constructor == 'ASTON MARTIN', 'Points'])
fer <- as.numeric(top_four_teams[top_four_teams$Constructor == 'FERRARI', 'Points'])

const_earliest <- const_points_remaining %>% 
  mutate(rb_max_points_after_race = rb + cumsum(maximum_points_available),
         
         merc_min_points_after_race = merc,
         ast_min_points_after_race = ast,
         fer_min_points_after_race = fer,
         
         rb_dif_to_merc = rb_max_points_after_race - merc_min_points_after_race,
         rb_dif_to_ast = rb_max_points_after_race - ast_min_points_after_race,
         rb_dif_to_fer = rb_max_points_after_race - fer_min_points_after_race,
         
         merc_in_contention = rb_dif_to_merc <= maximum_points_remaining_after_race,
         ast_in_contention = rb_dif_to_ast <= maximum_points_remaining_after_race,
         fer_in_contention = rb_dif_to_fer <= maximum_points_remaining_after_race,
  )



### Most likely scenario

## calculate conrbsion rates

# drirb points avaible so far
const_maximum_now <- sum(prev_points$constructor_maximum_points_available)
const_maximum_togo <- sum(const_points_remaining$maximum_points_available)
const_total_maximum <- sum(const_maximum_now + const_maximum_togo)

# team conv rates
rb_conv <- rb/const_maximum_now
merc_conv <- merc/const_maximum_now
ast_conv <- ast/const_maximum_now
fer_conv <- fer/const_maximum_now


# team points available so far

# const conv rates


## const won under most likely results

const_most_likely <- const_points_remaining %>% 
  mutate(rb_likely_points_after_race = rb + cumsum(maximum_points_available*rb_conv),
         merc_likely_points_after_race = merc + cumsum(maximum_points_available*merc_conv),
         ast_likely_points_after_race = ast + cumsum(maximum_points_available*ast_conv),
         fer_likely_points_after_race = fer + cumsum(maximum_points_available*fer_conv),
         
         rb_dif_to_merc = rb_likely_points_after_race - merc_likely_points_after_race,
         rb_dif_to_ast = rb_likely_points_after_race - ast_likely_points_after_race,
         rb_dif_to_fer = rb_likely_points_after_race - fer_likely_points_after_race,
         
         merc_in_contention = rb_dif_to_merc <= maximum_points_remaining_after_race,
         ast_in_contention = rb_dif_to_ast <= maximum_points_remaining_after_race,
         fer_in_contention = rb_dif_to_fer <= maximum_points_remaining_after_race,
  )



#### Plots 

### constructors - earliest

const_earliest <- const_earliest %>% 
  mutate(stage = case_when(format == 'Sprint' ~ paste0(stage, ' sprint'),
                           TRUE ~ stage))

const_earliest <- const_earliest %>% 
  mutate(stage = factor(stage, levels = const_earliest$stage))

ggplot(const_earliest) +
  geom_col(aes(x = stage, y = rb_dif_to_merc), fill = '#1D2F40') +
  geom_step(aes(x = stage, y = maximum_points_remaining_after_race, group = 1),
            colour = '#F22E52', position = position_nudge(x = -0.5), size = 1) +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        #axis.title.y = element_text(angle = 0),
        axis.text.x = element_text(size = 9),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(),
        title = element_text(size = 16)) +
  geom_text(aes(x = 'Hungary', y = 570), label = 'Available points remaining after race', 
            colour = '#F22E52', hjust = 0) +
  geom_text(aes(x = 'Netherlands', y = 380), label = 'Red Bull points lead over second',
            colour = '#1D2F40', hjust = 0.9) +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = 'The earliest Red Bull can win the constructors championship is in Monza',
       subtitle = 'Red Bull are unreachable when the navy bar surpasses the red line',
       caption = 'Assuming Red Bull recieve maximum points at each race, whilst second place team does not improve their points')

ggsave('constructors_earliest.png', height = 7, width = 10)
