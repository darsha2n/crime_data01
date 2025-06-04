
library(tidyverse)
library(janitor)
install.packages("stringr")
library(stringr)



"13_Police_killed_or_injured_on_duty.csv" %>% 
  read_csv() %>% 
  clean_names() -> police_killed_injured

police_killed_injured %>% colnames()

police_killed_injured %>% 
  rename("state_ut" = "area_name") -> police_killed_injured

colnames(police_killed_injured) %>% str_remove("police_") -> newnames

police_killed_injured %>% view()

police_killed_injured %>% 
  pull(state_ut) %>% unique()

police_killed_injured %>% 
  pull(year) %>% unique()

police_killed_injured %>% 
  pull(group_name) %>% unique()

police_killed_injured %>% 
  pull(sub_group_name) %>% unique()

police_killed_injured %>% 
  mutate(police_role = group_name %>% str_remove("Police -")) %>% 
  select(-sub_group_name, -group_name) ->police_killed_injured1
  View(police_killed_injured1)

police_killed_injured1 %>% 
  select(-police_injured_total_policemen, -police_killed_total_policemen) ->police_killed_injured2

police_killed_injured2 %>% 
  pivot_longer(c(3:14),
               names_to="category",
               values_to="count") -> police_killed_injured_cleaned

killed_injured <- function(cat) {
  if(str_detect(catergory, "injured")){
    return("Injured")
  }else{
    return("Killed")
  }
}

police_killed_injured_cleaned %>% 
  mutate(type= ifelse(str_detect(category, "injured"), "Injured", "Killed")) -> police_killed_injured_cleaned1

police_killed_injured_cleaned1 %>% 
  group_by(type) %>% 
  summarise(total= sum(count, na.rm= T)) %>% 
  ggplot(aes(type , total))+
  geom_col()+
  labs(title= "Police Killed or Injured",
       subtitle= "2001-2010",
       X= NULL,
       Y= "total count")+
  theme_minimal()

police_killed_injured_cleaned1 %>% 
  group_by(type, year) %>% 
  summarise(total= sum(count, na.rm= T)) %>% 
  ggplot(aes(year , total ,color= type ))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks= 2001:2010)+
  scale_y_continuous( seq(0 , 15000, 1000))+
  scale_color_manual(values= c("red", "blue"))+
  theme_minimal()

police_killed_injured_cleaned1 %>% 
  group_by(category, type) %>% 
  summarise(total= sum(count , na.rm=T)) %>% 
  ggplot(aes(category, total))+
  geom_col()+
  facet_wrap(~type)+
  coord_flip()

