# Visualization-of-Obesity-in-the-US
This was a group project for the course Data Visualization using R 


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r load_libraries, echo=FALSE, message=FALSE, warning= FALSE}
library(tidyverse)
library(maps)
library(usmap)
library(stringr)
library(lubridate)
library(cowplot)
library(rvest)
library(gridExtra)

```

## General Data Wrangling

```{r data_wrangle, warnings = FALSE, message=FALSE}

# This code block read the Obesity Dataset and then wrangles the dataframe so it is easier to view and use 
# This portion of wrangling is just the general that might be useful to answer multiple questions
# Further wrangling is required depending on the nature of the particular question


df <- read_csv('data/obesity.csv')

df_wrangle <- df %>% select(YearStart, LocationAbbr, Class, Question, Data_Value, Sample_Size, GeoLocation, StratificationCategory1, Stratification1) %>% 
  rename(year = YearStart, abbr = LocationAbbr) %>% separate(GeoLocation, c('Lat', 'Long'), sep = ',')

df_wrangle$Lat <- df_wrangle$Lat %>% str_remove("\\(") %>% as.numeric(.) 
df_wrangle$Long <- df_wrangle$Long %>% str_remove("\\)") %>% as.numeric(.)

map <- usmap::us_map("states")

us_states <-  unique(map$abbr)
states_and_territories <- unique(df_wrangle$abbr) 

extra_states <- setdiff(states_and_territories, us_states)

df_wrangle <- df_wrangle %>% filter(!(abbr %in% extra_states))

```

## Obesity in US States

``` {r obesity_us_states, warnings = FALSE, message=FALSE}

# This chunk wrangles the obesity rates in each US state in year 2016
# This chunk plots the US map showing states with low and high obesity rates

# Q1) How is obesity spread across different states in the US?

start_value2 <- 'Total'

df_total <- df_wrangle %>% filter(year == 2016, StratificationCategory1 == start_value2, 
                                  Question == 'Percent of adults aged 18 years and older who have obesity') 

total_join <- left_join(map, df_total)

obesity_bar <- df_total %>% select(abbr, Data_Value) %>% ggplot(mapping = aes(x = reorder(abbr, Data_Value), y = Data_Value, fill = abbr)) +
  geom_col() + coord_flip() + ylab('Obese Percentage') + xlab('States') + ggtitle('Obesity Percentage by State') +
  guides(fill = FALSE)

total_join %>% ggplot(mapping = aes(x = x, y = y, fill = Data_Value, group=group)) + geom_polygon() +
  theme_map() + coord_equal()  + ggtitle('Obesity Percentage by State')+
  labs(fill = 'Obesity Percentage') + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_distiller(palette = 'Set1')

```

## Obesity VS Median HouseHold Income

``` {r obesity_vs_income}
# This chunk attempts to justify the results from the previous plot
# It scrapes the web to get data for median household income in differnet US states in the year 2016
# It wrangles that data to convert it into a tibble and save that tibble (to avoid multiple request to website)
# Compares the states with lowest median household income and highest obesity rates
# Plots obesity rates (in descending order) and median household income (in ascending order) to clearly visualize the similarities

# Q1 (Part-2) How can we explain this association of certain states with high obesity rates?

page <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_income")
income_vector <-  page %>% html_nodes("td:nth-child(5)") %>% html_text()
income_vector <- income_vector[1:53]
income_vector <- income_vector[-21]
income_vector <- income_vector[-47]
income_vector <- as.numeric(str_remove_all(income_vector, "[$,]"))


state_vector <- page %>% html_nodes("td:nth-child(2)") %>% html_text()
state_vector <- state_vector[1:53]
state_vector <- state_vector[-21]
state_vector <- state_vector[-47]
state_vector <- str_trim(state_vector)
state_vector <- state.abb[match(state_vector, state.name)]
state_vector[1] = "DC"

income_tibble <- tibble(State = state_vector, Median_Household_Income = income_vector)

# write_csv(income_tibble, 'data/median_income.csv')

income_tibble <- read.csv('data/median_income.csv')
income_bottom_state <- income_tibble %>% arrange(Median_Household_Income) %>% head(10)
income_bottom_10 <- income_bottom_state$State
obese_top_state <- df_total %>% arrange(desc(Data_Value)) %>% head(10)
obese_top_10 <- obese_top_state$abbr

common_state <- income_bottom_10[income_bottom_10 %in% obese_top_10]

income_bar <- income_tibble %>% ggplot(mapping = aes(x = reorder(State, -Median_Household_Income), y = Median_Household_Income, fill = State)) +
  geom_col() + coord_flip() + ylab('Median HouseHold Income') + xlab('States') + ggtitle('Median Household Income by State') +
  guides(fill = FALSE)

grid.arrange(obesity_bar, income_bar, nrow=1, ncol=2)


```

## Obesity and Education

```{r obesity_education, warnings = FALSE, message=FALSE}

# This code chunk filters the Obesity dataset by "Education" Category. 
# It get the obese percentage by different education demographic
# It plots the obese percentage by different education demographics in the US states

# Q2)  Is there a relation between obesity and the highest level of education obtained by individuals?

strat_value <- 'Education'

df_edu <- df_wrangle %>% filter(year == 2016, StratificationCategory1 == strat_value) 

df_edu_weight <- df_edu %>% filter(Class == 'Obesity / Weight Status')

df_edu_habits <- df_edu %>% filter(Class == 'Physical Activity')



# Q1) Obesity in US  for education less than high school
df_no_ed <- df_edu_weight  %>% na.omit()


# df_no_ed_states <- df_no_ed %>% filter(abbr != extra_states)
df_no_ed_obesity <- df_no_ed %>% filter(Question == 'Percent of adults aged 18 years and older who have obesity')

no_ed_join = left_join(map, df_no_ed_obesity)



no_ed_join %>% ggplot(mapping = aes(x = x, y = y, fill = Data_Value, group=group)) + geom_polygon() +
  theme_map() + coord_equal()  + ggtitle('Obese Percentage by Education Groups') +
  labs(fill = 'Obesity Percentage') + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_distiller(palette = 'Set1') + facet_wrap(~Stratification1)

```

## Obesity and Physical Activities among Education wise Demographics

```{r obesity_education_habit, warnings = FALSE, message=FALSE}

# This chunk aims to justify the results from the previous plot
# It gets the percentage of people who are not involved in any physical activities by different education demographic
# It plots the percentage of no physical activities by different education demographics in the US states

# Q2 (Part-2)  If so, what are some possible explanations?

df_edu_habits_w <- df_edu_habits %>% na.omit()
ed_habits_join <- left_join(map, df_edu_habits_w)
ed_habits_join %>% ggplot(mapping = aes(x = x, y = y, fill = Data_Value, group=group)) + geom_polygon() +
  theme_map() + coord_equal()  + labs(title = 'Education Impact on Physical Activities')+
  labs(fill = 'No Exercise Percentage') + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_distiller(palette = 'Set1') + facet_wrap(~Stratification1)


```

## Obesity Rate by Age Group
```{r obesity-age group, message=FALSE, warning=FALSE}
# This code chunk grabs the data related to obesity among different age-groups from csv file and shows obesity rate in different age groups in bar charts. It also shows a correlation with the physical activity and obesity among different age groups

# Q3) How is obesity spread out among different age groups? Why might this be the case?


grouping <- 'Age (years)'

obesity_data<- df
obesity_data_age<-obesity_data%>%
  filter(StratificationCategory1== grouping)


obesity_data_age<-obesity_data_age%>%
  filter(Class=="Obesity / Weight Status")

obesity_data_age1<-obesity_data_age%>%
  select(YearStart, LocationAbbr,Class,Question,Data_Value,'Age(years)',GeoLocation,StratificationCategory1,Stratification1,LocationID,Sample_Size)

obesity_data_age1<-obesity_data_age1%>%
  filter(Question=="Percent of adults aged 18 years and older who have obesity")%>%
  arrange(LocationAbbr)

obesity_data_age_percent<-obesity_data_age1%>%
  rename(state=LocationAbbr)%>%
  filter(state!="US"& state!="PR", state!="VI"& state!="GU")

obesity_data_age_percent_2016<-obesity_data_age_percent%>%
  filter(YearStart==2016)

#Q3(Part1) Obesity rate by Age Group in 2016
obesity_data_age_percent_totalagegroup<-obesity_data_age_percent_2016%>%
  group_by(`Age(years)`)%>%
  summarize(meanValue=mean(Data_Value))

obesity_data_age_percent_totalagegroup%>%
  ggplot(mapping=aes(x=`Age(years)`,y=meanValue,fill=`Age(years)`))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Obesity Rate by Age Group in 2016",y="Obesity Rate")


#Q3(Part2) Obesity Rate by Age group and State in 2016
obesity_data_age_percent_2016%>%
  group_by(`Age(years)`,state)%>%
  ggplot(mapping=aes(x=`Age(years)`,y=Data_Value,fill=`Age(years)`))+
  geom_bar(stat='identity')+
  facet_wrap(~state)+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Obesity Rate by Age Group in every State in 2016",y="Obesity Rate")
  



#Q3(Part3) Obesity Rate by Age and Year

obesity_data_age_allYear1<-obesity_data_age_percent%>%
  group_by(`Age(years)`,YearStart)%>%
  summarize(meanValue=mean(Data_Value))

obesity_data_age_allYear1%>%
  ggplot(mapping=aes(x=`Age(years)`,y=meanValue,fill=`Age(years)`))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Obesity Rate by Age Group",y="Obesity Rate")+
  facet_wrap(~YearStart)



#Q3 (Part4) Physical Activity by Age Group in all States
df_edu <- df_wrangle %>% filter(year == 2016, StratificationCategory1 == grouping) 

df_edu_weight <- df_edu %>% filter(Class == 'Obesity / Weight Status')

df_edu_habits <- df_edu %>% filter(Class == 'Physical Activity')

df_edu_habits_w <- df_edu_habits %>% na.omit()
ed_habits_join <- left_join(map, df_edu_habits_w)
ed_habits_join %>% 
  ggplot(mapping = aes(x = x, y = y, fill = Data_Value, group=group)) +
  geom_polygon() +
  theme_map() +
  coord_equal()  + 
  labs(title = 'Physical Activity by Age Group and State')+
  labs(fill = 'No Exercise Percentage') + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_distiller(palette = 'Set1') + facet_wrap(~Stratification1)

```

## Correlation of Obesity with Median Household Income of Different Races
```{r obesity-race,message=FALSE, warning=FALSE }


#This code chunk grabs the data related to obesity among different race from the csv file and shows obesity rate in different races in bar charts taking different factors like State, year into account.

#And then it takes data from another csv file related to median household income of different races. And then we derive a conclusion about a negative correlation between them.

# Q4) How does obesity impact people of different races? 
#Why obesity impacts people of different race ?

grouping <- 'Race/Ethnicity'


obesity_data_race<-obesity_data%>%
  filter(StratificationCategory1== grouping)

obesity_data_race<-obesity_data_race%>%
  filter(Class=="Obesity / Weight Status")

obesity_data_race1<-obesity_data_race%>%
  select(YearStart, LocationAbbr,Class,Question,Data_Value,`Race/Ethnicity`,GeoLocation,StratificationCategory1,Stratification1,LocationID,Sample_Size)

obesity_data_race_filter<-obesity_data_race1%>%
  na.omit()%>%
  filter(`Race/Ethnicity`!="2 or more races"& `Race/Ethnicity`!="Other")%>%
  rename(state=LocationAbbr)%>%
  arrange(state)

obesity_data_race_filter<-obesity_data_race_filter%>%
  filter(state!="US"& state!="PR", state!="VI"& state!="GU")%>%
  filter(Question=="Percent of adults aged 18 years and older who have obesity")


obesity_data_race_filter_2016<-obesity_data_race_filter%>%
  filter(YearStart==2016)

#Q4(Part1) Obesity by average rate in all states in 2016
obesity_data_race_average_2016<-obesity_data_race_filter_2016%>%
  group_by(`Race/Ethnicity`)%>%
  summarize(meanValue=mean(Data_Value))

obesity_data_race_average_2016%>%
  ggplot(mapping=aes(x=`Race/Ethnicity`,y=meanValue,fill=`Race/Ethnicity`))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Obesity by Race in 2016",y="Obesity Percent")

#Q4(Part2) Obesity by Race and State in 2016
obesity_data_race_filter_2016%>%
  group_by(`Race/Ethnicity`,state)%>%
  ggplot(mapping=aes(x=`Race/Ethnicity`,y=Data_Value,fill=`Race/Ethnicity`))+
  geom_bar(stat='identity')+
  facet_wrap(~state)+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Obesity by Race among States in 2016",y="Obesity Percent")



#Q4(Part3) Obesity by Race and Year
obesity_data_race_average<-obesity_data_race_filter%>%
  group_by(`Race/Ethnicity`,YearStart)%>%
  summarize(meanValue=mean(Data_Value))

obesity_data_race_average%>%
  ggplot(mapping=aes(x=`Race/Ethnicity`,y=meanValue,fill=`Race/Ethnicity`))+
  geom_bar(stat='identity')+
  facet_wrap(~YearStart)+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Obesity by Race and Year",y="Obesity Percent")



#Q4(Part4) Bar chart to visualize the median household income of different Race/Ethnicity
data<-read_csv("data/median_household_income.csv")

#wrangling the dataset to get the average median household income of different race in different years
income_race<-data%>%
  select(-api_est,-api_se)%>%
  filter(year>=2011 & year<=2016)%>%
  select(name,year,total_est,aian_est,asian_est,black_est,hisp_est,nhopi_est,white_est)%>%
  rename("state"="name")%>%
  rename("Total Pop Income"="total_est")%>%
  rename("American_Indian and Alaska_Native"="aian_est")%>%
  rename("Asian"="asian_est")%>%
  rename("Black"="black_est")%>%
  rename("Hispanic"="hisp_est")%>%
  rename("White"="white_est")%>%
  rename("Native Hawaiian and Pacific Islander"="nhopi_est")%>%
  select(-"Total Pop Income")%>%
  pivot_longer("American_Indian and Alaska_Native":White,names_to="Race",values_to="Median Household Income")%>%
  na.omit() %>% 
  group_by(Race,year) %>% 
  summarize(average_median_income=mean(`Median Household Income`))
  

#Bar chart that shows the Median income of different races in different Year
income_race%>%
  group_by(Race)%>%
  ggplot(mapping=aes(x=Race,y=average_median_income,fill=Race))+
  geom_bar(stat='identity')+
  facet_wrap(~year)+
  theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(title="Average Household Median Income by Race and Year",y="Average Median Income")

```

```{r miss-state-map,message=FALSE, warning=FALSE}

us_counties<-us_map(region="counties")

us_counties<-us_counties%>%
  rename(long=x,
         lat=y,
         state=abbr)

df_ms<-df_wrangle%>%
  filter(abbr=="MS")%>%
  filter(StratificationCategory1 == "Education")%>%
  filter(Class=="Obesity / Weight Status")%>%
  filter(Question == 'Percent of adults aged 18 years and older who have obesity')
  
df_dc<-df_wrangle%>%
  filter(abbr=="DC")%>%
  filter(StratificationCategory1 == "Education")%>%
  filter(Class=="Obesity / Weight Status")%>%
  filter(Question == 'Percent of adults aged 18 years and older who have obesity')



```
