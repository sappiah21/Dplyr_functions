install.packages("tidyverse")
library("tidyverse")


install.packages("gapminder")

library(gapminder)

as_tibble(gapminder) #work the same as the traditional data.frame. They are data frames, but they make life more easier. 
vignette("tibble")

as_tibble(gapminder)

my_data <- gapminder %>%  
  view() 


my_data <- gapminder 

view(my_data) 

summary(my_data$lifeExp)

summary(my_data)

head(my_data, n = 10)

tail(my_data, n = 15)





#------------Filter-------------------------
my_data1 <- gapminder %>%
  filter(continent == "Americas") %>% 
  view()



my_data2 <- gapminder %>% 
  filter(country %in% c("choko", "Ghana", "Nigeria", "Chile")) %>% 
  view()



#---------------Mutate functions - ---------------
my_gap <- gapminder %>% 
  mutate(gdp = pop * gdpPercap, life2 = log(lifeExp)) %>%
  view()



#------------------Arrange()-------------------------
my_gap3 <- gapminder %>% 
  arrange(year, country) %>% 
  view()



my_gap3 <- gapminder %>% 
  arrange(desc(year, country)) %>% 
  view()




#-----------------------select verb------------------------
my_gap4 <- gapminder %>% 
  select( yr = year, lifeExp, gdpPercap, everything()) %>% 
  group_by(country) %>% 
  filter(continent %in% c("Asia","Americas")) %>% 
  arrange(yr, country) %>% 
  #select(gdpPercap, everything()) %>% 
  view()


my_gap4 <- gapminder %>% 
  group_by(continent) %>% 
  summarize(n = n()) %>% 
  view()

my_gap4 <- gapminder %>% 
  count(continent)  

view(my_gap4)



#-------------------------------Counting things up---------------------------

my_gap_sum <- gapminder %>%    #How many observations do we have per continent
  group_by(continent == "Asia") %>% 
  #filter(country) %>% 
  summarize( n = n()) %>% 
  view()


#Or

my_gap_sum <- gapminder %>%    #How many observations do we have per continent
  group_by(continent)%>% 
  tally() %>% 
  view()

#More robust
my_gap_sum <- gapminder %>%
  filter(continent =="Asia") %>% #The count function performs more efficient way which also does the grouping and counting
  group_by(continent)%>% 
  
  view()


#-----------------------General summarize function--------------------------------

#mean(), median(), var(), sd(), mad(), min(), max()

my_gap <- gapminder %>% 
  group_by (continent) %>% 
  summarize(ava_lifeExp = max(lifeExp)) %>% 
  view



my_gap <- gapminder %>% 
  
  filter(year %in% c(1962, 2007)) %>%
  
  group_by(continent, year) %>% 
  
  summarize_at(vars(lifeExp, gdpPercap), list(~mean(.), ~median(.))) %>% 
  
  view()


my_gap <- gapminder %>%                     #The minimum and maximum life experience in Africa
  
  filter(continent =="Africa") %>% 
  
  group_by (year) %>% 
  
  summarize(min_lifeEx = min(lifeExp), max_lifeExp = max(lifeExp)) %>% 
  
  view()



africa <- gapminder %>% 
  filter(continent == "Africa" & country %in% c("Ghana", "Nigeria","Canada") ) %>% 
  
  select(year, country, lifeExp) %>% 
  
  group_by(year) %>% 
  
  view()


#------------------Date and Time--------------------

install.packages("lubridate")

library("lubridate")

Sys.Date()              #They both give you today's date
today()

str(Sys.Date())

str(today())

class(Sys.Date())
class(today())


Sys.time()

now()

class(Sys.time())

ymd("2021-09-08")