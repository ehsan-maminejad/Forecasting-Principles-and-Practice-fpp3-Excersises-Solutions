#----Ex1-----

help(gafa_stock)
help(PBS)
help("vic_elec")
help("pelt")
###a###
pelt%>%
  autoplot(Hare)
vic_elec%>%
  autoplot(Demand)

###c###
EX1 = gafa_stock%>%
  filter(High == Close)%>%
  group_by(Symbol)
EX1%>%
  autoplot(Close)

## ----Ex2 ------

##a##
tute1 <- readr::read_csv("E:/Bussiness Analytics and data scince MBA/Business Forcasting/S2/EX2/tute1.csv")
View(tute1)

##b##
mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

##c##
mytimeseries %>%
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>%
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
  geom_line()+
  facet_grid(vars(Key), scales = "free_y")

# ----Ex3------
##---a---
tourism3 = read_excel("E:/Bussiness Analytics and data scince MBA/Business Forcasting/S2/EX2/tourism.xlsx")
view(tourism3)

##--b--
# Convert tibble to tsibble
tourism3ts = tourism3%>%
  mutate(Quarter = yearmonth(Quarter))%>%
  as_tsibble(index = Quarter,key = c(Region, State, Purpose))
view(tourism3ts)

##--c--
ex3c = tourism3ts%>%
  select(-State)%>%
  group_by(Region,Purpose)%>%
  summarise(Trips = mean(Trips))%>%
  ungroup()%>%
  filter(Trips==max(Trips))

##--d--
ex3d = tourism3ts%>%
  group_by(State)%>%
  summarise(total_trips = sum(Trips))%>%
  ungroup()
view(ex3d)

#----Ex4----

aus_production%>%
  autoplot(Bricks)

pelt%>%
  autoplot(Lynx)

gafa_stock%>%
  autoplot(Close)

vic_elec%>%
  autoplot(Demand)+
  labs(
    y = "Demand (GW)",
    title = "Half-hourly electricity demand"
  )

#-----Ex5----
aus_arrivals%>%
  autoplot(Arrivals)

aus_arrivals%>%
  gg_season(Arrivals)

aus_arrivals%>%
  gg_subseries(Arrivals)












#-----Ex6----
set.seed(12345) #select seed point for sample function(when you want the same sample nubmer...
#you should choose the same seed number)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries%>%
  autoplot(Turnover)

myseries%>%
  gg_season(Turnover)

myseries%>%
  gg_subseries(Turnover)

myseries%>%
  gg_lag(Turnover,geom = "point")

myseries%>%
  ACF(Turnover)%>%
  autoplot()


#----Ex7----
set.seed(123) #select seed point for sample function(when you want the same sample nubmer...
#you should choose the same seed number)
myseries7 <- us_employment %>%
  filter(`Series_ID` == sample(us_employment$`Series_ID`,1))

myseries7%>%
  autoplot(Employed)

myseries7%>%
  gg_season(Employed)

myseries7%>%
  gg_subseries(Employed)

myseries7%>%
  gg_lag(Employed,geom = "point")

myseries7%>%
  ACF(Employed)%>%
  autoplot()
# ---Bricks from aus_production
aus_production%>%
  autoplot(Bricks )

aus_production%>%
  gg_season(Bricks)

aus_production%>%
  gg_subseries(Bricks)

aus_production%>%
  gg_lag(Bricks,geom = "point")

aus_production%>%
  ACF(Bricks)%>%
  autoplot()

# ---Hare from pelt
pelt%>%
  autoplot(Hare)

pelt%>%
  gg_season(Hare)

pelt%>%
  gg_subseries(Hare)

pelt%>%
  gg_lag(Hare,geom = "point")

pelt%>%
  ACF(Hare)%>%
  autoplot()

#---H02 cost from PBS
myseries_H02 = PBS%>%
  filter(ATC2 == "H02")%>%
  mutate(Cost = Cost/1e06)

myseries_H02%>%
  autoplot(Cost)+
  labs( y = "$ million")

myseries_H02%>%
  gg_season(Cost)

myseries_H02%>%
  gg_subseries(Cost)

myseries_H02%>%
  filter(Type == "Co-payments",Concession =="Concessional")%>%
  gg_lag(Cost,geom = "point")

myseries_H02%>%
  filter(Type == "Safety net",Concession =="Concessional")%>%
  gg_lag(Cost,geom = "point")

myseries_H02%>%
  filter(Type == "Co-payments",Concession =="Concessional")%>%
  ACF(Cost)%>%
  autoplot()

myseries_H02%>%
  filter(Type == "Safety net",Concession =="Concessional")%>%
  ACF(Cost)%>%
  autoplot()


#-----Ex8----
# 1-B / 2-A / 3-D / 4-C


#----Ex9----
aus_livestock%>%
  filter(Animal == 'Pigs',State == "Victoria", year(Month)>=1990 & year(Month)<=1995)%>%
  autoplot(Count)

aus_livestock%>%
  filter(Animal == 'Pigs',State == "Victoria", year(Month)>=1990 & year(Month)<=1995)%>%
  ACF(Count)%>%
  autoplot()

aus_livestock%>%
  filter(Animal == 'Pigs',State == "Victoria", year(Month)>=1990 & year(Month)<=2007)%>%
  ACF(Count)%>%
  autoplot()
#----Ex10----
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog%>%
  autoplot(diff)

dgoog%>%
  ACF(diff)%>%
  autoplot()
