#Tidy ordenar
#weather = as_data_frame(tidyr::weather)

weather1 <- weather %>% 
  gather(d1:d31, key = "day", value = "day_value", na.rm = TRUE)

weather1 %>% 
  distinct(id)
  
weather2 <- weather1 %>% 
  select(-id, -year)

weather3 <- weather2 %>% 
  mutate(day = as.numeric(stringr::str_replace(day, "d", ""))) %>% 
  arrange(day)

weather3<-weather3%>%
  arrange(month,day)

weather4<-weather3%>%
  spread(element,day_value)

weather4%>%
  group_by(month)%>%
  summarise(MAX=max(TMAX),MIN=min(TMIN))%>%
  ggplot() +
  geom_bar(aes(factor(month), MAX),fill="red", stat="identity") +
  geom_bar(aes(factor(month), MIN),fill="blue", stat="identity")
  

ggplot(weather4, aes(day, month)) + 
  geom_tile(aes(fill = TMAX), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue")
  



