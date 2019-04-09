source('config/init.R')
library(reshape2)

bas <- 
  read_csv('Data/brandAmbasPerform.csv') %>% 
  select(-starts_with('X')) %>% 
  mutate(
    date = as.Date(date, '%m/%d/%y')
  ) %>% 
  melt(id.var='date') %>% 
  as_tibble() %>% 
  mutate(
    variable=as.character(variable)
    ,group=substr(variable, nchar(variable), nchar(variable)) %>% as.factor()
    ,variable=substr(variable, 1, nchar(variable)-2) %>% as.factor()
  ) %>% 
  filter(variable!='total_quantity')

levels(bas$variable) <- 
  c("Actions","Active Users","Reportbacks","Signups","Quantity")

ggplot(bas, aes(x=date, y=value, group=group, color=group)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~variable, scales='free_y')
