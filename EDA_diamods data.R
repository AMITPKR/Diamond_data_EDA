## Exploratory data Analysis in R
library(ggplot2)
library(tibble)
library(dplyr)
?diamonds
## summary of data, various functions
glimpse(diamonds)
summary(diamonds)
str(diamonds)
skimr::skim(diamonds)
head(diamonds)
tail(diamonds)
##Visualization --->>
## categorical variables
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut))

diamonds %>%
  count(cut)
table(diamonds$cut)
unique(diamonds$cut)
## comparison of variables
table(diamonds$cut,diamonds$clarity)

diamonds %>%
  count(cut,clarity) %>%
  arrange(n)
ggplot(diamonds,aes(x=cut,fill=clarity))+
  geom_bar(position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(diamonds,aes(x=cut,fill=clarity))+
  geom_bar(position = "dodge")+
  coord_flip()

ggplot(diamonds,aes(x=cut,fill=clarity))+
  geom_bar(position= "fill")+
  ylab("proportion")

prop.table(table(diamonds$cut,diamonds$clarity))

ggplot(diamonds,aes(x=cut))+
  geom_bar()+
  facet_wrap(~clarity)

##Continuous variable
ggplot(diamonds)+
  geom_histogram(aes(x=carat),binwidth = 0.1)

diamonds %>%
  count(cut_width(carat,0.5))
smaller<-diamonds %>%
  filter(carat<3)
ggplot(data = smaller,aes(x=carat))+
  geom_histogram(binwidth = 0.1)

ggplot(diamonds,aes(x=carat,color=cut))+
  geom_freqpoly(binwidth=0.1)

ggplot(diamonds)+
  geom_histogram(aes(x=y),binwidth =0.5 )+
  coord_cartesian(ylim = c(0,50))
diamonds %>%
  filter(y>3 | y<20) %>%
  select(price,x,y,z) %>%
  arrange(y)

diamonds %>%
  filter(between(y,3,20))
diamonds %>%
  mutate(y=ifelse(y<3|y>20,NA,y)) %>%
  ggplot(aes(x=x,y=y))+
  geom_point()

ggplot(diamonds,aes(x=x,y=y))+
  geom_point()

## co-variation-->>categorical data vs continuous data

ggplot(diamonds,aes(x=cut,y=price))+
  geom_boxplot()+
  coord_flip()

diamonds %>%
  count(color,cut) %>%
  ggplot(aes(x=color,y=cut))+
  geom_tile(aes(fill=n))

ggplot(diamonds)+
  geom_point(aes(x=carat,y=price))

ggplot(diamonds)+
  geom_bin2d(aes(x=carat,y=price))

  