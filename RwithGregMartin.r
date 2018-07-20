# Setting up the gapminder library
install.packages('gapminder')
library('gapminder')

View(gapminder)

# Summary of the gapminder dataset
summary(gapminder)
mean(gapminder$gdpPercap)
x <- mean(gapminder$gdpPercap)
x

attach(gapminder)


# Total population
total_population <- sum(as.numeric(pop))
median(pop)
hist(lifeExp)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ log(gdpPercap),
     xlab = 'log(GDP)',
     ylab = 'life expectancy')

# Activate library dplyr
library('dplyr')

df2 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == 'South Africa' |
           country == 'Ireland' | country == 'Kenya') %>%
  group_by(country) %>%
  summarise(Average_life = mean(lifeExp))

df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == 'South Africa' |
           country == 'Ireland')

t.test(data = df1, lifeExp ~ country)

install.packages('ggplot2')
library('ggplot2')

gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=year, size=pop))+
  geom_point(alpha=0.3)+
  geom_smooth(method = lm)+
  facet_wrap(~continent)

summary(lm(lifeExp ~ gdpPercap+pop))











