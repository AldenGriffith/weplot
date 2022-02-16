library(tidyverse)

load("~/Wellesley Courses/ES 220/ES 220 S22/Labs/04/Code/Lab 4 Data - Jan 6 2021.RData")

setwd("~/GitHub/weplot")
Glacier <- read_csv("Glacier Data Stacked.csv")
Hub <- read_csv("Hubbard Brook.csv")

weplot(x = Glacier$Site, y = Glacier$Size2, type = "boxplot", give.data = FALSE)

weplot(data = Glacier, x = "Site", y = "Size2",
       group = "Location", type = "bar", error = "se")

weplot(data = Glacier, x = "Site", y = Size2,
       group = "Location", type = "bar", error = "se")


weplot(x = Elec.Observed.W, type = "point")

weplot(x = Hub$Date, y = list(Hub$`Precip pH`, Hub$`Stream pH`))


weplot(x = Elec.Observed.W, type = "hist")

weplot(x = Hour, y = list(Elec.Observed.W, Solar.Radiation.Wm2),
       type = "point")


weplot(x = Date, y = Solar.Radiation.Wm2, type = "point")


weplot(x = Date, y = Solar.Radiation.Wm2, xlim = as.POSIXct(c("2012-01-01 00:00 UTC", "2012-01-02 00:00 UTC")))

d <- weplot(x = Date[1:26], y = Solar.Radiation.Wm2[1:26], give.data =TRUE)

weplot(x = Date, y = Solar.Radiation.Wm2, xlim = as.POSIXct(c("2012-01-01", "2012-04-01")))

ggplot(d, aes(x = X, y = Y)) +
  geom_point()

args <- c("Hour", "list(Elec.Observed.W, Solar.Radiation.Wm2)")
arg.names <- c("x", "y")



weplot(data = Glacier, x = Size1, y = log(Size2+300-50))
args <- c("Size1", "log(Size2 + 300)", "Glacier")

#ag adds spaces:  +, *, -
#no space /, ^

D <- weplot(x = Hub$`Precip pH`, y = Hub$`Stream pH`, give.data = TRUE)
weplot(data = Hub, x = "Precip pH", y = (`Stream pH`) + 400)

weplot(x = Hub$`Precip pH`, y = Hub$`Stream pH` + 200)

weplot(data = Hub, x = `Precip pH`, y = `Stream pH`)
weplot(data = Hub, x = "Precip pH", y = "Stream pH")

weplot(data = Hub, x = "Precip pH", y = log(`Stream pH`))
args <- c("Precip pH"   ,   "log(`Stream pH`)" ,"Hub"  )


weplot(data = Hub, x = log(`Precip pH`), y = log("Stream pH"))

args <- c("`Precip pH`", "`Stream pH`", "Hub")

arg.names <- c("x", "y", "data")
# args <- c("Date", "Precip pH", "Hub")
args <- c("Precip pH", 'log("Stream pH")', "Hub")



weplot(x = Glacier$Site, y = Glacier$Size2, type = "bar", error = "se",
       group = Glacier$Plot)


weplot(x = Glacier$Site, y = Glacier$Size2, size = 1, group = Glacier$Plot)


weplot(Glacier$Size2, type = "area", color = "green", edge.color = "red", size = 0,
       group = "Location")

ggplot(data = Glacier, aes(x = Size2)) +
  geom_histogram(color = "black", fill = "green")


ggplot(data = Glacier, aes(x = Site, y = Size2)) +
  geom_bar(stat = "summary", fun = mean)



ggplot(data = Glacier, aes(x = Site, y = Size2)) +
  geom_point(size = 1.5) +
  labs(title = "1.5")
