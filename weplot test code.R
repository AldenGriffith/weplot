


setwd("~/GitHub/weplot")
Glacier <- read_csv("Glacier Data Stacked.csv")
Hub <- read_csv("Hubbard Brook.csv")

weplot(x = Glacier$Site, y = Glacier$Size2, type = "boxplot", give.data = FALSE)

weplot(data = Glacier, x = "Site", y = "Size2",
       group = "Location", type = "bar", error = "se")

weplot(data = Glacier, x = "Site", y = Size2,
       group = "Location", type = "bar", error = "se")



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
