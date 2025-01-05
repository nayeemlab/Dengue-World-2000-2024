library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue World 2000-2024')
Dengue <- read.csv("Dengue_World_2010-2024.csv")

#Descriptive
describe.by(Dengue$Cases, Dengue$Year)
describe(Dengue$Cases)

YearwiseDC <- aggregate(Dengue$Cases, by=list(Category=Dengue$Year), FUN=sum)
YearwiseDC
describe(YearwiseDC)
summary(YearwiseDC$x)
YearwiseDD <- aggregate(Dengue$Deaths, by=list(Category=Dengue$Year), FUN=sum)
YearwiseDD
describe(YearwiseDD)
summary(YearwiseDD)
sum(YearwiseDD$x)

colnames(YearwiseDC) <- c("Year","DC")
YearwiseDC

colnames(YearwiseDD) <- c("Year","DD")
YearwiseDD

NROW(Dengue)
df2 <- data.frame(Dengue=rep(c("Cases", "Deaths"), each=25),
                  Years=rep(c(YearwiseDC$Year),2),
                  Numbers=c(YearwiseDC$DC,YearwiseDD$DD)+1)

# Change the colors manually
p <- ggplot(data=df2, aes(x=Years, y=Numbers, fill=Dengue, label = Numbers)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                            labels = trans_format("log10", math_format(10^.x))) + 
  geom_bar(position="dodge", stat="identity")+ 
  geom_text(size = 5, hjust = ifelse(df2$Numbers<9000, 0, 2), vjust = ifelse(df2$Numbers<9000, 1, 0),angle = 90, position = "stack")+
  theme_minimal()+  theme_bw() +
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.1, 0.9),
         text = element_text(size = 25))
# Use brewer color palettes
p<- p + scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

tiff("DCDDY.tiff", units="in", width=12, height=8, res=300)

gridExtra::grid.arrange(p, ncol = 1, nrow = 1)
dev.off()


worldDeng <- read.csv("DengWorld2024.csv")

worldDeng$Caselog <- log10(worldDeng$Case+1)
worldDeng$Deathlog <- log10(worldDeng$Death+1)

# COVID2022$location[COVID2022$location == 'United States'] <- 'USA'
# COVID2022$location[COVID2022$location == 'United Kingdom'] <- 'UK'
# COVID2022$location[COVID2022$location == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'
# COVID2022$location[COVID2022$location == 'Congo'] <- 'Republic of Congo'


library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot


worldgovt <- dplyr::select(worldDeng, region = Country, DC = Caselog)
head(worldgovt)


## Make the HDI numeric
worldgovt$DC <- as.numeric(as.character(worldgovt$DC))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldDeng <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = DC)) +
  scale_fill_distiller(palette ="Greens", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Dengue Cases \n(log10)") +
  plain
x <- plot(worldDeng)
x


#Death

worldDeng <- read.csv("DengWorld2024.csv")
worldDeng$Deathlog <- log10(worldDeng$Death+1)

# COVID2022$location[COVID2022$location == 'United States'] <- 'USA'
# COVID2022$location[COVID2022$location == 'United Kingdom'] <- 'UK'
# COVID2022$location[COVID2022$location == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'
# COVID2022$location[COVID2022$location == 'Congo'] <- 'Republic of Congo'


library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#Deaths
worldgovt <- dplyr::select(worldDeng, region = Country, DD = Deathlog)
head(worldgovt)


## Make the HDI numeric
worldgovt$DC <- as.numeric(as.character(worldgovt$DD))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldDeng <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = DD)) +
  scale_fill_distiller(palette ="YlOrRd", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Dengue Deaths \n(log10)") +
  plain
y <- plot(worldDeng)
y


library(gridExtra)
tiff("Dengue2024Map.tiff", units="in", width=6, height=6, res=300)
library(cowplot)
gridExtra::grid.arrange(plot_grid(x, y, labels = "AUTO", ncol = 1, nrow = 2))
dev.off()



Months <- rep(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11),3)
Hemisphere <- c(rep("Northern",11), rep("Southern",11), rep("Total",11))

Value <- c(159077,	122717,	101032,	104737,	154706,	183212,	266860,	274250,	281860,	295896,	140898,
           941177,	1930660,	2648205,	2568422,	1965403,	521583,	232424,	116815,	110148,	121150,	98972,
           1100254,	2053377,	2749237,	2673159,	2120109, 704795,	499284,	391065,	392008,	417046,	239870)

charts.data <- data.frame(Months, Hemisphere, Value)


p3 <- ggplot(charts.data, aes(x = Months, y = log10(Value), color = Hemisphere)) +
  geom_line() +  xlab("Months") + ylab("Dengue cases (log10)") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5),
        text=element_text(size=15))+
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov"))

p3

library(gridExtra)
tiff("CasesHem.tiff", units="in", width=10, height=6, res=300)
gridExtra::grid.arrange(p3, nrow=1, ncol=1)
dev.off()


setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue World 2000-2024')

worldDeng <- read.csv("DengWorld2024.csv")

fit <- glm(DeathPM ~ Old.age + urban + Popdens + Obesity_rate + Diabetes_prevalence + Hypertension + Avg_temp + Tot_Rain, data=worldDeng, family = poisson())
summary(fit)
library(car)
round(exp(fit$coefficients),2)
round(exp(confint(fit)),2)

