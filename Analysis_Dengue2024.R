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

# YearwiseDC <- aggregate(Dengue$Cases, by=list(Category=Dengue$Year), FUN=sum)
# YearwiseDC
# describe(YearwiseDC)
# summary(YearwiseDC$x)
# YearwiseDD <- aggregate(Dengue$Deaths, by=list(Category=Dengue$Year), FUN=sum)
# YearwiseDD
# describe(YearwiseDD)
# summary(YearwiseDD)
# sum(YearwiseDD$x)
# 
# colnames(YearwiseDC) <- c("Year","DC")
# YearwiseDC
# 
# colnames(YearwiseDD) <- c("Year","DD")
# YearwiseDD
# 
# NROW(Dengue)
# df2 <- data.frame(Dengue=rep(c("Cases", "Deaths"), each=25),
#                   Years=rep(c(YearwiseDC$Year),2),
#                   Numbers=c(YearwiseDC$DC,YearwiseDD$DD)+1)
# 
# # Change the colors manually
# p <- ggplot(data=df2, aes(x=Years, y=Numbers, fill=Dengue, label = Numbers)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                                                             labels = trans_format("log10", math_format(10^.x))) + 
#   geom_bar(position="dodge", stat="identity")+ 
#   geom_text(size = 5, hjust = ifelse(df2$Numbers<9000, 0, 2), vjust = ifelse(df2$Numbers<9000, 1, 0),angle = 90, position = "stack")+
#   theme_minimal()+  theme_bw() +
#   theme( legend.title=element_blank(),
#          legend.text = element_text(color = "Black", size = 25), legend.position = c(0.1, 0.9),
#          text = element_text(size = 25))
# # Use brewer color palettes
# p<- p + scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# p
# 
# tiff("DCDDY.tiff", units="in", width=12, height=8, res=300)
# 
# gridExtra::grid.arrange(p, ncol = 1, nrow = 1)
# dev.off()


worldDeng <- read.csv("Dengue_Data_2024.csv")

worldDeng$Caselog <- log10(worldDeng$Cases+1)
worldDeng$Deathlog <- log10(worldDeng$Deaths+1)

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

worldDeng <- read.csv("Dengue_Data_2024.csv")
worldDeng$Deathlog <- log10(worldDeng$Deaths+1)

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



Months <- rep(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11, 12),3)
Hemisphere <- c(rep("Northern",12), rep("Southern",12), rep("Total",12))

Value <- c(0.49729,
           0.405796,
           0.382057,
           0.366869,
           0.504508,
           0.71671,
           1.0488,
           1.220638,
           1.230886,
           1.350467,
           0.988329,
           0.646511,
           
           
           2.309801,
           4.087648,
           6.04945,
           6.127208,
           4.854144,
           1.80341,
           0.881101,
           0.585228,
           0.482874,
           0.536581,
           0.66792,
           0.733404,
           
           
           2.807091,
           4.493444,
           6.431507,
           6.494077,
           5.358652,
           2.52012,
           1.929901,
           1.805866,
           1.71376,
           1.887048,
           1.656249,
           1.379915
           
)

charts.data <- data.frame(Months, Hemisphere, Value)


p3 <- ggplot(charts.data, aes(x = Months, y = Value, color = Hemisphere)) +
  geom_line(size=1) +  xlab("Months") + ylab("Dengue cases (in millions)") + 
  ggtitle("Monthly Global dengue cases by hemisphere (2024)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5),
        text=element_text(size=15))+
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11","12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov","Dec"))

p3





Months <- rep(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11, 12),3)
ClimateZone <- c(rep("Subtropical",12), rep("Tropical",12), rep("Total",12))

Value <- c(0.14848,
           0.197444,
           0.395422,
           0.390997,
           0.239474,
           0.259625,
           0.38991,
           0.485473,
           0.583483,
           0.655086,
           0.403794,
           0.196731,
           
           
           
           2.658611,
           4.296,
           6.036085,
           6.10308,
           5.119178,
           2.260495,
           1.539991,
           1.320393,
           1.130277,
           1.231962,
           1.252455,
           1.183184,
           
           2.807091,
           4.493444,
           6.431507,
           6.494077,
           5.358652,
           2.52012,
           1.929901,
           1.805866,
           1.71376,
           1.887048,
           1.656249,
           1.379915
           
           
)

charts.data <- data.frame(Months, ClimateZone, Value)


p4 <- ggplot(charts.data, aes(x = Months, y = Value, color = ClimateZone)) +
  geom_line(size=1) +  xlab("Months") + ylab("Dengue cases (in millions)") + 
  guides(color = guide_legend(title = "Climate Zone")) +
  ggtitle("Monthly Global dengue cases by Climate Zone (2024)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5),
        text=element_text(size=15))+
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11","12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov","Dec"))

p4

library(gridExtra)
tiff("CasesCliZone.tiff", units="in", width=8, height=8, res=300)
library(cowplot)
gridExtra::grid.arrange(plot_grid(p3, p4, labels = "AUTO", ncol = 1, nrow = 2))
dev.off()


setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue World 2000-2024')

worldDeng <- read.csv("Dengue_Data_2024.csv")


fit <- glm(Cases.M ~ Old.age + urban + Popdens + Obesity_rate + 
              Avg_temp + Tot_Rain + Hemisphere, data=worldDeng, family = poisson())

vif(fit)
summary(fit)
library(car)
round(exp(fit$coefficients),2)
round(exp(confint(fit)),2)

fit <- glm(Deaths.M ~ Old.age + urban + Popdens + Obesity_rate + 
             Avg_temp + Tot_Rain + Hemisphere, data=worldDeng, family = poisson())
vif(fit)
summary(fit)
library(car)
round(exp(fit$coefficients),2)
round(exp(confint(fit)),2)




options(scipen = 999) ## To disable scientific notation
library(data.table) # for melt
library(ggplot2)

# Took the data example from @Istrel
set.seed(2018)

Values <- c(0.080023,	0.106178,	0.395135,	0.078208,	0.079677,	0.166187,	0.379778,	0.08446,	0.124971,	0.206015,	1.0992,
            0.09027,	0.210963,	0.597685,	0.090334,	0.074477,	0.227209,	0.514421,	0.123121,	0.151117,	0.357386,	2.050121,
            0.110044,	0.399804,	0.735043,	0.130417,	0.081449,	0.341659,	0.391,	0.205386,	0.444748,	0.815778,	2.768297,
            0.210804,	0.53011,	0.42597,	0.111638,	0.092428,	0.504686,	0.327885,	0.192091,	0.582905,	0.808498,	2.697818,
            0.130929,	0.305081,	0.206294,	0.145033,	0.096233,	0.673702,	0.186134,	0.154656,	0.522756,	0.77411,	2.145361,
            0.084829,	0.169044,	0.128096,	0.093326,	0.065573,	0.299467,	0.129786,	0.132128,	0.340775,	0.32389,	0.720203,
            0.09194,	0.144899,	0.08379,	0.086037,	0.062269,	0.263768,	0.119593,	0.074367,	0.166851,	0.264192,	0.532631,
            0.07145,	0.102476,	0.103361,	0.076476,	0.06825,	0.258848,	0.07432,	0.064292,	0.156933,	0.365031,	0.430143,
            0.082316,	0.114478,	0.063347,	0.056151,	0.060918,	0.218517,	0.094984,	0.074582,	0.128738,	0.346893,	0.451015,
            0.117901,	0.111475,	0.065712,	0.060306,	0.085406,	0.255836,	0.080621,	0.063728,	0.136563,	0.347346,	0.548492,
            0.071936,	0.115287,	0.078565,	0.070153,	0.07131,	0.176618,	0.073366,	0.071795,	0.165612,	0.371172,	0.379079,
            0.064202,	0.192047,	0.050002,	0.041929,	0.069587,	0.13297,	0.066615,	0.104281,	0.104852,	0.295173,	0.251306
            
            
)


Months <- c(rep("1", 11), 
            rep("2", 11),
            rep("3", 11),
            rep("4", 11),
            rep("5", 11),
            rep("6", 11),
            rep("7", 11),
            rep("8", 11),
            rep("9", 11),
            rep("10", 11),
            rep("11", 11),
            rep("12",11))

Years <- c("2014",	"2015",	"2016",	"2017",	"2018",	"2019",	"2020",	"2021",	"2022",	"2023",	"2024")

df <- data.frame(Values, Months, Years)

df

x  <- ggplot(data = df,
       aes(x = as.factor(sort(Months)), 
           y = Values,
           fill = as.factor(sort(Months)))) +
  geom_col() +
  facet_grid(cols = vars(df$Years),
             space = "free_x",
             scales = "free_x",
             switch = "x") +
    # remove space between plot area and x axis
  labs(x = "Years", y = "Dengue cases (in millions)") + 
  ggtitle("Monthly Global Dengue cases (2014-2024)") +
  ylim(0,3)+ # legend title
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position="none",
    axis.text=element_text(size=12,face="bold"),
    axis.title=element_text(size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
    panel.spacing = unit(0.05, "cm")) + scale_fill_brewer(palette="YlOrRd")



tiff("MGDCiM.tiff", units="in", width=10, height=6, res=300)

gridExtra::grid.arrange(x)
dev.off()












