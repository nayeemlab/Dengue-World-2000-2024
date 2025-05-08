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
Dengue <- read.csv("Dengue_World_2014-2024.csv")

#Descriptive
describe.by(Dengue$cases, Dengue$Year)
describe(Dengue$cases)

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
  ggtitle("Global dengue cases by country, 2024") + labs(fill = "Dengue Cases \n(log10)") +
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
  ggtitle("Global dengue deaths by country, 2024") + labs(fill = "Dengue Deaths \n(log10)") +
  plain
y <- plot(worldDeng)
y


library(gridExtra)
tiff("Dengue2024Map.tiff", units="in", width=8, height=6, res=300)
library(cowplot)
gridExtra::grid.arrange(plot_grid(x, y, labels = "AUTO", ncol = 1, nrow = 2))
dev.off()






###########################Continent#########################

worldDeng <- read.csv("Dengue_Data_2024.csv")

worldDeng$Caselog <- log10(worldDeng$Cases_Con+1)
worldDeng$Deathlog <- log10(worldDeng$Deaths_Con+1)

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
  ggtitle("Global dengue cases by continents, 2024") + labs(fill = "Dengue Cases \n(log10)") +
  plain
x <- plot(worldDeng)
x


#Death

worldDeng <- read.csv("Dengue_Data_2024.csv")
worldDeng$Deathlog <- log10(worldDeng$Deaths_Con+1)

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
  ggtitle("Global dengue deaths by continents, 2024") + labs(fill = "Dengue Deaths \n(log10)") +
  plain
y <- plot(worldDeng)
y


library(gridExtra)
tiff("Dengue2024MapCon.tiff", units="in", width=8, height=6, res=300)
library(cowplot)
gridExtra::grid.arrange(plot_grid(x, y, labels = "AUTO", ncol = 1, nrow = 2))
dev.off()



Months <- rep(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11, 12),3)
Hemisphere <- c(rep("Northern",12), rep("Southern",12), rep("Total",12))

Value <- c(0.16297,
           0.128472,
           0.107281,
           0.10876,
           0.160942,
           0.191873,
           0.296476,
           0.311406,
           0.337617,
           0.410619,
           0.239086,
           0.110987,
           
           
           
           0.936783,
           1.922266,
           2.664539,
           2.594878,
           1.993282,
           0.531681,
           0.238109,
           0.120162,
           0.114062,
           0.138769,
           0.144344,
           0.162071,
           
           
           
           1.099753,
           2.050738,
           2.77182,
           2.703638,
           2.154224,
           0.723554,
           0.534585,
           0.431568,
           0.451679,
           0.549388,
           0.38343,
           0.273058
           
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
                              "Oct", "Nov","Dec")) + scale_color_manual(values=c("#999999", "#56B4E9", "#E69F00"))

p3





Months <- rep(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11, 12),3)
ClimateZone <- c(rep("Subtropical",12), rep("Tropical",12), rep("Total",12))

Value <- c(0.074679,
           0.110134,
           0.256663,
           0.22513,
           0.078845,
           0.054154,
           0.108347,
           0.144036,
           0.182052,
           0.225375,
           0.120868,
           0.038988,
           
           
           
           
           1.025074,
           1.940604,
           2.515157,
           2.478508,
           2.075379,
           0.6694,
           0.426238,
           0.287532,
           0.269627,
           0.324013,
           0.262562,
           0.23407,
           
           
           
           1.099753,
           2.050738,
           2.77182,
           2.703638,
           2.154224,
           0.723554,
           0.534585,
           0.431568,
           0.451679,
           0.549388,
           0.38343,
           0.273058
           
           
)

charts.data <- data.frame(Months, ClimateZone, Value)


p4 <- ggplot(charts.data, aes(x = Months, y = Value, color = ClimateZone)) +
  geom_line(size=1) +  xlab("Months") + ylab("Dengue cases (in millions)") + 
  guides(color = guide_legend(title = "Climate Zone")) +
  ggtitle("Monthly Global dengue cases by climate zone (2024)") +
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
                              "Oct", "Nov","Dec")) + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

p4





Months <- rep(c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11, 12),7)
WHORegions <- c(rep("AFR",12), rep("AMR",12),
                rep("EMR",12), rep("EUR",12),
                rep("SEAR",12), rep("WPR",12),
                rep("Total",12))

Value <- c(0.011867,
           0.012995,
           0.008574,
           0.006465,
           0.008307,
           0.007083,
           0.009104,
           0.015953,
           0.019502,
           0.034594,
           0.014111,
           0.008445,
           
           
           1.027728,
           1.990053,
           2.726946,
           2.667496,
           2.10118,
           0.670203,
           0.438884,
           0.308114,
           0.307683,
           0.348461,
           0.255304,
           0.222174,
           
           
           
           
           0.0026,
           0.00268,
           0.002353,
           0.002304,
           0.009124,
           0.004295,
           0.003763,
           0.007369,
           0.009651,
           0.020649,
           0.011738,
           0.005382,
           
           
           0.000036,
           0.000018,
           0.000442,
           0.000344,
           0.00032,
           0.000125,
           0.000006,
           0.000063,
           0.000208,
           0.000037,
           0.00,
           0.00,
           
           
           0.027876,
           0.02096,
           0.015098,
           0.011161,
           0.015228,
           0.021266,
           0.053359,
           0.064087,
           0.083855,
           0.10357,
           0.073076,
           0.016208,
           
           
           0.029646,
           0.024032,
           0.018407,
           0.015868,
           0.020065,
           0.020582,
           0.029469,
           0.035982,
           0.03078,
           0.042077,
           0.029201,
           0.020849,
           
           1.099753,
           2.050738,
           2.77182,
           2.703638,
           2.154224,
           0.723554,
           0.534585,
           0.431568,
           0.451679,
           0.549388,
           0.38343,
           0.273058
           
           
)

charts.data <- data.frame(Months, WHORegions, Value)


p5 <- ggplot(charts.data, aes(x = Months, y = Value, color = WHORegions)) +
  geom_line(size=1) +  xlab("Months") + ylab("Dengue cases (in millions)") + 
  guides(color = guide_legend(title = "WHO Regions")) +
  ggtitle("Monthly Global dengue cases by WHO regions (2024)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"),
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

p5





library(gridExtra)
tiff("CasesCliZone.tiff", units="in", width=8, height=12, res=300)
library(cowplot)
gridExtra::grid.arrange(plot_grid(p3, p4, p5, labels = "AUTO", ncol = 1, nrow = 3))
dev.off()


setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue World 2000-2024')

worldDeng <- read.csv("Dengue_Data_2024.csv")


fit <- glm(Cases.M ~ Old.age + urban + Popdens + Obesity_rate + Avg_temp + Tot_Rain  + AQI + Hemisphere, data=worldDeng, family = poisson())

vif(fit)
summary(fit)
library(car)
round(exp(fit$coefficients),2)
round(exp(confint(fit)),2)

fit <- glm(Deaths.M ~ Old.age + urban + Popdens + Obesity_rate + Avg_temp + Tot_Rain  + AQI + Hemisphere, data=worldDeng, family = poisson())

vif(fit)
summary(fit)
library(car)
round(exp(fit$coefficients),2)
round(exp(confint(fit)),2)


fit <- glm(CFR ~ Old.age + urban + Popdens + Obesity_rate + Avg_temp + Tot_Rain   + AQI + Hemisphere, data=worldDeng, family = poisson())

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

Values <- c(0.080023,	0.106178,	0.395135,	0.078208,	0.079677,	0.166187,	0.379778,	0.08446,	0.124971,	0.206015,	1.099753,
            0.09027,	0.210963,	0.597685,	0.090334,	0.074477,	0.227209,	0.514421,	0.123121,	0.151117,	0.357386,	2.050738,
            0.110044,	0.399804,	0.735043,	0.130417,	0.081449,	0.341659,	0.391,	0.205386,	0.444748,	0.815778,	2.77182,
            0.210804,	0.53011,	0.42597,	0.111638,	0.092428,	0.504686,	0.327885,	0.192091,	0.582905,	0.808498,	2.703638,
            0.130929,	0.305081,	0.206294,	0.145033,	0.096233,	0.673702,	0.186134,	0.154656,	0.522756,	0.77411,	2.154224,
            0.084829,	0.169044,	0.128096,	0.093326,	0.065573,	0.299467,	0.129786,	0.132128,	0.340775,	0.32389,	0.723554,
            0.09194,	0.144899,	0.08379,	0.086037,	0.062269,	0.263768,	0.119593,	0.074367,	0.166851,	0.264192,	0.534585,
            0.07145,	0.102476,	0.103361,	0.076476,	0.06825,	0.258848,	0.07432,	0.064292,	0.156933,	0.365031,	0.431568,
            0.082316,	0.114478,	0.063347,	0.056151,	0.060918,	0.218517,	0.094984,	0.074582,	0.128738,	0.346893,	0.451679,
            0.117901,	0.111475,	0.065712,	0.060306,	0.085406,	0.255836,	0.080621,	0.063728,	0.136563,	0.347346,	0.549388,
            0.071936,	0.115287,	0.078565,	0.070153,	0.07131,	0.176618,	0.073366,	0.071795,	0.165612,	0.371172,	0.38343,
            0.064202,	0.192047,	0.050002,	0.041929,	0.069587,	0.13297,	0.063072,	0.104281,	0.104852,	0.295173,	0.273058
            
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
    panel.spacing = unit(0.05, "cm"))

x

tiff("MGDCiM2.tiff", units="in", width=10, height=6, res=300)

gridExtra::grid.arrange(x)
dev.off()





library(ggplot2)

df2 <- data.frame(Countries=rep(c("Bangladesh", "Burkina Faso", "Peru", "Guatemala", "Ecuador",
                             "Panama", "India", "Thailand", "Honduras", "Nepal", "Mexico",
                             "Colombia", "Guyana", "Argentina", "Sri Lanka", "Bolivia",
                             "Malaysia", "Brazil", "Paraguay", "Pakistan")),
                  CFR=c(0.525,
                        0.255,
                        0.125,
                        0.120,
                        0.114,
                        0.107,
                        0.106,
                        0.097,
                        0.092,
                        0.083,
                        0.082,
                        0.070,
                        0.067,
                        0.066,
                        0.056,
                        0.055,
                        0.055,
                        0.053,
                        0.042,
                        0.041
                  ))

head(df2)

f <- ggplot(df2, aes(x = CFR, y = reorder(Countries, CFR)))

# Label inside bars, vjust = 1.6
f <- f + geom_col(fill = "#0073C2FF")+
  geom_text(aes(label = CFR), vjust = 0.4, hjust = 1.1, color = "black") +
  labs(y = "Countries", x = "CFR (%)") + 
  ggtitle("Top 20 Countries with Highest CFR from 2022 to 2024") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position="none",
    axis.text=element_text(size=12,face="bold"),
    axis.title=element_text(size=14,face="bold"),
    plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
    panel.spacing = unit(0.05, "cm"))
f



tiff("Top20CFR.tiff", units="in", width=10, height=6, res=300)

gridExtra::grid.arrange(f)
dev.off()




