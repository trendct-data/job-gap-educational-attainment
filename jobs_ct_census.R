# Charting Connecticut's job changes over time by educational attainment
# Using the Census API

library(dplyr)
library(tidyr)
library(extrafont)
library(ggalt)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(gridExtra)
library(grid)

library("censusapi")

source("keys.R")
vars2014 <- listCensusMetadata(name="acs5", vintage=2014, "v")
View(vars2014)


# B23006_001E - Total
# B23006_002E - Less than high school graduate
# B23006_009E - High school graduate (includes equivalency)
# B23006_016E - Some college or associate's degree
# B23006_023E - Bachelor's degree or higher

# Replace `census_key` with your individual API key

data2009 <- getCensus(name="acs5", 
                      vintage=2009,
                      key=census_key, 
                      vars=c("NAME", "B23006_002E",
                             "B23006_009E", "B23006_016E", "B23006_023E"), 
                      region="state:*")

data2009$state <- 2009
colnames(data2009) <- c("state", "year", "Less than high school graduate", "High school graduate", "Some college or associate's degree", "Bachelor's degree or higher")
data2009$`High school or less` <- data2009$`High school graduate` + data2009$`Less than high school graduate`
data2009[,3] <- NULL
data2009[,3] <- NULL

data2009 <- data2009 %>%
  gather("category", "total", 3:5)


data2010 <- getCensus(name="acs5", 
                      vintage=2010,
                      key=census_key, 
                      vars=c("NAME", "B23006_002E",
                             "B23006_009E", "B23006_016E", "B23006_023E"), 
                      region="state:*")
data2010$state <- 2010
colnames(data2010) <- c("state", "year", "Less than high school graduate", "High school graduate", "Some college or associate's degree", "Bachelor's degree or higher")
data2010$`High school or less` <- data2010$`High school graduate` + data2010$`Less than high school graduate`
data2010[,3] <- NULL
data2010[,3] <- NULL

data2010 <- data2010 %>%
  gather("category", "total", 3:5)


data2011 <- getCensus(name="acs5", 
                      vintage=2011,
                      key=census_key, 
                      vars=c("NAME", "B23006_002E",
                             "B23006_009E", "B23006_016E", "B23006_023E"), 
                      region="state:*")
data2011$state <- 2011
colnames(data2011) <- c("state", "year", "Less than high school graduate", "High school graduate", "Some college or associate's degree", "Bachelor's degree or higher")
data2011$`High school or less` <- data2011$`High school graduate` + data2011$`Less than high school graduate`
data2011[,3] <- NULL
data2011[,3] <- NULL

data2011 <- data2011 %>%
  gather("category", "total", 3:5)



data2012 <- getCensus(name="acs5", 
                      vintage=2012,
                      key=census_key, 
                      vars=c("NAME", "B23006_002E",
                             "B23006_009E", "B23006_016E", "B23006_023E"), 
                      region="state:*")
data2012$state <- 2012
colnames(data2012) <- c("state", "year", "Less than high school graduate", "High school graduate", "Some college or associate's degree", "Bachelor's degree or higher")
data2012$`High school or less` <- data2012$`High school graduate` + data2012$`Less than high school graduate`
data2012[,3] <- NULL
data2012[,3] <- NULL

data2012 <- data2012 %>%
  gather("category", "total", 3:5)


data2013 <- getCensus(name="acs5", 
                      vintage=2013,
                      key=census_key, 
                      vars=c("NAME", "B23006_002E",
                             "B23006_009E", "B23006_016E", "B23006_023E"), 
                      region="state:*")

data2013$state <- 2013
colnames(data2013) <- c("state", "year", "Less than high school graduate", "High school graduate", "Some college or associate's degree", "Bachelor's degree or higher")
data2013$`High school or less` <- data2013$`High school graduate` + data2013$`Less than high school graduate`
data2013[,3] <- NULL
data2013[,3] <- NULL

data2013 <- data2013 %>%
  gather("category", "total", 3:5)


data2014 <- getCensus(name="acs5", 
                      vintage=2014,
                      key=census_key, 
                      vars=c("NAME", "B23006_002E",
                             "B23006_009E", "B23006_016E", "B23006_023E"), 
                      region="state:*")

data2014$state <- 2014
colnames(data2014) <- c("state", "year", "Less than high school graduate", "High school graduate", "Some college or associate's degree", "Bachelor's degree or higher")
data2014$`High school or less` <- data2014$`High school graduate` + data2014$`Less than high school graduate`
data2014[,3] <- NULL
data2014[,3] <- NULL

data2014 <- data2014 %>%
  gather("category", "total", 3:5)


data_0914 <- rbind(data2009, data2010, data2011, data2012, data2013, data2014)

# If you want to filter by another state, do so below

ct_data <- data_0914 %>%
  filter(state=="Connecticut")

library(ggplot2)
ggplot(ct_data, aes(x=year, y=total, group=category, color=category)) + geom_line()

ct_data2 <- ct_data %>%
  spread(category, total)

bach_base <- ct_data2[1,3]
hs_base <- ct_data2[1,4]
some_base <- ct_data2[1,5]

ct_data3 <- ct_data2
ct_data3[,3] <- ct_data2[,3] - bach_base
ct_data3[,4] <- ct_data2[,4] - hs_base
ct_data3[,5] <- ct_data2[,5] - some_base
ct_data3 <- ct_data3 %>%
  gather("category", "total", 3:5)
ggplot(ct_data3, aes(x=year, y=total, group=category, color=category)) + geom_line()



ct_colors <- c('#66c2a5', '#8da0cb', '#fc8d62'  )




names(ct_colors) <- unique(ct_data3$category)


gg <- ggplot(ct_data3)
gg <- gg + geom_hline(yintercept = 0)

gg <- gg + geom_line(aes(x=year, y=total, group=category, color=category))
#gg <- gg + geom_rect(aes(xmin=as.numeric(ymd("2007-12-01")), xmax=as.numeric(ymd("2010-01-01")), ymin=-Inf, ymax=Inf))
gg <- gg + annotate("rect", xmin = 2009, xmax =2010, ymin = -Inf, ymax = Inf,
                    alpha = .1)
gg <- gg + scale_color_manual(name="", values=ct_colors)
#gg <- gg + scale_y_continuous(label=percent)
gg <- gg + labs(x=NULL, y="Jobs", title="Job change in recession and recovery in Connecticut",
                subtitle="Connecticut mirrors the rest of the country for education levels.",
                caption="SOURCE: U.S. Census ACS 5-year")
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Black", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + geom_label(data=ct_data3, aes(x=2009, y=40000, label="Recession", hjust=-.1),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + geom_label(data=ct_data3, aes(x=2010, y=40000, label="Recovery", hjust=-.2),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + theme(legend.position="none")
gg <- gg + theme(plot.margin = unit(c(1, 15, 1, 1), "lines"))


gg <- gg + annotation_custom(grob=textGrob("High school or less (-14k)", hjust=.5,
                                           gp=gpar(fontsize=10, 
                                                   col='#8da0cb')),
                             xmin=2015,xmax=2015,
                             ymin=-14053, ymax=-14053)
gg <- gg + annotation_custom(grob=textGrob("Bachelor's degree or higher (48k)", hjust=.50,  
                                           gp=gpar(fontsize=10, 
                                                   col='#66c2a5')),
                             xmin=2015,xmax=2015,
                             ymin=48432, ymax=48432)
gg <- gg + annotation_custom(grob=textGrob("Associate's degree or some college (21k)", hjust=.30,  
                                           gp=gpar(fontsize=10, 
                                                   col='#fc8d62')),
                             xmin=2015,xmax=2015,
                             ymin=20722, ymax=20722)
gg
gb <- ggplot_build(gg)

gt <- ggplot_gtable(gb)

gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)