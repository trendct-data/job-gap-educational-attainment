
# Bringing in US data. Curated from http://www.census.gov/cps/data/cpstablecreator.html

us <- read.csv("data/education_jobs_us.csv")

# Cleaning up the columns
us <- us[c("Year","Type","Totals", "Children.under.15","No.high.school.diploma",
           "High.school.or.equivalent","Some.college..less.than.4.yr.degree","Bachelor.s.degree.or.higher" )]

# Formatting the numbers to eliminate the commas, convert to numeric
us$Totals <- gsub(",", "", us$Totals)
us$Totals <- as.numeric(us$Totals)
us$Children.under.15 <- gsub(",", "", us$Children.under.15)
us$Children.under.15 <- as.numeric(us$Children.under.15)
us$No.high.school.diploma <- gsub(",", "", us$No.high.school.diploma)
us$No.high.school.diploma <- as.numeric(us$No.high.school.diploma)
us$High.school.or.equivalent <- gsub(",", "", us$High.school.or.equivalent)
us$High.school.or.equivalent <- as.numeric(us$High.school.or.equivalent)
us$Some.college..less.than.4.yr.degree <- gsub(",", "", us$Some.college..less.than.4.yr.degree)
us$Some.college..less.than.4.yr.degree <- as.numeric(us$Some.college..less.than.4.yr.degree)
us$Bachelor.s.degree.or.higher <- gsub(",", "", us$Bachelor.s.degree.or.higher)
us$Bachelor.s.degree.or.higher <- as.numeric(us$Bachelor.s.degree.or.higher)

# Bringing in data wrangling libraries
library(dplyr)
library(tidyr)

# Years came in an awkward format, so fixing
us$year <- round(us$Year,0)

# Cleaning up and narrowing the data
us_all <- us %>%
  filter(Type=="Employed") %>%
  mutate(hs_or_less=No.high.school.diploma+High.school.or.equivalent, some_college=Some.college..less.than.4.yr.degree) %>%
  select(year, hs_or_less, some_college, Bachelor.s.degree.or.higher)

# Restructuring the data in a way that will make it graphable with ggplot2
us_all2 <- us_all %>%
  gather("type", "employed", 2:4)

library(ggplot2)

# Exploratory chart
ggplot(us_all2, aes(x=as.factor(year), y=employed, group=type, color=type)) + geom_line()


# Change since 2008
us_all <- us_all %>%
  filter(year>2007)

us_all2 <- us_all2 %>%
  filter(year>2007)

# Extracting the 2008 values
hs_base <- us_all$hs_or_less[1]
some_base <- us_all$some_college[1]
bach_base <- us_all$Bachelor.s.degree.or.higher[1]

# Creating new columns finding the difference between each year versus 2008's stat
us_change <- us_all
us_change$hs_or_less <- us_change$hs_or_less- hs_base
us_change$some_college <- us_change$some_college - some_base
us_change$Bachelor.s.degree.or.higher <- us_change$Bachelor.s.degree.or.higher- bach_base

# Exporting optional
# write.csv(us_change, "data/us_change.csv")

# Restructuring the dataframe so it can be charted
us_change <- us_change %>%
  gather("type", "employed", 2:4)

ggplot(us_change, aes(x=as.factor(year), y=employed, group=type, color=type)) + geom_line()

us_change$type <- gsub("Bachelor.s.degree.or.higher", "Bachelor's degree or higher", us_change$type)
us_change$type <- gsub("hs_or_less", "High school or less", us_change$type)
us_change$type <- gsub("some_college", "Associate's degree or some college", us_change$type)

us_colors <- c('#8da0cb', '#fc8d62','#66c2a5'  )

names(us_colors) <- unique(us_change$type)

us_change$employed <- us_change$employed/1000

us_change_hs_2015 <- us_change %>% filter(year==2015 & type=="High school or less")
us_change_hs_2015 <- us_change_hs_2015$employed
us_change_bd_2015 <- us_change %>% filter(year==2015 & type=="Bachelor's degree or higher")
us_change_bd_2015 <- us_change_bd_2015$employed
us_change_sc_2015 <- us_change %>% filter(year==2015 & type=="Associate's degree or some college")
us_change_sc_2015 <- us_change_sc_2015$employed

#loading libraries
library(extrafont)
library(ggalt)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(gridExtra)
library(grid)

# Charting job change since 2008

gg <- ggplot(us_change)
gg <- gg + geom_line(aes(x=year, y=employed, group=type, color=type))
gg <- gg + annotate("rect", xmin = 2008, xmax =2010, ymin = -Inf, ymax = Inf,
                    alpha = .1)
gg <- gg + scale_color_manual(name="", values=us_colors)
gg <- gg + labs(x=NULL, y="Jobs (millions)", title="Job change in recession and recovery in the U.S.",
                subtitle="For the first time, workers with a Bachelor's degree or higher make up a larger share of the workforce (36%) \nthan those with a high school diplomar or less (34%)",
                caption="SOURCE: U.S. Census CPS, Georgetown Center on Education and the Workforce")
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Black", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + geom_label(data=us_change, aes(x=2008, y=max(us_change$employed) - max(us_change$employed)*.15, label="Recession", hjust=-.1),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + geom_label(data=us_change, aes(x=2010, y=max(us_change$employed) - max(us_change$employed)*.15, label="Recovery", hjust=-.2),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + theme(legend.position="none")
gg <- gg + theme(plot.margin = unit(c(1, 15, 1, 1), "lines"))
gg <- gg + annotation_custom(grob=textGrob(paste0("High school or less (", us_change_hs_2015, ")"), hjust=-.3,
                                           gp=gpar(fontsize=10, 
                                                   col='#8da0cb')),
                             xmin=2015,xmax=2015,
                             ymin=us_change_hs_2015, ymax=us_change_hs_2015)
gg <- gg + annotation_custom(grob=textGrob(paste0("Associate's degree or some college (", us_change_sc_2015, ")"), hjust=-.17,
                             gp=gpar(fontsize=10, 
                                     col='#fc8d62')),
xmin=2015,xmax=2015,
ymin=us_change_sc_2015, ymax=us_change_sc_2015)
gg <- gg + annotation_custom(grob=textGrob(paste0("Bachelor's degree or higher (", us_change_bd_2015, ")"), hjust=-.20,  
                             gp=gpar(fontsize=10, 
                                     col='#66c2a5')),
xmin=2015,xmax=2015,
ymin=us_change_bd_2015, ymax=us_change_bd_2015)
gg
gb <- ggplot_build(gg)

gt <- ggplot_gtable(gb)

gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)

##


#us_change <- read.csv("us_change.csv")
us_change <- read.csv("data/us_all_raw.csv")
us_change$Date <- paste0("1-", us_change$Date)
us_change$Date <- dmy(us_change$Date)

colnames(us_change) <- c("Date", "Bachelor's degree or higher", "Associate's degree or some college", "High school or less")

us_long <- mutate(gather(us_change, area, value, -Date),
                      area=factor(area, levels=colnames(us_change)[2:4],
                                  ordered=TRUE))

edu_colors <- c('#66c2a5', '#fc8d62', '#8da0cb')
names(edu_colors) <- colnames(us_change)[2:4]

last_vals <- sapply(colnames(us_change)[2:4], function(x) last(na.exclude(us_change[,x])))
last_date <- tail(us_change$Date)#+1 # doing this ^ wld have made it a double

# jobs change (monthly)

gg <- ggplot(us_long)
gg <- gg + geom_hline(yintercept = 0)
gg <- gg + geom_line(aes(x=Date, y=value, group=area, color=area))
#gg <- gg + geom_rect(aes(xmin=as.numeric(ymd("2007-12-01")), xmax=as.numeric(ymd("2010-01-01")), ymin=-Inf, ymax=Inf))
gg <- gg + annotate("rect", xmin = ymd("2007-12-01"), xmax =ymd("2010-01-01"), ymin = -Inf, ymax = Inf,
                    alpha = .1)

gg <- gg + scale_color_manual(name="", values=edu_colors)
#gg <- gg + scale_y_continuous(label=percent)
gg <- gg + labs(x=NULL, y="Employment change (millions)", title="Job change in recession and recovery in the U.S.",
                subtitle="Workers with more than a high school education gained 11.5 million of the 11.6 million jobs added in the recovery.",
                caption="SOURCE: U.S. Census CPS, Georgetown Center on Education and the Workforce")
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Black", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + geom_label(data=us_long, aes(x=ymd("2008-4-01"), y=5, label="Recession", hjust=0),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + geom_label(data=us_long, aes(x=ymd("2010-4-01"), y=5, label="Recovery", hjust=0),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + theme(legend.position="none")
gg <- gg + theme(plot.margin = unit(c(1, 15, 1, 1), "lines"))
gg


for (i in 1:length(last_vals)) {
  gg <- gg + annotation_custom(grob=textGrob(paste0(names(last_vals)[i], " (", round(last_vals[i],1), ")"), hjust=-.3,
                                             gp=gpar(fontsize=10, 
                                                     col=edu_colors[names(last_vals)[i]])),
                               xmin=as.numeric(last_date[i]),xmax=as.numeric(last_date[i]),
                               ymin=last_vals[i], ymax=last_vals[i])
}

gb <- ggplot_build(gg)
gt <- ggplot_gtable(gb)

gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)