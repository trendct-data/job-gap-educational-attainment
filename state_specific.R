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
states <- read.csv("data/education_jobs.csv")

#states <- states[c("Year","Type","Totals", "Children.under.15","No.high.school.diploma",
  #         "High.school.or.equivalent","Some.college..less.than.4.yr.degree","Bachelor.s.degree.or.higher" )]

states$Totals <- gsub(",", "", states$Totals)
states$Totals <- as.numeric(states$Totals)
states$Children..armed..or.not.in.labor.force <- gsub(",", "", states$Children..armed..or.not.in.labor.force)
states$Children..armed..or.not.in.labor.force <- as.numeric(states$Children..armed..or.not.in.labor.force)
states$Employed <- gsub(",", "", states$Employed)
states$Employed <- as.numeric(states$Employed)
states$Unemployed <- gsub(",", "", states$Unemployed)
states$Unemployed <- as.numeric(states$Unemployed)

states_filtered <- states %>%
  select(Year, Type, AL, Employed, Unemployed) %>%
  gather("Employment", "Estimate", 4:5) %>%
  filter(Type=="Bachelor's degree or higher" |  Type == "High school or equivalent") %>%
  filter(Employment=="Employed") %>%
  filter(AL!="Totals")

# facets of all state employment over time
ggplot(states_filtered, aes(x=as.factor(Year), y=Estimate, group=Type, color=Type)) + geom_line() + facet_wrap(~AL, ncol=4)

# Hm... looks off...

# NEW

ct_only <- read.csv("data/ct_only.csv")
ct_only$Employed <- gsub(",", "", ct_only$Employed)
ct_only$Employed <- as.numeric(ct_only$Employed)

ggplot(ct_only, aes(x=as.factor(Year), y=Employed, group=State..CT, color=State..CT)) + geom_line()

ct_all <- ct_only %>%
  filter(Year>2007)


hs_base <- ct_all$Employed[1]
bach_base <- ct_all$Employed[2]

ct_all <- unique(ct_all)

ct_change <- ct_all %>%
  spread(State..CT, Employed)

colnames(ct_change) <- c("year", "Bachelor.s.degree.or.higher", "hs_or_less")
ct_change$hs_or_less <- ct_change$hs_or_less- hs_base
#ct_change$some_college <- ct_change$some_college - some_base
ct_change$Bachelor.s.degree.or.higher <- ct_change$Bachelor.s.degree.or.higher- bach_base


# after csv alteration

change <- read.csv("data/change.csv")
ggplot(change, aes(x=as.factor(Year), y=Employed, group=Type, color=Type)) + geom_line()


#ct_colors <- c('#8da0cb', '#fc8d62','#66c2a5'  )
ct_colors <- c( '#66c2a5', '#8da0cb'  )

names(ct_colors) <- unique(change$Type)


gg <- ggplot(change)
gg <- gg + geom_hline(yintercept = 0)

gg <- gg + geom_line(aes(x=Year, y=Employed, group=Type, color=Type))
#gg <- gg + geom_rect(aes(xmin=as.numeric(ymd("2007-12-01")), xmax=as.numeric(ymd("2010-01-01")), ymin=-Inf, ymax=Inf))
gg <- gg + annotate("rect", xmin = 2008, xmax =2010, ymin = -Inf, ymax = Inf,
                    alpha = .1)
gg <- gg + scale_color_manual(name="", values=ct_colors)
#gg <- gg + scale_y_continuous(label=percent)
gg <- gg + labs(x=NULL, y="Jobs (thousands)", title="Job change in recession and recovery in Connecticut",
                subtitle="CT mirrors the rest of the country for education levels. But margin of error for this data is high.",
                caption="SOURCE: U.S. Census CPS")
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Black", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + geom_label(data=change, aes(x=2008, y=49, label="Recession", hjust=-.1),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + geom_label(data=change, aes(x=2010, y=49, label="Recovery", hjust=-.2),
                      family="Helvetica", lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
gg <- gg + theme(legend.position="none")
gg <- gg + theme(plot.margin = unit(c(1, 15, 1, 1), "lines"))
gg <- gg + annotation_custom(grob=textGrob("High school or less (-67)", hjust=-.3,
                                           gp=gpar(fontsize=10, 
                                                   col='#8da0cb')),
                             xmin=2015,xmax=2015,
                             ymin=-67, ymax=-67)
gg <- gg + annotation_custom(grob=textGrob("Bachelor's degree or higher (125)", hjust=-.20,  
                                           gp=gpar(fontsize=10, 
                                                   col='#66c2a5')),
                             xmin=2015,xmax=2015,
                             ymin=125, ymax=125)
gg
gb <- ggplot_build(gg)

gt <- ggplot_gtable(gb)

gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)