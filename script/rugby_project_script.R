# load the relevant r packages

library(tidyverse) # contains useful packages such as ggplot2
library(knitr) # to knit the RMarkdown file
library(extrafont) # to change my title and axis title fonts
library(plotly) # for rollover effect
library(here) # to indicate file locations in working directory
library(png) # to save visualisations as png files
library(webshot) # to save my plotly plot as a .html

# load the dataset into r 

data1 <- read_csv(here("data/international_rugby_fixtures_dataset.csv"))

# tidy the dataset before making a plot

# remove fixtures from before rugby became professional on 26/8/1995 (using the subset function)

specific_date <- as.Date("1995-08-26")
data <- subset(data1, date >= specific_date)

# remove fixtures from any World Cup
data <- data[data$world_cup == FALSE, ]

# remove fixtures played at a neutral ground

data <- data[data$neutral == FALSE, ]

# remove columns not relevant to analysis 

data <- select(data, -competition, -neutral, -world_cup)

# create column of home wins with a value of 'Yes' if the home team won and 'No' if the away team won 

data <- data %>% 
  mutate(home_win = if_else(home_score > away_score, "Yes", "No"))

# create column of away wins with a value of 'Yes' if the away team won and 'No' if the home team won

data <- data %>%
  mutate(away_win = if_else(away_score > home_score, "Yes", "No"))

# calculate the number of home wins for each country

home_win_counts <- data %>%
  group_by(home_team) %>%
  summarise(home_wins = sum(ifelse(home_win == "Yes", 1, 0)))

# calculate the number of away wins for each country 

away_win_counts <- data %>%
  group_by(away_team) %>%
  summarise(away_wins = sum(ifelse(away_win == "Yes", 1, 0)))

# add the away win number from the away_win_count dataframe into the home_win_count dataframe

home_win_counts$away_wins <- away_win_counts$away_wins

# rename final dataframe and column headings

final_data <- rename(home_win_counts, country = home_team)

# reshape data from wide to long format for ggplot2

final_data_long <- final_data %>%
  pivot_longer(cols = c(home_wins, away_wins),
               names_to = "win_type",
               values_to = "count")

# plot the bar graph

p <- ggplot(final_data_long, aes(x = country, y = count, fill = forcats::fct_rev(win_type))) + # reorder bars 
  geom_bar(stat = "identity", position = "dodge", color = "black") + # position = "dodge" clusters bars as opposed to stacking them
  
  # add x and y axis labels 
  labs(y = "Number of Wins From 1995-2023",
       x = "Nation", #add y axis label
       fill = "Win Type") + 

  # add a title 
  ggtitle("Do Any Top 10 Rugby Union Nations Have a Home Advantage?") + 
  
  # add a subtitle
  
  labs(subtitle = "The Number of Home and Away Wins for Each Nation from 1995-2023") +
  
  # change size of plot title
  # align title centrally
  # make title bold 
  # change font
  theme_minimal() +
  theme(plot.title = element_text(size = 17, hjust = 0.5, face = "bold", family = "Times New Roman"),
  
  # change size of subtitle 
  # align subtitle centrally
  # change font
        
        plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Times New Roman"),
        
  # set legend to right of graph
  # add border in black
  # set border width
  # centre align title
        legend.position = "right",
        legend.box.background = element_rect(color = "black", linewidth = 1),
  
  # remove gridlines from graph 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
  
  # add x and y axis lines
  # make the colour black
  # set axis linewidth
        axis.line = element_line(color = "black", linewidth = 0.5),
  
  # add x and y axis ticks for each value
  # make the colour black
  # set tick size
        axis.ticks = element_line(color = "black", linewidth = 0.5),
  
  # set the size of country names on x axis, 
  # adjust the text angle so no names overlap
  # set alignment so country names are below axis line
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1, family = "Times New Roman"),
  
  #change font of numbers on y axis
        axis.text.y = element_text(size = 10, family = "Times New Roman"),
  
  # add solid border around entire plot in black
        panel.border = element_rect(color = "black", linetype = "solid", fill = "NA"), 
  
  # make axis titles bigger
  # change fonts
        axis.title.x = element_text(size = 14, family = "Times New Roman"), 
        axis.title.y = element_text(size = 14, family = "Times New Roman")) +
  
  # adjust my y axis range to include maximum value
  # set y axis ticks for every value
  # lower bars to align with y axis lower limit as previously hovering above 0 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110), breaks = seq(0, 110, by = 5)) +
  
  # set legend title
  guides(fill = guide_legend(title = "Win Type")) +
  
  # centre align title
  # change title and labels font and size
  # make whole legend larger
  theme(legend.title = element_text(hjust = 0.5, family = "Times New Roman", size = 11), 
        legend.text = element_text(family = "Times New Roman", size = 9), 
        legend.key.size = unit(1, "lines"), 
        legend.key.height = unit(2, "lines")) +
  
  #change colour of bars
  scale_fill_manual(values = c("home_wins" = "#50b47b", "away_wins" = "#8650a6"))

# change my x axis labels to the kit colours of the countries
# change font

label_colours <- c("#43A1D5", "#f0af00", "#b5282f", "#0000c0", "#00845c", "#0052b1", "black", "navy", "darkgreen", "#ee2922")

p <- p +
  theme(axis.text.x = element_text(color = label_colours, family = "Times New Roman"))

print(p)

# save plot

ggsave(here("figures", "viz230170357.png"), plot = p, width = 7.3, height = 5, units = "in")

# convert ggplot2 plot to plotly for rollover effect

p2 <- ggplotly(p)

# reformat plot

p2 <- p2 %>%
  
  # format title
  layout(
    title = list(
      text = "Do Any Top 10 Rugby Union Nations Have a Home Advantage?", 
      font = list(size = 17, hjust = 0.5, family = "Times New Roman", face = "bold")),
    
  # format legend
    legend = list(
      title = list(
        text = "Win Type",
        font = list(size = 14, family = "Times New Roman", color = "black"),
        x = 0.5 
      )),
  
  # format x axis
    xaxis = list(
       title = "Nation", 
       family = "Times New Roman",
       titlefont = list(
         family = "Times New Roman",
         size = 14),
       tickfont = list(
       size = 9, 
       family = "Times New Roman", 
       face = "bold")),
  
  #format y axis
     yaxis = list(
       title = "Number of Wins From 1995-2023", 
       titlefont = list(
         family = "Times New Roman",
         size = 14),
       tickfont = list(
         size = 10, 
         family = "Times New Roman", 
         face = "bold")))

print(p2)


saveWidget(p2, file = here("figures", "plotly.html"))


