#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                        NAME  : JUSTIN ONG CHIN IT                      :::#
#:::                        TP    : TP063682                                :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                            Data Importing                              :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Step 1: Place your filepath here
filepath = "C:\\Users\\60115\\OneDrive - Asia Pacific University\\Documents\\APU\\Y2 Sem1\\PFDA\\PFDA Assg\\student.csv"

# Step 2: RUN this code again
stud_data = read.csv(filepath, header = T)
View(stud_data)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                            Installing Packages                         :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggthemes")
install.packages("plotly")
install.packages("tibble")
install.packages("formattable")
install.packages("remotes")
install.packages("waffle", repos = "https://cinc.rud.is")
install.packages("treemapify")
install.packages("ggbeeswarm")
install.packages("ggridges")

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                            Loading Packages                            :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(plotly)
library(tibble)
library(formattable)
library(remotes)
library(waffle)
library(treemapify)
library(ggbeeswarm)
library(ggridges)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                        Basic Data Exploration                          :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

head(stud_data)     # getting the first 6 rows
tail(stud_data)     # getting the last 6 rows

dim(stud_data)      # getting the number of rows and columns

sapply(stud_data, class)  # getting the class of each column

str(stud_data)      # display compact structure of data set

summary(stud_data)  # display summary statistics of data set

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                            Data Processing                             :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

sapply(stud_data, anyNA)  # checking for missing values in the dataset

df = stud_data    # copy original data set to variable, df (good RESET measure if need be)
head(df)

#------------------------------------------------------------------------------#

# Creating New Column (AVG_G) -> Average Math Marks Of Students
AVG_G = round((df$G1 + df$G2 + df$G3) / 3, digits = 1)
AVG_G      # average of G1, G2 and G3 marks, rounded by 1 d.p.

df = cbind(df, AVG_G)   # placing AVG_G column as last column in df

# Creating New Column (AVG_ALC) -> Average Alcohol Consumption of Students
AVG_ALC = round((df$Dalc + df$Walc) / 2, digits = 0)  
AVG_ALC     # average of Dalc and Walc, rounded with 0 d.p.

df = add_column(df, AVG_ALC, .after = "Walc")   # placing AVG_ALC right of Walc column

# Deleting Dalc and Walc column
df = select(df, - c(Dalc, Walc)) 

# Creating New Column (Pedu) -> Parents Education Level
Pedu = round((df$Medu + df$Fedu) / 2, digits = 0)     
Pedu        # average of Medu and Fedu, rounded with 0 d.p.

df = add_column(df, Pedu, .after = "Fedu")   # placing Pedu right of Fedu column

# Deleting Medu and Fedu column
df = select(df, - c(Medu, Fedu))

# Comparing number of columns of original data set with df's data set
ncol(stud_data)
ncol(df)    # added 3 (AVG_G, AVG_ALC, Pedu), deleted 4 (Dalc, Walc, Medu, Fedu)
str(df)

#------------------------------------------------------------------------------#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!                                QUESTION 1                              !!!#
#!!!                      (Why do some students have                        !!!#
#!!!                       high rates of absenteeism ?)                     !!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#------------------------------------------------------------------------------#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#^^^                          Q1 Data Manipulation                          ^^^#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#------------------------------------------------------------------------------#

# Step 1: Inspecting extreme outliers (students with extremely high absences)

check_outliers = 
  
  # using suspected outliers to filter minor and extreme outliers
  plot_ly(
    data = df,
    y = ~ absences,
    name = "",
    type = "box",
    boxpoints = "suspectedoutliers"
  ) %>% 
  
  layout(title = "Distribution Of The Number of Absences From Students",
         yaxis = list(title = "Number of Absences"))

check_outliers  # absences greater than 30 are considered extreme outliers

outliers_info = 
  
  # finding out no_of_students and proportions of extreme outliers
  df %>% 
  filter(absences > 30) %>% 
  summarise(NO_OF_STUDENTS = n(),
            PROPORTION = percent(n()/nrow(df)))

outliers_info   # approximately 1 percent of students were in this category

#------------------------------------------------------------------------------#

# Step 2: Omitting extreme outliers (the 1 percent) for this question's analysis

abs_df = 
  
  df %>% 
  filter(absences <= 30)

sample_n(abs_df , 10)    
nrow(abs_df)    # df containing students with absences <= 30

levels(factor(abs_df$absences)) # prove that absences > 30 have been removed

#------------------------------------------------------------------------------#

# Step 3: Finding the mean of the absence column

avg_abs = mean(abs_df$absences) 
avg_abs   # Values greater than this constitutes as high absenteeism

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 1.1                            ####
####            (Investigating the relationship of students'                ####
####             age of those that have high absences)                      ####
################################################################################
#------------------------------------------------------------------------------#

df1.1 = 
  
  df %>% 
  filter(absences > avg_abs) %>%
  count(age)

df1.1     # df of students with above average (high) absences, 
          # group by age

df1.1$age = as.factor(df1.1$age)
class(df1.1$age)  # converted age to factor to allow diff color for plot fill

#------------------------------------------------------------------------------#

plot1.1 = 

  # creating bar plot with ggpubr package
  ggbarplot(
    data = df1.1,
    x = "age",
    y = "n",
    fill = "age",
    label = TRUE
  ) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "BAR CHART",
    subtitle = "Number Of High Absenteeism Students Based On Students' Age",
    xlab = "Age",
    ylab = "Number Of High Absenteeism Students",
    legend = "right",
    legend.title = "Age",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_economist()
  )  + 
  
  # customizing the caption 
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) + 
  
  # modifying y scale to be in increment of 10
  scale_y_continuous(breaks = seq(0, 90, 10))

plot1.1

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 1.2                            ####
####        (Investigating the relationship of students' social             ####
####         interaction frequency of those that have high absences)        ####
################################################################################
#------------------------------------------------------------------------------#

df1.2 = 
  
  # changed column names and assigned Socialization_Frequency with factor labels
  df %>% 
  filter(absences > avg_abs) %>% 
  count(goout) %>%
  summarize(Socialization_Frequency = factor(goout, labels = c("Very Low", 
                                                               "Low", 
                                                               "Medium", 
                                                               "High", 
                                                               "Very High")), 
            Number_Of_High_Absent_Students = n)

df1.2     # df of students with above average (high) absences, 
          # group by socialization_frequency

#------------------------------------------------------------------------------#

lol1.2 = 
  
  # creating lollipop chart with ggpubr package, added segments to connect the dots
  ggdotchart(
    data = df1.2,
    x = "Socialization_Frequency",
    y = "Number_Of_High_Absent_Students",
    color = "Socialization_Frequency",
    sorting = "none",
    dot.size = 9,
    label = df1.2$Number_Of_High_Absent_Students,
    add = "segments",
    add.params = list(color = "Socialization_Frequency", 
                      size = 1),
    font.label = list(color = "black",
                      size = 10, 
                      vjust = 0.5)) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "LOLLIPOP CHART",
    subtitle = paste("Number Of High Absenteeism Students", 
                     "Based On Their Socialization Frequency"),
    xlab = "Socialization Frequency",
    ylab = "Number Of High Absenteeism Students",
    legend = "right",
    legend.title = "Socialization Frequency",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_economist()
  )  +
  
  # customizing the caption 
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1))

lol1.2      # lollipop chart -> further analysis required (refer pie1.2)

#------------------------------------------------------------------------------#

# NOTE: To illustrate the data visualization better,  
#       another plot (pie chart) will be drawn to COMPARE: 

#       students with ["Very Low" or "Low"] socializing frequency  
#       AGAINST those that are ["Very High" or "High"] socializing frequency 

#       students with ["Medium"] socializing frequency are OMITTED  
#       since they are just neutral, do not sway either side 

#------------------------------------------------------------------------------#

low_sum = 
  
  sum(df1.2$Number_Of_High_Absent_Students
    [which(df1.2$Socialization_Frequency == "Very Low" | 
             df1.2$Socialization_Frequency == "Low")])

low_sum   # total number of students with ["Very Low" or "Low"] social_freq

low_prop = 

  # divide by total number of students with 
  # ["Very Low", "Low", "High", "Very High] social_freq
  low_sum / 
  sum(df1.2$Number_Of_High_Absent_Students
      [which(! df1.2$Socialization_Frequency == "Medium")])

low_prop   # student proportions with ["Very Low" or "Low"] social_freq

#------------------------------------------------------------------------------#

high_sum = 

  sum(df1.2$Number_Of_High_Absent_Students
      [which(df1.2$Socialization_Frequency == "Very High" | 
               df1.2$Socialization_Frequency == "High")])

high_sum  # total number of students with ["Very High" or "High"] social_freq

high_prop = 
  
  # divide by total number of students with 
  # ["Very Low", "Low", "High", "Very High] social_freq
  high_sum / 
  sum(df1.2$Number_Of_High_Absent_Students
      [which(! df1.2$Socialization_Frequency == "Medium")])

high_prop   # student proportions with ["Very High" or "High"] social_freq

#------------------------------------------------------------------------------#

qc1.2 = ifelse(low_prop + high_prop == 1, "YES", "NO")
qc1.2     # ensuring student proportions are accurate (adds up to 1)

#------------------------------------------------------------------------------#

# Creating new data frame (pie_df1.2) for the pie chart using above proportions
social_freq = c("Lower", 
                "Higher")   # 1st column: socialization frequency 

stud_num = c(low_sum, 
             high_sum)      # 2nd column: total number of students

stud_pct = c(percent(low_prop, digits = 0), 
             percent(high_prop, digits = 0))  # 3rd column: group proportions -> converted
                                              # to pct with formattable::percent() 

pie_df1.2 = data.frame(social_freq,
                       stud_num,
                       stud_pct, 
                       stringsAsFactors = TRUE)

pie_df1.2   # new high absent students df, group by new social_freq ["Lower" or "Higher"]

#------------------------------------------------------------------------------#

pie1.2 = 
  
  # Creating pie chart with ggpubr package
  ggpie(
    data = pie_df1.2,
    x = "stud_pct",
    fill = "social_freq",
    size = 1,
    label = "stud_pct",
    lab.pos = "in",
    lab.font = c(9, "bold", "black")
  ) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "PIE CHART",
    subtitle = paste0("Proportion of High Absenteeism Students Based ", 
                     "On Students' Socialization Frequency\n", 
                     "(Omitting Medium Socialization Frequency)"),
    legend = "right",
    legend.title = "Socialization Frequency",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_economist()
  )  +
  
  # customizing the caption
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) +
  
  # removing some ggplot components
  rremove("grid") +
  rremove("axis") +
  rremove("xylab") +
  rremove("xy.text") + 
  
  #   (labels -> to add stud_num for each Socialization Frequency)
  #   (limits -> to reverse legend items' order to match the pie chart)
  scale_fill_discrete(labels = paste0(pie_df1.2$social_freq,
                                      " (",
                                      pie_df1.2$stud_num, 
                                      " students)"), 
                      limits = c("Lower",
                                 "Higher"))

pie1.2      # pie chart -> *Zoom* for better image

#------------------------------------------------------------------------------#

# NOTE: To look at both lollipop chart and pie chart side by side,  
#       refer (plot1.2) below: 

#------------------------------------------------------------------------------#

plot1.2 = 
  
  ggarrange(
    lol1.2, NULL, pie1.2,
    nrow = 1,
    widths = c(1, 0.05, 1),
    labels = c("A", "", "B")
)

plot1.2     # Both lollipop chart and pie chart arranged in one page 
            # -> *Zoom* for better image

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 1.3                            ####
####          (Investigating the relationship of students' past             ####
####           class failures of those that have high absences)             ####
################################################################################
#------------------------------------------------------------------------------#

df1.3 = 
  
  df %>% 
  filter(absences > avg_abs) %>% 
  count(failures)

df1.3       # df of students with above average (high) absences,  
            # group by number of pass class failures

df1.3$failures = as.factor(df1.3$failures)    
class(df1.3$failures)   # failures column converted to factor,
                        # to fill bars with different colours.

#------------------------------------------------------------------------------#

plot1.3 = 
  
  # creating bar plot with ggpubr package
  ggbarplot(data = df1.3,
            x = "failures",
            y = "n",
            fill = "failures") %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "BAR CHART",
    subtitle = "Number Of High Absenteeism Students Based On Students' Past Class Failures",
    xlab = "Past Class Failures",
    ylab = "Number Of High Absenteeism Students",
    legend = "right",
    legend.title = "Past Class Failures",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_economist()
  )  +
  
  # customizing the caption
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) +
  
  # convert to horizontal bar chart
  rotate() +    
  
  # place the labels in midpoint of bars
  geom_text(aes(label = n, y = n / 2)) 

plot1.3       # bar chart

#------------------------------------------------------------------------------#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!                                QUESTION 2                              !!!#
#!!!              (What are the reasons that influence                      !!!#
#!!!               heavy alcohol consumption among students ?)              !!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#------------------------------------------------------------------------------#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#^^^                          Q2 Data Manipulation                          ^^^#
#^^^                  (To filter students that have HIGH(4)                 ^^^#
#^^^                   and VERY HIGH(5) alcohol consumption)                ^^^#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#------------------------------------------------------------------------------#

alc_df = 
  
  df %>% 
  filter(AVG_ALC == 4 | AVG_ALC == 5)   # AVG_ALC = (Dalc + Walc) / 2 (no d.p.)

sample_n(alc_df , 10)
nrow(alc_df)    # will be the base df for Q2 analysis

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 2.1                            ####
####              (Investigating if students' sex                           ####
####               determine their heavy alcohol consumption)               ####
################################################################################
#------------------------------------------------------------------------------#

df2.1 = 

  alc_df %>%
  count(sex) %>%
  arrange(desc(sex)) %>%
  summarise(Sex = replace(sex, values = c("Male",
                                          "Female")),
            No_Of_Students = n)   

df2.1       # df of above average (high) alc_consume students, group by sex
#------------------------------------------------------------------------------#

tb2.1 = ggtexttable(df2.1, 
                    rows = NULL, 
                    theme = ttheme("mOrange"))

tb2.1       # textual table for plot

#------------------------------------------------------------------------------#

plot2.1 = 
  
  # creating waffle chart with waffle package
  waffle(df2.1,
         rows = 6,
         colors = c("#00BFC4", "#C77CFF")) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "WAFFLE CHART",
    subtitle = "Students With High Alcohol Consumption Distributed By Sex",
    xlab = "1 square = 3 students",
    font.x = "bold",
    legend = "top",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_economist_white()
  ) +
  
  # customizing the caption
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) +
  
  # removing some ggplot components
  rremove("grid") +
  rremove("axis") +
  rremove("xy.text") +
  rremove("ticks") +
  
  # adding textual table to plot
  annotation_custom(
    ggplotGrob(tb2.1),
    xmin = 11,
    ymin = 8.5,
    xmax = 14
  )

plot2.1       # waffle chart -> *Zoom* for better image

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 2.2                            ####
####   (Investigating the relationship of students' social interaction      ####
####    frequency of those that have heavy alcohol consumption)             ####
################################################################################
#------------------------------------------------------------------------------#

df2.2 = 
  
  alc_df %>% 
  count(goout) %>% 
  summarise(Social_Freq = factor(goout, labels = c("Very Low", 
                                                   "Low", 
                                                   "Medium", 
                                                   "High", 
                                                   "Very High")), 
            No_Of_Students = n)

df2.2       # df of above average (high) alc_consume students, group by goout,
            # which is changed to Social_Freq and factored with new labels

#------------------------------------------------------------------------------#

treemp2.2 = 
  
  # inserting df2.2 data to ggplot
  ggplot(df2.2,
         aes(area = No_Of_Students,
             fill = Social_Freq,
             label = paste0(Social_Freq, "\n", No_Of_Students))) +
  
  # creating treemap with treemapify package
  geom_treemap() +
  
  # center treemap text and expand it according to box
  geom_treemap_text(place = "centre",
                    grow = TRUE)

plot2.2 = 
  
  # adding titles, axis labels, captions and theme, removing legend
  ggpar(
    treemp2.2,
    title = "TREE MAP",
    subtitle = paste("Number Of Students With High Alcohol Consumption", 
                     "Distributed By Thier Socialization Frequency"),
    caption = "Source:student-mat.csv & student-por.csv",
    legend = "none",
    ggtheme = theme_economist_white()
  )  +
  
  # customizing the caption
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) +
  
  # removing the x and y axis from ggplot
  rremove("axis")   

plot2.2     # tree map

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 2.3                            ####
####              (Investigating the relationship of students' age          ####
####               with those that have heavy alcohol consumption)          ####
################################################################################
#------------------------------------------------------------------------------#

sample_n(alc_df , 10)
nrow(alc_df)    # base df from Q2 Data Manipulation, sufficient to calculate
                # frequency by age for histogram plot

#------------------------------------------------------------------------------#

tb2.3 = 
  
  # getting df of above average (high) alc_consume students,
  # group by age, added student proportions column
  alc_df %>%
  count(age) %>%
  summarise(Age = age,
            No_Of_Students = n,
            Student_Proportions = paste0(round(n / sum(n), 
                                              digits = 3) * 100, 
                                        "%")) %>%
  
  # creating textual table from the df
  ggtexttable(rows = NULL,
              theme = ttheme("mBlue"))

tb2.3       # textual table

#------------------------------------------------------------------------------#

plot2.3 = 
  
  # creating histogram with ggpubr package
  gghistogram(alc_df,
              x = "age",
              bins = 8,
              fill = "#F8766D") %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "HISTOGRAM",
    subtitle = "Students With High Alcohol Consumption Distributed By Age",
    xlab = "Age",
    ylab = "Number of Students",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_economist_white()
  ) +
  
  # customizing the caption
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) +
  
  # modifying x and y scales to adjust its tick labels
  scale_y_continuous(breaks = seq(0, 26, 2)) +      
  scale_x_continuous(breaks = seq(15, 22, 1)) +
  
  # removing some ggplot components
  rremove("grid") +
  rremove("x.ticks") +
  
  # adding textual table into histogram
  annotation_custom(
    ggplotGrob(tb2.3),
    xmin = 20,
    ymin = 10,
    xmax = 22
  )

plot2.3     # histogram -> *Zoom* for better image

#------------------------------------------------------------------------------#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!                                QUESTION 3                              !!!#
#!!!              (Why do some students consistently improve or             !!!#
#!!!               maintain their Math marks for each degree level ?)       !!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#------------------------------------------------------------------------------#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#^^^                          Q3 Data Manipulation                          ^^^#
#^^^              (To filter students that improve or maintain              ^^^#
#^^^               their Math marks for each degree level)                  ^^^#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#------------------------------------------------------------------------------#

good_stud_df = 
  
  df %>% 
  filter(G2 >= G1 & G3 >= G2)

sample_n(good_stud_df , 10)
nrow(good_stud_df)    # will be the base df for Q3 analysis

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 3.1                            ####
####        (Investigating if students' relationship status contribute      ####
####         towards them improving or maintaining their Math scores)       ####
################################################################################
#------------------------------------------------------------------------------#
df3.1 =
  
  good_stud_df %>%
  count(romantic) %>%
  summarise(
    Relationship_Status = replace(romantic, values = c("Single", 
                                                       "In A Relationship")),
    No_Of_Students = n,
    Student_Proportion = percent(n / sum(n), digits = 0)
  )

df3.1       # df of students who improved or maintained Math marks each year, 
            # group by relationship status, added student proportions column

#------------------------------------------------------------------------------#

labs3.1 = paste0(df3.1$Relationship_Status,
                 "\n(",
                 df3.1$Student_Proportion,
                 ")")

labs3.1     # labels for donut chart

#------------------------------------------------------------------------------#

plot3.1 = 
  
  # creating donut chart with ggpubr package
  ggdonutchart(
    data = df3.1,
    x = "No_Of_Students",
    label = labs3.1,
    lab.pos = "in",
    lab.font = c(5, "bold", "black"),
    fill = "Relationship_Status",
    color = "white"
  ) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "DONUT CHART",
    subtitle = paste("Relationship Status of Students Who Improved", 
                     "or Maintained Math Marks For Each Degree Level"),
    legend.title = "Relationship Status",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_fivethirtyeight()
  ) +
  
  # customizing the caption and legend
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1), 
        legend.position = "right") +
  
  # removing some ggplot components
  rremove("grid") +
  rremove("axis") +
  rremove("xylab") +
  rremove("xy.text") +
  rremove("ticks") +
  
  #   (labels -> to add No_Of_Students for each Relationship Status)
  #   (limits -> to reverse legend items' order to match the donut chart)
  scale_fill_discrete(labels = paste0(df3.1$Relationship_Status,
                                      " (",
                                      df3.1$No_Of_Students, 
                                      " students)"), 
                      limits = c("Single", 
                                 "In A Relationship"))

plot3.1    # donut chart -> *Zoom* for better image

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 3.2                            ####
####  (Investigating if students' average alcohol consumption contribute    ####
####        towards them improving or maintaining their Math scores)        ####
################################################################################
#------------------------------------------------------------------------------#

df3.2 =
  
  good_stud_df %>%
  count(AVG_ALC) %>%
  summarise(Avg_Alc_Consumption = factor(AVG_ALC, labels = c("Very Low", 
                                                             "Low", 
                                                             "Medium", 
                                                             "High", 
                                                             "Very High")), 
            No_Of_Students = n)

df3.2     # df of students who improved or maintained Math marks each year, 
          # group by AVG_ALC, factored with labels

#------------------------------------------------------------------------------#

tb3.2 = ggtexttable(df3.2, rows = NULL, 
                         theme = ttheme("mOrange")) 

tb3.2  # creating textual table for plot

#------------------------------------------------------------------------------#

plot3.2 = 
  
  # creating line chart with ggpubr package
  ggline(
    data = df3.2,
    x = "Avg_Alc_Consumption",
    y = "No_Of_Students",
    linetype = "dashed",
    size = 2,
    point.color = "#F8766D",
    point.size = 5,
  ) %>%
  
  # adding titles, axis labels, captions and theme
  ggpar(
    title = "Line Chart",
    subtitle = paste("Average Alcohol Consumption of Students Who Improved", 
                     "Or Maintained Math Marks For Each Degree Level"),
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_fivethirtyeight()
  ) +
  
  # customizing the caption
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1)) +
  
  # removing ticks on x and y axis
  rremove("ticks") +
  
  # adding textual table to plot
  annotation_custom(ggplotGrob(tb3.2),
                    xmin = 3.5,
                    ymin = 75,
                    xmax = 5)       

plot3.2   # line chart

#------------------------------------------------------------------------------#      
################################################################################
####                                ANALYSIS 3.3                            ####
####  (Investigating if students' desire for higher education contribute    ####
####      towards them improving or maintaining their Math scores)          ####
################################################################################
#------------------------------------------------------------------------------#

df3.3 =
  
  good_stud_df %>%
  count(higher) %>%
  summarise(
    Higher_Edu_Desire = replace(higher, values = c("No", 
                                                   "Yes")),
    No_Of_Students = n,
    Student_Proportion = percent(n / sum(n), digits = 0))

df3.3       # df of students who improved or maintained Math marks each year, 
            # group by higher education desire, added student proportions column

#------------------------------------------------------------------------------#  

plot3.3 = 
  
  # creating pie chart with ggpubr package
  ggpie(
    data = df3.3,
    x = "Student_Proportion",
    size = 1,
    label = "Student_Proportion",
    lab.pos = "out",
    fill = "Higher_Edu_Desire"
  ) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "PIE CHART",
    subtitle = paste(
      "(Yes Or No) Percentage of Students' Higher Education",
      "Desire Of Those That\nHave Improved or Maintained",
      "Their Math Marks For Each Degree Level"
    ),
    legend.title = "Higher Education Desire",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_fivethirtyeight()
  )  +
  
  # customizing the caption, labels and legend
  theme(
    plot.caption = element_text(size = 8, face = "italic", hjust = 1),
    axis.text.x = element_text(size = 20, face = "bold", colour = "black"), 
    legend.position = "right") +
  
  #   (labels -> to add No_Of_Students for each Higher Education Desire)
  scale_fill_discrete(labels = paste0(df3.3$Higher_Edu_Desire,
                                      " (",
                                      df3.3$No_Of_Students,
                                      " students)")) +
  
  # removing some ggplot components
  rremove("grid") +
  rremove("axis") +
  rremove("xylab") +
  rremove("y.text") +
  rremove("ticks") 

plot3.3     # pie chart -> *Zoom* for better image

#------------------------------------------------------------------------------#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!                                QUESTION 4                              !!!#
#!!!                      (Why do some students' Math marks                 !!!#
#!!!                       decline over degree level ?)                     !!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#------------------------------------------------------------------------------#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#^^^                          Q4 Data Manipulation                          ^^^#
#^^^                  (To filter students whose Math marks                  ^^^#
#^^^                   dropped upon each degree level)                      ^^^#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#------------------------------------------------------------------------------#

bad_stud_df = 
  
  df %>% 
  filter(G2 < G1 & G3 < G2)

sample_n(bad_stud_df, 10)
nrow(bad_stud_df)    # will be the base df for Q4 analysis

#------------------------------------------------------------------------------#      
################################################################################
####                                ANALYSIS 4.1                            ####
####        (Investigating if the educational level of students'            ####
####         parents contribute towards their declining Math scores)        ####
################################################################################
#------------------------------------------------------------------------------#

sample_n(bad_stud_df, 10)
nrow(bad_stud_df)     # base df from Q4 Data Manipulation, sufficient to show
                      # distribution of Pedu and AVG_G in swarm plot

#------------------------------------------------------------------------------#

# REMINDER: Pedu    -> Average of Medu and Fedu, rounded to 0 d.p.
#           AVG_G   -> Average of G1, G2 and G3, rounded to 1 d.p.

#           (Both columns were created in Data Processing Stage)

#------------------------------------------------------------------------------#

min(bad_stud_df$AVG_G)  # Res:  3.7
max(bad_stud_df$AVG_G)  # Res:  14
                        # Why:  to know the min and max AVG_G marks to manually 
                        #       adjust the scale_y_continuous values in plot

factor(bad_stud_df$Pedu)  # Res: Levels: 1 2 3 4 (no '0' education level, 
                          # means all these students' parents attended school)
                          # to know correct length of factor labels to be used 
                          # when factoring Pedu column in swarm plot below.

#------------------------------------------------------------------------------#

plot4.1  = 
  
  # inserting bad_stud_df data to ggplot,
  # factored Pedu's x tick values with better labels for easy understanding, 
  # swarm colors differ by if their avg. Math marks surpass min. pass grade (10)
  ggplot(data = bad_stud_df,
         aes(x = factor(Pedu, 
                        labels = c("Primary Education\n(1st to 4th Grade)",
                                   "Primary Education\n(5th to 9th Grade)",
                                   "Secondary Education\n(10th to 12th Grade)",
                                   "Higher Education\n(University or Polytechnic)")),
             y = AVG_G,
             col = AVG_G > 10)) %>%
  
  # adding titles, axis labels, legends, captions and theme
  ggpar(
    title = "BEE SWARM",
    subtitle = paste("Distribution Of The Average Math Marks From Declining",
                     "Students Based On Their Parents' Educational Level"),
    xlab = "Parents' Educational Level",
    ylab = "Students' Average Math Marks",
    legend.title = paste("Students' Average Math", "Marks Greater Than 10", 
                         "(Minimum Passing Grade)" , sep = "\n"),
    legend = "right",
    font.legend = c(10, "bold", "black"),
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_stata()
  ) +
  
  # adding y-intercept as minimum passing grade line to plot
  geom_hline(yintercept = 10, 
             linetype="dashed", 
             color = "red") +
  
  # adding text annotation above the y-intercept line
  annotate("text", 
           x = "Higher Education\n(University or Polytechnic)",
           y = 10, 
           label = "Minimum Passing Grade", 
           vjust = -0.5, 
           size = 3, 
           color = "red") +
  
  # creating swarm plot with ggbeeswarm package
  geom_beeswarm(size = 2) +
  
  # customizing caption and y axis tick values
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1), 
        axis.text.y = element_text(angle = 0)) +
  
  # altering the order of x axis tick labels
  scale_x_discrete(limits = c("Primary Education\n(1st to 4th Grade)",
                              "Primary Education\n(5th to 9th Grade)",
                              "Secondary Education\n(10th to 12th Grade)",
                              "Higher Education\n(University or Polytechnic)")) +
  
  # altering the y scale values knowing the min and max AVG_G from before
  scale_y_continuous(breaks = seq(3, 15, 1)) + 
  
  # altering the legend labels and its corresponding colour values
  scale_color_manual(labels = c("No", "Yes"), 
                     values = c("#F8766D", "#00BFC4")) 

plot4.1     # bee swarm chart -> *Zoom* for better image

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 4.2                            ####
####              (Investigating if the study time of students              ####
####               contribute towards their declining Math scores)          ####
################################################################################
#------------------------------------------------------------------------------#

sample_n(bad_stud_df, 10)
nrow(bad_stud_df)   # base df from Q4 Data Manipulation, sufficient to show 
                    # study time frequency in histogram

#------------------------------------------------------------------------------#

plot4.2 = 
  
  # creating histogram with ggpubr package
  gghistogram(bad_stud_df,
              x = "studytime",
              bins = 4,
              fill = "#C77CFF") %>%
  
  # adding titles, axis labels, captions and theme
  ggpar(
    title = "HISTOGRAM",
    subtitle = paste("Study Time Of Students With Declining",
                     "Math Scores Upon Each Degree Level"),
    xlab = "Weekly Study Time",
    ylab = "Number of Students",
    caption = "Source:student-mat.csv & student-por.csv",
    ggtheme = theme_stata()
  ) + 
  
  # create panels for each study time and rename the panel title
  facet_wrap(~studytime, 
             labeller = labeller(studytime = c("1" = "1 hour",
                                               "2" = "2 hour",
                                               "3" = "3 hour",
                                               "4" = "4 hour"))) +
  
  # display the student proportions in each histogram bar 
  geom_text(stat= "count", 
            aes(label= percent(..count.. / sum(..count..),
                               digits = 0)), 
            position=position_stack(vjust = 0.5)) +
  
  # customizing the caption and y axis tick values
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        axis.text.y = element_text(angle = 0)) +
  
  # altering the y scale values to be separated by increments of 2
  scale_y_continuous(breaks = seq(0, 26, 2)) +
  
  # removing some ggplot components
  rremove("x.axis") +
  rremove("x.ticks") +
  rremove("x.text")

plot4.2     # histogram -> *Zoom* for better image

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 4.3                            ####
####            (Investigating if the students' number of absences          ####
####             contribute towards their declining Math scores)            ####
################################################################################
#------------------------------------------------------------------------------#

df4.3 = 
  
  bad_stud_df %>% 
  count(absences)

df4.3       # df of total number of declining Math scores students, 
            # group by absences

df4.3$absences = as.factor(df4.3$absences)
class(df4.3$absences)   # converted absences column to factor, so that its tick   
# mark labels on the plot only show the factor levels

#------------------------------------------------------------------------------#

# Creating the lollipop chart
lol4.3 =
  
  # inserting df4.3 data to ggplot,
  ggplot(data = df4.3, aes(x = absences, 
                           y = n)) +
  
  # the line for the lollipop chart, 
  # different color and size to emphasize line of (absences == 0) from others
  geom_segment(aes(x = absences, 
                   xend = absences, 
                   y = 0, 
                   yend = n),
               color = ifelse(df4.3$absences == 0, "#F8766D", "#D3D3D3"), 
               size = ifelse(df4.3$absences == 0, 1.3, 0.7)) +
  
  # the point for the lollipop chart
  # different color and size to emphasize point of (absences == 0) from others
  geom_point(
    color = ifelse(df4.3$absences == 0, "#F8766D", "#D3D3D3"),
    size = ifelse(df4.3$absences == 0, 16, 9)
  ) + 
  
  # adding frequency in the geom_point()
  geom_text(aes(label = n)) +
  
  # rotating it to be horizontal lollipop chart
  coord_flip() +
  
  # adding titles, axis labels, captions and theme
  labs(title = "LOLLIPOP CHART",
       subtitle = paste("Students' Number Of Absences With Declining",
                        "Math Scores Upon Each Degree Level"),
       x = "Number Of Absences", 
       y = "Number of Students",
       caption = "Source:student-mat.csv & student-por.csv") + 
  theme_stata() +
  
  # customizing the caption and y axis tick labels, removing legend
  theme(
    legend.position="none",
    plot.caption = element_text(size = 8, face = "italic", hjust = 1),
    axis.text.y = element_text(angle = 0))

# Adding annotations to the highlighted group (absences == 0)
plot4.3 = 
  
  lol4.3 + 
  
  # x = 1.5, y = 25 are coordinates to position the annotation on the plot
  annotate("text", 
           x = 1.5,
           y = 25,
           label = "0 Absences Despite Declining Math Scores !",
           color = "#F8766D",
           size = 4,
           angle = 0,
           fontface = "bold")

plot4.3       # lollipop chart -> *Zoom* for better image

#------------------------------------------------------------------------------#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!                                QUESTION 5                              !!!#
#!!!                    (What are the factors that influence                !!!#
#!!!                     students' average Math marks ?)                    !!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#------------------------------------------------------------------------------#

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#^^^                          Q5 Data Manipulation                          ^^^#
#^^^              (Analysis 5.1 and 5.3 will get the mean of their          ^^^#
#^^^               grouping variable prior to data visualization)           ^^^#
#^^^                                                                        ^^^#
#^^^          (Analysis 5.2 will be using original data frame, df)          ^^^#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 5.1                            ####
####            (Investigating if the students' travel time to              ####
####             school has an impact on their average Math scores)         ####
################################################################################
#------------------------------------------------------------------------------#

df5.1 = df    
sample_n(df5.1, 10)
nrow(df5.1)     # df5.1 -> same as original df, but required for data processing

df5.1$traveltime = as.factor(df5.1$traveltime)
class(df5.1$traveltime)   # factoring travel time to fill diff colors in plot
    
    
levels(df5.1$traveltime) = c("1 hour", "2 hour", "3 hour", "4 hour")
factor(df5.1$traveltime)    # rename the factor levels for easier understanding

#------------------------------------------------------------------------------#

grp5.1 = 
  
  df %>%
  group_by(traveltime) %>%
  summarise(grp_mean = mean(AVG_G))

grp5.1      # df containing avg Math mark for each travel time group

#------------------------------------------------------------------------------#

plot5.1 = 
  
  # inserting df5.1 data to ggplot
  ggplot(df5.1, aes(x = AVG_G, 
                    y = traveltime)) +
  
  # adding density ridgeline plot
  geom_density_ridges(aes(fill = traveltime), 
                      size = 1, 
                      alpha = 0.5) +
  
  # adding avg Math mark for each distribution as labels to plot using grp5.1 
  geom_text(data = grp5.1, aes(x = grp_mean, 
                               label = round(grp_mean, digits = 1), 
                               vjust = -0.5)) +
  
  # adding titles, axis labels, captions and theme
  labs(title = "DENSITY RIGDELINE PLOT",
       subtitle = paste0("Average Math Scores Of Students\n",
                        "Based On Their Travel Time To School"),
       x = "Average Math Score", 
       y = "Travel Time To School",
       caption = "Source:student-mat.csv & student-por.csv") + 
  theme_wsj() +
  
  # customizing the caption, remove legend and y grid lines, show axis titles and x grid lines
  theme(
    legend.position="none",
    plot.caption = element_text(size = 8, face = "italic", hjust = 1),
    axis.title = element_text(),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_blank()
  )

plot5.1       # density ridgeline plot -> *Zoom* for better image

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 5.2                            ####
####              (Investigating if students' legal guardian                ####
####               has an impact on their average Math scores)              ####
################################################################################
#------------------------------------------------------------------------------#

sample_n(df, 10)
nrow(df)    # will be using original data frame, df for this question

#------------------------------------------------------------------------------#

hline = function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash = "dot")
  )
}           # function to insert a minimum passing grade line on box plot

plot5.2 = 
  
  # creating box plot with plotly package, guardian column is factored
  plot_ly(
    data = df,
    x = ~ guardian,
    y = ~ AVG_G,
    type = "box",
    color = ~ factor(guardian)) %>% 
  
  # adding the minimum passing grade line and a text annotation
  layout(shapes = list(hline(10)),
         annotations = list(x = 2.7, 
                            y = 11, 
                            text = "Minimum\nPassing\nGrade", 
                            showarrow = FALSE, 
                            xanchor = "right")) %>% 
  
  # adding titles, axis labels and legend, customizing background colour
  # best effort to make plotly less horrible  :(
  layout(title = list(text = paste0("BOX PLOT (PLOTLY)",
                                     "<br>", "<sup>",
                                     "Average Math Scores Of Students ",
                                     "Based On Their Legal Guardians",
                                     "</sup>")), 
         xaxis = list(title = "Legal Guardian"), 
         yaxis = list(title = "Average Math Score"), 
         legend = list(title = list(text="<b> Legal Guardian </b>")), 
         plot_bgcolor = "#F8F2E4")

plot5.2       # boxplot (plotly)

#------------------------------------------------------------------------------#
################################################################################
####                                ANALYSIS 5.3                            ####
####            (Investigating if students' past class failures             ####
####             has an impact on their average Math scores)                ####
################################################################################
#------------------------------------------------------------------------------#

grp5.3 = 
  
  df %>%
  group_by(failures) %>%
  summarise(grp_mean = mean(AVG_G))

grp5.3        # df containing avg Math mark for each class failure group

#------------------------------------------------------------------------------#

plot5.3 = 
  
  # inserting original data frame, df data to ggplot
  ggplot(df, aes(x = AVG_G)) + 
  
  # adding density plot, position = "fill" (each distribution add up to 100%) 
  geom_density(aes(fill = factor(failures)), 
               alpha = 0.4, 
               position = "fill") + 
  
  # adding x-intercept of avg Math mark for each distribution using grp5.3 
  geom_vline(data = grp5.3, aes(xintercept = grp_mean, 
                                color = factor(failures)), 
             linetype = "dashed", 
             size = 2) +
  
  # adding avg Math mark for each distribution as label to plot using grp5.3
  geom_text(data = grp5.3, aes(x = grp_mean -0.5, 
                               y = 0,
                               label = round(grp_mean, digits = 1), 
                               vjust = 1, 
                               color = factor(failures))) +
  
  # renaming legend title and reversing its items (better readability)
  guides(fill = guide_legend(title = "Number of Past Class Failures", 
                             reverse = TRUE ), 
         color = guide_legend(title = "Number of Past Class Failures", 
                              reverse = TRUE)) +
  
  # adding titles, axis labels, caption and theme
  labs(title = "DENSITY PLOT",
       subtitle = paste0("Average Math Scores Of Students\n",
                         "Based On Their Number of Past Class Failures"),
       x = "Average Math Score", 
       y = "Density",
       caption = "Source:student-mat.csv & student-por.csv") + 
  theme_wsj() +
  
  # customizing the legend and caption, show axis titles, and remove grid lines
  theme(
    legend.title = element_text(family = "serif", size = 12),
    plot.caption = element_text(size = 8, face = "italic", hjust = 1),
    axis.title = element_text(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

plot5.3       # density plot -> *Zoom* for better image

#------------------------------------------------------------------------------#

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#:::                        Additional Features (AF)                        :::#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

#------------------------------------------------------------------------------#

# AF_1 (Density Plots)

    # ggridges::geom_density_ridges()
          
        # ->    Able to examine the distribution of continuous variable
        #       separated by grouping 
        
        # E.g.  examines the distribution of average Math marks for
        #       each grouping of travel time to school (Analysis 5.1)

    # ggplot2::geom_density()

        # ->    Demonstrates the distribution of continuous data points 
        #       with a single continuous curve or by grouping.

        # E.g.  displays the distribution of average Math marks for 
        #       each grouping of number of past class failures (Analysis 5.3)

#------------------------------------------------------------------------------#

# AF_2 (Annotations)

    # ggplot2::annotation_custom()

        # ->    Displays static annotations on plot without extending the x
        #       and y scales to provide supplementary information to plot

        # E.g.  adds a customized data frame to the waffle chart to indicate 
        #       total number of students for each sex grouping (Analysis 2.1)

        # E.g.  adds a customized data frame to the histogram to indicate 
        #       total number of students and student proportions for each age 
        #       grouping (Analysis 2.3)

        # E.g.  adds a customized data frame to the line chart to indicate 
        #       total number of students for each average alcohol consumption 
        #       grouping (Analysis 3.2)

    # ggplot2::annotate()
      
        # ->    Able to add additional text or shapes on the plot to draw the
        #       attention of the reader
        
        # E.g.  adding the text "Minimum Passing Grade" on the y-intercept 
        #       representing the minimum passing grade (Analysis 4.1),
        
        # E.g.  adding the text "0 Absences Despite Declining Math Scores !"
        #       on the line (absences == 0) to inform reader that absences are 
        #       not to blame for declining math marks in students (Analysis 4.3)

    # plot_ly::layout.annotations[]
    
        # ->    Able to place textual elements in a plotly plot by providing
        #       x and y coordinates that correspond to the plot
        
        # E.g.  adding the text "Minimum Passing Grade" with coordinates 
        #       (x = 2.7, y = 11), which is just above the y-intercept of  
        #       minimum passing grade line (Analysis 5.2),
        

#------------------------------------------------------------------------------#

# AF_3 (All Types Of Pie Charts)

    # ggpubr::ggpie()

        # ->    Creates elegant pie charts, which is sufficient to
        #       illustrate part and whole relationships, given that the parts
        #       are minimal between 2 to 3

        # E.g.  adopted pie chart to compare students with ["Very Low" or "Low"]   
        #       Zocializing frequency AGAINST those that are
        #       [["Very Low" or "Low"]] socializing frequency (Analysis 1.2)

        #       adopted pie chart to compare (Yes Or No) percentage of students' 
        #       higher education desire of those that have improved or 
        #       maintained their math marks for each degree level (Analysis 3.3)

    # ggpubr::ggdonutchart()

        # ->    Creates elegant donut charts, which is an alternative to
        #       pie charts

        # E.g.  used donut chart to compare relationship status of  
        #       students who improved or maintained their math marks 
        #       for each degree level (Analysis 3.1)

    # ggpubr::waffle()
        
        # ->    also known as square pie charts, which can be useful for 
        #       displaying progress towards a milestone or proportions. 
        #       However, it can also be an alternative to pie charts

        # E.g.  used waffle chart to compare the distribution of sex of   
        #       students with high alcohol consumption (Analysis 2.1)

#------------------------------------------------------------------------------#

# AF_4 (Line Intercepts)

    # ggplot2::geom_hline()
          
        # ->    add one or more horizontal lines on the plot

        # E.g.  adding y-intercept to indicate the minimum passing grade
        #       for math is 10 on the bee swarm plot (Analysis 4.1)

    # ggplot2: geom_vline()

        # ->    add one or more vertical lines on the plot

        # E.g.  adding x-intercept to indicate avg Math mark 
        #       for each distribution on the density plot (Analysis 5.3)

    # plotly::layout.shapes[]

        # ->    add lines or geometric shapes to a plotly plot

        # E.g.  adding a horizontal line by passing a function which  
        #       matches the coordinates of y-intercept that
        #       indicates the minimum passing grade (Analysis 5.2).  

#------------------------------------------------------------------------------#

# AF_5 (Swarm Plot)

    # ggbeeswarm: geom_beeswarm()

        # ->    similar to scatter plots, the distribution of data points
        #       are organized in a manner that there is no overlap between them
        
        # E.g.  used bee swarm plot to show the distribution of  
        #       students' average math marks based on their 
        #       parents' educational level, albeit students
        #       with declining mathematics marks at each 
        #       degree level (Analysis 4.1)

#------------------------------------------------------------------------------#

# AF_6 (Tree Map)

    # treemapify: geom_treemap()

        # ->    rectangle boxes are used to portray data that are
        #       hierarchical with the data value proportionately
        #       represented by the area of the rectangle boxes
        
        # E.g.  used tree map to show the number of high alcohol consumption
        #       students for each grouping of socialization frequency, which
        #       has a hierarchical order of ["Very Low", "Low", "Medium", 
        #       "High", and "Very High"] (Analysis 2.2)

#------------------------------------------------------------------------------#

# AF_7 (Plotly)

    # plotly::plot_ly

        # ->    a graphing package acting as an alternative to ggplot2 
        #       for designing more dynamic and interactive graphs 
        #       which displays useful hover info
        
        # E.g.  used plot_ly to quickly design a box plot to filter 
        #       minor and extreme outliers for absences column via 
        #       suspected outliers (Analysis 1.1)

        # E.g.  used plot_ly to design a box plot to determine the 
        #       summary information when plotting student' travel time 
        #       to school against their average math scores (Analysis 5.2)

#------------------------------------------------------------------------------#

# AF_8 (Lollipop Chart)

    # ggpubr::ggdotchart()

        # ->    an alternative to bar charts, just that 
        #       each dot (geom_point()) has a line segment connecting
        #       it from the y axis, which is beneficial for interpreting
        #       large set of data points

        # E.g.  used lollipop chart to represent the number   
        #       of high absenteeism students based on 
        #       their socialization frequency (Analysis 1.2)

        # E.g.  used lollipop chart to represent number of    
        #       absences of students that have declining math scores 
        #       upon each degree level, which has 8 data points  (Analysis 4.3)

#------------------------------------------------------------------------------#

# AF_9 (Formattable)

    # formattable:percent()

        # ->    formats numeric vectors to percentages of desired digits,    
        #       with "%" sign allocated to each value post formatting

        # ->    it is a shorter version of:
                
        #           paste(round(x, digits = n), "%")

        # E.g.  used percent() to display student proportions in
        #       percentages that are classified as extreme outliers 
        #       when progressing towards finding an accurate mean 
        #       number of absences for each student to know how many
        #       rows from the original data set will be neglected
        #       (Q1 Data Manipulation -> outliers info)

        # E.g.  used percent() to convert student proportions into
        #       percentages for those that are group under
        #       ["Very Low" or "Low"] socialization frequency and
        #       ["Very High" or "High"] socialization frequency 
        #       respectively, , for which the percentages can be 
        #       immediately labelled in its corresponding pie chart
        #       (Analysis 1.2 -> stud_pct)

        # E.g.  used percent() to convert student proportions into
        #       percentages to distinguish distribution of 
        #       students that improved or maintained their math
        #       marks based on their relationship status of single or
        #       in a relationship, for which the percentages can be 
        #       immediately labelled in its corresponding donut chart
        #       (Analysis 3.1)

        # E.g.  used percent() to convert student proportions into
        #       percentages to distinguish distribution of 
        #       students that improved or maintained their math
        #       marks based on their higher education desire of yes 
        #       or no, for which the percentages can be immediately
        #       labelled in its corresponding pie chart
        #       (Analysis 3.3)

        # E.g.  used percent() to convert the proportionate frequency 
        #       into percentages to distinguish distribution of 
        #       students with declining math scores based on their
        #       study time and then reflect the percentages as labels 
        #       in each corresponding histogram (Analysis 4.2)

#------------------------------------------------------------------------------#

# AF_10 (Tibble)

    # tibble:add_column()

        # ->    able to add one or more columns to any position of
        #       an existing data frame 

        # E.g.  used add_column() to insert students' average alcohol
        #       consumption "AVG_ALC" on the right of students' weekend
        #       alcohol consumption "Walc" (Data Processing)

        # E.g.  used add_column() to insert the average education
        #       level of students' parents "Pedu" on the right of 
        #       education level of students' father "Fedu"
        #       (Data Processing)





 





















