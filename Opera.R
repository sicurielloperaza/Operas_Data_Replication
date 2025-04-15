rm(list = ls()) # remove all objects, start from scratch
cat("\014")

library(tidyverse) 
library(huxtable) 
library(tidyr)
library(broom)
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
library(ggplot2)
library(openxlsx)
library(writexl)
library(knitr)
library(kableExtra)
library(xtable)

setwd("/Users/danielperaza/Downloads/Opera_DataLab") 

#Shortcuts
indb <- "/Users/danielperaza/Downloads/Opera_DataLab"
outdb <- "/Users/danielperaza/Downloads/Opera_DataLab"
tabs <- "/Users/danielperaza/Downloads/Opera_DataLab/tables"
grap <- "/Users/danielperaza/Downloads/Opera_DataLab/graphs"

#opening the dtset
operas_df <- read_stata(file.path(indb, "operas_data.dta"))


#some data work
print(unique(operas_df$state))

##############################################################################
#                                   Question 1                               #
##############################################################################

#FIGURE 1 SECOND VERSION
operas_c <- operas_df %>% mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            operas_mean = operas_count/regions) %>%
  mutate(treated_name = ifelse(treated == 0, "Other States", "Lombardy & Venetia"))

plot_1 <- ggplot(operas_c, aes(x = year, y = operas_mean, color = factor(treated_name), linetype = factor(treated_name))) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_vline(xintercept = 1801, linetype = "dashed") +
  annotate("text", x = 1801, y = max(operas_c$operas_mean), label = "1801 Copyright Law", vjust = -1.5, hjust = -0.1) +
  labs(color = "", linetype = "") +
  xlab("Year") +
  ylab("Mean New Operas per Year") +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("black", "black")) +
  theme_minimal() +
  ylim(0, 8) +
  theme(legend.position = "bottom")
print(plot_1)


file_name <- "graph_1.png"
file_path <- file.path(grap, "graph_1.png")
ggsave(filename = file_path, plot = plot_1, width = 15, height = 6, dpi = 300)


#WE HAVE LESS OBS IN THIS DATASET THATS WHY THE GRAPH DOES NOT LOOK THE SAME



##############################################################################
#                                   Question 2                               #
##############################################################################

# Here the reshaping is done with "spread"
# Refer to the code I have uploaded about ttest, reg and reshape
# for more details on this command

str(operas_df)

#---------------------------#
#  FIRST PANEL OF THE TABLE #
#---------------------------#
operas_table <- operas_df %>%
  filter(year %in% 1781:1820) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion) 

#View(operas_table) ADDING A ROW FOR THE ENTIRE PERIOD
total_period_row <- operas_df %>%
  filter(year %in% 1781:1820) %>% 
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) 
total_period_row <-  total_period_row   %>%
  mutate(yr00_20 = -1)
operas_table <- bind_rows(operas_table, total_period_row) 

#sorting the rows
operas_table <- operas_table %>%
  arrange(yr00_20)
#Sorting columns 
operas_table <- operas_table %>%
  select(yr00_20, `1`, `0`)


#---------------------------#
# SECOND PANEL OF THE TABLE #
#---------------------------#
operas_table2 <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(annals)) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = sum(annals==1),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion) 

#View(operas_table) ADDING A ROW FOR THE ENTIRE PERIOD
total_period_row2 <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(annals)) %>% 
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated) %>% 
  summarise(operas_count = sum(annals==1),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) 
total_period_row2 <-  total_period_row2   %>%
  mutate(yr00_20 = -1)
operas_table2 <- bind_rows(operas_table2, total_period_row2) 

#sorting the rows
operas_table2 <- operas_table2 %>%
  arrange(yr00_20)
#Sorting columns 
operas_table2 <- operas_table2 %>%
  select(yr00_20, `1`, `0`)


#---------------------------#
# THIRD PANEL OF THE TABLE  #
#---------------------------#
operas_table3 <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(amazon)) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = sum(amazon==1),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion) 

#View(operas_table) ADDING A ROW FOR THE ENTIRE PERIOD
total_period_row3 <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(amazon)) %>% 
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated) %>% 
  summarise(operas_count = sum(amazon==1),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) 
total_period_row3 <-  total_period_row3   %>%
  mutate(yr00_20 = -1)
operas_table3 <- bind_rows(operas_table3, total_period_row3) 

#sorting the rows
operas_table3 <- operas_table3 %>%
  arrange(yr00_20)
#Sorting columns 
operas_table3 <- operas_table3 %>%
  select(yr00_20, `1`, `0`)


#---------------------------#
# FOURTH PANEL OF THE TABLE #
#---------------------------#
operas_table4 <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(amazon), !is.na(annals)) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), 
         popular = ifelse(annals == 0 & amazon == 0, 0, 1), 
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = sum(popular == 1),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion) 

total_period_row4 <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(amazon), !is.na(annals)) %>% 
  mutate(popular = ifelse(annals == 0 & amazon == 0, 0, 1),
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated) %>% 
  summarise(operas_count = sum(popular == 1),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) 
total_period_row4 <- total_period_row4 %>%
  mutate(yr00_20 = -1)
operas_table4 <- bind_rows(operas_table4, total_period_row4) 

operas_table4 <- operas_table4 %>%
  arrange(yr00_20)
operas_table4 <- operas_table4 %>%
  select(yr00_20, `1`, `0`)


#APPENDING AL TABLES 
operas_table_total <- bind_rows(operas_table, operas_table2, operas_table3, operas_table4)
operas_table_total$`1` <- round(operas_table_total$`1`, 3)
operas_table_total$`0` <- round(operas_table_total$`0`, 3)

#EXPORTING THE TABLES TO AN EXCEL FILE
excel_file <- file.path(tabs, "your_file_name.xlsx")
write.xlsx(operas_table_total, excel_file, colNames = TRUE, startCol = 6)


filtered_data <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(annals), annals == 1)
cat("Number of observations:", nrow(filtered_data), "\n")

filtered_data <- operas_df %>%
  filter(year %in% 1781:1820, !is.na(amazon), amazon == 1)
cat("Number of observations:", nrow(filtered_data), "\n")


filtered_data <- operas_df %>%
  mutate(filter_condition = ifelse(year %in% 1781:1820 & (annals != 0 | amazon != 0), 1, 0)) %>%
  filter(filter_condition == 1) %>%
  select(-filter_condition)
cat("Number of observations:", nrow(filtered_data), "\n")



##############################################################################
#                                   Question 3                               #
##############################################################################


#3A REGRESSIONS FOR ALL 
operas_forreg <- operas_df %>%
  filter(year %in% 1781:1820, first_name!="", title!="") %>% #drop the 1821 operas
  mutate(post = as.numeric(year>=1800), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(year, post, treated, state) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length),
         post_treat = post*treated) 

#First regression with 
reg_3a <- felm(operas_peryar_perregion ~ post_treat| 
                 state +year| # two FEs? use a + to add them
                 0,
               data=operas_forreg)
summary(reg_3a)

#3B
operas_reg_popular <- operas_df %>%
  filter(title != "" & first_name != "") %>%
  mutate(post = as.numeric(year>=1800), 
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(year, post, treated, state) %>% 
  summarise(historically_popular_count = sum(annals, na.rm = TRUE),
            durable_count = sum(amazon, na.rm = TRUE),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(historically_pypr = historically_popular_count/(regions*length),
         durable_pypr = durable_count/(regions*length),
         post_treat = post*treated)

reg_3b1 <- felm(historically_pypr ~ post_treat | state + year,
                data=operas_reg_popular)

summary(reg_3b1)

reg_3b2 <- felm(durable_pypr ~ post_treat | state + year,
                data=operas_reg_popular)



#3C1 SAME AS 3A BUT WITHOUT FIXED EFFECTS
operas_forreg <- operas_df %>%
  filter(year %in% 1781:1820, first_name!="", title!="") %>% #drop the 1821 operas
  mutate(post = as.numeric(year>=1800), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(year, post, treated, state) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length),
         post_treat = post*treated) 

#First regression with 
reg_3c <- felm(operas_peryar_perregion ~ post + treated + post_treat| 
                 0| # two FEs? use a + to add them
                 0,
               data=operas_forreg)
summary(reg_3c)

#3D SAME AS 3A BUT WITH ROBUST 
summary(reg_3a)
summary(reg_3a, robust=T)


#3E CLUSTERING 
reg_3e <- felm(operas_peryar_perregion ~ post_treat| 
                 state +year| # two FEs? use a + to add them
                 0 | state,
               data=operas_forreg)
summary(reg_3e)


#a b1 b2 and c for one table

#PUTTING AL REGRESSIONS IN A SINGLE TABLE
reg_table1 <- huxreg(reg_3a, reg_3b1, reg_3b2, reg_3c, 
                     stars = c(`*` = 0.05, `**` = 0.01, `***` = 0.001),
                     coefs = c("Treated X post-1800" = "post_treat", # renaming variables
                               "Post-1800" = "post",
                               "Treated" = "treated"),
                     statistics = c("N" = "nobs", 
                                    "R^2" = "r.squared")) %>%
  add_rows(rbind(c("State FE", "Yes", "Yes", "Yes", "No"), #note! you need more "yes" if you have >3 models
                 c("Year FE", "Yes", "Yes", "Yes", "No")),
           copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) 
print(reg_table1)

print(xtable(reg_table1, type = "latex"), digits = 3)


reg_table1 %>%
  kable("latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  as.character()



#%>% 
# quick_docx("report_reg.docx")


