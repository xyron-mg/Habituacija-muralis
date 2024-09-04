# Load the 'tidyr' library for data manipulation
library(tidyr)

# Rename the 'Group' column to 'Group_age' in the 'measurespod' dataframe
measurespod <- measurespod %>% 
  rename(
    Group_age = Group
  )

# Unite columns 'Group_age' to 'Sex' into a new column 'Group' separated by '-' in the 'measurespod' dataframe
measurespod <- measurespod %>% 
  unite("Group", Group_age:Sex, sep = "-", remove = FALSE)
  
# Write the 'measurespod' dataframe to an Excel file named "data_table_new group_PM.xlsx"
write.xlsx(measurespod, "data/data_table_new group_PM.xlsx")


##### Subseting ############## 

# Filter rows in 'measurespod' dataframe where 'a' column is greater than 0 and store it in 'measurespod_sub'
measurespod_sub <- filter(measurespod,  > 0)
measurespod_sub

#Control
#Delete part of column in all rows 
measurespod <- measurespod %>%
  mutate(Trial  = gsub("T", "", Trial))

# Filter rows in 'measurespod' dataframe where 'Species' column equals "PK" and store it in 'measurespod_sub'
measurespod <- filter(measurespod, Experiment == "Test")
measurespod
#Remove rows specific row from 'measurespod' dataframe
measurespod <- filter(measurespod, !Subject == "M20")

#Test
#Delete part of column in all rows 
measurespod <- measurespod %>%
  mutate(Trial  = gsub("T", "", Trial))

# Filter rows in 'measurespod' dataframe where 'Species' column equals "PK" and store it in 'measurespod_sub'
measurespod <- filter(measurespod, Experiment == "Test")
measurespod
#Remove rows specific row from 'measurespod' dataframe
measurespod <- filter(measurespod, !Subject == c("M12"))
measurespod <- filter(measurespod, !Subject %in% c("M12", "F16"))

str(measurespod)


# Find the maximum value in the 'a' column of 'measurespod_sub'
max(measurespod_sub$a)

# Subset the 'measurespod' dataframe by removing the 'Settlement_distance_m' column
measurespod <- subset(measurespod, select = -Settlement_distance_m)

# Remove rows 20, 22, and 35 from 'measurespod' dataframe
measurespod <- measurespod[-c(20, 22, 35), ]

#Remove rows specific row from 'measurespod' dataframe
measurespod <- filter(measurespod, !Subject == "F17")

# Subset 'measurespod' dataframe to extract rows where 'Independent.Variable' equals "Mainland 26"
measurespod[measurespod$Independent.Variable == "Mainland 26", ]

# Subset 'measurespod_sub' dataframe to extract the 26th row
measurespod_sub[26, ]
