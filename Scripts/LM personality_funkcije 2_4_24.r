
####Update R 
install.packages("installr")
require(installr)
check.for.updates.R() # tells you if there is a new version of R or not.
install.R() # download and run the latest R installer
copy.packages.between.libraries()# copy your packages to the newest R installation 
#from the one version before it (if ask=T, it will ask you between 
#which two versions to perform the copying)
#for citing
library(bibtex)
write.bib('sandwich', file='sandwich_ref')
write.bib(c('car', 'lme4', 'DescTools', 'multcomp'), file='references') ## for multiple packages
citation("lme4")
	
library(installr)
updateR()

	#PACKAGES NEEDED
install.packages("beepr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("ggpubr")
install.packages("ggsignif")
install.packages("agricolae")
install.packages("DescTools")
install.packages("DescTools")
install.packages("rcompanion")
install.packages("lindia")
install.packages("lsmeans")
install.packages("emmeans")
install.packages("conover.test")
install.packages("RColorBrewer")
install.packages("svglite")
install.packages("dplyr")
install.packages("MASS")
install.packages("car")
install.packages("multcomp")
install.packages("broom")
install.packages("rstatix")
install.packages("xlsx")
install.packages("sandwich")
install.packages("lme4")

library(lme4)
library(rcompanion)
library(agricolae)
library(DescTools)
library(ggpubr)
library(ggsignif)
library(ggplot2)
library(ggfortify)
library(ggstatsplot)
library(RColorBrewer)
library(svglite)
library(dplyr)
library(MASS)
library(lindia)
library(car)
library(multcomp)
library(emmeans)
library(conover.test)
library(broom)
library(rstatix)
library(PMCMRplus)
library(interactions)
library(xlsx)
library(sandwich)
library(Hmisc)
library(beepr)
# CLEAN SCREEN AND MEMORY

rm(list = ls())		#Remove all objects
cat("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n") # I like clean spaces when trying the same analysis with slight changes or differents datasets

sessionInfo() 

# SETTINGS:


# 1) INPUT FILE(S)



# DIRECTORY

getwd() #check
#Change working directory if needed
setwd("D:/Dropbox/R/gusteri zagrijavanje")


measurespod = read.table("zagrijavanje_sve.csv", sep = ',', header =TRUE)	  			#read the data into a table, read headers
str(measurespod)
tail(measurespod)

measurespod

#Set up the number of initial columns with individual or non-relevant/usaable data
otherstuff = 1
#Set up the number of columns with grouping variable
groupvar = 3
randomvar = 0



################################## Below this don't touch anythink unless you know what you are doing

coltags <- names(measurespod)
maxcol <- ncol (measurespod)								#number of columns
randomgrup = randomvar
lastgrup = otherstuff + groupvar + randomvar				#column with the last grouping variable

firstvar = lastgrup + 1										# column with the first variable
firstgrup = otherstuff+1
numvar = maxcol - lastgrup
varinfo <- measurespod[c(firstvar:maxcol)]									#take a subset including the data of the measured variables
groupinfo <- measurespod[c(firstgrup:lastgrup)]									#take a subset with the grouping variables to test
groupnames <- names (groupinfo)
colgroup = c(firstgrup:lastgrup)
colgrouprand = c(randomgrup)
colvariab = c(firstvar:maxcol)
varnames <- names(varinfo)

#CHECK BEFORE PROCEEDING
cat ("Check that this is alright:\n\t", maxcol, "columns in TOTAL\n\t", otherstuff, "with individual/non-relevant data\n\t", groupvar, " columns with grouping variables:", groupnames, "\n\t", numvar, "numeric variables:", varnames, "\n")

#measurespod$visina <- as.factor(measurespod$visina)

#run all functions for this code

##################################   run functions for normalizations from here ############################

# Define a function that performs Shapiro-Wilk normality test on all numeric columns in a data frame
shapiro_test_dataframe_numeric <- function(dataframe) {
  
  # Select only the numeric columns from the data frame
  numeric_cols <- sapply(dataframe, is.numeric) # returns a logical vector indicating which columns are numeric
  dataframe_numeric <- dataframe[, numeric_cols] # subset the data frame to include only numeric columns
  
  # Get the names of the numeric columns
  col_names <- colnames(dataframe_numeric)
  
  # Initialize a list to store the names of the non-normal columns
  non_normal_cols <- list()
  
  # Loop through each column and perform the Shapiro-Wilk test
  for (col in col_names) {
    col_data <- dataframe_numeric[, col] # extract the column data
    shapiro_test_result <- shapiro.test(col_data) # perform the Shapiro-Wilk normality test
    
    # Print the result of the test
    print(paste("Shapiro-Wilk normality test for column", col, "p-value:", shapiro_test_result$p.value))
    
    # Check if the data is normal or not based on the significance level of 0.05
    if (shapiro_test_result$p.value > 0.05) {
      print("Data is normal")
    } else {
      print("Data is not normal")
      non_normal_cols <- c(non_normal_cols, col) # add the name of the non-normal column to the list
    }
  }
  
  # Return the names of the non-normal columns
  return(non_normal_cols)
}


# Define function to normalize and transform variable
normalize_and_predict <- function(data, col_to_normalize) {
  
  # Normalize the column to be predicted
  norm_obj <- bestNormalize(data[[col_to_normalize]]) # Obtain normalization object
  predicted <- predict(norm_obj, newdata = data[[col_to_normalize]]) # Normalize the column
  
  # Add the predicted values to the data frame
  data[paste0(col_to_normalize, "_Transf")] <- predicted # Add normalized column to data frame
  
  # Check the normality of the transformed variable
  normality_test <- shapiro.test(predicted) # Perform normality test
  p_value <- normality_test$p.value # Extract p-value
  
  # Print the normalization object
  print(norm_obj)
  
  # Determine if transformed variable is normal
  if (p_value >= 0.05) {
    print("Transformed variable is normal.")
  } else {
    print("Transformed variable is not normal")
  }
  
 
}

# Define function to visualize plots of normalization and transformation variable
normalize_and_predict_plots <- function(data, col_to_normalize) {
  
  # Normalize the column to be predicted
  norm_obj <- bestNormalize(data[[col_to_normalize]]) # Obtain normalization object
  
  # Additional transformations and plots
  arcsinh_obj <- arcsinh_x(data[[col_to_normalize]])
  yeojohnson_obj <- yeojohnson(data[[col_to_normalize]])
  orderNorm_obj <- orderNorm(data[[col_to_normalize]])
  sqrt_x_obj <- sqrt_x(data[[col_to_normalize]])
  
  # Check if column contains values less than or equal to 0
  if (any(data[[col_to_normalize]] <= 0)) {
    cat("Box Cox and Log transformations not performed because input data contains non-positive values.\n")
    par(mfrow = c(3, 2)) # Adjust for additional plots
    
    MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 12)
    MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
    MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)
    MASS::truehist(sqrt_x_obj$x.t, main = "Sqrt transformation", nbins = 12)
    
    # Shapiro-Wilk normality test for each transformation
    cat("Shapiro-Wilk normality test for Arcsinh transformation:\n")
    print(shapiro.test(arcsinh_obj$x.t))
    
    cat("Shapiro-Wilk normality test for Yeo-Johnson transformation:\n")
    print(shapiro.test(yeojohnson_obj$x.t))
    
    cat("Shapiro-Wilk normality test for orderNorm transformation:\n")
    print(shapiro.test(orderNorm_obj$x.t))
    
    cat("Shapiro-Wilk normality test for Sqrt transformation:\n")
    print(shapiro.test(sqrt_x_obj$x.t))
  } else {
    # Additional transformations and plots
    boxcox_obj <- boxcox(data[[col_to_normalize]])
    log_x_obj <- log_x(data[[col_to_normalize]])
    
    par(mfrow = c(3, 2)) # Adjust for additional plots
    
    MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 12)
    MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
    MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)
    MASS::truehist(sqrt_x_obj$x.t, main = "Sqrt transformation", nbins = 12)
    MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 12)
    MASS::truehist(log_x_obj$x.t, main = "Log transformation", nbins = 12)
    
    # Shapiro-Wilk normality test for each transformation
    cat("Shapiro-Wilk normality test for Arcsinh transformation:\n")
    print(shapiro.test(arcsinh_obj$x.t))
    
    cat("Shapiro-Wilk normality test for Yeo-Johnson transformation:\n")
    print(shapiro.test(yeojohnson_obj$x.t))
    
    cat("Shapiro-Wilk normality test for orderNorm transformation:\n")
    print(shapiro.test(orderNorm_obj$x.t))
    
    cat("Shapiro-Wilk normality test for Sqrt transformation:\n")
    print(shapiro.test(sqrt_x_obj$x.t))
    
    cat("Shapiro-Wilk normality test for Box Cox transformation:\n")
    print(shapiro.test(boxcox_obj$x.t))
    
    cat("Shapiro-Wilk normality test for Log transformation:\n")
    print(shapiro.test(log_x_obj$x.t))
  }
}

#function perform boxcox transformation and paste at the end of dataframe
best_normalize_dataframe <- function(dataframe) {
  # Select only the numeric columns from the data frame
  numeric_cols <- sapply(dataframe, is.numeric)
  dataframe_numeric <- dataframe[, numeric_cols]
  
  # Get the names of the numeric columns
  col_names <- unique(colnames(dataframe_numeric))
  
  # Create an empty data frame to store the lambda values
  lambda_df <- data.frame(column = character(),
                           lambda = numeric(),
                           stringsAsFactors = FALSE)
  
  # Loop through each column and find the best normalizing transformation
  for (col in col_names) {
    col_data <- dataframe_numeric[, col]
    
    # Use the bestNormalize package to find the best normalizing transformation
    transformation <- bestNormalize(col_data)
    
    # Check if a valid transformation was found
    if (is.null(transformation$other_transforms$boxcox$lambda)) {
      warning(sprintf("No valid transformation found for column %s", col))
    } else {
      # Add a new row to the lambda data frame with the column name and lambda value from boxcox transformation
      lambda_df <- rbind(lambda_df, data.frame(column = col,
                                               lambda = transformation$other_transforms$boxcox$lambda,
                                               stringsAsFactors = FALSE))
      
      # Transform the column using the boxcox function with the lambda value
      transformed_col <- boxcox(col_data)
      
      # Add the predicted values to the data frame
      new_col_name <- paste0(col, "_Transf")
      while (new_col_name %in% colnames(dataframe)) {
        new_col_name <- paste0(new_col_name, "_1")
      }
      dataframe[new_col_name] <- transformed_col # Add normalized column to data frame
    }
  }
  
  # Print the lambda data frame as a table
  print(kable(lambda_df))
  
  # Return the transformed data frame
  return(dataframe)
}
#function for manual transformations
apply_transformations <- function(dataframe, column, transformation = c("arcsinh", "yeojohnson", "orderNorm", "sqrt", "boxcox", "log")) {
  library(bestNormalize)
  
  # Select the specified column
  col_data <- dataframe[[column]]
  
  # Apply specified transformation
  if (transformation == "arcsinh") {
    obj <- arcsinh_x(col_data)
  } else if (transformation == "yeojohnson") {
    obj <- yeojohnson(col_data)
  } else if (transformation == "orderNorm") {
    obj <- orderNorm(col_data)
  } else if (transformation == "sqrt") {
    obj <- sqrt_x(col_data)
  } else if (transformation == "boxcox") {
    obj <- boxcox(col_data)
  } else if (transformation == "log") {
    obj <- log_x(col_data)
  } else {
    stop("Invalid transformation specified.")
  }
  
  # Apply transformation to the column and add to data frame
  transformed_col <- predict(obj, newdata = col_data)
  col_name <- paste(column, "_", transformation, sep = "")
  dataframe[col_name] <- transformed_col
  
  # Return the transformed data frame
  return(dataframe)
}


#function for saving pca score
add_variables_to_pca_scores <- function(pca_scores, data_frame, var_names) {
  # Convert pca_scores to data frame
  pca_scores_df <- as.data.frame(pca_scores)
  
  # Add variables from original data frame to pca_scores
  for (var_name in var_names) {
    pca_scores_df[[var_name]] <- data_frame[[var_name]]
  }
  
  # Reorder the columns of pca_scores_df
  pca_scores_df <- pca_scores_df %>% 
    relocate(all_of(var_names), .before = 1)
  
  # Return the modified pca_scores data frame
  return(pca_scores_df)
}

####bartlet test function
test_homogeneity <- function(response_var, grouping_var1, grouping_var2, data, transform = NULL) {
  if (!is.null(transform)) {
    data[[response_var]] <- transform(data[[response_var]])
  }
  
  # Bartlett test
  bartlett_res <- bartlett.test(data[[response_var]] ~ data[[grouping_var1]])
  bartlett_res2 <- bartlett.test(data[[response_var]] ~ data[[grouping_var2]])
  bartlett_pvalue <- bartlett_res$p.value
  bartlett_pvalue2 <- bartlett_res2$p.value
  
  # Bartlett test with interaction term
  bartlett_int_res <- bartlett.test(data[[response_var]] ~ interaction(data[[grouping_var1]],data[[grouping_var2]]))
  bartlett_int_pvalue <- bartlett_int_res$p.value
  
 
  # Print results
  cat("Bartlett test by", grouping_var1, ":", "p-value =", bartlett_pvalue, "\n")
  cat("Bartlett test by", grouping_var2, ":", "p-value =", bartlett_pvalue2, "\n")
  cat("Bartlett test with interaction term by", grouping_var1, ",", grouping_var2, ":", "p-value =", bartlett_int_pvalue, "\n")
  
}
################              BOX PLOT            #####################################

# This function creates a plot that shows the relationship between a categorical x variable, a continuous y variable,
# and an optional fill variable using ggplot2. It also adds annotations for significant differences between groups
# using Dunn's test.

plot_social_pref_by_pop_pvalue <- function(data, x_var, y_var, fill_var = NULL) {
   # Coerce x_var and fill_var to factors
  data[[x_var]] <- as.factor(data[[x_var]])
  if (!is.null(fill_var)) {
    data[[fill_var]] <- as.factor(data[[fill_var]])
  }
  
  # Clean up the y variable name for use in plot labels
  y_label <- gsub("_", " ", y_var)
  x_label <- gsub("_", " ", x_var)
  
  if (is.null(fill_var)) {
    # Create the plot without fill variable
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), shape = 2, size = 2) +
      scale_x_discrete(limits = unique(data[[x_var]])) +
      labs(x = x_label, y = y_label) +
	  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
      theme_classic()
	  # Add pairwise comparisons between groups and dose levels using the bxp.complex function
  bxp.complex <- p +
    # pairwise comparisons between the two groups
    geom_pwc(
      aes(group = !!sym(x_var)), tip.length = 0,
      method = "t_test", label = "p.adj.signif",
      p.adjust.method = "bonferroni", p.adjust.by = "panel"
    ) 
	  
  } else {
    # Create the plot with fill variable
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
      geom_boxplot() +
	  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), shape = 2, size = 2) +
      scale_x_discrete(limits = unique(data[[x_var]])) +
      labs(x = x_label, y = y_label, fill = fill_var) +
      scale_fill_brewer(palette = "Paired") + # add this line
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
      theme_classic()
  
  
  # Add pairwise comparisons between groups and dose levels using the bxp.complex function
  bxp.complex <- p +
    # pairwise comparisons between the two groups
    geom_pwc(
      aes(group = !!sym(x_var)), tip.length = 0,
      method = "t_test", label = "p.adj.signif",
      p.adjust.method = "bonferroni", p.adjust.by = "panel"
    ) +
    
	# pairwise comparisons between dose levels
    geom_pwc(
      method = "t_test", label = "p.adj.signif",
      p.adjust.method = "bonferroni",
      bracket.nudge.y = 0.2
    )

  }
  # Save the plot as a tiff file
  tiff(filename = paste0("graph_", y_var, x_var, fill_var, "_p-value", ".tiff"), units = "cm", width = 9, height = 8, res = 600)
  print(bxp.complex)
  dev.off()
  
  # Save the plot as a svg file for editing options
  svglite(filename = paste0("graph_", y_var, x_var, fill_var, "_p-value", ".svg"), width = 3.54, height = 3.15)
  print(bxp.complex)
  dev.off()
}

#plot nice graphs
plot_social_pref_by_pop <- function(data, x_var, y_var, fill_var = NULL) {
  y_label <- gsub("_", " ", y_var)
  if (is.null(fill_var)) {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), shape = 2, size = 1.5) +
      scale_x_discrete(limits = unique(data[[x_var]])) +
      labs(x = "Location", y = y_label) +
      theme_classic()
  } else {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), shape = 2, size = 1.5) +
      scale_x_discrete(limits = unique(data[[x_var]])) +
      labs(x = "Location", y = y_label, fill = fill_var) +
	  scale_fill_brewer(palette = "Paired") + # add this line
      theme_classic()
  }
  	
  # Save as tiff
  tiff(filename = paste0("graph_", y_var,x_var,fill_var, ".tiff"), units = "cm", width = 9, height = 8, res = 600)
  print(p)
  dev.off()
  
  # Save as svg file for editing options
  svglite(filename = paste0("graph_", y_var,x_var,fill_var, ".svg"), width = 3.54, height = 3.15)
  print(p)
  dev.off()
}

beep(4)
#################################            end of functions        ##############################################
#############################################################################################################

#####   Subseting   ##############    ^2
measurespod_sub <- filter(measurespod,  a >0)
measurespod_sub
measurespod_sub <- filter(measurespod, Location == "PK")
measurespod_sub
max(measurespod_sub$a )
m.lm <- lm( (a    )~ Location* Sex ,data=measurespod_sub)
#if data needs to be transformed use his line - put it in the script
t_measurespod <- measurespod %>% mutate_at(.vars = vars(firstvar:maxcol), funs (.+1))
str(t_measurespod)

#if data needs to be transformed use his line - put it in the script - square root
t_measurespod <- measurespod %>% mutate_at(.vars = vars(firstvar:maxcol), funs (sqrt(.+1)))
(t_measurespod)

measurespod[63,]
#if data is normal use fixed or mixed model
######################             fixed model      #####################################
m.lm <- lmer(  a ~  spec*svijetlo+ 1|ID ,data=measurespod)
str(measurespod)
# 1st plot -  if residuals have non-linear patterns.  If you find equally spread residuals around a 
#horizontal line without distinct patterns, that is a good indication you don’t have non-linear relationships.
# 2nd plot - normality
#3rd plot homoscedasticity- This plot shows if residuals are spread equally along the ranges of predictors
# It’s good if you see a horizontal line with equally (randomly) spread points.
#4th plot-  find influential cases - Not all outliers are influential in linear regression analysis
#par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
#par(mfrow=c(1,1)) # Change back to 1 x 1

plot(m.lm)
qqPlot(m.lm)
gg_reshist(m.lm)#look for gaus distribution
shapiro.test(residuals(m.lm)) # needs to be non significant
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
test_homogeneity("a", "Location", "Sex", measurespod, transform = NULL)
### if significant scroll to end of script and do welch or brown forsythe F test
#interact_plot(m.lm, pred = Location , modx = Location)
### to save run everthing in order to get proper save
anova(m.lm)
m = anova(m.lm)
linear_model = as.matrix(m)
linear_model
###save results
print(m)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/personality odrasli/linear_model_a.xlsx")


##### automatski save u excel 25.3.24.
install.packages("devtools")
devtools::install_github("kassambara/r2excel")

library(r2excel)
if (file.exists("linear_results.xlsx")) {
  wb <- loadWorkbook("linear_results.xlsx")
  sheet <- getSheets(wb)$sheet1
} else {
  wb <- createWorkbook()
  sheet <- createSheet(wb, sheetName = "sheet1")
}

# Add results to the workbook
xlsx.addHeader(wb, sheet, value = "food_eaten ") # edit name of analysed variable
xlsx.addLineBreak(sheet, 1)
xlsx.addTable(wb, sheet, m)  # m = object you are saving
xlsx.addLineBreak(sheet, 2)

# Save the workbook
saveWorkbook(wb, "linear_results.xlsx")


m= Anova(m.lm, white.adjust =TRUE)##white corrected anova
m
###### post hoc      ###############
#res=glht(m.lm, lsm(pairwise~Location*Sex))  #rastavljeni kod ako se nesto pošemeri
#lsm = summary(res, test =adjusted("BH"))
## ako je heteroscedasticity m.lm, lsm(pairwise~Group*Sex), vcov = vcovHC(m.lm, type = "HC3")),
library(multcomp)
lsm = summary(glht(m.lm, lsm(pairwise~Location*Sex)), test=adjusted("BH"))##pazi na zagrade: "(m.lm, lsm(pairwise~Location*Sex))" je jedna cjelina #### ako je heteroscedasticity m.lm, lsm(pairwise~Group*Sex), vcov = vcovHC(m.lm, type = "HC3")),
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/personality odrasli/posthoc_a .xlsx")  ####if p value is less than 0.001 it gives 0 in save. just replace in excel to <0.001

str(measurespod)

#########print plots
# Call the function without specifying a fill variable by Location
plot_social_pref_by_pop(measurespod, "Species", "SVL(mm)")

# Call the function with "Sex" as the fill variable by Location and Sex
plot_social_pref_by_pop(measurespod, "Species", "SVL(mm)", "Sex")


# Call the function without specifying a fill variable - with P - values
plot_social_pref_by_pop_pvalue(measurespod, "Species", "SVL(mm)")

# Call the function with "Sex" as the fill variable  with P - values
plot_social_pref_by_pop_pvalue(measurespod, "Species", "SVL(mm)", "Sex")
beep(4)


measurespod= measurespod %>% 
  rename(
    "SVL(mm)(mm)" = SVL(mm)
       )
#select row by id 
measurespod[measurespod$Independent.Variable == "Mainland 26", ]
measurespod_sub[26,]
str(measurespod)
31,72,87,22
##													subset data for outliers                        #######################################
t_measurespod_corr <- measurespod_sub [-c(15),]
t_measurespod_corr
######################             fixed model without outliers             ##################
m.lm <- lm(  a     ~ Location ,data=t_measurespod_corr)
#plot dispersion of errors - 3rd plot Homogenost varijanci
plot(m.lm)
qqPlot(m.lm)
gg_reshist(m.lm)#look for gaus distribution
shapiro.test(residuals(m.lm)) # needs to be non significant
## Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
test_homogeneity("a", "Location", "Sex", measurespod, transform = NULL)
### if significant scroll to end of script and do welch or brown forsythe F test
#type 3 for unbalanced design :
#Anova(m.lm, contrasts=list(Location=contr.sum, Sex=contr.sum), type=3)
interact_plot(m.lm, pred = Social preference index, modx = Location)
### to save run everthing in order to get proper save
anova(m.lm)
m = anova(m.lm)
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/personality odrasli/linear_model_a _bez_outliera.xlsx")
####if bartlet is significant use anova with adjustment -> Anova(m.lm, white.adjust =TRUE)
m=Anova(m.lm, white.adjust =TRUE)
m   

vif(m.lm)

#for other adjustment when needed - post hoc 
library(multcomp)
library(sandwich)
lsm <- summary(glht(m.lm, lsm(pairwise~Group), test = adjusted("BH"))) #vcov = vcovHC(m.lm2, type = "HC5"
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/personality odrasli/posthoc_a   _bez_outliera .xlsx")

##### plot BOX plots ###################

# Call the function without specifying a fill variable
plot_social_pref_by_pop(t_measurespod_corr, "Location", "a")

# Call the function with "Sex" as the fill variable
plot_social_pref_by_pop(t_measurespod_corr, "Location", "a", "Sex")


# Call the function without specifying a fill variable - with P - values
plot_social_pref_by_pop_pvalue(t_measurespod_corr, "Location", "a")

# Call the function with "Sex" as the fill variable  with P - values
plot_social_pref_by_pop_pvalue(t_measurespod_corr, "Location", "a", "Sex")


############################### 		^2			trasnformed variable 			###################################################
#install.packages("metafor")

library(metafor)  ##google transformacije 
##transf.logit

m.lm2 <- lm( sqrt(a) ~ Location*Sex, data=t_measurespod_corr) #**(-2) is transformation
plot(m.lm2)
gg_reshist(m.lm2)
qqPlot(m.lm2)
shapiro.test(residuals(m.lm2))
summary(m.lm2)
anova(m.lm2)
m=anova(m.lm2)
m
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/personality odrasli/linear_model_a_transf_bez_outliera.xlsx")

coefficients(m.lm2)
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
test_homogeneity("a", "Location", "Sex", measurespod, transform = NULL)
### if significant  do welch or brown forsythe F test
m=Anova(m.lm2, white.adjust ="hc3")
m
#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm2, lsm(pairwise~Location*Sex)), test = adjusted("BH"))
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/personality odrasli/posthoc_a_transf_bez_outliera.xlsx")

##### plot BOX plots ###################

# Call the function without specifying a fill variable
plot_social_pref_by_pop(t_measurespod_corr, "Location", "a")

# Call the function with "Sex" as the fill variable
plot_social_pref_by_pop(t_measurespod_corr, "Location", "a", "Sex")


# Call the function without specifying a fill variable - with P - values
plot_social_pref_by_pop_pvalue(t_measurespod_corr, "Location", "a")

# Call the function with "Sex" as the fill variable  with P - values
plot_social_pref_by_pop_pvalue(t_measurespod_corr, "Location", "a", "Sex")



#find valid transformation
library(MASS)
library(VGAM)
#install.packages("VGAM")
boxcox(m.lm, lambda = seq(-0.3, 0.3, length = 10))
b <- boxcox(m.lm, lambda = seq(-0.3, 0.3, length = 10))
b 
lambda <- b$x # lambda values
lik <- b$y # log likelihood values for SSE
bc <- cbind(lambda, lik) # combine lambda and lik
sorted_bc <- bc[order(-lik),] # values are sorted to identify the lambda value for the maximum log likelihood for obtaining minimum SSE
head(sorted_bc, n = 10)

#Table 1: Common Box-Cox Transformations
# l			Y’
# -2	Y-2 = 1/Y2
# -1	Y-1 = 1/Y1
# -0.5	Y-0.5 = 1/(Sqrt(Y))
# 0		log(Y)
# 0.5	Y0.5 = Sqrt(Y)
# 1		Y1 = Y
# 2			Y2


shapiro.test(residuals(m.lm))
library(metafor)
###########################  analyse trasnformed variable  ^3    #############################
m.lm2 <- lm( log(a)  ~ Location* Sex, data=measurespod_sub) #**(-2) is transformation
measurespod
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
par(mfrow=c(1,1)) # Change back to 1 x 1

plot(m.lm2)
gg_reshist(m.lm2)
qqPlot(m.lm2)
shapiro.test(residuals(m.lm2))
summary(m.lm2)
anova(m.lm2)
coefficients(m.lm2)
# Bartlett Test of Homogeneity of Variances - parametric variables - needs to be non significant 
test_homogeneity("a", "Location", "Sex", measurespod, transform = NULL)
#type 3 for unbalanced design :
### to save run everthing in order to get proper save
anova(m.lm2)
m = anova(m.lm2)
linear_model = as.matrix(m)
linear_model
###save results
print(linear_model)
library(xlsx)
write.xlsx(linear_model, "D:/Dropbox/R/personality odrasli/linear_model_transf_a.xlsx")
####if bartlet is significant use anova with adjustment -> Anova(m.lm, white.adjust =TRUE)

Anova(m.lm2, white.adjust =TRUE) ###heteroscedasticity-corrected

#posthoc test
lsm <- lsmeans(m.lm2, list(pairwise ~ Location*Sex), adjust = "tukey")
lsm
lsm <- lsmeans(m.lm2, list(pairwise ~ Location*Sex))
lsm.glht <- as.glht(lsm)
summary(lsm.glht, lsm(pairwise~Location*Sex), test = adjusted("BH"))

#for other adjustment when needed - post hoc 
library(multcomp)
lsm <- summary(glht(m.lm2, lsm(pairwise~Location*Sex)), test = adjusted("BH")))
lsm 
tidy(lsm)
posthoc = tidy(lsm)
posthoc
#save posthoc test to output
write.xlsx(posthoc, "D:/Dropbox/R/personality odrasli/posthoc_a.xlsx")


##### plot BOX plots ###################

# Call the function without specifying a fill variable
plot_social_pref_by_pop(measurespod, "Location", "a")

# Call the function with "Sex" as the fill variable
plot_social_pref_by_pop(measurespod, "Location", "a", "Sex")


# Call the function without specifying a fill variable - with P - values
plot_social_pref_by_pop_pvalue(measurespod, "Location", "a")

# Call the function with "Sex" as the fill variable  with P - values
plot_social_pref_by_pop_pvalue(measurespod, "Location", "a", "Sex")

##############################           end             ###########################



###outlier finder in plots + nice summary
install.packages(ggstatsplot)
library(ggstatsplot)

p=ggbetweenstats(
    data =t_measurespod_corr,
    x = Group,
    y = a,
    grouping.var = Group,
    plot.type = "box",
    outlier.tagging = TRUE,
    outlier.label = Independent.Variable,
    outlier.coef = 2,
    messages = FALSE
	)

p





# alternative plot  by facet wrap - 2 plots in one
p <- ggplot(measurespod, aes(x=Location, y=Steroid,)) +
   geom_boxplot() +
   facet_wrap(~ Type_steroid)+
   scale_fill_brewer(palette="Greys") +  #####palette="BuPu", Paired, 
   stat_summary(fun = mean, geom="point", position = position_dodge(width = 0.75),  shape=2, size=2)  ## position_dodge(width = 0.75) play with number to position mean value correctly

print(p+labs(y = "Steroid ng/mL")+theme_classic())  #+labs for editing title, x or y axis...google  #+theme_bw- remove grey bacground , theme_classic() - white background
