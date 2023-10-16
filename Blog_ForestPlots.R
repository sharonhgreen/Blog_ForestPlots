################################################################################
# Using Forest Plots to Report Regression Estimates: 
# A Useful Data Visualization Technique
# Berkeley D-Lab Blog Post
# Sharon H. Green
# 10/16/2023
################################################################################


##### Step 1: Load the necessary libraries and inspect the data ################

library(tidyverse)
library(broom)
library(ggplot2)
library(forestplot)

head(mtcars)

a <- ggplot(data = mtcars, aes(x= wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "Weight", 
       y = "Miles per gallon", 
       title = "Relationship between weight and miles per gallon")
a


##### Step 2: Run regression models and review model output ####################

# Model 1: unadjusted
m1 <- lm(mpg ~ wt, data = mtcars) 
summary(m1)

# Model 2: adjusted for quarter-mile time
m2 <- lm(mpg ~ wt + qsec, data = mtcars) 
summary(m2)

# Model 3: adjusted for horsepower
m3 <- lm(mpg ~ wt + hp, data = mtcars) 
summary(m3)

# Model 4: adjusted for quarter-mile time and transmission type
m4 <- lm(mpg ~ wt + qsec + am, data = mtcars)
summary(m4)


##### Step 3: Summarize the regression output ##################################

df_m1 <- tidy(m1, conf.int = TRUE) 
df_m2 <- tidy(m2, conf.int = TRUE) 
df_m3 <- tidy(m3, conf.int = TRUE) 
df_m4 <- tidy(m4, conf.int = TRUE) 

head(df_m1)

# Extract the weight coefficient and 95% confidence interval from each model
df <- rbind(df_m1, df_m2, df_m3, df_m4) %>%
  filter(term == "wt") %>%
  select(term,estimate,conf.low,conf.high)

# Create a vector with labels for each model
label <- c("Model 1", "Model 2", "Model 3", "Model 4")

# Create a data frame containing a row for each model, and each row containing 
# the model’s weight coefficient and 95% confidence interval
df <- cbind(label,df)

head(df)


##### Step 4 Option 1: Create forest plot using ggplot2 ########################

df %>%
  ggplot(aes(x=fct_rev(label),y=estimate,ymin=conf.low,ymax=conf.high)) +
  geom_pointrange(color = "black", size = .5) +
  geom_hline(yintercept = 0, color = "steelblue") +  
  coord_flip() +
  xlab(" ") +
  ylab("Coefficient (95% Confidence Interval)") +
  labs(title ="Linear Regression Models Estimating the Effects of Car Weight \n on Gas Mileage") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12))


##### Step 4 Option 2: Create forest plot using forestplot package #############

forestplot(labeltext = df$label,
           mean = df$estimate,
           lower = df$conf.low,
           upper = df$conf.high,
           xlab = "Adjusted Coefficients and 95% Confidence Intervals",
           boxsize = 0.1,
           col = fpColors(box = "black", line = "black", summary = "black",
                          zero = "steelblue"),
           txt_gp = fpTxtGp(label  = gpar (cex = 1.0),
                            xlab = gpar(cex = 1.0),
                            ticks = gpar (cex = 1.0),
                            title = gpar (cex = 1.0)),
           grid = TRUE,
           title = "Linear Regression Models Estimating the Effects of Car Weight on Gas Mileage")
