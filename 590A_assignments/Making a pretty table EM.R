#EEB590: Make a pretty table from model results

# see this site for more ideas - https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/

# 1. Load libraries -------
library(tidyverse)
library(broom) # for making tibbles from model results. 
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
library(gtsummary) # another way to make a table from model results
library(gt) # for making pretty tables
library(webshot) # for saving gt objects as pdf or png

# 2. Load data -------
transplant<-read.csv("data/tidy/transplant_tidy_clean.csv", header=T)

# 3. Run model -------
webmod1 <- lm(websize ~ island * native, transplant)
summary(webmod1)

# 4. Make results from model into tidy table/tibble -------
tidywebmod <- tidy(webmod1) # the broom package
tbl_regression(webmod1) #the gt summary package

#other tables you can create from model object using broom function
augment(webmod1) 
glance(webmod1)

# 5. Make pretty table with gt -------
webmodtable <- tidywebmod %>% 
  gt()%>%
  tab_header(title = md("**Spider Model Results**")) %>%
  fmt_number(columns = c(estimate, std.error, statistic, p.value), decimals = 2)

# 6. Save table ----------
# This is actually a bit trickier than I'd like because gt produces an html file, and then you have to rely on another package (webshot) and an executable (PhantomJS) to get a screenshot of the html. 

install_phantomjs() #already did this do not need to do again

webmodtable %>%
  gtsave("8. Writing a manuscript/tables/webmodtable.png")
