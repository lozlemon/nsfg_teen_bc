setwd("~/OneDrive/Documents/Projects/Hormonal BC Teens")

# read.table("2017_2019_FemRespData.dat")

library(devtools)
# https://www.cynkra.com/blog/2021-03-16-gfortran-macos/
# Still couldn't find fortran to compile
# installed older version of Hmisc from binary so I didn't have to use fortran to compile it
install_github("ajdamico/lodown",dependencies=T)

library(lodown)

nsfg_cat <-
  get_catalog( "nsfg" ,
               output_dir = file.path( path.expand( "." ) , "NSFG" ) )

# 2017-2019 only
nsfg_cat <- subset( nsfg_cat , grepl( "2017_2019" , full_url ) )
# download the microdata to your local computer
nsfg_cat <- lodown( "nsfg" , nsfg_cat )

options( survey.lonely.psu = "adjust" )

library(survey)

nsfg_df <- readRDS( file.path( path.expand( "." ) , "NSFG" , "2017_2019_FemRespData.rds" ) )


nsfg_design <- 
  svydesign( 
    id = ~ secu , 
    strata = ~ sest , 
    data = nsfg_df , 
    weights = ~ wgt2017_2019 , 
    nest = TRUE 
  )

teen_data <- nsfg_df[which(nsfg_df$age_a < 19),]

names(teen_data)

# Why someone uses the pill. can say up to 7 reasons. Column 7 all missing. 1: birth control
table(teen_data$yusepill1)

pill_reason_columns <- paste0("yusepill",1:6)

use_pill_bc <- rowSums(teen_data[,pill_reason_columns] == 1,na.rm=T) > 0
use_pill <- rowSums(!is.na(teen_data[,pill_reason_columns])) > 0
nrow(teen_data)

sum(use_pill_bc)/sum(use_pill)
sum(use_pill)/nrow(teen_data)

## Variables to indicate abuse?
# Education of current partner
table(use_pill,teen_data$cpeduc)

# not enough data
# First partner education
table(use_pill,teen_data$fpeduc)
# Highest education of first partner is 'Some College'

# First sexual intercourse came before first menstrual period
table(use_pill,teen_data$which1st)
# Missing data

## Oh, check the audio portion questionnaire, this has more sensitive questions
#Age of first vaginal intercourse
table(use_pill,teen_data$agevagr)
#pct
table(use_pill,teen_data$agevagr)/rowSums(table(use_pill,teen_data$agevagr))

# Table of age/partner education/uses the pill
# Age on rows
# First partner education on columns. 1: < hs, 2: hs, 3: some college
table(teen_data$agevagr,teen_data$fpeduc,use_pill)

#Age of partner at first vaginal intercourse
# table(use_pill,teen_data$agevagm) Doesn't exist?

# Argh, where did the audio questions go. One of them had age gap with current partner. :/
