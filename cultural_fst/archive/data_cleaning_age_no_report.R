# set working directory
#setwd("/Users/mac/Documents/myDocuments/academia/phd/asu/research/cultural_fst/data/norms/")

# Yes[i] ~ dbern(p[i])
# logit(p[i]) = group - baseline + geo_effect + schooling + town + sex + age + marital status
data = read.csv("cultural_fst_norms.csv", header = T, quote = "",
                stringsAsFactors = TRUE)

#norm
agemate_no_report = as.numeric(data$AGEMATE_NO_REPORT)-1
data = subset(data, agemate_no_report != 2)
agemate_no_report = as.numeric(data$AGEMATE_NO_REPORT)-1

#ethnic group (0 -> 3)
ethnic_group = as.numeric(data$ETHNIC_GROUP)-1
#sex (0 or 1)
sex = as.numeric(data$SEX)-1
# marital status (0 -> 4)
marital_status = as.numeric(data$MARITAL_STATUS)-1
#territorial section (Turkana only)
territorial_section = data$TERRITORIAL_SECTION
levels(territorial_section)[3]="999"
territorial_section=as.numeric(territorial_section)-1
#whether or not the individual has attended school
attend_school = as.numeric(data$ATTEND_SCHOOL)-1
#number of school years
school_yrs = data$SCHOOL_YRS
#whether or not the individual has lived in a town
town = as.numeric(data$TOWN)-1
#number of years lived in a town
town_yrs = data$TOWN_YRS

#store the vectors into a dataframe
data = data.frame(agemate_no_report, ethnic_group, sex, marital_status, territorial_section, 
                  attend_school, school_yrs, town, town_yrs)

#save the modified dataframe into a new csv file
write.csv(data, "cultural_fst_age_no_report.csv",  row.names = FALSE)
