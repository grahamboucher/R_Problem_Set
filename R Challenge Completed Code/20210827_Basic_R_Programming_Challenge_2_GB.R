####initial set-up####
library(stringr)
library(dplyr)
setwd("C:/Users/graha/Documents/Dennis_Lab/Onboarding Modules/Basic R Challenges - for external share - v2/Basic R Challenge 2")

#Output formating
output_formating<-function(desc,indiv,ext){
  output_file_name_default<-"DATE_DESC_INITIALS.extension"
  date<-gsub("-","",Sys.Date())
  ofn<-gsub("DATE",date,output_file_name_default)
  ofn<-gsub("DESC",desc,ofn)
  ofn<-gsub("INITIALS",indiv,ofn)
  ofn<-gsub("extension",ext,ofn)
}

#visualization formatting
vis_formatting<-function(desc,plt,indiv){
  output_file_name_default<-"DATE_DESC_PLOTTYPE_INITIALS"
  date<-gsub("-","",Sys.Date())
  ofn<-gsub("DATE",date,output_file_name_default)
  ofn<-gsub("DESC",desc,ofn)
  ofn<-gsub("PLOTTYPE",plt,ofn)
  ofn<-gsub("INITIALS",indiv,ofn)
}

#Setting initials for output formatting
lab_member<-"GB"

####Creating df####
#Reading in Values
ldl_values<-read.delim("OMNI_Q_LDL_VALUES.txt",sep="\t")
lipid_demo<-read.delim("OMNI_Q_LIPIDS_SUBSET_DEMOGRAPHICS.txt")

df<-merge(ldl_values,lipid_demo,by.x="ID",by.y="STUDYID")

#Creating age at event (rounded)
df$AGE_EVENT<-as.numeric(str_sub(df$LAB_DATE,-4,-1))-as.numeric(str_sub(df$DOB,-4,-1))

#Filtering df into pediatrics and adults
ped_df<-df%>%filter(AGE_EVENT<18)
adult_df<-df%>%filter(AGE_EVENT>=18)

#Summary Stats for each new df written as text files
ped_sum_stats<-summary(ped_df[c("ID","LAB_VALUE","GENDER","RACE","ETHNICITY","AGE_EVENT")])
adult_sum_stats<-summary(adult_df[c("ID","LAB_VALUE","GENDER","RACE","ETHNICITY","AGE_EVENT")])

output_file_name<-output_formating("ped_sum_stats",lab_member,"txt")
ped_sum_stat_output<-write.table(ped_sum_stats,file=output_file_name,quote=FALSE,sep="\t")
output_file_name<-output_formating("adult_sum_stats",lab_member,"txt")
ped_sum_stat_output<-write.table(adult_sum_stats,file=output_file_name,quote=FALSE,sep="\t")

#Generate Mean lab values for each individual in each population
ped_mm_ind<-as.data.frame(ped_df%>%group_by(ID)%>%summarise_at(c("LAB_VALUE"),list(Mean=mean,Median=median)))
adult_mm_ind<-as.data.frame(adult_df%>%group_by(ID)%>%summarise_at(c("LAB_VALUE"),list(Mean=mean,Median=median)))

#Generate df
ped_mm_ind_added_fields<-as.data.frame(ped_df%>%group_by(ID,GENDER,RACE,ETHNICITY,AGE_EVENT,DOB)%>%summarise_at(c("LAB_VALUE"),list(Mean=mean,Median=median)))
adult_mm_ind_added_fields<-as.data.frame(adult_df%>%group_by(ID,GENDER,RACE,ETHNICITY,AGE_EVENT,DOB)%>%summarise_at(c("LAB_VALUE"),list(Mean=mean,Median=median)))

output_file_name<-output_formating("ped_indiviual_mean_median",lab_member,"txt")
ped_mm_ind_added_fields_output<-write.table(ped_mm_ind_added_fields,file=,quote=FALSE,sep="\t")
output_file_name<-output_formating("adult_indiviual_mean_median",lab_member,"txt")
adult_mm_ind_added_fields_output<-write.table(adult_mm_ind_added_fields,file="ped_sum.txt",quote=FALSE,sep="\t")

####Creating plots####
# Scatter
plot_title<-"Median_Lab_Values_Adult_Population"
plot_file_name<-vis_formatting(plot_title,"Scatter",lab_member)
pdf(paste(plot_file_name,"pdf",sep="."))
plot(adult_mm_ind$Median,main=plot_title)
dev.off()

#Histogram
plot_file_name<-vis_formatting(plot_title,"Histogram",lab_member)
pdf(paste(plot_file_name,"pdf",sep="."))
hist(adult_mm_ind$Median,main=plot_title)
dev.off()

#Boxplot (Unclear)
plot_file_name<-vis_formatting(plot_title,"Boxplot",lab_member)
pdf(paste(plot_file_name,"pdf",sep="."))
boxplot(Median ~ GENDER,data=adult_mm_ind_added_fields, main="Boxplot by Gender")
dev.off()

