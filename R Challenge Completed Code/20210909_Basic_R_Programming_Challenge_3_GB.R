rm(list=ls());cat("\014") #clear workspace & console

####initial set-up####
library(stringr)
library(dplyr)
setwd("C:/Users/graha/Documents/Dennis_Lab/Onboarding Modules/Basic R Challenges - for external share - v2/Basic R Challenge 3")

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
some_labs<-read.table("SOME_LABS.txt",header=TRUE,sep="\t")
hdl_ldl_values<-read.delim("OMNI_Q_HDL_LDL_TRIGS_DEMO.txt",sep="\t")

#merge df on id
df<-merge(some_labs,hdl_ldl_values,by="id")
count(df)

##clean data##

#Remove lab values that are non-numeric
df$lab_value<-as.numeric(as.character(df$lab_value))
df_cleaned<-df%>%filter(is.na(df$lab_value)==FALSE)
non_numeric_lab_values<-df%>%filter(is.na(df$lab_value)==TRUE)

#Clean DOB ear (illustrative as none of the values actually require cleaning)
df_cleaned<-df_cleaned%>%filter(as.numeric(str_sub(df$dob,-4,-1))<=1900 || as.numeric(str_sub(df$dob,-4,-1))<=2022 || is.na(str_sub(df$dob,-4,-1))==FALSE )

##Add age @ event
#Creating age at event (rounded)
df_cleaned$AGE_EVENT<-as.numeric(str_sub(df_cleaned$lab_date,-4,-1))-as.numeric(str_sub(df_cleaned$dob,-4,-1))

#Filtering df into pediatrics and adults
ped_df<-df_cleaned%>%filter(AGE_EVENT<18)
adult_df<-df_cleaned%>%filter(AGE_EVENT>=18)

###Generate mean lab values / median lab values for each lab
#Generate Mean lab values for each individual in each population
ped_mm_ind<-as.data.frame(ped_df%>%group_by(id,lab_shortname)%>%summarise_at(c("lab_value"),list(Mean=mean,Median=median)))
adult_mm_ind<-as.data.frame(adult_df%>%group_by(id,lab_shortname)%>%summarise_at(c("lab_value"),list(Mean=mean,Median=median)))

#Create df that includes id,lab_name,lab_value (orig, mean, median) for each individual, lab date, DOB, gender, age
ped_record<-merge(ped_df,ped_mm_ind,by=c("id","lab_shortname"))
ped_record<-ped_record[c(1:5,8:11)]
adult_record<-merge(adult_df,adult_mm_ind,by=c("id","lab_shortname"))
adult_record<-adult_record%>%select(-c(6:7))
head(adult_record)


# adult_mm_ind<-as.data.frame(adult_df%>%group_by(ID)%>%summarise_at(c("LAB_VALUE"),list(Mean=mean,Median=median)))

#output
output_file_name<-output_formating("ped_indiviual_mean_median",lab_member,"txt")
ped_mm_ind_added_fields_output<-write.table(ped_record,file="ped_sum.txt",quote=FALSE,sep="\t")
output_file_name<-output_formating("adult_indiviual_mean_median",lab_member,"txt")
adult_mm_ind_added_fields_output<-write.table(adult_record,file="adult_sum.txt",quote=FALSE,sep="\t")

####Creating plots####
# Scatter
plot_title<-"Median_Lab_Values_Adult_Population"
plot_file_name<-vis_formatting(plot_title,"Scatter",lab_member)
pdf(paste(plot_file_name,"pdf",sep="."))
plot(adult_record$Median,main=plot_title)
dev.off()

#Histogram
plot_file_name<-vis_formatting(plot_title,"Histogram",lab_member)
pdf(paste(plot_file_name,"pdf",sep="."))
hist(adult_record$Median,main=plot_title)
dev.off()

#Boxplot (Unclear)
plot_file_name<-vis_formatting(plot_title,"Boxplot",lab_member)
pdf(paste(plot_file_name,"pdf",sep="."))
boxplot(Median ~ GENDER,data=adult_record, main=paste("Dennis Lab:",plot_file_name))
dev.off()
