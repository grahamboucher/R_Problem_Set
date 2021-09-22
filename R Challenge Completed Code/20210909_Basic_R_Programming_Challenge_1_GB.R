
setwd("C:/Users/graha/Documents/Dennis_Lab/Onboarding Modules/Basic R Challenges - for external share - v2/Basic R Challenge 1")

ldl_values<-read.delim("OMNI_Q_LDL_VALUES.txt",sep="\t")
lipid_demo<-read.delim("OMNI_Q_LIPIDS_SUBSET_DEMOGRAPHICS.txt")

df<-merge(ldl_values,lipid_demo,by.x="ID",by.y="STUDYID")
head(df)

# Scatter
pdf("file_name.pdf")
plot(df$LAB_VALUE,main="Histogram of Lab Values")
dev.off()

#Histogram
hist(df$LAB_VALUE,main="Histogram of Lab Values")

#Boxplot (Unclear)
boxplot(LAB_VALUE ~ GENDER,data=df, main="Boxplot by Gender")

### Extra Credit
#Issue with merging via ID
ldl_values[ldl_values$ID %in% c(490),]
lipid_demo[lipid_demo$STUDYID %in% c(490),]
df[df$ID %in% c(490),]

#Since there is no other information, it is impossible to put any parameters on how to match the data in a better manner