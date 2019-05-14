# Emotion Differentation script from Kalokerinos et al doi: 10.1177/0956797619838763

######2c. Making emotion differentiation index#####
#first making wave-by-wave datasets, since we want one index per wave per person
ldata_w1 <-subset(ldata, Wave == 1)
ldata_w2 <-subset(ldata, Wave == 2)
ldata_w3 <-subset(ldata, Wave == 3)

#####2ci)  Wave 1####
subjects_w1=unique(ldata_w1$PpID)

icc_list_w1=NULL
for (sub in subjects_w1){
  sub_ldata_w1=ldata_w1[ldata_w1[,1]==sub,c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")]  #selects data for the current subject
  icc=icc(sub_ldata_w1, model="twoway",unit="average")  #computes ICC
  icc_temp=icc$value

  fz_iccTemp=fisherz(icc_temp)

  row_temp=c(sub,icc_temp,fz_iccTemp)
  icc_list_w1=rbind(icc_list_w1,row_temp)
  print(fz_iccTemp)
}

colnames(icc_list_w1)=c("PpID","ICCneg","ICCneg.fz")
icc_list_w1=as.data.frame(icc_list_w1)


icc_list_w1=icc_list_w1[complete.cases(icc_list_w1),]  #choose subjects where both icc and icc_fz
icc_list_w1=icc_list_w1[icc_list_w1[,2]>0,]
