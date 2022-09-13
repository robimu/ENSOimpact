# SKrip mengukur dampak ENSO secara komposit
# Referensi Supari et al., 2017
# Skrip by : Robi Muharsyah (robimuharsyah@gmail.com)
# Bekasi, 22 April 2021
#===============================================================================
dirDataInput  = "D:/000_Rutin_Work_Kantor/04_Kerjaan_2022/07_MATERI_STMKG/Materi_1A_PRAKTEK_DAMPAK_ENSO/KOMPOSIT/Data_Input_Seasonal_PROV/"
dirDataOutput = "D:/000_Rutin_Work_Kantor/04_Kerjaan_2022/07_MATERI_STMKG/Materi_1A_PRAKTEK_DAMPAK_ENSO/KOMPOSIT/Data_Output_Seasonal_PROV/"
fileDataHujan = paste(dirDataInput,"Input_data_SeasonalTotal_allpos_BALI.csv",sep="")
fileInfoPos   = paste(dirDataInput,"Input_info_SeasonalTotal_allpos_BALI.csv",sep="")
fileONI   = paste(dirDataInput,"ONI_update.csv",sep="")

DataHujan  = read.csv(fileDataHujan,header=T)
InfoPos    = read.csv(fileInfoPos,header=T)
ONI        = read.csv(fileONI,header=T)
Avail      = InfoPos[,"SEAS_avail"]

#==== setting
tress  = 70 #persentase ketersediaan data yang dipilih
season = "MAM"#DJF, JFM, ...
rep    = 1000
alpha  = 5
  
#===== process
idxsel = which(Avail>tress)
InfoPossel = InfoPos[idxsel,]
getONI = ONI[,season]
getData = DataHujan[which(DataHujan[,2]==season),4:ncol(DataHujan)]
getData_run = getData[,idxsel] 

getData_LA = getData_run[which(getONI==-1),]
getData_NT = getData_run[which(getONI==0),]
getData_EL = getData_run[which(getONI==1),]


allout= array(NaN,dim=c(nrow(InfoPossel),7))
for (itpos in 1:nrow(InfoPossel))
  # for (itpos in 100:100)
{
  chnina   = getData_LA[,itpos]
  chnetral = getData_NT[,itpos]
  chnino   = getData_EL[,itpos]
  
  chnetralS = array(NaN,dim=c(length(chnetral),rep))
  for (iter1 in 1:rep)
  {
    chnetralS[,iter1] = sample(chnetral, replace = TRUE)
  }
  
  avchnino   = mean(chnino,na.rm=T)
  avchnina   = mean(chnina,na.rm=T)
  avchnetral = mean(chnetral,na.rm=T)
  avchnetralS= colMeans(chnetralS,na.rm=T)
  bb         = quantile(avchnetralS,alpha/100,na.rm=T)
  ba         = quantile(avchnetralS,1-alpha/100,na.rm=T)
  
  if (avchnino>ba || avchnino<bb)
    {nino_sig = 1} else {nino_sig = 0}
  if (avchnina>ba || avchnina<bb)
  {nina_sig = 1} else {nina_sig = 0}
  
  getout = cbind((100-alpha),avchnino-avchnetral,(avchnino-avchnetral)/avchnetral*100,nino_sig,
                 avchnina-avchnetral,(avchnina-avchnetral)/avchnetral*100,nina_sig)
  allout[itpos,]=getout

#==== rekap hasil
allout = data.frame(allout)
colnames(allout) = c("Sig.Lev","delta_Nino (mm)","delta_Nino (%)","Hip_Nino","delta_Nina (mm)","delta_Nina (%)","Hip_Nina")
alloutpos = cbind(InfoPossel,allout)

fileOut1   = paste(dirDataOutput,season,"_ENSO_Impact_KOMPOSIT.csv",sep="")
write.csv(alloutpos,fileOut1,col.names=T,row.names=F)

#===== histogram per pos
# library(ggplot2)
# library(easyGgplot2)
# # Change line color and fill color
# df = data.frame(avchnetralS)
# p= ggplot2.histogram(data=df,xName=avchnetralS,bins=30)
# p= ggplot2.customize(p, mainTitle="Histogram", xtitle="Curah Hujan", ytitle="Frekuensi")
# p+ geom_vline(aes(xintercept=avchnetral),
#               color="green", linetype="solid", size=1) + geom_vline(aes(xintercept=bb),
#               color="green", linetype="dashed", size=1)+ geom_vline(aes(xintercept=ba),
#               color="green", linetype="dashed", size=1) + geom_vline(aes(xintercept=avchnino),
#               color="red", linetype="solid", size=1) + geom_vline(aes(xintercept=avchnina),
#               color="blue", linetype="solid", size=1)+ 
#               geom_text(x=1200, y=85, label="El Nino (Not Sig.)")+
#               geom_text(x=1200, y=80, label="La Nina (Not Sig.)")
}
