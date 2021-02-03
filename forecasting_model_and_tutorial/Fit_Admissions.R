  DoubleTime <- function(dat, timev, npts=length(dat), meth="ML", FE='None', plt=FALSE,thck=1.5, figtitle=""){
  #### this function fits an exponential growth rate using a generalised additive model  
    res<- data.frame(sdt=rep(0,npts),sdtup=rep(0,npts),sdtlow=rep(0,npts)
                     ,doub=rep(0,npts),doubup=rep(0,npts),doublow=rep(0,npts))#,expdt=rep(0,3))
    Tv <- timev #seq(1,length(dat),1)
    if(FE=='None'){
      #MGAM <- gam(dat~s(Tv), family = quasipoisson, method="REML")
      MGAM <- gam(dat~s(Tv, k = -1), family = quasipoisson, method="REML", gamma = 2)
      
    }else{
      DW <- weekdays(as.Date(Tv, origin = "2019-12-31"))
      if(FE=='WE'){
        DW <- ifelse(DW=='Sunday','WE',ifelse(DW=='Saturday','WE','WD'))
      }
      MGAM <- gam(dat~s(Tv)+DW, family=quasipoisson, method=meth)
    }
    
    xv<-seq(min(Tv),max(Tv), length=npts)
    if(FE=='None'){
      newd <- data.frame(Tv=xv) # data.frame(Tv=xv, DW=DW)
    }else{
      dow <- weekdays(as.Date(xv, origin = "2019-12-31"))
      if(FE=='WE'){
        dow <- ifelse(dow=='Sunday','WE',ifelse(dow=='Saturday','WE','WD'))
      }
      newd <- data.frame(Tv=xv, DW=dow) # data.frame(Tv=xv, DW=DW)
    }
    X0 <- predict(MGAM, newd,type="lpmatrix")
    
    eps <- 1e-7 ## finite difference interval
    xv<-seq(min(Tv),max(Tv), length=npts)+eps
    if(FE=='None'){
      newd <- data.frame(Tv=xv) # data.frame(Tv=xv, DW=DW)
    }else{
      dow <- weekdays(as.Date(xv, origin = "2019-12-31"))
      if(FE=='WE'){
        dow <- ifelse(dow=='Sunday','WE',ifelse(dow=='Saturday','WE','WD'))
      }
      newd <- data.frame(Tv=xv, DW=dow) # data.frame(Tv=xv, DW=DW)
    }
    X1 <- predict(MGAM, newd,type="lpmatrix")
    Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives
    
    off<-ifelse(FE=='None',1,ifelse(FE=='WE',2,7))  
    Xi <- Xp*0 
    k = 10
    Xi[,1:k-1+off] <- Xp[,1:k-1+off] ## weekend Xi%*%coef(MGAM) = smooth deriv i
    df <- Xi%*%coef(MGAM)              ## ith smooth derivative 
    df.sd <- rowSums(Xi%*%MGAM$Vp*Xi)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
    
    res$sdt <- df
    res$sdtup <- df+2*df.sd
    res$sdtlow <- df-2*df.sd
    res$doub <- ifelse(res$sdt < 0, 100, log(2)/res$sdt)
    res$doubup <- ifelse(res$sdtup < 0, 100, log(2)/res$sdtup)
    res$doublow <- ifelse(res$sdtlow < 0, 100, log(2)/res$sdtlow)
    
    MGLM <- glm(dat~(Tv), family=quasipoisson)
    Doubling<-c(MGLM$coefficients[2],confint(MGLM)[2,1],confint(MGLM)[2,2])
    
    if(plt==TRUE){
      par(mar = c(5,4,4,4) + 0.1)
      par(mfrow=c(1,2))
      plot(xv,df,type="l",ylim=range(c(df+2*df.sd,df-2*df.sd)), ylab='Instantaneous growth rate', xlab='Time', main=figtitle, lwd=2*thck)
      lines(xv,df+2*df.sd,lty=2, lwd=thck);
      lines(xv,df-2*df.sd,lty=2, lwd=thck)
      abline(h=0, col=4)
      text(as.Date(43930, origin = startDate),max(df),ifelse(df[length(df)]-2*df.sd[length(df)]>0,'Growth',ifelse(df[length(df)]+2*df.sd[length(df)]<0,'Decay','Plateau')))
  
      axis(4,at=c(-log(2)/c(2:14,21,28), 0,log(2)/c(28,21,14:2)), labels=c(-c(2:14,21,28),'Inf',c(28,21,14:2)))
      mtext("Doubling time", side = 4, line = 3)
      
      plot(Tv, dat, main='Fit compared with model', ylab='Number', xlab='Time', pch=16, col=4)
      lines(Tv, fitted(MGAM), lwd=2*thck)
  
      p <- predict(MGAM, type = "link", se.fit = TRUE)
      upr <- p$fit + (2 * p$se.fit)
      lwr <- p$fit - (2 * p$se.fit)
      upr <- MGAM$family$linkinv(upr)
      lwr <- MGAM$family$linkinv(lwr)
      lines(as.Date(Tv, origin = startDate), upr, col=1, lty=2, lwd=thck)
      lines(as.Date(Tv, origin = startDate), lwr, col=1, lty=2, lwd=thck)
      
  
    }
    
    print(fitted(MGAM))
    return(list(df,df.sd,tail(fitted(MGAM),n=1)))
  }
  
  
  setwd("C:/Users/overt/OneDrive/Documents/GitHub/Man_Epi_local") # set working directory
  #setwd("C:/Users/Instructor/Documents/GitHub/Man_Epi_local/Bolton")
  library(mgcv)
  library(dplyr)
  library(openxlsx)
  #library(imguR)
  
  ##
  ##
  
  # say how much of timeseries to use
  Lag<- 0 ## Ignore the past few days in case of back-filling due to delayed test results
  startDate <- as.Date("2020-01-01")
  startDatePlot <- as.Date("2020-03-01")
  start<- 1
  
  ## open data files
  stringnamedate <-"simulated_25Jan"

  print("Reading admissions data, please wait...")
  admission_data <- read.csv(paste("admissions_",stringnamedate,".csv", sep =""),sep = ",") # load admissions data for each age group

  # Prepare table
  alldates <- c(admission_data$dates)
  mindate <- min(alldates)+1
  maxdate <- max(alldates)+1 
  alldates <- seq(mindate,maxdate,1)
  ndates <- length(alldates)
  
  wb <- createWorkbook()
  
  #### create admissions time series for each age group
  admissions_1 <- (admission_data$counts_1) # 0-25 admissions 
  admissions_2 <- (admission_data$counts_2) # 26-50 admissions
  admissions_3 <- (admission_data$counts_3) # 51-75 admissions
  admissions_4 <- (admission_data$counts_4) # 76+ admissions
  admissions_5 <- (admission_data$counts_5) # all admissions
  
  #### Constract data frame
  mytable <- data.frame( Day = c(1:length(alldates))-1, Date = alldates, `0-25` = admissions_1, `26-50` = admissions_2, `51-75` = admissions_3, `76+` = admissions_4, `All` = admissions_5,check.names = FALSE )
  
  addWorksheet(wb,sheetName = "MFT")
  writeData(wb,"MFT",mytable)
  out_mean = matrix(0,5,1)
  out_sd = matrix(0,5,1)
  initial_value = matrix(0,5,1)
  avg_len = 1
  pdf(file=paste('MFT_cases_trends_full_',stringnamedate,'.pdf', sep=''))
  
  #### For each admissions time series fit a growth rate using a generalised additive model - this calls the function at the top of the workbook
  for(i in 1:(ncol(mytable)-2)){
    df <-DoubleTime(mytable[(start):(dim(mytable)[1]-Lag),2+i],mytable$Day[start:(dim(mytable)[1]-Lag)], plt=T, figtitle=paste("MFT",colnames(mytable)[2+i],sep = "."))
    mean <-df[[1]]
    sd <- df[[2]]
    out_mean[i] = sum(mean[(length(mean)-avg_len+1):length(mean)])/avg_len
    out_sd[i] = sum(sd[(length(sd)-avg_len+1):length(sd)])/avg_len
    initial_value[i] = df[[3]]
  }
  admissions_conditions <- data.frame(out_mean,initial_value)
  write.csv(admissions_conditions,paste("admissions_conditions_",stringnamedate,".csv", sep =""))
  dev.off()