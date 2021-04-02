#The CircularBoxplot function produces a box-and-whisker-plot  for circular data, according to the procedure described 
#in Buttarazzi D., Pandolfo G., Porzio G.C. (2018). A boxplot for circular data, Biometrics, under review.

CircularBoxplotStats <- function(A, animal, units="degrees", constant="optimal") {
  
  # checking if package are installed, if not they will be installed
  library(circular) #| install.packages("circular", dep=T)

  #Check if Median is uniquely defined
  if(is.na(median.circular(A))==T){stop("The median is not unique for this data set. \n \ The circular boxplot is not drawn.")}
  
  
  if(constant=="optimal"){
    conc     <- A1inv(rho.circular(A))
    q1       <- qvonmises(0.25, mu=circular(0), kappa = conc)
    me       <- qvonmises(0.5 , mu=circular(0), kappa = conc)
    q3       <- qvonmises(0.75, mu=circular(0), kappa = conc)
    box      <- range(c(q1,me,q3))
    q9965    <- qvonmises(1-(0.007/2), mu=circular(0),kappa = conc)
    q0035    <- qvonmises((0.007/2), mu=circular(0),kappa = conc)
    constant <- range(c(q9965,q3))/box  
  }
  
  # inspect and re-specify the input circular vector A.
  
  if(!is.circular(A)){stop("argument A must be entered as a vector of class circular")}
  set1  <- conversion.circular(A, units = "radians", modulo="2pi", zero=0, rotation="counter") 
  
  
  # median and IQR 
  
  x <- set1
  AM <- circular((median(x)+pi), modulo="2pi")
  x <- as.vector(na.omit(replace(as.vector(x),as.vector(x)==as.vector(AM), NA)))
  x2 <- as.matrix(sort(circular( (x-AM), modulo="2pi")))
  
  AnticlockRank <- as.matrix(seq(1,length(x2), by=1))
  ClockRank     <- as.matrix(rev(seq(1,length(x2), by=1)))
  Combined <- cbind(AnticlockRank,ClockRank)
  Tukeyway <- numeric(length(x2))
  for(i in 1:length(x2)){
    Tukeyway[i]   <- Combined[i,][which.min((Combined[i,]))]
  }
  OuterInward   <-  as.matrix(Tukeyway)
  TukeyRanking <- as.matrix(cbind(circular((x2+AM), modulo="2pi"),OuterInward))
  colnames(TukeyRanking) <- c("observations",  "depth")
  
  data <- TukeyRanking
  data <- as.matrix(data)
  CTM <- which(data[,2]>=which.max(data[,2]))
  CTM <- circular(mean(as.circular(data[c(CTM), 1])), modulo="2pi")
  
  n <- length(x)
  depthofmedian <- round(((1+n)/2)-0.1)
  depthofquartiles <- (1+depthofmedian)/2
  
  if (depthofquartiles%%1==0) {    
    quartiles <- which(data[,2] == round(1+depthofmedian)/2)
    qA <-  circular(as.vector(data[quartiles[1],1]),modulo="2pi") 
    qC <-  circular(as.vector(data[quartiles[2],1]),modulo="2pi")

  }
  else  {
    depthq1 <- depthofquartiles+0.5
    depthq2 <- depthofquartiles-0.5
    q1 <- which(data[,2] == depthq1)
    q2 <- which(data[,2] == depthq2) 
    qA  <- mean(circular(data[c(q1[1],c(q2[1])),1], modulo="2pi"))  
    qC  <- mean(circular(data[c(q1[2],c(q2[2])),1], modulo="2pi"))
  }
  
  
  IQRdepth <- which(data[,2] >= depthofquartiles)
  IQR <- c(data[IQRdepth,1],qA,qC)  
  IQRange <- range(c(qA,qC))
  set_1 <- set1
  fi <- as.circular(CTM)

  
  # controlling wrap-around effect in case of median at pi (180°)
  
  if (rad(round(deg(circular((fi+pi), modulo="2pi"))))==0){
    fi <- pi
    AM<- 2*pi} 
  
  else{AM <- rad(round(deg(circular((fi+pi), modulo="2pi"))))} 
  
  # controlling wrap-around effect
  
  if (range(as.circular(IQR))< ((2*pi)/(2*(constant + (1/2)))) ) {  
   
    if (fi<pi) {
      setAnti <- subset(IQR, IQR>=fi & IQR<=AM)
      setClock<- subset(IQR, IQR<=fi | IQR>=AM)
      QAnti   <- rad(round(deg(circular(max(setAnti), modulo="2pi"))))
      Qc      <- QAnti-rad(round(deg(range(as.circular(IQR)))))
      QClock  <- rad(round(deg(circular(Qc, modulo="2pi"))))
      
      d <- (rad(round(deg(range(as.circular(IQR))))))
      
      
      fA<- rad(round(deg(QAnti + d*constant))) 
      fC<- rad(round(deg(QClock - d*constant)))
      
      semicircleClock <- subset(as.vector(set_1),as.vector(set_1)<=fi | as.vector(set_1)>=AM)
      semicircleAnti <- subset(as.vector(set_1),as.vector(set_1)>=fi & as.vector(set_1)<=AM)
      
      semicircleClock <- c(semicircleClock, QClock)
      semicircleAnti <- c(semicircleAnti, QAnti)
      if (fC<0) {
        swc <- subset(semicircleClock, semicircleClock>= rad(round(deg(circular(fC, modulo="2pi"))))| semicircleClock<= QClock) 
        swc <- c(swc, QClock)
        whiskerC <- range(as.circular(swc))
        wC <- QClock-whiskerC
        faroutClock  <- subset(semicircleClock, semicircleClock>=AM & semicircleClock<rad(round(deg(circular(fC, modulo="2pi")))))        
      }
      else if (fC>=0 & QClock>=pi){
        swc <- subset(semicircleClock, semicircleClock>=fC)
        swc <- c(swc, QClock)
        wC <- min(swc)
        faroutClock <- subset(semicircleClock, semicircleClock>=AM & semicircleClock<fC)  
      } 
      else if (fC>=0 & QClock<pi){
        swc <- subset(semicircleClock, semicircleClock>=fC)
        swc <- c(swc, QClock)
        wC <- min(swc)
        faroutClock <- subset(semicircleClock, semicircleClock>=AM | semicircleClock<fC)
      }
      
      swa <- subset(semicircleAnti, semicircleAnti<=fA)
      swa <- c(swa, QAnti)
      wA <- max(swa)
      faroutAnti  <- subset(semicircleAnti, semicircleAnti>fA)
    }
    
    if (fi==pi) {
      setAnti <- subset(IQR, IQR>=fi & IQR<=2*pi)
      setClock<- subset(IQR, IQR<=fi | IQR>=0)
      QAnti   <- rad(round(deg(circular(max(setAnti), modulo="2pi"))))
      Qc      <- QAnti-rad(round(deg(range(as.circular(IQR)))))
      QClock  <- rad(round(deg(circular(Qc, modulo="2pi"))))
      
      # defining the whiskers
      d <- (rad(round(deg(range(as.circular(IQR))))))
      #cat("IQ-RANGE")
      #print(d)
      fA<- rad(round(deg(QAnti + d*constant))) 
      fC<- rad(round(deg(QClock - d*constant)))
      
      semicircleClock <- subset(as.vector(set_1),as.vector(set_1)<=fi | as.vector(set_1)>= 0)
      semicircleAnti <- subset(as.vector(set_1),as.vector(set_1)>=fi & as.vector(set_1)<= 2*pi)
      
      semicircleClock <- c(semicircleClock, QClock)
      semicircleAnti <- c(semicircleAnti, QAnti)
      if (fC<0) {
        swc <- subset(semicircleClock, semicircleClock>= rad(round(deg(circular(fC, modulo="2pi"))))| semicircleClock<= QClock) 
        swc <- c(swc, QClock)
        whiskerC <- range(as.circular(swc))
        wC <- QClock-whiskerC
        
        #drawing the whiskers
        faroutClock  <- subset(semicircleClock, semicircleClock>=0 & semicircleClock<rad(round(deg(circular(fC, modulo="2pi")))))        
      }
      else if (fC>=0){
        swc <- subset(semicircleClock, semicircleClock>=fC)
        swc <- c(swc, QClock)
        wC <- min(swc)
        faroutClock <- subset(semicircleClock, semicircleClock>=0 | semicircleClock<fC)  
      } 
      swa <- subset(semicircleAnti, semicircleAnti<=fA)
      swa <- c(swa, QAnti)
      wA <- max(swa)
      faroutAnti  <- subset(semicircleAnti, semicircleAnti>fA)
      #print(faroutAnti)
    }
    
    else if (fi>pi) {
      setAnti <- subset(IQR, IQR>=fi | IQR<=AM)
      setClock<- subset(IQR, IQR<=fi & IQR>=AM)
      QClock   <- min(setClock)
      Qa      <- QClock+range(as.circular(IQR))
      QAnti  <- rad(round(deg(circular(Qa, modulo="2pi"))))
      
      # defining the whiskers
      d <- range(as.circular(IQR))
      fC<- rad(round(deg(QClock - d*constant))) 
      fA<- rad(round(deg(QAnti + d*constant)))
      semicircleClock <- subset(as.vector(set_1),as.vector(set_1)<=fi & as.vector(set_1)>=AM)
      semicircleAnti <- subset(as.vector(set_1),as.vector(set_1)>=fi | as.vector(set_1)<=AM)
      
      semicircleClock <- c(semicircleClock, QClock)
      semicircleAnti <- c(semicircleAnti, QAnti)
      swc <- subset(semicircleClock, semicircleClock>=fC)
      swc <- c(swc, QClock)
      wC <- min(swc)
      faroutClock <- subset(semicircleClock, semicircleClock<fC )
      
      if (fA>2*pi ) {
        swa <- subset(semicircleAnti, semicircleAnti<= rad(round(deg(circular(fA, modulo="2pi")))) | semicircleAnti>= rad(round(deg(circular(QAnti, modulo="2pi"))))) 
        swa <- c(swa, QAnti)
        whiskerA <- range(as.circular(swa))
        wA <- QAnti+whiskerA
        
        # drawing the whiskers
        faroutAnti <- subset(semicircleAnti, semicircleAnti> circular(fA, modulo = "2pi") & semicircleAnti <= AM ) 
      }
      else if (fA<=2*pi & QAnti>=pi) {
        swa <- subset(semicircleAnti, semicircleAnti<=fA)
        swa <- c(swa, QAnti)
        wA <- max(swa)
        faroutAnti <- subset(semicircleAnti, semicircleAnti>fA | semicircleAnti<= AM  )
      }
      else if (fA<=2*pi & QAnti<pi) {
        swa <- subset(semicircleAnti, semicircleAnti<=fA)
        swa <- c(swa, QAnti)
        wA <- max(swa)
        faroutAnti <- subset(semicircleAnti, semicircleAnti>fA & semicircleAnti<= AM  )
      }
    }
    
    # plotting and printing far out values
    
    faroutvalues1 <- c(faroutClock, faroutAnti)
    compare <- set_1
    faroutvalues2 <- compare[compare %in% faroutvalues1]
    faroutvalues <- as.circular(circular(faroutvalues2), modulo="2pi")
    farout<- as.matrix(faroutvalues2)
    colnames(farout) <- c("Far out values")
    
}
  
  
  
  #####from here on is in case the range(box)>= (360/2(c+1/2))
  
  
  else { 
    if (fi<=pi) {
      setAnti <- subset(IQR, IQR>=fi & IQR<=AM)
      setClock<- subset(IQR, IQR<=fi | IQR>=AM)
      QAnti   <- rad(round(deg(circular(max(setAnti), modulo="2pi"))))
      Qc      <- QAnti-range(as.circular(IQR))
      QClock  <- rad(round(deg(circular(Qc, modulo="2pi"))))
      
      # defining the whiskers
      semicircleClock <- subset(as.vector(set_1),as.vector(set_1)<=fi | as.vector(set_1)>=AM)
      semicircleAnti <- subset(as.vector(set_1),as.vector(set_1)>=fi & as.vector(set_1)<=AM)
      
      semicircleClock <- c(semicircleClock, QClock)
      semicircleAnti <- c(semicircleAnti, QAnti)
      if (QClock <= pi){
        swc <- subset(semicircleClock, semicircleClock >= AM | semicircleClock <= QClock) 
      }
      else if (QClock > pi){
        swc <- subset(semicircleClock, semicircleClock >= AM & semicircleClock <= QClock)
      }
      
      whiskerC <- range(as.circular(swc))
      wC <- QClock-whiskerC
      
      # drawing the whiskers
      swa <- subset(semicircleAnti, semicircleAnti<=AM)
      wA <- max(swa)
    }
    
    else if (fi>=pi) {
      setAnti <- subset(IQR, IQR>=fi | IQR<=AM)
      setClock<- subset(IQR, IQR<=fi & IQR>=AM)
      QClock   <- min(setClock)
      Qa      <- QClock+range(as.circular(IQR))
      QAnti  <- rad(round(deg(circular(Qa, modulo="2pi"))))
      
      semicircleClock <- subset(as.vector(set_1),as.vector(set_1)<=fi & as.vector(set_1)>=AM)
      semicircleAnti <- subset(as.vector(set_1),as.vector(set_1)>=fi | as.vector(set_1)<=AM)
      semicircleClock <- c(semicircleClock, QClock)
      semicircleAnti <- c(semicircleAnti, QAnti)
      swc <- subset(semicircleClock, semicircleClock>=AM)
      wC <- min(swc)

      if(QAnti<pi){
        swa <- subset(semicircleAnti, semicircleAnti<=AM & semicircleAnti >= QAnti)
      }
      else if(QAnti>pi){
        swa <- subset(semicircleAnti, semicircleAnti<=AM | semicircleAnti >= QAnti)  
      }
      whiskerA <- range(as.circular(swa))
      wA <- QAnti+whiskerA
    }
    
  }  
  gradi <- (as.matrix(deg(data[,1])))
  output <- as.matrix(cbind(data,gradi))
  colnames(output) <- c("Obs.Radians", "Ranking", "Obs.Degrees")
  #print(output)

  
  # output object
  out = list()
  if(exists("faroutvalues")==TRUE){
    if(length(faroutvalues)!=0){out$farout = faroutvalues}
    else{out$farout = c("no far out values detected")}  
  }
  else{out$farout = c("no far out values detected")}
  
  out$constant = constant
  out$data <- output
  if (typeof(out$farout) != "character") {
  out$farout <- deg(as.vector(out$farout))
  }
  out$median <- deg(fi)
  out$quartiles <- deg(c(QClock, QAnti))
  out$whiskers <- deg(c(wC, wA))
  if(!exists("fC") & !exists("fA")){
    out$fences <- c("no fences?")
  } else if(!exists("fC")){
    out$fences <- c("FA" = fA)
    } else if(!exists("fA")){
    out$fences <- c("fC" = fC)
    } else {
      out$fences <- deg(c(fC, fA))
    }
   
  out_df <- data.frame("median" = as.numeric(out$median), 
                       "q25" = as.numeric(out$quartiles[1]),
                       "q75" = as.numeric(out$quartiles[2]), 
                       "wlow" = as.numeric(out$whiskers[1]),
                       "whigh" = as.numeric(out$whiskers[2]), 
                       "animal" = animal)
  out <- list(out_df, out$farout)
}  



linear_circular_boxplot <- function(cyclic_times_to_minima, animal){
  circular_objects <- list()
  for (i in c(1:ncol(cyclic_times_to_minima))){
    x <- circular(cyclic_times_to_minima[,i], units = "degrees")
    circular_objects[[i]] <- CircularBoxplotStats(x, "chewbacca")
    circular_objects[[i]][[1]]$variable <- colnames(cyclic_times_to_minima)[i]
   # circular_objects[[i]]$column <- colnames(cyclic_times_to_minima)[i]
  }
  names(circular_objects) <- colnames(cyclic_times_to_minima)
  
  box_stats <- list()
  outliers <- list()
  for (i in c(1:length(circular_objects))){
    box_stats[[i]] <- circular_objects[[i]][[1]]
    outliers[[i]] <- circular_objects[[i]][[2]]
  }
  names(outliers) <- colnames(cyclic_times_to_minima)
  box_stats <- bind_rows(box_stats)
  list(box_stats, outliers)
}
