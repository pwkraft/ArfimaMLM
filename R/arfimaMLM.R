arfimaMLM <-
function(formula, data, d="GPH"
                      , ecmformula=NULL, decm="GPH", drop=1, ...){
  
  # select variable names from formula
  dv <- paste(formula)[2]
  tmp <- strsplit(paste(formula)[3], split=" + (", fixed=T)[[1]]
  iv <- sub("1 + ","", tmp[1], fixed=T)
  iv <- strsplit(tmp[1], split=" + ", fixed=T)[[1]]
  tmp <- strsplit(sub(")","", tmp[2], fixed=T), split=" | ", fixed=T)[[1]]
  rand <- strsplit(sub("1 + ","", tmp[1], fixed=T), split=" + ", fixed=T)[[1]]
  timevar <- strsplit(tmp[2], split=" + ", fixed=T)[[1]]
  rm(tmp)
  
  # select original varnames for transformation
  dv.dif <- dv[grep(".dif", dv, fixed=T)]
  dv.dif <- list(dv.dif, sub(".dif","", dv.dif, fixed=T))
  iv.dif <- iv[grep(".dif", iv, fixed=T)]
  iv.dif <- list(iv.dif, sub(".dif","", iv.dif, fixed=T))
  iv.fd <- iv[grep(".fd", iv, fixed=T)]
  iv.fd <- list(iv.fd, sub(".fd","", iv.fd, fixed=T))
  rand.dif <- rand[grep(".dif", rand, fixed=T)]
  rand.dif <- list(rand.dif, sub(".dif", "", rand.dif, fixed=T))
  
  # step 1: aggregate .dif+.fd variables over timevar
  varlist <- unique(c(dv.dif[[2]],iv.dif[[2]],iv.fd[[2]],rand.dif[[2]]))
  data.mean <- aggregate(data[,varlist],list(data[,timevar]), mean, na.rm=T)
  
  # step 2: fractionally difference .fd variables (see fd function)
  # note: this might be more efficient with a function like apply/plyr
  # but then the integration with manual d-vals would be more complicated
  varlist <- unique(c(dv.dif[[2]],iv.fd[[2]],rand.dif[[2]]))
  data.fd <- cbind(data.mean[,1],data.mean[,varlist])
  res.d <- NULL
  if(is.character(d)) {
    for(i in 1:length(varlist)){
      tmp <- fd(data.mean[,varlist[i]],dval=d, ...)
      data.fd[,varlist[i]] <- tmp$series
      res.d <- rbind(res.d,c(varlist[i],tmp$estimator,tmp$value))
      rm(tmp)
    }
  } 
  else if(length(d)==length(varlist)){
    if (length(setdiff(varlist,names(d)))!=0) {
      stop("List of d-parameters does not fit to model specification!")}
    else for(i in 1:length(varlist)){
      tmp <- fd(data.mean[,varlist[i]],dval=d[[grep(varlist[i], names(d),fixed=T)]], ...)
      data.fd[,varlist[i]] <- tmp$series
      res.d <- rbind(res.d,c(varlist[i],tmp$estimator,tmp$value))
      rm(tmp)
    }
  }
  else stop("List of d-parameters does not have correct length!")
  
  # step 3: adjust varnames
  names(data.mean) <- paste0(names(data.mean),".mean")
  names(data.mean)[1] <- timevar
  names(data.fd) <- paste0(names(data.fd),".fd")
  names(data.fd)[1] <- timevar  
  
  # step 4: calculate ecm
  if(!is.null(ecmformula)){
    res.ecm <- lm(ecmformula,data=data.mean)
    tmp  <- fd(residuals(res.ecm), decm, ...)
    ecm <- tmp$series
    ecm <- c(NA,ecm[1:(length(ecm)-1)])
    data.fd <- cbind(data.fd,ecm)
    res.d <- rbind(res.d,c("ecm",tmp$estimator,tmp$value))
    rm(tmp)
  }
  
  # step 5: merge datasets
  data.merged <- merge(data, cbind(data.mean,data.fd[,-1]), by=timevar) 
  
  # step 6: calculate dv_dif
  dv_dif <- data.merged[,dv.dif[[2]]]-(data.merged[,grep(paste0(dv.dif[[2]],".mean")
                                                         ,names(data.merged))]
                                       -data.merged[,grep(paste0(dv.dif[[2]],".fd")
                                                          ,names(data.merged))])
  data.merged <- cbind(data.merged, dv_dif)
  names(data.merged)[ncol(data.merged)] <- dv.dif[[1]]
  
  # step 7: calculate iv_dif
  iv_dif <- data.merged[,iv.dif[[2]]]-data.merged[,grep(paste0(iv.dif[[2]],".mean")
                                                        ,names(data.merged))]
  data.merged <- cbind(data.merged, iv_dif)
  names(data.merged)[(ncol(data.merged)
                      -length(iv.dif[[2]])+1):ncol(data.merged)] <- iv.dif[[1]]  
  
  # step 8: drop certain number of initial observations
  data.merged <- subset(data.merged
                        , data.merged[,timevar]>(min(data.merged[,timevar])+drop-1))
  
  # step 9: estimate multilevel model
  res <- lmer(formula, data=data.merged)
  
  # output
  rownames(res.d) <- res.d[,1]; res.d <- data.frame(res.d[,2],as.numeric(res.d[,3]))
  colnames(res.d) <- c("Method","Estimate")
  if(is.null(ecmformula)) out <- list(result=res,d=res.d,data.mean=data.mean
                                      ,data.fd=data.fd,data.merged=data.merged)
  else out <- list(result=res,ecm=res.ecm,d=res.d,data.mean=data.mean
                   ,data.fd=data.fd,data.merged=data.merged)
  class(out) <- "arfimaMLM"
  out
}
