timePointReliability2 <- function(tolerance=3:0, secCoded){
    
    #create new columns for JH and NS
    pg$V5 <- NA; pg$V6 <- NA; pg$V7 <- NA; pg$V8 <- NA; pg$V9 <- NA; pg$V10 <- NA; pg$V11 <- NA; pg$V12 <- NA; pg$V13 <- NA; pg$V14 <- NA; pg$V15 <- NA; pg$V16 <- NA; pg$V17 <- NA; pg$V18 <- NA; pg$V19 <- NA; pg$V20 <- NA
    
    #write column names for JH and NS
    colnames(pg) <- c("coder","NA","beats","hand","nearBeat","beatsOther", "dist", "distAbs", "distCat", "dist0f", "dist1f", "dist2f", "dist3f", "dist4f", "dist5f", "dist6f", "dist7f", "dist8f", "dist9f", "dist10f")
    
    #set one vector to each raters’ number observations
    #JHn <<- length (grep('JH', pg$coder))
    #NSn <<- length (grep('NS', pg$coder)) 
    
    
    ## consider only the first 15 seconds
    
    # warning that only the first 15 seconds are being compared
    print("Note: Only the first 15 seconds are being compared! up to 21min")
    
    
    #long version; works but cannot display result via view()...
    pg <- pg[
        which(
            pg$beats>=0    & pg$beats<15  |
                pg$beats>=60   & pg$beats<75  |
                pg$beats>=120  & pg$beats<135 |
                pg$beats>=180  & pg$beats<195 |
                pg$beats>=240  & pg$beats<255 |
                pg$beats>=300  & pg$beats<315 |
                pg$beats>=360  & pg$beats<375 |
                pg$beats>=420  & pg$beats<435 |
                pg$beats>=480  & pg$beats<495 |
                pg$beats>=540  & pg$beats<555 |
                pg$beats>=600  & pg$beats<615 |
                pg$beats>=660  & pg$beats<675 |
                pg$beats>=720  & pg$beats<735 |
                pg$beats>=780  & pg$beats<795 |
                pg$beats>=840  & pg$beats<855 |
                pg$beats>=900  & pg$beats<915 |
                pg$beats>=960  & pg$beats<975 |
                pg$beats>=1020 & pg$beats<1035|
                pg$beats>=1080 & pg$beats<1095|
                pg$beats>=1140 & pg$beats<1155|
                pg$beats>=1200 & pg$beats<1215 
        ),
        ]
    
    JHn <<- length (grep('JH', pg$coder))
    NSn <<- length (grep('NS', pg$coder)) 
    
    #more than 15sec of the last minute?
    if (secCoded/60 - floor(secCoded/60) > .25){
        moreThan15last <- TRUE
    } else {
        moreThan15last <- FALSE}
    
    if (moreThan15last==TRUE) {
        secCoded <- floor(secCoded/60)*15 + 15
    } else {
        secCoded <- floor(secCoded/60)*15 + (secCoded/60 - floor(secCoded/60))*60
    }
    
    
    
    #write to columns “nearBeat” the number of the other rater's value (“beat”) that is closest to the value of the jh rater of each row
    pg$nearBeat <- sapply (1:(NSn+JHn), function(x) which(abs(pg$beats[(JHn+1):(NSn+JHn)]-pg$beats[x])==min(abs(pg$beats[(JHn+1):(NSn+JHn)]-pg$beats[x]))))
    
    #write to columns beatsOther the value of the other rater that is closest to the value of this rater of each row 
    pg$beatsOther <- pg$beats[
        JHn + sapply (
            1:(JHn+NSn), function(x) 
                which (pg$nearBeat[(JHn+1):(JHn+NSn)]==pg$nearBeat[x])
        )
        ]
    
    #compute difference between beats and beatsOhter
    pg$dist <- pg$beats - pg$beatsOther
    
    #compute absolute difference between beats and beatsOhter
    pg$distAbs <- abs(pg$beats - pg$beatsOther)
    
    #show whether it is perfect (0), within one to four frames tolerance (0/1/2/3/4/5/6/7/8/9/10/more)
    pg$distCat <- sapply (1:(JHn+NSn), function(x) if(pg$distAbs[x]<0.003) "0" else if(pg$distAbs[x]<0.045) "1" else if(pg$distAbs[x]<0.087) "2" else if(pg$distAbs[x]<0.129) "3" else if(pg$distAbs[x]<0.171) "4" else if(pg$distAbs[x]<0.213) "5" else if(pg$distAbs[x]<0.255) "6" else if(pg$distAbs[x]<0.297) "7" else if(pg$distAbs[x]<0.339) "8" else if(pg$distAbs[x]<0.381) "9" else if(pg$distAbs[x]<0.423) "10" else "more")
    
    #write a column that only shows the tolerated deviations
    pg$dist0f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0") pg$dist[x] else NA)
    
    pg$dist1f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1") pg$dist[x] else NA)
    
    pg$dist2f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2") pg$dist[x] else NA)
    
    pg$dist3f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3") pg$dist[x] else NA)
    
    pg$dist4f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4") pg$dist[x] else NA)
    
    pg$dist5f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5") pg$dist[x] else NA)
    
    pg$dist6f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6") pg$dist[x] else NA)
    
    pg$dist7f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7") pg$dist[x] else NA)
    
    pg$dist8f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7"|pg$distCat[x]=="8") pg$dist[x] else NA)
    
    pg$dist9f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7"|pg$distCat[x]=="8"|pg$distCat[x]=="9") pg$dist[x] else NA)
    
    pg$dist10f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7"|pg$distCat[x]=="8"|pg$distCat[x]=="9"|pg$distCat[x]=="10") pg$dist[x] else NA)
    
    
    pg <<- pg
    pg <- pg
    
    
    #calculate the standard deviation, ignore NAs
    sd(pg$dist0f[1:JHn], na.rm=TRUE)
    sd(pg$dist1f[1:JHn], na.rm=TRUE)
    sd(pg$dist2f[1:JHn], na.rm=TRUE)
    sd(pg$dist3f[1:JHn], na.rm=TRUE)
    sd(pg$dist4f[1:JHn], na.rm=TRUE)
    sd(pg$dist5f[1:JHn], na.rm=TRUE)
    sd(pg$dist6f[1:JHn], na.rm=TRUE)
    sd(pg$dist7f[1:JHn], na.rm=TRUE)
    sd(pg$dist8f[1:JHn], na.rm=TRUE)
    sd(pg$dist9f[1:JHn], na.rm=TRUE)
    sd(pg$dist10f[1:JHn], na.rm=TRUE)
    
    #show how many beats are in which confInt
    table (pg$distCat[1:JHn])
    
    
    # Now, resolve conflicts:
    
    #  1.  
    #a.  When nearBeat contains two values (“[2:3]”) AND
    #b.    (looking into Elan) they are both either occupied or unoccupied by another neighbour (ELSE write the number of the unoccupied candidate into the nearBeat cell )
    #replace the nearBeat value with a distant number (e.g. if it is 1, put 11; if it is 33, put 1). 
    
    #  2.	
    #a.	When nearBeat is equal for more than one row AND 
    #b.	more than one of these rows are tolerated AND
    #c.	two are equally the closest (else replace the farther) AND
    #d.	they are both either occupied or unoccupied by another neighbour (ELSE replace only occupied), 
    #replace the nearBeat value of all except the closest neighbour by a distant number (e.g. if it is 1, put 11; if it is 33, put 1). 
    
    #Then rerun the commands beginning exactly with “#write to columns beatsOther …”
    
    
    #prepare for interrater test by a 2x2 table
    
    #input length of recording
    #secCoded <- 105 #fill in the stretch of video coded (by all raters) in the imported data by duration in seconds
    #print(c("secCoded =", secCoded, "!!"))
    
    #loading the library that the kappa test is part of
    library("fmsb")
    
    
    ##now the confidence tables for 0:3 tolerance
    
    if(any(tolerance==0)) {
        print("0 FRAMES TOLERANCE")
        #both yes
        a <- (length(which(!is.na(pg$dist0f[1:JHn]))))
        #JHyes+NSno
        b <- (length(which(is.na(pg$dist0f[1:JHn]))))
        #JHno+NSyes
        c <- (NSn - length(which(!is.na(pg$dist0f[1:JHn]))))
        #both no
        d <- ((secCoded*24)- (length(which(!is.na(pg$dist0f[1:JHn]))) + length(which(is.na(pg$dist0f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist0f[1:JHn]))))))
        tolmatrix <- matrix(c(a,b,c,d),2,2)
        print(tolmatrix)
        print(Kappa.test(tolmatrix))
    }
    
    if(any(tolerance==1)) {
        print("1 FRAME TOLERANCE")
        #both yes
        a <- (length(which(!is.na(pg$dist1f[1:JHn]))))
        #JHyes+NSno
        b <- (length(which(is.na(pg$dist1f[1:JHn]))))
        #JHno+NSyes
        c <- (NSn - length(which(!is.na(pg$dist1f[1:JHn]))))
        #both no
        d <- ((secCoded*24/3)- (length(which(!is.na(pg$dist1f[1:JHn]))) + length(which(is.na(pg$dist1f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist1f[1:JHn]))))))
        tolmatrix <- matrix(c(a,b,c,d),2,2)
        print(tolmatrix)
        print(Kappa.test(tolmatrix))
    }
    
    if(any(tolerance==2)) {
        print ("2 FRAMES TOLERANCE")
        #both yes
        a <- (length(which(!is.na(pg$dist2f[1:JHn]))))
        #JHyes+NSno
        b <- (length(which(is.na(pg$dist2f[1:JHn]))))
        #JHno+NSyes
        c <- (NSn - length(which(!is.na(pg$dist2f[1:JHn]))))
        #both no
        d <- ((secCoded*24/5)- (length(which(!is.na(pg$dist2f[1:JHn]))) + length(which(is.na(pg$dist2f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist2f[1:JHn]))))))
        tolmatrix <- matrix(c(a,b,c,d),2,2)
        print(tolmatrix)
        print(Kappa.test(tolmatrix))
    }
    
    if(any(tolerance==3)) {
        print("3 FRAMES TOLERANCE")
        #both yes
        a <- (length(which(!is.na(pg$dist3f[1:JHn]))))
        #JHyes+NSno
        b <- (length(which(is.na(pg$dist3f[1:JHn]))))
        #JHno+NSyes
        c <- (NSn - length(which(!is.na(pg$dist3f[1:JHn]))) )
        #both no
        d <- ((secCoded*24/7)- (length(which(!is.na(pg$dist3f[1:JHn]))) + length(which(is.na(pg$dist3f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist3f[1:JHn]))))))
        tol3matrix <- matrix(c(a,b,c,d),2,2)
        print(tol3matrix)
        print(Kappa.test(tol3matrix))
    }
    
    
}
