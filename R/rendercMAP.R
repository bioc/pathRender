
rendercMAPPathway <- function(pname, ino=0) {
     kp <- as.list(cMAPKEGGPATHWAY)
     bp <- as.list(cMAPCARTAPATHWAY)

     if ( any(names(kp) == pname) ) { 
        print("KEGG") 

        ki <- as.list(cMAPKEGGINTERACTION)
        km <- as.list(cMAPKEGGMOLECULE)
        i_mem <- apply(as.matrix(kp[[pname]]$component), 2, paste)

        if ( ino == 0 ) renderOnecMAPPathway(i_mem, ki, km)
        else renderOnecMAPInteraction(ino, i_mem, ki, km)
     }
     else if ( any(names(bp) == pname) ) { 
        print("BioCarta") 
        
        bi <- as.list(cMAPCARTAINTERACTION)
        bm <- as.list(cMAPCARTAMOLECULE)
        i_mem <- apply(as.matrix(bp[[pname]]$component), 2, paste)

        if ( ino == 0 ) renderOnecMAPPathway(i_mem, bi, bm)
        else renderOnecMAPInteraction(ino, i_mem, bi, bm)
     }
     else { 
        stop("Cannot find given pathname in cMAP") 
     }
}

