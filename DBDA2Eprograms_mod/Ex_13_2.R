source("minNforHDIpower.R") 

#A
sampSize = minNforHDIpower( genPriorMode=0.8, genPriorN=2000,
                            HDImaxwid=0.20, nullVal=NULL, ROPE=NULL,
                            desiredPower=0.8,
                            audPriorMode=0.5, audPriorN=2,
                            HDImass=0.95, initSampSize=50, verbose=TRUE )

#C
sampSize = minNforHDIpower( genPriorMode=0.8, genPriorN=2,
                            HDImaxwid=NULL, nullVal=0.5, ROPE=c (0.48,0.52),
                            desiredPower=0.8,
                            audPriorMode=0.5, audPriorN=2,
                            HDImass=0.95, initSampSize=5, verbose=TRUE )

#D
sampSize = minNforHDIpower( genPriorMode=0.8, genPriorN=2,
                            HDImaxwid=NULL, nullVal=0.5, ROPE=c (0.,0.5),
                            desiredPower=0.8,
                            audPriorMode=0.5, audPriorN=2,
                            HDImass=0.95, initSampSize=5, verbose=TRUE )

w = 0.8            # Specify the prior MODE.
k = 2               # Specify the effective prior sample size.
a = w*(k-2) + 1      # Convert to beta shape parameter a.
b = (1-w)*(k-2) + 1  # Convert to beta shape parameter b.
curve(dbeta(x, shape1 = a, shape2 = b), lwd = 2)