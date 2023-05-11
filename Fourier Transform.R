#Fundamentals of Fourier series
  #Preparing Sine waves
    xs <- seq(-2*pi,2*pi,pi/100)
      wave.1 <- sin(3*xs)
      wave.2 <- sin(10*xs)
    par(mfrow = c(1, 2))
    plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
    plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)\
  #combining the waves
    wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
    plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)
    
#Fourier series explains that any irregualar curve can be explained as a sum of simple sine waves.
  #creating a function to plot fourier series
    plot.fourier <- function(fourier.series, f.0, ts) {
      w <- 2*pi*f.0
      trajectory <- sapply(ts, function(t) fourier.series(t,w))
      plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
    }
    
    # An eg
    plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 
    
#where w is angular weights, ts is time intervals and f.0 is frequency.
    #Now plotting our base equation using this function.
    
    acq.freq <- 100                    # data acquisition frequency (Hz)
    time     <- 6                      # measuring time interval (seconds)
    ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
    f.0      <- 1/time                 # fundamental frequency (Hz)
  #DC components refer to shifts to the curve  
    dc.component       <- 0
    component.freqs    <- c(3,10)      # frequency of signal components (Hz)
    component.delay    <- c(0,0)       # delay of signal components (radians)
    component.strength <- c(.5,.25)    # strength of signal components
  #function for the equation.  
    f <- function(t,w) { 
      dc.component + 
        sum( component.strength * sin(component.freqs*w*t + component.delay)) 
    }
    
  #Applying our fourier transform equation
    plot.fourier(f,f.0,ts)
    
#Visualizing fourier series as cycles.
    #each cycle has a size(z) , delay(d), speed( change of rate of d)
    
#Fast Fourier Transform
    # what should the cycles be so that their combine strengths result on a given known trajectory?
    library(stats)
    fft(c(1,1,1,1)) / 4
  # this means at different trajectories, how the sine waves look like.
    #fft gives us the frequency domain and inverse fourier transform(ift)gives us the original curve using frequency.
     
    # returns the x.n time series for a given time sequence (ts) and
    # a vector with the amount of frequencies k in the signal (X.k)
    get.trajectory <- function(X.k,ts,acq.freq) {
      
      N   <- length(ts)
      i   <- complex(real = 0, imaginary = 1)
      x.n <- rep(0,N)           # create vector to keep the trajectory
      ks  <- 0:(length(X.k)-1)
      
      for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
        x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
      }
      
      x.n * acq.freq 
    }
    #Plot frequency of a given set of values of X.k\
        plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
          plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
          
          # TODO: why this scaling is necessary?
          plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
          
          plot(plot.data, t="h", lwd=2, main="", 
               xlab="Frequency (Hz)", ylab="Strength", 
               xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
        }
    
    #Example
      #defining the fourier series
      acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
      time     <- 6                      # measuring time interval (seconds)
      ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
      f.0 <- 1/time
      
      dc.component <- 2
      component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
      component.delay <- c(0,0,0)         # delay of signal components (radians)
      component.strength <- c(.75,.25,.5) # strength of signal components
      
      f   <- function(t,w) { 
        dc.component + 
          sum( component.strength * sin(component.freqs*w*t + component.delay)) 
      }
      
      plot.fourier(f,f.0,ts=ts)
      
    #now we are going to find its frequency peaks
      w <- 2*pi*f.0
      trajectory <- sapply(ts, function(t) f(t,w))
      head(trajectory,n=30)
      
      #therefore using the trajectory, the frequency peaks are 
        X.k <- fft(trajectory)                   # find all harmonics with fft()
        plot.frequency.spectrum(X.k, xlimits=c(0,20))      
    # now we can use these frequency peaks to find the original curve using inverse fourier
        x.n <- get.trajectory(X.k,ts,acq.freq) / acq.freq  
        plot(ts,x.n, type="l"); abline(h=0,lty=3)
    
    
    
    
    
    
    