## rat vocal detector
library(multitaper)
library(seewave)
library(tuneR)
library(multitaper)

#### spectro based

rat_vocal_detect_50khz <- function(wav, threshold = 25, margin =  3, f = 250000, noise_scale = 2.3,
                                   from = 0, to = 0.5, tag = ""){
  ## load .wav file
  wav1 <- readWave(wav,
                   from = from, to = to, units = 'seconds')
  ## extract spectrum from 40 to 60 khz
  pas_ext <- ffilter(wav1, f = f, from = 45000, to = 55000, bandpass = TRUE,
                     custom = NULL, wl = 1024, ovlp = 50, wn = "hanning", fftw = FALSE,
                     rescale = FALSE, listen = FALSE, output = "Wave")
  ## extract energy
  sp_ext <- spectro(pas_ext, 
                    noisereduction = NULL,
                    scale = FALSE,
                    fastdisp = TRUE, flim = c(30, 85),
                    tlab = "", flab = "", alab = "", main = "", axisX = FALSE, axisY = FALSE,
                    plot = FALSE)
  
  time_scale <- length(sp_ext$time)
  sp_amp <- sp_ext$amp
  detect_points <- c()
  for (i in 1:time_scale){
    mean_sp <- mean(sp_amp[, i])
    sd_sp <- sd(sp_amp[, i])
    tr <- mean_sp + noise_scale*sd_sp
    t1 <- (sp_amp[, i] > tr)
    detect_points <- c(detect_points, length(t1[t1 == TRUE]))
    
  }
  
  index_detect_points <- which(detect_points > 0)
  
  if(length(index_detect_points) < 2){
    print(paste('maybe no vocalisations, few detection: ', wav, tag))
    return()
  }else{
    start_seq <-c()
    end_seq <-c()
    
    i = 1
    start_seq <- c(start_seq, i)
    repeat{
      ### if there are more than 2 points within the set time threshold, treat these points within a single vocalisation  
      f_temp <- index_detect_points[index_detect_points < (index_detect_points[i] + threshold) & index_detect_points >= index_detect_points[i]]
      if(length(f_temp) >= 2){
        i <- (i + length(f_temp) - 1)
      }else{
        end_seq <- c(end_seq, i)
        if(length(f_temp) != 1){
          i <- length(index_detect_points[index_detect_points <(index_detect_points[i] + threshold)])
          start_seq <- c(start_seq, i)
        }else{
          i <- i + 1
          start_seq <- c(start_seq, i)
        }
      }
      ### until the end of the .wav file
      if(i>= length(index_detect_points)){
        end_seq <- c(end_seq, i)
        break
      }
    }
  }
  
  ## remove selections that are too short to be vocalisations
  logic_temp <- (index_detect_points[end_seq] - index_detect_points[start_seq] >= 2)
  start_seq_cor <- start_seq[logic_temp]
  end_seq_cor <- end_seq[logic_temp]
  if(length(start_seq_cor) > 0){
    s1_cor <- index_detect_points
    ## avoid cutting ahead the clip
    s1_cor[start_seq_cor[index_detect_points[start_seq_cor] - margin < 0]] <- 0
    s1_cor[start_seq_cor[index_detect_points[start_seq_cor] - margin > 0]] <- index_detect_points[start_seq_cor[index_detect_points[start_seq_cor] - margin > 0]] - margin
    ## avoid cutting after the clip
    s1_cor[end_seq_cor[index_detect_points[end_seq_cor] + margin > time_scale]] <- time_scale
    s1_cor[end_seq_cor[index_detect_points[end_seq_cor] + margin < time_scale]] <- index_detect_points[end_seq_cor[index_detect_points[end_seq_cor] + margin < time_scale]] + margin
    
    ## plot the results
    spectro(pas_ext, scale = FALSE,
            fastdisp = TRUE, flim = c(30, 85),
            tlab = "", flab = "", alab = "", main = "", axisX = FALSE, axisY = FALSE,
            noisereduction = NULL)
    segments(x0 = (s1_cor[start_seq_cor])*(to - from)/(time_scale), y0 = 80,
             x1 = (s1_cor[end_seq_cor])*(to - from)/(time_scale), y1 = 80,
             col = 'red', lwd = 5)
    print(paste('vocalisations: ', wav, tag))
  }else{
    print(paste('maybe no vocalisations: no cutting points', wav, tag))
  }
}


## set parameters to visualise results, please change these parameters accordingly

par(mfrow=c(4,4))
par(mar=c(4,0,0,0))
## batch process
### get a .wav file's duration then process every 0.5 second of the file
path <- "C:/Users/bibio/OneDrive/desktop/Rat Tickle USV sound files/RatcombB035.wav"

sound <- readWave(path,
                  header=TRUE)
dur_sec <- sound$samples/250000
t <- floor(dur_sec/0.5)

for(i in 1 : t){
  rat_vocal_detect_50khz(path, threshold = 25, margin =  3, f = 250000,
                         from = (i-1)*0.5, to = i*0.5, tag = as.character((i-1)*0.5))
}



