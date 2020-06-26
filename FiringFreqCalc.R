# datacount = total number of rasters to average

windows <- 20 #total number of windows
total_time <- 2 #seconds
window_size <- total_time/windows

mfreq <- matrix(data = NA, nrow = datacount, ncol = windows)
tfreq <- matrix(data = NA, nrow = datacount, ncol = windows)

nmfreq <- mfreq
ntfreq <- tfreq
# ^ 4 different firing frequencies made, m and nm are of the same length


for(i in 1:datacount){
  data <- filter(nmechcomblist[[i]],nmechcomblist[[i]]$Nociceptive==0)
  for(window in 1:windows-1) {
    mfreq[i,window+1] <- sum(data$On.time >= (window_size * window) & data$On.time < window_size * (window+1))
  }
  data <- filter(nmechcomblist[[i]],nmechcomblist[[i]]$Nociceptive==1)
  for(window in 1:windows-1) {
    nmfreq[i,window+1] <- sum(data$On.time >= (window_size * window) & data$On.time < window_size * (window+1))
  }
  data <- filter(nthercomblist[[i]],nthercomblist[[i]]$Nociceptive==0)
  for(window in 1:windows-1) {
    tfreq[i,window+1] <- sum(data$On.time >= (window_size * window) & data$On.time < window_size * (window+1))
  }
  data <- filter(nthercomblist[[i]],nthercomblist[[i]]$Nociceptive==1)
  for(window in 1:windows-1) {
    ntfreq[i,window+1] <- sum(data$On.time >= (window_size * window) & data$On.time < window_size * (window+1))
  }
  
}
mfreq <- mfreq/window_size
tfreq <- tfreq/window_size
nmfreq <- nmfreq/window_size
ntfreq <- ntfreq/window_size
#then average down rows