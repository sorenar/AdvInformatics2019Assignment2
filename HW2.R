library(tidyverse)

# Analysis using the base R
data = read_tsv("data.txt")
peaks = which(data==1)
data$closest = lapply(c(1:dim(data)[1]),function(x) peaks[which.min(abs(x-peaks))])
                     

# Analysis using tidyverse
data = read_tsv("data.txt")
data = data %>% mutate(position = 1:length(value)) %>% 
  mutate(peaks = position*value) %>%
  mutate(peaks_up = replace(peaks, peaks==0, NA)) %>% mutate(peaks_down = replace(peaks, peaks==0, NA)) %>% 
  fill(peaks_up,.direction = "down") %>% fill(peaks_down,.direction ="up") %>%
  fill(peaks_up,.direction = "up") %>% fill(peaks_down,.direction ="down") %>%
  mutate(closest = replace(peaks_up,abs(position-peaks_up) > abs(peaks_down-position),peaks_down[abs(position-peaks_up) > abs(peaks_down-position)])) %>% 
  select(-position,-peaks,-peaks_up,-peaks_down)



