require(tidyverse)
require(openxlsx)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"
dir_data = "/Users/Yuki/Dropbox/業務/拡大種/2020/data/宮城県拡大種"

setwd(dir_data)
path = dir_data
files = list.files(path)

miya = NULL
for(i in 1:length(files)){
  if(str_detect(files[i], pattern = "漁獲量")){
    data = read.xlsx(paste0(files[i]), sheet = "元データ") 
    data = data %>% 
      mutate(time = as.POSIXct('1899-12-30') + as.difftime(data$年月日, units = 'days'))
    colnames(data)
    data = data %>% 
      select(市場コード, 魚種コード, 漁業種コード, "日別水揚量(キロ)", time) %>% 
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), day = as.numeric(str_sub(time, 9, 10))) %>%
      select(-time)
    
    miya = rbind(miya, data)
  }
  
  if(str_detect(files[i], pattern = "いかなご")){
    data = read.xlsx(paste0(files[i]), sheet = "元データ") 
    data = data %>% 
      select(市場コード, 魚種コード, 漁業種コード, "日別水揚量(キロ)", 年, 月, 日) %>% 
      dplyr::rename(year = 年, month = 月, day = 日)
    
    miya = rbind(miya, data)
  }
  
  if(str_detect(files[i], pattern = "アイナメ2000")){
    data = read.xlsx(paste0(files[i]), sheet = "漁業種別統計", startRow = 4) %>% 
      select_if(negate(anyNA)) 
    data = data[, -ncol(data)]
    data = data %>% gather(key = 漁業種コード, value = "日別水揚量(キロ)", 2:ncol(data)) %>% 
      mutate(year = as.numeric(str_sub(X1, 1, 4)), 市場コード = NA, month = NA, day = NA, 魚種コード = "アイナメ") %>%
      select(-X1)
    
    miya = rbind(miya, data)
  }
  
  if(str_detect(files[i], pattern = "エゾイソ")){
    data = read.xlsx(paste0(files[i]), sheet = "漁業種別統計", startRow = 4)
    data = data[, -ncol(data)]
    data = data %>% gather(key = 漁業種コード, value = "日別水揚量(キロ)", 2:ncol(data)) %>% 
      mutate(year = as.numeric(str_sub(X1, 1, 4)), 市場コード = NA, month = NA, day = NA, 魚種コード = "エゾイアイナメ") %>%
      select(-X1)
    miya = rbind(miya, data)
  }
  
  if(str_detect(files[i], pattern = "アイナメ2000")){
    data = read.xlsx(paste0(files[i]), sheet = "漁業種別統計", startRow = 4) %>% 
      select_if(negate(anyNA)) 
    data = data[, -ncol(data)]
    data = data %>% gather(key = 漁業種コード, value = "日別水揚量(キロ)", 2:ncol(data)) %>% 
      mutate(year = as.numeric(str_sub(X1, 1, 4)), 市場コード = NA, month = NA, day = NA, 魚種コード = "アイナメ") %>%
      select(-X1)
    
    miya = rbind(miya, data)
  }
  
  
  if(str_detect(files[i], pattern = "いらこあなご")){
    data = read.xlsx(paste0(files[i]), sheet = "データ") 
    data = data %>% 
      select(市場コード, 魚種コード, 漁業種コード, "日別水揚量(キロ)", 年, 月, 日) %>% 
      dplyr::rename(year = 年, month = 月, day = 日)
    miya = rbind(miya, data)
  }
  
  if(str_detect(files[i], pattern = "ケガニ")){
    data = read.xlsx(paste0(files[i]), sheet = "データ") 
    data = data %>% 
      select(市場コード, 魚種コード, 漁業種コード, "日別水揚量(キロ)", 年, 月, 日) %>% 
      dplyr::rename(year = 年, month = 月, day = 日)
    miya = rbind(miya, data)
  }
}

summary(miya) #catch < 0がある!!!
test = miya
colnames(test)
test = test %>% dplyr::rename(catch = "日別水揚量(キロ)") %>% filter(catch < 0)



unique(miya$魚種コード)

setwd(dir_save)
write.csv(miya, "catch_miya2020.csv", fileEncoding = "CP932")
