require(tidyverse)
require(openxlsx)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"
dir_data = "/Users/Yuki/Dropbox/業務/拡大種/2020/data/【岩手県(修正版)】1994.1_2019.12_月別漁獲量(新規対象14種)"

setwd(dir_data)
path = dir_data
files = list.files(path)

iwate = NULL
for(i in 1:length(files)){
  #エクセルデータの読み込み
  #ミギガレイだけ合計という列がないので，調整する
  if(str_detect(files[i], pattern = "ミギガレイ")){
    data = read.xlsx(paste0(files[i]))
  }else{
    data = read.xlsx(paste0(files[i])) %>% select(-合計)
  }
  
  
  if(str_detect(files[i], pattern = "ミズダコ")){
    tag = data.frame(tf = is.na(data[, 1])) %>% filter(tf == "FALSE")
    data = data[1:nrow(tag), ] %>% 
      gather(key = time, value = catch, 5:ncol(data)) %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), file = paste0(files[i]), species = "ミズダコ") %>% 
      dplyr::rename(分類名 = 魚種)
    
    iwate = rbind(iwate, data)
  }else{
    if(str_detect(files[i], pattern = "マダコ")){
      tag = data.frame(tf = is.na(data[, 1])) %>% filter(tf == "FALSE")
      data = data[1:nrow(tag), ] %>% 
        gather(key = time, value = catch, 5:ncol(data)) %>%
        mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), file = paste0(files[i]), species = "マダコ")
      
      iwate = rbind(iwate, data)
    }else{
      tag = data.frame(tf = is.na(data[, 1])) %>% filter(tf == "FALSE")
      data = data[1:nrow(tag), ] %>% 
        gather(key = time, value = catch, 4:ncol(data)) %>%
        mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), file = paste0(files[i])) %>% 
        mutate(分類名 = NA)
      
      # 種名を入れる
      # %in%だとうまく引っかけられなかったので，stringrを使う
      if(str_detect(data[1, "file"], pattern = "アオメエソ")){
        data = data %>% mutate(species = "アオメエソ")
      }
      if(str_detect(data[1, "file"], pattern = "アカガレイ")){
        data = data %>% mutate(species = "アカガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "イラコアナゴ")){
        data = data %>% mutate(species = "イラコアナゴ")
      }
      if(str_detect(data[1, "file"], pattern = "エゾイソアイナメ")){
        data = data %>% mutate(species = "エゾイソアイナメ")
      }
      if(str_detect(data[1, "file"], pattern = "ケガニ")){
        data = data %>% mutate(species = "ケガニ")
      }
      if(str_detect(data[1, "file"], pattern = "サヨリ")){
        data = data %>% mutate(species = "サヨリ")
      }
      if(str_detect(data[1, "file"], pattern = "ジンドウイカ")){
        data = data %>% mutate(species = "ジンドウイカ")
      }
      if(str_detect(data[1, "file"], pattern = "スズキ")){
        data = data %>% mutate(species = "スズキ")
      }
      if(str_detect(data[1, "file"], pattern = "ババガレイ")){
        data = data %>% mutate(species = "ババガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "ホシガレイ")){
        data = data %>% mutate(species = "ホシガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "ミギガレイ")){
        data = data %>% mutate(species = "ミギガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "ヤナギダコ")){
        data = data %>% mutate(species = "ヤナギダコ")
      }
    }
    iwate = rbind(iwate, data) 
  }
}
summary(iwate)

setwd(dir_save)
write.csv(iwate, "iwate2020.csv", fileEncoding = "CP932")