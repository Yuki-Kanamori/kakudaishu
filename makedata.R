require(tidyverse)
require(openxlsx)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"


# ---------------------------------------------------------------
# make data -----------------------------------------------------
# ---------------------------------------------------------------

# iwate ---------------------------------------------------------
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
head(iwate)



# ibaraki --------------------------------------------------------
dir_data = "/Users/Yuki/Dropbox/業務/拡大種/2020/data/茨城県拡大種"

setwd(dir_data)
path = dir_data
files = list.files(path)

iba = NULL

for(i in 1:length(files)){
  for(j in 1990:2019){
    data = read.xlsx(paste0(files[i]), sheet = paste0(j), startRow = 3) 
    data = data[, c(2, 4:15)]
    data = data %>% 
      filter(!is.na(漁法)) %>% 
      dplyr::rename(m01 = １月, m02 = ２月, m03 = ３月, m04 = ４月, m05 = ５月, m06 = ６月, m07 = ７月, m08 = ８月, m09 = ９月, m10 = １０月, m11 = １１月, m12 = １２月) %>% 
      gather(key = month2, value = catch, 2:ncol(data)) %>% 
      mutate(month = as.numeric(str_sub(month2, 2, 3))) %>% 
      select(-month2) %>% 
      mutate(year = as.numeric(paste0(j)), file = paste0(files[i]))
    if(nrow(data) != 0){
      if(str_detect(data[1, "file"], pattern = "未満")){
        data = data %>% mutate(species = "アイナメ80kg未満")
      }
      if(str_detect(data[1, "file"], pattern = "アオメエソ")){
        data = data %>% mutate(species = "アオメエソ")
      }
      if(str_detect(data[1, "file"], pattern = "アカガレイ")){
        data = data %>% mutate(species = "アカガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "イカナゴ")){
        data = data %>% mutate(species = "イカナゴ")
      }
      if(str_detect(data[1, "file"], pattern = "イシガレイ")){
        data = data %>% mutate(species = "イシガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "イシカワシラウオ")){
        data = data %>% mutate(species = "イシカワシラウオ")
      }
      if(str_detect(data[1, "file"], pattern = "イラコアナゴ")){
        data = data %>% mutate(species = "エゾイソアイナメ")
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
      if(str_detect(data[1, "file"], pattern = "サワラ")){
        data = data %>% mutate(species = "サワラ")
      }
      if(str_detect(data[1, "file"], pattern = "シロメバル")){
        data = data %>% mutate(species = "シロメバル")
      }
      if(str_detect(data[1, "file"], pattern = "ジンドウイカ")){
        data = data %>% mutate(species = "ジンドウイカ")
      }
      if(str_detect(data[1, "file"], pattern = "スズキ")){
        data = data %>% mutate(species = "スズキ")
      }
      if(str_detect(data[1, "file"], pattern = "タチウオ")){
        data = data %>% mutate(species = "タチウオ")
      }
      if(str_detect(data[1, "file"], pattern = "ツノナシオキアミ")){
        data = data %>% mutate(species = "ツノナシオキアミ")
      }
      if(str_detect(data[1, "file"], pattern = "ババガレイ")){
        data = data %>% mutate(species = "ババガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "マアナゴ")){
        data = data %>% mutate(species = "マアナゴ")
      }
      if(str_detect(data[1, "file"], pattern = "マガレイ")){
        data = data %>% mutate(species = "マガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "マコガレイ")){
        data = data %>% mutate(species = "マコガレイ")
      }
      if(str_detect(data[1, "file"], pattern = "マダコ")){
        data = data %>% mutate(species = "マダコ")
      }
      if(str_detect(data[1, "file"], pattern = "ミズダコ")){
        data = data %>% mutate(species = "ミズダコ")
      }
      if(str_detect(data[1, "file"], pattern = "ヤナギダコ")){
        data = data %>% mutate(species = "ヤナギダコ")
      }
      iba = rbind(iba, data) 
    }
  }

}
unique(iba$species)
