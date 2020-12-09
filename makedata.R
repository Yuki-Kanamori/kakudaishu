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
i = 2
for(i in 1:length(files)){
  data = read.xlsx(paste0(files[i]))
  
  if(files[i] %in% "マダコ"){
    tag = data.frame(tf = is.na(data[, 1])) %>% filter(tf == "FALSE")
    data = data[1:nrow(tag), ] %>% 
      gather(key = time, value = catch, 5:ncol(data)) %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), file = paste0(files[i]), species = "マダコ") %>%
      filter(time != "合計")
    
    iwate = rbind(iwate, data)
  }
  if(files[i] %in% "ミズダコ"){
    tag = data.frame(tf = is.na(data[, 1])) %>% filter(tf == "FALSE")
    data = data[1:nrow(tag), ] %>% 
      gather(key = time, value = catch, 5:ncol(data)) %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), file = paste0(files[i]), species = "ミズダコ") %>% 
      filter(time != "合計") %>% dplyr::rename(分類名 = 魚種)
  }else{
    tag = data.frame(tf = is.na(data[, 1])) %>% filter(tf == "FALSE")
    data = data[1:nrow(tag), ] %>% 
      gather(key = time, value = catch, 4:ncol(data)) %>%
      mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7)), file = paste0(files[i])) %>% 
      filter(time != "合計") %>% mutate(分類名 = NA)
    
    #種名を入れる
    if(files[i] %in% "アオメエソ"){
      data = data %>% mutate(species = "アオメエソ")
    }
    if(files[i] %in% "アカガレイ"){
      data = data %>% mutate(species = "アカガレイ")
    }
    if(files[i] %in% "エゾイソアイナメ"){
      data = data %>% mutate(species = "エゾイソアイナメ")
    }
    if(files[i] %in% "ケガニ"){
      data = data %>% mutate(species = "ケガニ")
    }
    if(files[i] %in% "サヨリ"){
      data = data %>% mutate(species = "サヨリ")
    }
    if(files[i] %in% "ジンドウイカ"){
      data = data %>% mutate(species = "ジンドウイカ")
    }
    if(files[i] %in% "スズキ"){
      data = data %>% mutate(species = "スズキ")
    }
    if(files[i] %in% "ババガレイ"){
      data = data %>% mutate(species = "ババガレイ")
    }
    if(files[i] %in% "ホシガレイ"){
      data = data %>% mutate(species = "ホシガレイ")
    }
    # if(files[i] %in% "マダコ"){
    #   data = data %>% mutate(species = "マダコ")
    # }
    if(files[i] %in% "ミギガレイ"){
      data = data %>% mutate(species = "ミギガレイ")
    }
    # if(files[i] %in% "ミズダコ"){
    #   data = data %>% mutate(species = "ミズダコ")
    # }
    if(files[i] %in% "ヤナゴダコ"){
      data = data %>% mutate(species = "ヤナギダコ")
    }
  }
  
  iwate = rbind(iwate, data)
}
summary(iwate)
test = iwate
test[is.na(test)] = 0
summary(test)
test2 = test %>% filter(year == 0)
