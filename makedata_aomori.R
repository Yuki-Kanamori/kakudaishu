require(tidyverse)
require(openxlsx)
require(readxl)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"
dir_data = "/Users/Yuki/Dropbox/業務/拡大種/2020/data"

setwd(dir_data)

path = paste0(dir_data, "/R2東北ブロック新規対象魚種の漁獲量（%uFFFD青森県）.xlsx")
files = excel_sheets(path) #シート名の取得

ao = NULL
for(i in 4:length(files)){
  if(str_detect(files[i], pattern = "アイナメ")){# アイナメだけ年の列に列名が付いている
    data = read.xlsx(path, sheet = i, startRow = 6)
    tag = grep("合計", colnames(data)) 
    data = data[, 1:(tag-1)]
    data = data %>% 
      gather(key = 漁業種名, value = catch, 2:ncol(data)) %>% 
      mutate(species = paste0(files[i]))
    
    ao = rbind(ao, data)
  }else{
    if(str_detect(files[i], pattern = "メバル3種")){ #データが始まる行数が他種と異なる
      data = read.xlsx(path, sheet = i, startRow = 7)
      tag = grep("合計", colnames(data)) 
      data = data[, 1:(tag-1)]
      data = data %>% 
        gather(key = 漁業種名, value = catch, 2:ncol(data)) %>% 
        mutate(species = paste0(files[i])) %>% 
        dplyr::rename(年 = X1)
      
      ao = rbind(ao, data)
    }else{
      data = read.xlsx(path, sheet = i, startRow = 6)
      tag = grep("合計", colnames(data)) 
      data = data[, 1:(tag-1)]
      data = data %>% 
        gather(key = 漁業種名, value = catch, 2:ncol(data)) %>% 
        mutate(species = paste0(files[i])) %>% 
        dplyr::rename(年 = X1)
      
      ao = rbind(ao, data)
    }

  }
}

summary(ao)
setwd(dir_save)
write.csv(ao, "catch_ao2020.csv")