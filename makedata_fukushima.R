require(tidyverse)
require(openxlsx)
require(readxl)

dir_save = "/Users/Yuki/Dropbox/業務/拡大種/2020/output"
dir_data = "/Users/Yuki/Dropbox/業務/拡大種/2020/data/福島拡大種漁獲量"

setwd(dir_data)

path = paste0(dir_data, "/漁獲量.xlsx")
files = excel_sheets(path) #シート名の取得

# 半角がエラーを引き起こしているので，全角に変換
hankana2zenkana <- function(x){
  # character変換
  if (!is.character(x)){ x <- as.character(x) }
  
  # 濁点、半濁点文字の置換
  dh <- c("ｶﾞ","ｷﾞ","ｸﾞ","ｹﾞ","ｺﾞ","ｻﾞ","ｼﾞ","ｽﾞ","ｾﾞ","ｿﾞ","ﾀﾞ","ﾁﾞ","ﾂﾞ","ﾃﾞ","ﾄﾞ","ﾊﾞ","ﾋﾞ","ﾌﾞ","ﾍﾞ","ﾎﾞ","ﾊﾟ","ﾋﾟ","ﾌﾟ","ﾍﾟ","ﾎﾟ")
  dz <- c("ガ","ギ","グ","ゲ","ゴ","ザ","ジ","ズ","ゼ","ゾ","ダ","ヂ","ヅ","デ","ド","バ","ビ","ブ","ベ","ボ","パ","ピ","プ","ペ","ポ")
  for( i in 1:length(dz) ){ x <- gsub(dh[i],dz[i],x) }
  
  # 1bite文字の置換
  x <- chartr("ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ"
              , "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー"
              , x)
  
  #print(x)
}

files2 = hankana2zenkana(files)

fuk = NULL
for(i in 1:length(files2)){
  data = read.xlsx(path, sheet = i, startRow = 3)
  
  if(str_detect(files2[i], paste(c("エゾイソ", "サワラ", "タチウオ", "カンパチ", "ヒラマサ"), collapse = "|"))){
    tag = grep("合計", colnames(data))
    tag2 = which(str_detect(data$X1, pattern = "平均"))
    data = data[1:(min(tag2)-1), 1:(min(tag)-1)]
    
    if(str_detect(files2[i], paste(c("カンパチ", "ヒラマサ"), collapse = "|"))){
      data = data %>% 
        dplyr::rename(年 = "X1") %>%
        mutate(年 = rep(1989:2019)) %>% 
        gather(key = 漁業種名, value = catch, 2:ncol(data)) %>% 
        mutate(species = paste0(files2[i]))
    }else{
      data = data %>% 
        dplyr::rename(年 = "X1") %>%
        mutate(年 = rep(1969:2019)) %>% 
        gather(key = 漁業種名, value = catch, 2:ncol(data)) %>% 
        mutate(species = paste0(files2[i]))
    }
    
    if(str_detect(files2[i], pattern = "☑")){
      data = data %>% mutate(species = str_sub(species, 2, 15))
    }
    
    fuk = rbind(fuk, data)
  }else{
    tag = grep("合計", colnames(data))
    # tag2 = which(data$年 == "平均")
    tag2 = which(str_detect(data$年, pattern = "平均"))
    data = data[1:(min(tag2)-1), 1:(min(tag)-1)]
    
    data = data %>%
      mutate(年 = rep(1969:2019)) %>% 
      gather(key = 漁業種名, value = catch, 2:ncol(data)) %>% 
      mutate(species = paste0(files2[i]))
    
    if(str_detect(files2[i], pattern = "☑")){
      data = data %>% mutate(species = str_sub(species, 2, 15))
    }
    
    fuk = rbind(fuk, data)
  }
}
summary(fuk)

#catchがnumericでない
fuk = fuk %>% mutate(catch = as.numeric(as.character(fuk$catch)))
summary(fuk)

# setwd(dir_save)
# write.csv(fuk, "catch_fuk2020.csv", fileEncoding = "CP932")
