
######################
#######packages#######
######################
install.packages("readxl")  # 用於讀取 Excel 文件
install.packages("rvest")
install.packages("httr")
install.packages("gganimate")
install.packages("ggplot2")  # gganimate 基於 ggplot2



##########################################
##########正式匯入+合併變數中英文#########
##########################################

# 引入必要套件
library(httr)

# 定義年份和季節
years <- 101:113
seasons <- c("S1", "S2", "S3", "S4")
land_types <- c("A", "B", "C")  # A_lvr_land_A, A_lvr_land_B, A_lvr_land_C

# 初始化清單以存放讀取的資料
data_list <- list()

# 遍歷年份、季節和土地類型，生成 URL
for (year in years) {
  for (season in seasons) {
    for (land_type in land_types) {
      
      # 排除不存在的檔案
      if ((year == 113 && season == "S4") || (year == 101 && season != "S4")) {
        next  # 跳過不存在的檔案
      }
      
      # 建立 URL
      file_name <- paste0(year, season, "_A_lvr_land_", land_type, ".csv")
      file_url <- paste0("https://raw.githubusercontent.com/helumis/-/main/101S4~113S3%E4%B8%8D%E5%8B%95%E7%94%A2%E3%80%81%E9%A0%90%E5%94%AE%E5%B1%8B%E3%80%81%E7%A7%9F%E8%B3%83/", file_name)
      
      # 嘗試讀取 CSV 檔案
      message("正在讀取: ", file_url)
      data <- tryCatch({
        read.csv(file_url, header = FALSE, stringsAsFactors = FALSE)  # 設定 header = FALSE 以保留所有行
      }, error = function(e) {
        message(paste("讀取檔案時出現錯誤:", file_url, "\n錯誤訊息:", e$message))
        return(NULL)  # 返回 NULL 如果出錯
      })
      
      # 將讀取的資料存入清單
      if (!is.null(data)) {
        data_list[[file_name]] <- data  # 成功讀取後加入清單
      }
    }
  }
}

# 檢查是否成功讀取資料
if (length(data_list) == 0) {
  stop("沒有成功讀取任何檔案。")
} else {
  message("所有檔案均成功讀取。")
}

# 定義函數來合併變數名稱
process_data <- function(data) {
  # 檢查資料行數是否足夠
  if (nrow(data) < 2) {
    warning("資料行數不足，無法合併變數名稱。")
    return(data)  # 返回原始資料
  }
  
  # 提取中文與英文變數名稱
  cn_names <- data[1, ]  # 第一行：中文名稱
  en_names <- data[2, ]  # 第二行：英文名稱
  
  # 檢查英文變數名稱是否為空或 NA
  if (all(is.na(en_names) | en_names == "")) {
    # 如果第二行全是空值或 NA，僅保留中文變數名稱
    combined_names <- cn_names
  } else {
    # 合併中文和英文變數名稱
    combined_names <- paste(cn_names, en_names, sep = "_")
  }
  
  # 移除前兩行，剩下的為資料
  data <- data[-c(1, 2), ]
  
  # 設置新的列名稱
  colnames(data) <- combined_names
  
  return(data)
}

# 對 data_list 中的所有資料進行處理
for (file_name in names(data_list)) {
  data_list[[file_name]] <- process_data(data_list[[file_name]])
}


#################################################################################
##############################合併成單一資料集###################################
#################################################################################
merged_data <- bind_rows(data_list)
colnames(merged_data)
################################
##########資料檢視函數##########
################################

get_unique_values <- function(df, col_index) {
  # 檢查col_index是否有效
  if (col_index > ncol(df) || col_index < 1) {
    stop("col_index超出範圍")
  }
  
  # 取得唯一值
  unique_values <- unique(df[[col_index]])
  
  # 顯示所有唯一值
  print(unique_values)
}


get_unique_values(merged_data, 2)

####################################
##########排除"土地","車位"#########
####################################
# 排除第2個變數內為"土地"或"車位"的觀察值
merged_data_BU <- merged_data[!(merged_data[[2]] %in% c("土地", "車位")), ]
################################
##########資料檢視:使用分區#####
################################
get_unique_values(merged_data_BU, 5)
count_value_occurrences <- function(df, col_index, value) {
  # 檢查col_index是否有效
  if (col_index > ncol(df) || col_index < 1) {
    stop("col_index超出範圍")
  }
  
  # 計算該值的出現次數
  count <- sum(df[[col_index]] == value, na.rm = TRUE)
  
  # 返回計數
  return(count)
}

count_value_occurrences(merged_data_BU, 5, "住") 
count_value_occurrences(merged_data_BU, 5, "商") 
count_value_occurrences(merged_data_BU, 5, "工") 
count_value_occurrences(merged_data_BU, 5, "農") 
count_value_occurrences(merged_data_BU, 5, "其他") 
###########################
##########只保留住#########
###########################
# 篩選資料，只保留第五個變數為"住"的觀察值
merged_data_BULI <- merged_data_BU[merged_data_BU[[5]] == "住", ]
colnames(merged_data_BULI)
###########################
##########合併變數V1#######
###########################
combine_variables_if_missing <- function(df, var1, var2) {
  # 檢查變數是否存在於資料框中
  if (!all(c(var1, var2) %in% names(df))) {
    stop("其中一個或兩個變數不在資料框中")
  }
  
  # 檢查兩變數是否在所有觀察值下至少一個為缺失值
  if (all(is.na(df[[var1]]) | is.na(df[[var2]]))) {
    # 合併兩變數，優先取非缺失值
    df[[var1]] <- ifelse(is.na(df[[var1]]), df[[var2]], df[[var1]])
    
    # 刪除第二個變數
    df[[var2]] <- NULL
    
    # 回傳更新後的資料框
    return(df)
  } else {
    warning("兩變數在部分觀察值中同時存在數值，無法合併")
    return(df)
  }
}

###########################
##########合併變數V2#######
###########################
combine_similar_vars <- function(df) {
  # 取得變數名稱列表
  var_names <- names(df)
  
  # 建立合併過的資料框副本
  new_df <- df
  
  # 儲存被移除的變數名稱
  removed_vars <- c()
  
  # 遍歷所有變數名稱組合
  for (i in 1:(length(var_names) - 1)) {
    for (j in (i + 1):length(var_names)) {
      # 分割變數名稱
      var1_split <- strsplit(var_names[i], "_")[[1]]
      var2_split <- strsplit(var_names[j], "_")[[1]]
      
      # 檢查是否為 NA 或無法分割，若是則跳過
      if (is.na(var1_split[1]) || is.na(var2_split[1])) {
        next
      }
      
      # 檢查變數名稱的前部分是否相同
      if (length(var1_split) < 2) {
        # 若 `var_names[i]` 僅有前半部分，檢查 `var_names[j]` 是否以相同部分開頭
        if (startsWith(var_names[j], paste0(var1_split[1], "_"))) {
          var2_split <- var1_split  # 調整以進行合併
        } else {
          next  # 不符合則跳過
        }
      } else if (length(var2_split) < 2) {
        # 若 `var_names[j]` 僅有前半部分，檢查 `var_names[i]` 是否以相同部分開頭
        if (startsWith(var_names[i], paste0(var2_split[1], "_"))) {
          var1_split <- var2_split
        } else {
          next
        }
      }
      
      # 檢查兩變數的前後部分是否相同
      if (var1_split[1] == var2_split[1] || var1_split[2] == var2_split[2]) {
        # 檢查是否符合至少n-1個為缺失值的條件
        combined_na_count <- rowSums(is.na(new_df[c(var_names[i], var_names[j])]))
        
        if (all(combined_na_count >= (length(c(var_names[i], var_names[j])) - 1))) {
          # 合併兩變數，優先取非缺失值
          new_df[[var_names[i]]] <- ifelse(is.na(new_df[[var_names[i]]]), new_df[[var_names[j]]], new_df[[var_names[i]]])
          
          # 加入刪除的變數名稱到 removed_vars 清單
          removed_vars <- c(removed_vars, var_names[j])
          
          # 刪除第二個變數
          new_df[[var_names[j]]] <- NULL
          
          # 更新變數名稱列表
          var_names <- names(new_df)
        }
      }
    }
  }
  
  # 顯示被移除的變數名稱
  print("Removed variables:")
  print(removed_vars)
  
  return(new_df)
}

colnames(merged_data_BULI)
tidy_merged_data_BULI <- combine_similar_vars(merged_data_BULI)
colnames(tidy_merged_data_BULI)
tidy1_merged_data_BULI <- combine_variables_if_missing(tidy_merged_data_BULI,  "有無電梯_elevator" , "電梯_" )
colnames(tidy1_merged_data_BULI)
get_unique_values(tidy1_merged_data_BULI, 2)
################################
############資料檢查############
################################
# 定義檢查函數
check_no_na_in_variable <- function(data_list, file_suffix, variable_name) {
  # 儲存結果的清單
  result <- list()
  
  # 遍歷 data_list 中的所有資料集
  for (name in names(data_list)) {
    # 檢查檔名是否以指定結尾 file_suffix
    if (grepl(paste0(file_suffix, "$"), name)) {
      # 取得資料集
      df <- data_list[[name]]
      
      # 檢查是否存在指定變數 variable_name，且該變數無缺失值
      if (variable_name %in% names(df) && all(!is.na(df[[variable_name]]))) {
        result[[name]] <- "No missing values"
      } else {
        result[[name]] <- "Has missing values or variable not found"
      }
    }
  }
  
  # 顯示結果
  return(result)
}

# 使用範例
# 假設我們要檢查結尾為 "_B.csv" 且 "建案名稱_build case" 變數是否有缺失值
result <- check_no_na_in_variable(data_list, "_B.csv", "建案名稱_build case")
print(result)
################################################################################################################################
############資料合併V2不動產、租賃、預售屋分別合併。############################################################################
################################################################################################################################
# 載入 dplyr 套件以使用 bind_rows 函數
library(dplyr)

# 定義合併函數
bind_rows_by_suffix <- function(data_list, file_suffix) {
  # 儲存符合條件的資料集
  selected_dfs <- list()
  
  # 遍歷 data_list 中的所有資料集
  for (name in names(data_list)) {
    # 檢查檔名是否以指定結尾 file_suffix
    if (grepl(paste0(file_suffix, "$"), name)) {
      # 將符合條件的資料集加入 selected_dfs 清單
      selected_dfs[[name]] <- data_list[[name]]
    }
  }
  
  # 檢查是否有符合條件的資料集
  if (length(selected_dfs) == 0) {
    stop("No datasets found with the specified suffix.")
  }
  
  # 使用 bind_rows 將符合條件的資料集合併
  combined_df <- bind_rows(selected_dfs, .id = "source")
  
  # 返回合併後的資料集
  return(combined_df)
}


#個別合併不動產、預售屋、租賃
combined_data_A <- bind_rows_by_suffix(data_list, "_A.csv")
combined_data_B <- bind_rows_by_suffix(data_list, "_B.csv")
combined_data_C <- bind_rows_by_suffix(data_list, "_C.csv")
#檢視標的
get_unique_values(combined_data_A, 3)
get_unique_values(combined_data_B, 3)
get_unique_values(combined_data_C, 3)
# 排除第變數內為"土地"或"車位"的觀察值
combined_data_A_BU <- combined_data_A [!(combined_data_A [[3]] %in% c("土地", "車位")), ]
combined_data_B_BU <- combined_data_B [!(combined_data_B [[3]] %in% c("土地", "車位")), ]
combined_data_C_BU <- combined_data_C [!(combined_data_C [[3]] %in% c("土地", "車位")), ]
############################
##########計數結果##########
############################
count_value_occurrences_summary <- function(data, column_index, values) {
  # 計算欄位中特定值的出現次數
  value_counts <- table(data[[column_index]])
  
  # 計算指定的類別數量
  counts <- sapply(values, function(val) {
    if (val %in% names(value_counts)) {
      value_counts[val]
    } else {
      0
    }
  })
  
  # 計算未指定的類別數量
  total_count <- nrow(data)
  specified_count <- sum(counts)
  unspecified_count <- total_count - specified_count
  
  # 將結果存入清單
  result <- as.list(counts)
  result$未指定 <- unspecified_count
  
  return(result)
}
#各類別用地交易數
count_value_occurrences_summary(combined_data_A_BU, 6, c("住", "商", "工", "農", "其他"))
count_value_occurrences_summary(combined_data_B_BU, 6, c("住", "商", "工", "農", "其他"))
count_value_occurrences_summary(combined_data_C_BU, 6, c("住", "商", "工", "農", "其他"))


combined_data_A_BULI <- combined_data_A_BU[combined_data_A_BU[[5]] == "住", ]
colnames(combined_data_A_BULI)
combined_data_B_BULI <- combined_data_B_BU[combined_data_B_BU[[5]] == "住", ]
colnames(combined_data_B_BULI)
combined_data_C_BULI <- combined_data_C_BU[combined_data_C_BU[[5]] == "住", ]
colnames(combined_data_C_BULI)
###############合併同類變數###########
tidy_merged_data_BULI <- combine_similar_vars(merged_data_BULI)