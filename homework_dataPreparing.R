
# Author      : Andri Kusumawardhana

# melakukan Install Package 'readxl' untuk membaca data input bertipe excel (.xls, .xlsx)
install.packages('readxl')

# Load package 'readxl'
library('readxl')

# untuk mengetahui directory file
getwd()

# mengimport dataset bertipe excel
df_online_retail = read_excel('D:/Training MBA Iykra/dataset/Online Retail.xlsx')

# Menampilkan data 6 data teratas 
head(df_online_retail)

# Menampilkan data 6 data terbawah
tail(df_online_retail)

# Menampilkan summary data
summary(df_online_retail)

# Menampilkan jumlah missing data ke dalam plot
library(DataExplorer)
plot_missing(df_online_retail)
# hasilnya Pada Field CustomerID Terdapat Missing Data sebanyak 24.93% 

# Menampilkan struktur dataset Online_retail
str(df_online_retail)

# membuat dataset clean data dgn menghapus CustomerID = Na
df_online_retail_clean = df_online_retail[!is.na(df_online_retail$CustomerID),] 

# melakukan pengecekan Missing data menggunakan plot
plot_missing(df_online_retail_clean)
#Sudah tidak ada missing data di field CustomerID

summary(df_online_retail_clean)

# Membuat tabel dengan 2 variabel: 
# CustomerID | frequency
# frequency adalah berapa kali satu orang membeli barang
library('tidyverse')
frequency = df_online_retail_clean %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))

head(frequency)

# Membuat tabel dengan 2 variabel:
# CustomerID | monetary
# monetary adalah berapa jumlah uang yang dibelanjakan oleh masing-masing orang
monetary = df_online_retail_clean %>% group_by(CustomerID) %>% summarise(monetary = sum(UnitPrice*Quantity))
head(monetary)

# Menambahkan satu kolom di tabel df_online_retail_drop bernama 'recency'
# recency berisi berapa hari sejak pembelian terakhir customer ke tanggal 31 Desember 2011

library('lubridate')
recency = df_online_retail_clean %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>% filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate, ymd('2011-12-31'))))/86400)
head(recency)

# Join ketiganya
df_rfm = recency %>% left_join(frequency, by = 'CustomerID') %>% left_join(monetary, by = 'CustomerID')

# Tampilkan hanya CustomerID, Recency, Frequency, dan Monetary sesuai soal:
df_result = df_rfm %>% select(CustomerID, recency, frequency, monetary)
head(df_result)

# membentuk file hasil clean dalam bentuk .csv
write.csv(df_result, "D:/Training MBA Iykra/Homework/Data_online_retail_clean_Andri.csv", quote=F, row.names = F)
