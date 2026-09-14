library(tidyverse)  
library(cluster)    # Algoritma klastering
library(factoextra) # Algoritma klastering dan visualisasi
library(readxl)

dataclus <- read_excel("Boats_dataset_UTS - Spring 2024.xlsx")
dataclus <- data.frame(dataclus)
str(dataclus)
head(dataclus)

dataclus1 <- na.omit(dataclus) #untuk menghilangkan data missing

# Analisis Deskriptif dan Visualisasi Frekuensi
# Analisis Deskriptif
summary(dataclus1) 

# Visualisasi Frekuensi
table(dataclus1$condition)
barplot(table(dataclus1$condition), main="Boat's Condition", xlab="Condition", ylab="Count")

Frequency <- dataclus1%>%
  group_by(dataclus1$condition) %>%
  summarise("1" = length(condition[which(numEngines==1)]),
            "2" = length(condition[which(numEngines==2)]),
            "3" = length(condition[which(numEngines==3)]),
            "4" = length(condition[which(numEngines==4)]))
Frequency <- as.matrix(Frequency)
Frequency <- Frequency[,-1] 
Frequency
bp <- barplot(Frequency, main="Number Engines and Condition", xlab="Number Engines", ylab="Count")

# Tambahkan legenda
legend("topright", # posisi legenda
       legend = c("1","0"), # teks legenda
       fill = bp, 
       title = "Condition") # judul legenda

# Frekuensi berdasarkan tahun
# Membagi tahun-tahun menjadi interval 10 tahun menggunakan fungsi cut()
interval <- cut(dataclus1$year, breaks = seq(1940, 2020, by = 10), labels = FALSE)

# Menghitung jumlah baris dalam setiap interval menggunakan fungsi table()
count <- table(interval)

# Membuat data frame baru dengan jumlah baris dalam setiap interval
Freq_by_year <- data.frame(
  interval = c("1941-1950","1951-1960","1961-1970",
               "1971-1980","1981-1990","1991-2000",
               "2001-2010","2011-2020"),
  count = c(count[1],count[2],count[3],count[4],
            count[5],count[6],count[7],count[8])
)
Freq_by_year

Freq_by_year1 <- matrix(Freq_by_year$count,nrow = 1)
colnames(Freq_by_year1) <- Freq_by_year$interval

barplot(Freq_by_year1, main="Boat Production per 10 Years", xlab="Year", ylab="Count")

# Analisis model regresi
str(dataclus1)
dataclus1$condition <- as.factor(dataclus1$condition)
dataclus1$numEngines <- as.factor(dataclus1$numEngines)
str(dataclus1)

# Model Regresi Linear Berganda
OLS <- lm(price ~ condition+length_ft+beam_ft+dryWeight_lb+numEngines+totalHP,data = dataclus1)
summary(OLS)
s.resids <- rstudent(OLS)

# Normality test
shapiro.test(s.resids)

# Heteroskedastisity test
lmtest::bptest(OLS)

# Multicolinierity test
library(car)
vif(OLS)

# Analisis Cluster with K-Means Clustering
dataclus1$condition <- as.numeric(dataclus1$condition)
dataclus1$numEngines <- as.numeric(dataclus1$numEngines)

set.seed(123)
datafix <- scale(dataclus1) #standarisasi data
Clust_model <- kmeans(datafix, 4)
print(Clust_model)

fviz_cluster(Clust_model, data = datafix)

dataclus %>%
  mutate(Cluster = Clust_model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
