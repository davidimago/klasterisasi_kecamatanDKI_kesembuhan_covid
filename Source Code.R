##### Loading & Pra Proses Awal #####
# Load
setwd("dataset/")
library(readxl)
temp = list.files(pattern="*.xlsx")
myfiles = lapply(temp, read_excel, sheet = 'data_kecamatan')

summary(myfiles)
str(myfiles[[1]])
str(myfiles[[18]])
str(myfiles[[19]]) # nama kolom "meninggal" berbeda untuk file 1-18 dg 19-98

# Rename dan trim kolom, filter hanya untuk daerah DKI dan kolom yang dibutuhkan
library(dplyr)
library(stringr)
data = list()
# untuk file 1 - 18
for (i in 1:18){
  data[[i]] <- myfiles[[i]] %>%
    select(ID_KEC, Nama_provinsi, nama_kota, nama_kecamatan,
           POSITIF, Dirawat, `Self Isolation`, Sembuh, Meninggal...29) %>%
    mutate(id = str_squish(ID_KEC), prov = str_squish(Nama_provinsi),
           kota = str_squish(nama_kota), kecamatan = str_squish(nama_kecamatan),
           positif = POSITIF, dirawat = Dirawat, mandiri = `Self Isolation`,
           sembuh = Sembuh, meninggal = Meninggal...29) %>%
    filter(prov == "DKI JAKARTA") %>%
    select(id, prov, kota, kecamatan, positif, sembuh)
}
# untuk file 19 - 98
for (i in 19:length(myfiles)){
  data[[i]] <- myfiles[[i]] %>%
    select(ID_KEC, Nama_provinsi, nama_kota, nama_kecamatan,
           POSITIF, Dirawat, `Self Isolation`, Sembuh, Meninggal...30) %>%
    mutate(id = str_squish(ID_KEC), prov = str_squish(Nama_provinsi),
           kota = str_squish(nama_kota), kecamatan = str_squish(nama_kecamatan),
           positif = POSITIF, dirawat = Dirawat, mandiri = `Self Isolation`,
           sembuh = Sembuh, meninggal = Meninggal...30) %>%
    filter(prov == "DKI JAKARTA") %>%
    select(id, prov, kota, kecamatan, positif, sembuh)
}

# Cek apakah setiap file memiliki baris (jumlah kelurahan) yang sama
for (i in 1:length(myfiles)){
  print(nrow(data[[i]]))
}

# Cek missing value
for (i in 1:length(myfiles)){
  print(sum(unique(is.na(data[[i]]))))
}

##### Pra Proses untuk Mining #####
# Memisahkan data sembuh dan positif
kesembuhan = list()
for (i in 1:length(data)){
  kesembuhan[[i]] <- select(data[[i]], id, sembuh)
}
head(kesembuhan[[1]],5)

positif = list()
for (i in 1:length(data)){
  positif[[i]] <- select(data[[i]], id, positif)
}
head(positif[[1]],5)

# Renaming kolom 'sembuh' menjadi 'sembuh01' berdasarkan urutan hari, begitu juga utk 'positif'
for (i in 1:9){
  new_name <- paste('sembuh','0',i,sep="")
  kesembuhan[[i]][new_name] <- kesembuhan[[i]]['sembuh']
  kesembuhan[[i]] <- select(kesembuhan[[i]], -sembuh)
  
  new_name1 <- paste('positif','0',i,sep="")
  positif[[i]][new_name1] <- positif[[i]]['positif']
  positif[[i]] <- select(positif[[i]], -positif)
}

for (i in 10:length(data)) {
  new_name <- paste('sembuh',i,sep="")
  kesembuhan[[i]][new_name] <- kesembuhan[[i]]['sembuh']
  kesembuhan[[i]] <- select(kesembuhan[[i]], -sembuh)
  
  new_name1 <- paste('positif',i,sep="")
  positif[[i]][new_name1] <- positif[[i]]['positif']
  positif[[i]] <- select(positif[[i]], -positif)
}

# Setiap file dalam list digabung menjadi 1 dataset
datasembuh = select(kesembuhan[[1]], id, sembuh01)
for (i in 2:length(kesembuhan)) {
  datasembuh <- datasembuh %>% left_join(kesembuhan[[i]], by="id")
}
sum(unique(is.na(datasembuh)))

datapositif = select(positif[[1]], id, positif01)
for (i in 2:length(kesembuhan)) {
  datapositif <- datapositif %>% left_join(positif[[i]], by="id")
}
sum(unique(is.na(datapositif)))

# Eliminasi kolom kecamatan, sehingga terbentuk data siap mining
mining <- datasembuh[-1]

##### Kmeans #####
# Ekstrak SSE untuk setiap nilai k
library(purrr)
sse <- map_dbl(1:10, function(k) {
  model <- kmeans(mining, centers = k, nstart = 20)
  model$tot.withinss
})

# Membuat dataset untuk setiap nilai k dengan nilai SSE-nya
sse_df <- data.frame(k = 1:10, sse = sse)

# Visualisasi SSE berdasarkan nilai k, ditentukan nilai k berdasarkan "elbow"
library(ggplot2)
ggplot(sse_df, aes(k, sse)) + geom_line() + scale_x_continuous(breaks = 1:10)

# Ekstrak Avg Silhouette Width untuk setiap nilai k
library(cluster)
silwid <- map_dbl(2:10, function(k) {
  model <- pam(mining, k = k)
  model$silinfo$avg.width
})

# Membuat dataset untuk setiap nilai k dengan nilai avg.width-nya
silwid_df <- data.frame(k = 2:10, silwid = silwid)

# Visualisasi avg.Width berdasarkan k, ditentukan nilai k berdasarkan avg.width tertinggi
ggplot(silwid_df, aes(k, silwid)) + geom_line() + scale_x_continuous(breaks = 2:10)

# Clustering sebanyak 2 kali, sesuai hasil SSE dan avg width
k4mining <- kmeans(mining, centers = 4, nstart = 20)
k6mining <- kmeans(mining, centers = 6, nstart = 20)

# Ekstrak dan menyimpan hasil clustering
k4clust <- k4mining$cluster
k6clust <- k6mining$cluster

# Memasukkan nilai cluster ke dalam dataset utama
cluster_result <- mutate(datasembuh, clustk4 = k4clust, clustk6 = k6clust)

# Persebaran data pada masing-masing cluster
table(k4clust)
table(k6clust)

# Mengubah data dari melebar menjadi memanjang
library(tidyr)
gathered_data <- gather(data = cluster_result,
                          key = date,
                          value = sembuh,
                          -id, -clustk4, -clustk6)

# Visualisasi hasil clustering
ggplot(gathered_data, aes(date, sembuh, col = factor(clustk4))) +
  geom_line(aes(group = id))

ggplot(gathered_data, aes(date, sembuh, col = factor(clustk6))) +
  geom_line(aes(group = id))

##### Hierarchical #####
# Melakukan clustering dengan 3 metode
dist_mining <- dist(mining)
hclust_mining1 <- hclust(dist_mining, "complete")
hclust_mining2 <- hclust(dist_mining, "average")
hclust_mining3 <- hclust(dist_mining, "single")

# Visualisasi dendogram masing-masing metode, dipilih yang terbaik (yang grafiknya balance)
plot(hclust_mining1, main = "comp")
plot(hclust_mining2, main = "avg")
plot(hclust_mining3, main = "sing")

# Estimasi nilai k (berdasar dendogram) sehingga penyebaran data cukup merata antar cluster
h4cluster <- cutree(hclust_mining1, k = 4)
table(h4cluster)

# Ekstrak dan menyimpan hasil clustering
cluster_result <- mutate(cluster_result, clusth4 = h4cluster)

# Mengubah data dari melebar menjadi memanjang
gathered_data <- gather(data = cluster_result,
                          key = date,
                          value = sembuh,
                          -id, -clustk4, -clustk6, -clusth4)

# Visualisasi hasil clustering
ggplot(gathered_data, aes(date, sembuh, col = factor(clusth4))) +
  geom_line(aes(group = id))

##### Analisis Post-Mining #####
# Penggalian nilai slope(koefisian pertumbuhan grafik)
int_slp <- data.frame(intercept=0, slope=0)
x <- c(1:98)
for (i in 1:length(datasembuh)) {
  y <- unlist(gather(datasembuh[i,-1], key = date, value = num)[-1], use.names = F)
  mod <- coef(lm(y~x))
  int_slp[i,1] <- mod[1]
  int_slp[i,2] <- mod[2]
}

long_smbh <- gather(datasembuh, key = date, value = smbh, -id)
long_pstf <- gather(datapositif, key = date, value = pstf, -id)

# Ratio kesembuhan dari total kasus
ratio <- long_pstf %>%
  left_join(long_smbh, by = "id") %>%
  mutate(ratio_kesembuhan = smbh/pstf) %>%
  group_by(id) %>% summarize(ratio_kesembuhan = mean(ratio_kesembuhan))
ratio <- as.data.frame(ratio)

# Tabel final, berisi data kecamatan dengan cluster dan nilai slope nya
final <- mutate(cluster_result[,c("id","clustk4","clustk6","clusth4")],
                intercept = int_slp[,1], slope = int_slp[,2],
                ratio_kesembuhan = ratio[,2])

# Melihat irisan cluster pada kedua metode (kmeans-4 & hierarcy-4)
table(k4clust, h4cluster)

# Menganalisa perbedaan tingkat pertumbuhan kesembuhan di masing-masing cluster
final %>% group_by(clustk4) %>%
  summarize(jml_kec = n(),
            intercept = mean(intercept), xslope = mean(slope),
            ratio_kesembuhan = mean(ratio_kesembuhan))

final_result <- final %>%
  left_join(data[[1]], by="id") %>%
  select(kota, kecamatan, clustk4, intercept, slope, ratio_kesembuhan)

# Persebaran cluster kecamatan pada masing-masing kota
kota_spread <- final_result %>% group_by(kota, clustk4) %>% count()
reshape2::dcast(kota_spread, kota ~ clustk4, value.var = "n")

final_result %>% group_by(kota) %>%
  summarize(intercept = mean(intercept), slope = mean(slope))

