setwd(getwd())

# načítanie datasetu
labor_measurements <- read.csv('./data/measurements.csv', sep='\t')
labor_stations <- read.csv('./data/stations.csv', sep='\t')

cat('Zobrazenie prvých riadkov datasetov, ich rozmerov a typov atribútov \n\n')

cat('Merenia: \n')
head(labor_measurements)
cat('Number of records: ', nrow(labor_measurements), '\n')
cat('Number of attributes: ', ncol(labor_measurements), '\n\n')
cat('Types of attributes:\n')
str(labor_measurements)

cat('Stanice: \n')
head(labor_stations)
cat('Number of records: ', nrow(labor_stations), '\n')
cat('Number of attributes: ', ncol(labor_stations), '\n\n')
cat('Types of attributes:\n')
str(labor_stations)


cat('Zjednotenie atribútov:')
# Data frame labor_measurements obsahuje iba číselné údaje float takže problém rozdielnych formátov nie je.
# Ked sa pozrieme na labor_stations tam už to tak nieje preto si skontrolujeme formáty

colnames(labor_stations)

# Kontrola jedinečných hodnôt a ich počtu
qos_count <- table(labor_stations$QoS)
qos_count
# Nahradenie špecifických hodnôt
labor_stations$QoS[labor_stations$QoS == "acceptable"] <- "accep"
labor_stations$QoS[labor_stations$QoS == "maitennce"] <- "maintenance"
# Opätovná kontrola jedinečných hodnôt a ich počtov po nahradení
qos_count <- table(labor_stations$QoS)
qos_count

# Kontrola jedinečných hodnôt a ich počtu
revision_count <- table(labor_stations$revision)
revision_count
# Nahradenie špecifických hodnôt
source("part1/utils.R")
labor_stations$revision <- sapply(labor_stations$revision, parse_dates)
cat('Identifikované rozdielne formáty dátumu sme zjednotili na formát dd mmm yyyy')
# Opätovná kontrola jedinečných hodnôt a ich počtov po nahradení
revision_count <- table(labor_stations$revision)
revision_count

cat('Nahradenie chýbajúcich hodnôt', '\n')

# Nahradenie prázdnych reťazcov reťazcami NA
labor_measurements[labor_measurements == ""] <- NA
labor_stations[labor_stations == ""] <- NA

missing_data_percentage <- sum(is.na(labor_measurements)) / nrow(labor_measurements) * 100
cat(sprintf("Chýbajúce dáta v labor_measurements tvoria %.5f%% dát", missing_data_percentage))
cat("\n")
missing_data_percentage_stations <- sum(is.na(labor_stations)) / nrow(labor_stations) * 100
cat(sprintf("Chýbajúce dáta v labor_stations tvoria %.5f%% dát", missing_data_percentage_stations))
cat("\n")

# Spočítanie a vypísanie riadkov s hodnotami NA pre labor_measurements
na_count_measurements <- sum(!complete.cases(labor_measurements))
cat('NaN hodnoty pre labor_measurements:', na_count_measurements, '\n')

# Spočítanie a vypísanie riadkov s hodnotami NA pre labor_stations
na_count_stations <- sum(!complete.cases(labor_stations))
cat('NaN hodnoty pre labor_stations:', na_count_stations, '\n')

# Drop rows with any NA values
labor_measurements <- na.omit(labor_measurements)
labor_stations <- na.omit(labor_stations)

# Kontrola duplicitných záznamov
cat('Duplicitné záznamy:', '\n')
if (any(duplicated(labor_measurements))) {
  duplicates_count <- sum(duplicated(labor_measurements))
  cat('labor_measurements pocet duplikatov', duplicates_count, '\n')
} else {
  cat('labor_measurements no duplicates\n')
}
if (any(duplicated(labor_stations))) {
  duplicates_count_stations <- sum(duplicated(labor_stations))
  cat('labor_stations pocet duplikatov', duplicates_count_stations, '\n')
} else {
  cat('labor_stations no duplicates\n')
}

labor_measurements <- unique(labor_measurements)
labor_stations <- unique(labor_stations)

cat('Nan záznamy a duplicitné záznamy boli odstránené', '\n')

# Zlúčenie súborov údajov
cat('Nasleduje zmergovanie datasetov:', '\n')
# Odstránenie nepotrebných stĺpcov
labor_stations <- labor_stations[, -c(2, 4:5)]
# Merge
df <- merge(labor_measurements, labor_stations, by = c("latitude", "longitude"), all = FALSE)
# Odstránenie stĺpcov 'latitude' a 'longitude' lebo ich už viac nepotrebujeme
df <- df[, !names(df) %in% c('latitude', 'longitude')]
head(df)

