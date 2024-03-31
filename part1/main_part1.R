setwd(getwd())

# načítanie datasetu
labor_measurements <- read.csv('./data/measurements.csv', sep='\t')
labor_stations <- read.csv('./data/stations.csv', sep='\t')

print('Zobrazenie prvých riadkov datasetov, ich rozmerov a typov atribútov \n\n')

print('Merenia: \n')
head(labor_measurements)
cat('Number of records: ', nrow(labor_measurements), '\n')
cat('Number of attributes: ', ncol(labor_measurements), '\n\n')
cat('Types of attributes:\n')
str(labor_measurements)

print('Stanice: \n')
head(labor_stations)
cat('Number of records: ', nrow(labor_stations), '\n')
cat('Number of attributes: ', ncol(labor_stations), '\n\n')
cat('Types of attributes:\n')
str(labor_stations)


print('Zjednotenie atribútov:')
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
print('Identifikované rozdielne formáty dátumu sme zjednotili na formát dd mmm yyyy')
# Opätovná kontrola jedinečných hodnôt a ich počtov po nahradení
revision_count <- table(labor_stations$revision)
revision_count