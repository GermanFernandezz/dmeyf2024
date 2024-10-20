# Especifica la ruta del directorio que contiene tus archivos .csv

dir_path <- "D:/MaestriaDataMining/DMEyF/exp/KA7450_2024_10_13_A/losquefaltan"
competition <- "dm-ey-f-2024-primera"  # Reemplaza con el nombre de la competencia en Kaggle

# Lista los archivos en el directorio
files <- list.files(path = dir_path, pattern = "*.csv", full.names = TRUE)

# Función para subir un archivo a Kaggle
upload_to_kaggle <- function(file_path, competition) {
  system2("kaggle", args = c("competitions", "submit", "-c", competition, "-f", file_path, "-m", basename(file_path)))
}

# Subir todos los archivos .csv al concurso
for (file in files) {
  cat("Subiendo archivo:", basename(file), "\n")
  upload_to_kaggle(file, competition)
}

cat("¡Todas las subidas están completas!")