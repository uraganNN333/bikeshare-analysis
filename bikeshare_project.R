# Загрузка библиотек
library(ggplot2)
library(dplyr)
library(lubridate)

# ============================================
# ФУНКЦИЯ ЗАГРУЗКИ И ПРЕДОБРАБОТКИ ДАННЫХ
# ============================================

load_and_clean <- function(city) {
  
  # Формируем имя файла
  filename <- paste0(city, ".csv")
  
  # Читаем CSV
  df <- read.csv(filename)
  
  # Преобразуем Start.Time в формат даты/времени
  df$Start.Time <- ymd_hms(df$Start.Time)
  
  # Извлекаем месяц, день недели, час
  df$month <- month(df$Start.Time, label = TRUE, abbr = FALSE)
  df$day_of_week <- wday(df$Start.Time, label = TRUE, abbr = FALSE)
  df$hour <- hour(df$Start.Time)
  
  # Создаем колонку с маршрутом (старт -> финиш)
  df$route <- paste(df$Start.Station, "->", df$End.Station)
  
  # Добавляем колонку с названием города (пригодится для объединения)
  df$city <- city
  
  return(df)
}

# Проверяем на Чикаго
chi <- load_and_clean("chicago")
print("Чикаго загружен. Первые 3 строки:")
head(chi, 3)

# ============================================
# ФУНКЦИЯ ДЛЯ ВЫВОДА СТАТИСТИКИ ПО ГОРОДУ
# ============================================

explore_city <- function(df, city_name) {
  
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("ГОРОД:", city_name, "\n")
  cat(rep("=", 50), "\n\n")
  
  # ----- 1. ПОПУЛЯРНОЕ ВРЕМЯ -----
  cat("1. ПОПУЛЯРНОЕ ВРЕМЯ ПОЕЗДОК:\n")
  cat("   Самый частый месяц:        ", names(sort(table(df$month), decreasing = TRUE)[1]), "\n")
  cat("   Самый частый день недели:  ", names(sort(table(df$day_of_week), decreasing = TRUE)[1]), "\n")
  cat("   Самый частый час:           ", names(sort(table(df$hour), decreasing = TRUE)[1]), ":00\n\n")
  
  # ----- 2. ПОПУЛЯРНЫЕ СТАНЦИИ И МАРШРУТЫ -----
  cat("2. ПОПУЛЯРНЫЕ СТАНЦИИ И МАРШРУТЫ:\n")
  cat("   Самая частая станция старта:\n   ", names(sort(table(df$Start.Station), decreasing = TRUE)[1]), "\n\n")
  cat("   Самая частая станция финиша:\n   ", names(sort(table(df$End.Station), decreasing = TRUE)[1]), "\n\n")
  cat("   Самый частый маршрут:\n   ", names(sort(table(df$route), decreasing = TRUE)[1]), "\n\n")
  
  # ----- 3. ДЛИТЕЛЬНОСТЬ ПОЕЗДОК -----
  cat("3. ДЛИТЕЛЬНОСТЬ ПОЕЗДОК (в секундах):\n")
  cat("   Общее время:  ", sum(df$Trip.Duration), "\n")
  cat("   Среднее время:", round(mean(df$Trip.Duration), 2), "\n\n")
  
  # ----- 4. ИНФОРМАЦИЯ О ПОЛЬЗОВАТЕЛЯХ -----
  cat("4. ИНФОРМАЦИЯ О ПОЛЬЗОВАТЕЛЯХ:\n")
  cat("   Типы пользователей:\n")
  print(table(df$User.Type))
  
  # Проверка на наличие колонок Gender и Birth.Year (для Вашингтона их нет)
  if ("Gender" %in% colnames(df) & "Birth.Year" %in% colnames(df)) {
    cat("\n   Пол:\n")
    print(table(df$Gender))
    cat("\n   Год рождения:\n")
    cat("   Самый ранний: ", min(df$Birth.Year, na.rm = TRUE), "\n")
    cat("   Самый поздний: ", max(df$Birth.Year, na.rm = TRUE), "\n")
    cat("   Самый частый:  ", names(sort(table(df$Birth.Year), decreasing = TRUE)[1]), "\n")
  } else {
    cat("\n   (Для этого города нет данных о поле и годе рождения)\n")
  }
  
  cat("\n")
}

# ============================================
# БЛОК 3: ЗАГРУЗКА И АНАЛИЗ ВСЕХ ТРЁХ ГОРОДОВ
# ============================================

# Загружаем данные для каждого города
chi <- load_and_clean("chicago")
nyc <- load_and_clean("new-york-city")
was <- load_and_clean("washington")

# Выводим статистику по каждому городу
explore_city(chi, "CHICAGO")
explore_city(nyc, "NEW YORK CITY")
explore_city(was, "WASHINGTON")

# ============================================
# ГРАФИК 1: Распределение поездок по часам (Нью-Йорк)
# ============================================

ggplot(nyc, aes(x = hour)) +
  geom_bar(fill = "#2E86AB", color = "white", alpha = 0.8) +
  labs(title = "Распределение поездок по часам суток (Нью-Йорк)",
       subtitle = "Данные за первое полугодие 2017",
       x = "Час дня (0 = полночь, 23 = 11 вечера)",
       y = "Количество поездок") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme_minimal(base_size = 12)

# ============================================
# ГРАФИК 2: Типы пользователей по городам
# ============================================

# Объединяем данные
all_cities <- bind_rows(
  chi %>% mutate(City = "Chicago"),
  nyc %>% mutate(City = "New York"),
  was %>% mutate(City = "Washington")
)

ggplot(all_cities, aes(x = City, fill = User.Type)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Сравнение типов пользователей по городам",
       x = "Город",
       y = "Количество поездок",
       fill = "Тип пользователя") +
  scale_fill_manual(values = c("Subscriber" = "#F18F01", "Customer" = "#006E90")) +
  theme_minimal(base_size = 12)

# ============================================
# ГРАФИК 3: Сравнение длительности поездок по городам
# ============================================

# Переводим секунды в минуты для удобства
all_cities$duration_min <- all_cities$Trip.Duration / 60

ggplot(all_cities, aes(x = City, y = duration_min, fill = City)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray60", outlier.size = 0.5) +
  labs(title = "Сравнение длительности поездок по городам",
       x = "Город",
       y = "Длительность поездки (минуты)") +
  scale_fill_manual(values = c("Chicago" = "#C73E1D", 
                               "New York" = "#2E86AB", 
                               "Washington" = "#6A994E")) +
  coord_cartesian(ylim = c(0, 30)) +  # Ограничиваем Y для наглядности (основная масса поездок до 30 мин)
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")