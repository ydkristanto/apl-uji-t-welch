library(tidyverse)

# Fungsi set_sampel ----

#' Membuat sampel-sampel berukuran n1 dan n1 dari populasi N(mu1, sigma1)
#' dan N(mu2, sigma2) sebanyak k
n1 <- 10
n2 <- 15
mu1 <- 50
sigma1 <- 10
mu2 <- 30
sigma2 <- 10
k <- 20

matriks <- replicate(k, c(rnorm(n1, mean = mu1, sd = sigma1),
               rnorm(n2, mean = mu2, sd = sigma2)))
vektor <- c(matriks)
data <- tibble(
  id_sampel = rep(1:k, each = n1 + n2),
  populasi = rep(c(rep(1, n1), rep(2, n2)), k),
  nilai = vektor
)
print(data, n = 51)

# Membuat fungsi set_sampel
set_sampel <- function(n1, n2, k, mu1, sigma1, mu2, sigma2) {
  # Membuat nilai-nilai sampel
  matriks_nilai <- replicate(k, c(rnorm(n1, mean = mu1, sd = sigma1),
                                  rnorm(n2, mean = mu2, sd = sigma2)))
  vektor_nilai <- round(c(matriks_nilai), 1)
  # Membuat set sampel
  data <- tibble(
    id_sampel = rep(1:k, each = n1 + n2),
    populasi = rep(c(rep(1, n1), rep(2, n2)), k),
    nilai = vektor_nilai
    )
  
  return(data)
}

# Mengujicoba fungsi set_sampel
set_sampel1 <- set_sampel(8, 10, 100, 10, 2, 20, 3)
set_sampel1 %>% 
  group_by(id_sampel) %>% 
  summarise(n = n())
set_sampel1 %>% 
  group_by(id_sampel, populasi) %>% 
  summarise(n = n(),
            mean = mean(nilai),
            sd = sd(nilai))


# Fungsi stat_sampel ----

# Merangkum data set sampel
stat_sampel1 <- set_sampel1 %>% 
  group_by(id_sampel, populasi) %>% 
  summarise(n = n(),
            mean = mean(nilai),
            var = var(nilai),
            .groups = "drop")

# Membuat data lebar
stat_sampel1_lebar <- stat_sampel1 %>% 
  pivot_wider(names_from = populasi, values_from = c(n, mean, var))

# Menghitung statistik-statistik untuk uji t dan Welch
stat_sampel1_inf <- stat_sampel1_lebar %>% 
  mutate(selisih_mean = mean_1 - mean_2,
         var_gabung = ((n1 - 1) * var_1 + (n2 - 1) * var_2) / (n1 + n2 - 2),
         A = var_1 / n_1,
         B = var_2 / n_2) %>% 
  mutate(df_t = n_1 + n_2 - 2,
         df_w = (A + B)^2 / (A^2 / (n_1 - 1) + B^2 / (n_2 - 1)),
         stat_t = selisih_mean / sqrt(var_gabung * (1 / n_1 + 1 / n_2)),
         stat_t_w = selisih_mean / sqrt(var_1 / n_1 + var_2 / n_2)) %>% 
  # Uji dua ekor
  mutate(p_t = 2 * pt(stat_t, df = df_t, lower.tail = TRUE),
         p_t_w = 2 * pt(stat_t, df = df_w, lower.tail = TRUE))

#' Membuat fungsi stat_sampel, inputnya adalah sebuah tibble yang memiliki
#' variabel id_sampel, populasi, dan nilai
stat_sampel <- function(data) {
  rangkuman <- data %>% 
    group_by(id_sampel, populasi) %>% 
    summarise(n = n(),
              mean = mean(nilai),
              var = var(nilai),
              .groups = "drop")
  rangkuman_lebar <- rangkuman %>% 
    pivot_wider(names_from = populasi, values_from = c(n, mean, var))
  data_stat <- rangkuman_lebar %>% 
    mutate(selisih_mean = mean_1 - mean_2,
           var_gabung = ((n_1 - 1) * var_1 + (n_2 - 1) * var_2) / 
             (n_1 + n_2 - 2),
           A = var_1 / n_1,
           B = var_2 / n_2) %>% 
    mutate(df_t = n_1 + n_2 - 2,
           df_w = (A + B)^2 / (A^2 / (n_1 - 1) + B^2 / (n_2 - 1)),
           stat_t = selisih_mean / sqrt(var_gabung * (1 / n_1 + 1 / n_2)),
           stat_t_w = selisih_mean / sqrt(var_1 / n_1 + var_2 / n_2)) %>% 
    # Uji dua ekor
    mutate(p_t = 2 * pt(-abs(stat_t), df = df_t, lower.tail = TRUE),
           p_t_w = 2 * pt(-abs(stat_t_w), df = df_w, lower.tail = TRUE)) %>% 
    select(id_sampel, n_1, n_2, selisih_mean, df_t, df_w,
           stat_t, stat_t_w, p_t, p_t_w)
  
  return(data_stat)
}

# Mengujicoba fungsi stat_sampel
stat_sampel(set_sampel1)


# Fungsi komposisi ----
data_stat <- function(n1, n2, k, mu1, sigma1, mu2, sigma2) {
  set_sampel <- set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2)
  data_stat <- stat_sampel(set_sampel)
  return(data_stat)
}

# Fungsi baru ----


