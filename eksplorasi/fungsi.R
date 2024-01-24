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
  vektor_nilai <- c(matriks_nilai)
  data <- tibble(
    id_sampel = rep(1:k, each = n1 + n2),
    populasi = rep(c(rep(1, n1), rep(2, n2)), k),
    nilai = vektor_nilai
    )
}

# Uji fungsi set_sampel
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

stat_sampel1 <- set_sampel1 %>% 
  group_by(id_sampel, populasi) %>% 
  summarise(n = n(),
            mean = mean(nilai),
            var = var(nilai),
            .groups = "drop")
stat_sampel1_lebar <- stat_sampel1 %>% 
  pivot_wider(names_from = populasi, values_from = c(n, mean, var))
stat_sampel1_inf <- stat_sampel1_lebar %>% 
  mutate(selisih_mean = mean_1 - mean_2)
