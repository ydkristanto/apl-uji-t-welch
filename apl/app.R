# Paket ----
library(shiny)
library(tidyverse)

# UI ----
ui <- navbarPage(
  title = "Ketegaran dan Kuasa Uji t dan Welch",
  ## Tab eksplorasi ----
  tabPanel("Eksplorasi",
           sidebarLayout(
             ### Input ----
             sidebarPanel(
               wellPanel(
                 sliderInput("n_1", "Ukuran sampel 1:",
                             min = 5, max = 50, value = 10, step = 1),
                 sliderInput("n_2", "Ukuran sampel 2:",
                             min = 5, max = 50, value = 10, step = 1),
                 sliderInput("k_pasang", "Banyak pasangan sampel:",
                             min = 20, max = 1000, value = 100, step = 1)
               ),
               wellPanel(
                 sliderInput("mu_1", "Rerata populasi 1:",
                             min = 10, max = 100, value = 50),
                 sliderInput("sigma_1", "Simpangan baku populasi 1:",
                             min = 1, max = 20, value = 10),
                 hr(),
                 sliderInput("mu_2", "Rerata populasi 2:",
                             min = 10, max = 100, value = 50),
                 sliderInput("sigma_2", "Simpangan baku populasi 2:",
                             min = 1, max = 20, value = 10)
               ),
               wellPanel(
                 selectInput("alternatif", "Hipotesis alternatif:",
                             choices = c("Tidak sama dengan" = "tak_sama",
                                         "Lebih dari" = "lebih",
                                         "Kurang dari" = "kurang"),
                             selected = "tak_sama"),
                 hr(),
                 sliderInput("sig", "Tingkat signifikansi:",
                             min = .01,
                             max = .2,
                             step = .01,
                             value = .05)
               )
             ),
             ### Panel utama ----
             mainPanel(
               tabsetPanel(
                 tabPanel("Ketegaran dan Kuasa",
                          br(),
                          plotOutput("plot_dist_pop", height = "300px"),
                          textOutput("teks_dist_pop"),
                          br(),
                          plotOutput("plot_dist_stat"),
                          textOutput("teks_dist_stat"),
                          br(),
                          plotOutput("plot_df")
                          ),
                 tabPanel("Ringkasan")
               )
             )
           )
           ),
  ## Tab informasi ----
  tabPanel("Informasi",
           sidebarLayout(
             sidebarPanel(
               
             ),
             mainPanel(
               
             )
           )
           )
)


# Fungsi peladen ----
seed = as.numeric(Sys.Date())
server <- function(input, output) {
  # Mengatur pilihan hipotesis alternatif
  observe({
    mu_1 <- input$mu_1
    mu_2 <- input$mu_2
    
    if (mu_1 < mu_2) {
      updateSelectInput(getDefaultReactiveDomain(), "alternatif",
                        choices = c("Tidak sama dengan" = "tak_sama",
                                    "Kurang dari" = "kurang"),
                        selected = "tak_sama")
    } else if (mu_1 > mu_2) {
      updateSelectInput(getDefaultReactiveDomain(), "alternatif",
                        choices = c("Tidak sama dengan" = "tak_sama",
                                    "Lebih dari" = "lebih"),
                        selected = "tak_sama")
    } else {
      updateSelectInput(getDefaultReactiveDomain(), "alternatif",
                        choices = c("Tidak sama dengan" = "tak_sama",
                                    "Lebih dari" = "lebih",
                                    "Kurang dari" = "kurang"),
                        selected = "tak_sama")
    }
  })
  
  ## Fungsi set_sampel ----
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
  
  rep_set_sampel <- repeatable(set_sampel)
  
  ## Fungsi stat_sampel ----
  stat_sampel <- function(data, alternatif = "tak_sama") {
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
      mutate(df_klasik = n_1 + n_2 - 2,
             df_welch = (A + B)^2 / (A^2 / (n_1 - 1) + B^2 / (n_2 - 1)))
    data_stat_panjang <- data_stat %>% 
      pivot_longer(cols = c(df_klasik, df_welch),
                   names_to = "uji_t",
                   values_to = "df") %>% 
      mutate(uji_t = ifelse(uji_t == "df_klasik", "klasik", "welch")) %>% 
      mutate(stat_uji = ifelse(uji_t == "klasik",
                               selisih_mean /
                                 sqrt(var_gabung * (1 / n_1 + 1 / n_2)),
                               selisih_mean / 
                                 sqrt(var_1 / n_1 + var_2 / n_2)))
    
    if (alternatif == "tak_sama") {
      # Uji dua ekor
      data_stat_panjang <- data_stat_panjang %>% 
        mutate(p = 2 * pt(-abs(stat_uji), df = df, lower.tail = TRUE)) %>%
        select(id_sampel, n_1, n_2, selisih_mean, uji_t, df, stat_uji, p)
    } else if (alternatif == "lebih") {
      # Uji ekor kanan
      data_stat_panjang <- data_stat_panjang %>% 
        mutate(p = pt(stat_uji, df = df, lower.tail = FALSE)) %>%
        select(id_sampel, n_1, n_2, selisih_mean, uji_t, df, stat_uji, p)
    } else if (alternatif == "kurang") {
      # Uji ekor kiri
      data_stat_panjang <- data_stat_panjang %>% 
        mutate(p = pt(stat_uji, df = df, lower.tail = TRUE)) %>%
        select(id_sampel, n_1, n_2, selisih_mean, uji_t, df, stat_uji, p)
    }
    
    return(data_stat_panjang)
  }
  
  ## Fungsi komposisi ----
  komposisi_data_stat <- function(n1, n2, k, mu1, sigma1,
                                  mu2, sigma2, alternatif = "tak_sama") {
    
    data_sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2)
    data_stat <- stat_sampel(data_sampel, alternatif)
    
    return(data_stat)
  }
  
  ## Fungsi reaktif ----
  stat_set_sampel <- reactive({
    komposisi_data_stat(n1 = input$n_1, n2 = input$n_2, k = input$k_pasang,
                        mu1 = input$mu_1, sigma1 = input$sigma_1,
                        mu2 = input$mu_2, sigma2 = input$sigma_2,
                        alternatif = input$alternatif)
  })
  
  ## Plot distribusi populasi ----
  output$plot_dist_pop <- renderPlot({
    
    # Parameter populasi
    mu_1 <- input$mu_1
    sigma_1 <- input$sigma_1
    mu_2 <- input$mu_2
    sigma_2 <- input$sigma_2
    
    # Batas plot
    bts_ki <- min(mu_1 - 4 * sigma_1, mu_2 - 4 * sigma_2)
    bts_ka <- max(mu_1 + 4 * sigma_1, mu_2 + 4 * sigma_2)
    
    # Plot distribusi populasi
    ggplot(data = data.frame(x = c(bts_ki, bts_ka)), aes(x = x)) +
      stat_function(aes(fill = "Populasi 2"),
                    fun = dnorm, args = list(mean = mu_2, sd = sigma_2),
                    geom = "area", alpha = .3) +
      stat_function(fun = dnorm, args = list(mean = mu_2, sd = sigma_2),
                    geom = "line", linewidth = 1) +
      stat_function(aes(fill = "Populasi 1"),
                    fun = dnorm, args = list(mean = mu_1, sd = sigma_1),
                    geom = "area", alpha = .3) +
      stat_function(fun = dnorm, args = list(mean = mu_1, sd = sigma_1),
                    geom = "line", linewidth = 1) +
      theme_bw(base_size = 16) +
      scale_fill_brewer(palette = "Dark2",
                        name = "Distribusi") +
      theme(legend.position = "top",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(title = "Distribusi Populasi")
    
  })
  
  ## Teks distribusi populasi ----
  output$teks_dist_pop <- renderText({
    mu_1 <- input$mu_1
    sigma_1 <- input$sigma_1
    mu_2 <- input$mu_2
    sigma_2 <- input$sigma_2
    
    paste0("Gambar 1: Distribusi dua populasi yang semuanya berdistribusi normal. Populasi pertama memiliki rerata ", mu_1, " dan simpangan baku ", sigma_1, " sedangkan rerata dan simpangan baku populasi kedua secara berturut-turut adalah ", mu_1, " dan ", sigma_1, ".")
  })
  
  ## Plot distribusi t ----
  output$plot_dist_stat <- renderPlot({
    sig <- input$sig
    k <- input$k_pasang
    n_1 <- input$n_1
    n_2 <- input$n_2
    mu_1 <- input$mu_1
    sigma_1 <- input$sigma_1
    mu_2 <- input$mu_2
    sigma_2 <- input$sigma_2
    selisih_mu <- mu_1 - mu_2
    se <- sqrt(sigma_1^2 / n_1 + sigma_2^2 / n_2)
    
    dist_selisih_baku <- function(x) {
      dnorm(se * x, mean = selisih_mu, sd = se) * se
    }
    
    alfa_ruas <- function(x) {
      -1 / 1372000 * (x - 20)^2 + 1
    }
    
    data_stat <- stat_set_sampel() %>% 
      mutate(p_signif = p <= sig)
    data_df <- data_stat %>% 
      filter(uji_t == "welch") %>% 
      summarise(min = min(df),
                med = median(df),
                maks = max(df))
    
    # Plot distribusi
    if (mu_1 == mu_2) {
      data_stat %>% 
        ggplot(aes(color = factor(p_signif))) +
        geom_segment(aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf),
                     linewidth = 1, alpha = alfa_ruas(k)) +
        stat_function(fun = dt, args = list(df = n_1 + n_2 - 2),
                      linewidth = 1.5, color = "black",
                      data = subset(data_stat, uji_t == "klasik")) +
        stat_function(fun = dt, args = list(df = data_df$min[1]),
                      linewidth = 1, color = "black", alpha = .4,
                      data = subset(data_stat, uji_t == "welch")) +
        stat_function(fun = dt, args = list(df = data_df$med[1]),
                      linewidth = 1, color = "black", alpha = .8,
                      data = subset(data_stat, uji_t == "welch")) +
        stat_function(fun = dt, args = list(df = data_df$maks[1]),
                      linewidth = 1, color = "black", alpha = .4,
                      data = subset(data_stat, uji_t == "welch")) +
        ylim(0, .5) +
        facet_grid(uji_t ~ .) +
        theme_bw(base_size = 16) +
        scale_color_manual(name = "Menolak H_0",
                           labels = c("TRUE" = "Ya",
                                      "FALSE" = "Tidak"),
                           values = c("TRUE" = "#d95f02",
                                      "FALSE" = "#1b9e77")) +
        theme(legend.position = "top",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Distribusi Statistik t",
             x = "t")
    } else {
      data_stat %>% 
        ggplot(aes(color = factor(p_signif))) +
        geom_segment(aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf),
                     linewidth = 1, alpha = alfa_ruas(k)) +
        stat_function(fun = dt, args = list(df = n_1 + n_2 - 2),
                      linewidth = 1.5, color = "black",
                      data = subset(data_stat, uji_t == "klasik")) +
        stat_function(fun = dt, args = list(df = data_df$min[1]),
                      linewidth = 1, color = "black", alpha = .4,
                      data = subset(data_stat, uji_t == "welch")) +
        stat_function(fun = dt, args = list(df = data_df$med[1]),
                      linewidth = 1, color = "black", alpha = .8,
                      data = subset(data_stat, uji_t == "welch")) +
        stat_function(fun = dt, args = list(df = data_df$maks[1]),
                      linewidth = 1, color = "black", alpha = .4,
                      data = subset(data_stat, uji_t == "welch")) +
        stat_function(fun = dist_selisih_baku,
                      linewidth = 1.5,
                      color = "blue") +
        ylim(0, .5) +
        facet_grid(uji_t ~ .) +
        theme_bw(base_size = 16) +
        scale_color_manual(name = "Menolak H_0",
                           labels = c("TRUE" = "Ya",
                                      "FALSE" = "Tidak"),
                           values = c("TRUE" = "#d95f02",
                                      "FALSE" = "#1b9e77")) +
        theme(legend.position = "top",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Distribusi Statistik t",
             x = "t")
    }
    
    
  })
  
  ## Teks distribusi t ----
  output$teks_dist_stat <- renderText({
    sig <- input$sig
    data_stat <- stat_set_sampel() %>% 
      mutate(p_signif = p <= sig)
    stat_signif <- data_stat %>% 
      group_by(uji_t) %>% 
      summarise(persen_menolak = mean(p_signif) * 100)
    persen_klasik <- round(stat_signif$persen_menolak[1], 2)
    persen_welch <- round(stat_signif$persen_menolak[2], 2)
    
    paste0("Gambar 2: Statistik t dari selisih rerata setiap pasangan sampel yang dipilih secara acak dari populasi pertama dan kadua. Dengan tingkat signifikansi ", sig, ", persentase penolakan hipotesis nol ketika menggunakan uji t klasik adalah ", persen_klasik, "%. Ketika menggunakan uji Welch, persentasenya adalah ", persen_welch, "%.")
    
  })
  
}

# Membuat objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)
