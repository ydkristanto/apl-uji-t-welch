# Paket ----
library(shiny)
library(tidyverse)
library(scales)

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
                             min = 45, max = 55, value = 50),
                 sliderInput("sigma_1", "Simpangan baku populasi 1:",
                             min = 1, max = 20, value = 10),
                 hr(),
                 sliderInput("mu_2", "Rerata populasi 2:",
                             min = 45, max = 55, value = 50),
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
                 #### Populasi ----
                 tabPanel("Populasi",
                          br(),
                          plotOutput("plot_dist_pop", height = "480px"),
                          textOutput("teks_dist_pop"),
                          br()
                 ),
                 #### Beberapa sampel ----
                 tabPanel("Beberapa Sampel",
                          br(),
                          fluidRow(
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_1",
                                              height = "300px")),
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_2",
                                              height = "300px")),
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_3",
                                              height = "300px"))
                          ),
                          fluidRow(
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_4",
                                              height = "300px")),
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_5",
                                              height = "300px")),
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_6",
                                              height = "300px"))
                          ),
                          fluidRow(
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_7",
                                              height = "300px")),
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_8",
                                              height = "300px")),
                            column(4,
                                   br(),
                                   plotOutput("plot_pasangan_9",
                                              height = "300px"))
                          ),
                          br(),
                          div(textOutput("teks_pasangan"),
                              align = "center",
                              style = "font-size: 1.1em"),
                          br(),
                          textOutput("teks_gambar_pasangan"),
                          br(),
                 ),
                 #### Distribusi sampling ----
                 tabPanel("Distribusi Sampling",
                          br(),
                          plotOutput("plot_dist_selisih_rerata"),
                          textOutput("teks_dist_selisih_rerata"),
                          br()
                          ),
                 #### Uji t klasik dan Welch ----
                 tabPanel("Ketegaran dan Kuasa",
                          br(),
                          conditionalPanel(condition = "input.alternatif == 'tak_sama'",
                           div(p("Hipotesis Nol dan Alternatifnya",
                                 style = "font-weight: bold;"),
                               p("Hipotesis nol dan hipotesis alternatif dalam uji ini adalah sebagai berikut."),
                               tags$ul(
                                 tags$li("H_0: Rerata kedua populasi sama. Hal ini dapat disimbolkan dengan μ1 = μ2 atau μ1 - μ2 = 0."),
                                 tags$li("H_A: Rerata kedua populasi tersebut tidak sama, yaitu μ1 ≠ μ2 atau μ1 - μ2 ≠ 0.")
                                 ), style = "padding: 10px; background-color: whitesmoke;")
                                           ),
                          conditionalPanel(condition = "input.alternatif == 'lebih'",
                           div(p("Hipotesis Nol dan Alternatifnya",
                                 style = "font-weight: bold;"),
                               p("Hipotesis nol dan hipotesis alternatif dalam uji ini adalah sebagai berikut."),
                               tags$ul(
                                 tags$li("H_0: Rerata kedua populasi sama. Hal ini dapat disimbolkan dengan μ1 = μ2 atau μ1 - μ2 = 0."),
                                 tags$li("H_A: Rerata populasi pertama lebih dari populasi kedua, yaitu μ1 > μ2 atau μ1 - μ2 > 0.")
                               ), style = "padding: 10px; background-color: whitesmoke;")
                          ),
                          conditionalPanel(condition = "input.alternatif == 'kurang'",
                           div(p("Hipotesis Nol dan Alternatifnya",
                                 style = "font-weight: bold;"),
                               p("Hipotesis nol dan hipotesis alternatif dalam uji ini adalah sebagai berikut."),
                               tags$ul(
                                 tags$li("H_0: Rerata kedua populasi sama. Hal ini dapat disimbolkan dengan μ1 = μ2 atau μ1 - μ2 = 0."),
                                 tags$li("H_A: Rerata populasi pertama kurang dari populasi kedua, yaitu μ1 < μ2 atau μ1 - μ2 < 0.")
                               ), style = "padding: 10px; background-color: whitesmoke;")
                          ),
                          br(),
                          plotOutput("plot_dist_stat"),
                          textOutput("teks_dist_stat"),
                          br(),
                          plotOutput("plot_perbandingan"),
                          textOutput("teks_perbandingan"),
                          hr(),
                          helpText(textOutput("teks_info_perbandingan")),
                          br()
                 )
                 )
             )
           )
           ),
  ## Tab informasi ----
  tabPanel("Informasi",
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                 div(h4("Deskripsi",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Aplikasi Shiny ini dapat digunakan untuk membandingkan ketegaran dan kuasa uji t klasik dan Welch."))
               ),
               wellPanel(
                 div(h4("Kode sumber",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-uji-t-welch", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-uji-t-welch/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-uji-t-welch/pulls", target = "_blank"), "di repositori tersebut."))
               ),
               wellPanel(
                 div(h4("Lisensi",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Lisensi MIT"),
                     p("Copyright (c) 2024 Yosep Dwi Kristanto"))
               )
             ),
             mainPanel(
               fluidRow(
                 column(9,
                        div(h3("Aplikasi Shiny Ketegaran dan Kuasa Uji t Klasik dan Welch")),
                        div(p("Aplikasi interaktif ini dapat digunakan untuk membandingkan dua jenis uji t dua sampel, yaitu uji t klasik dan uji t Welch. Kedua jenis uji t tersebut memiliki perbedaan dalam hal asumsi-asumsi statistiknya. Uji t klasik mengasumsikan homogenitas variansi sedangkan uji t Welch tidak."), align = "justify"),
                        div(p("Aplikasi ini menggunakan dua ukuran dalam membandingkan kedua uji t tersebut. Kedua ukuran tersebut adalah ketegaran dan kuasa ujinya. Ketegaran dapat dilihat pada galat jenis I. Kuasa uji dapat diselidiki ketika hipotesis nolnya salah. Hipotesis nol yang digunakan dalam aplikasi ini adalah bahwa kedua populasinya memiliki rerata yang sama."), align = "justify"),
                        hr(),
                        div(p("Aplikasi interaktif ini dikembangkan dengan menggunakan bahasa pemrogram", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah."), align = "justify"),
                        div(p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta."), align = "justify")
                 )
                 # di sini
               )
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
    set.seed(seed)
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
  
  ## Fungsi memilih pasangan acak ----
  pilih_pasangan <- function(n, k) {
    set.seed(seed)
    id_pasangan <- sort(sample(1:k, n, replace = FALSE))
    return(id_pasangan)
  }
  
  rep_pilih_pasangan <- repeatable(pilih_pasangan)
  
  reak_pilih_pasangan <- reactive({
    pilihan <- rep_pilih_pasangan(n = 9, k = input$k_pasang)
    return(pilihan)
  })
  
  ## Plot distribusi populasi ----
  output$plot_dist_pop <- renderPlot({
    
    # Parameter populasi
    mu_1 <- round(input$mu_1, 2)
    sigma_1 <- round(input$sigma_1, 2)
    mu_2 <- round(input$mu_2, 2)
    sigma_2 <- round(input$sigma_2, 2)
    
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
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(title = "Distribusi Populasi",
           subtitle = paste0("Distribusi dua populasi N(", mu_1, ", ", sigma_1, "), dan N(", mu_2, ", ", sigma_2, ")."),
           x = "Nilai")
    
  })
  
  ## Teks distribusi populasi ----
  output$teks_dist_pop <- renderText({
    mu_1 <- input$mu_1
    sigma_1 <- input$sigma_1
    mu_2 <- input$mu_2
    sigma_2 <- input$sigma_2
    
    paste0("Gambar 1: Distribusi dua populasi yang semuanya berdistribusi normal. Populasi pertama memiliki rerata ", mu_1, " dan simpangan baku ", sigma_1, " sedangkan rerata dan simpangan baku populasi kedua secara berturut-turut adalah ", mu_2, " dan ", sigma_2, ".")
  })
  
  ## Plot pasangan sampel 1 ----
  output$plot_pasangan_1 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[1])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                                  data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 2 ----
  output$plot_pasangan_2 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[2])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                                  data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 3 ----
  output$plot_pasangan_3 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[3])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                                  data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 4 ----
  output$plot_pasangan_4 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[4])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                                  data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 5 ----
  output$plot_pasangan_5 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[5])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                                  data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 6 ----
  output$plot_pasangan_6 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[5])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                                  data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 7 ----
  output$plot_pasangan_7 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[7])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                              data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 8 ----
  output$plot_pasangan_8 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[8])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                              data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Plot pasangan sampel 9 ----
  output$plot_pasangan_9 <- renderPlot({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel == pilihan[9])
    data_rerata <- sampel %>% 
      group_by(populasi) %>% 
      summarise(mean = mean(nilai))
    selisih_rerata <- round(data_rerata$mean[1] - 
                              data_rerata$mean[2], 2)
    
    sampel %>% 
      ggplot(aes(y = nilai, x = factor(populasi),
                 color = factor(populasi))) +
      geom_violin() +
      geom_crossbar(stat = "summary", color = "black") +
      geom_point(position = position_jitter(width = .2),
                 size = 4, alpha = .6) +
      stat_summary(fun = mean, color = "darkred", size = 1) +
      scale_color_brewer(palette = "Dark2",
                         name = "Sampel") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "Sampel", y = "Nilai",
           subtitle = paste0("Selisih rerata = ", selisih_rerata))
  })
  
  ## Teks pasangan sampel ----
  output$teks_pasangan <- renderText({
    k <- input$k_pasang
    paste0("... dan seterusnya sampai sampel ke-", k,".")
  })
  
  ## Teks gambar pasangan sampel ----
  output$teks_gambar_pasangan <- renderText({
    n1 <- input$n_1
    n2 <- input$n_2
    k <- input$k_pasang
    mu1 <- input$mu_1
    sigma1 <- input$sigma_1
    mu2 <- input$mu_2
    sigma2 <- input$sigma_2
    pilihan <- reak_pilih_pasangan()
    
    sampel <- rep_set_sampel(n1, n2, k, mu1, sigma1, mu2, sigma2) %>% 
      filter(id_sampel %in% pilihan)
    data_rerata <- sampel %>% 
      group_by(id_sampel, populasi) %>% 
      summarise(mean = mean(nilai)) %>% 
      summarise(selisih = round(-diff(mean), 2), .groups = "drop")
    vektor_rerata <- c(data_rerata$selisih)
    
    paste0("Gambar 2: Distribusi nilai sembilan dari ", k, " pasang sampel yang disertai dengan selisih reratanya. Selisih rerata tersebut secara berturut-turut adalah ", 
           vektor_rerata[1], ", ",
           vektor_rerata[2], ", ",
           vektor_rerata[3], ", ",
           vektor_rerata[4], ", ",
           vektor_rerata[5], ", ",
           vektor_rerata[6], ", ",
           vektor_rerata[7], ", ",
           vektor_rerata[8], ", dan ",
           vektor_rerata[9], "."
           )
    
  })
  
  ## Plot distribusi sampling selisih rerata ----
  output$plot_dist_selisih_rerata <- renderPlot({
    # Memanggil nilai input
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
    
    # Membuat data
    data_stat <- stat_set_sampel() %>% 
      filter(uji_t == "klasik")
    
    data_stat %>% 
      ggplot(aes(x = selisih_mean)) +
      geom_histogram(aes(y = after_stat(density)),
                     color = "white", alpha = .8) +
      stat_function(fun = dnorm, args = list(mean = selisih_mu,
                                             sd = se),
                    linewidth = 1.5) +
      theme_bw(base_size = 16) +
      theme(plot.title = element_text(face = "bold")) +
      labs(title = "Distribusi Sampling Selisih Rerata",
           x = "Selisih rerata")
    
  })
  
  ## Teks distribusi sampling selisih rerata ----
  output$teks_dist_selisih_rerata <- renderText({
    k <- input$k_pasang
    data_stat <- stat_set_sampel() %>% 
      filter(uji_t == "klasik")
    ringkasan <- data_stat %>% 
      summarise(n = n(),
                mean = mean(selisih_mean),
                sd = sd(selisih_mean))
    
    paste0("Gambar 3: Distribusi selisih rerata dari ", k, " pasang sampel. Selisih rerata tersebut kurang lebih berdistribusi normal dengan rerata ", round(ringkasan$mean, 2), " dan simpangan baku ", round(ringkasan$sd, 2), ".")
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
      1 / 1372000 * (x - 1000)^2 + 3 / 10
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
        geom_point(aes(x = stat_uji, y = 0), alpha = alfa_ruas(k),
                   size = 3, position = position_jitter(height = .03)) +
        ylim(-.1, .5) +
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
        geom_point(aes(x = stat_uji, y = 0), alpha = alfa_ruas(k),
                   size = 3, position = position_jitter(height = .03)) +
        ylim(-.1, .5) +
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
    
    paste0("Gambar 4.a: Statistik t dari selisih rerata setiap pasangan sampel yang dipilih secara acak dari populasi pertama dan kedua. Dengan tingkat signifikansi ", sig, ", persentase penolakan hipotesis nol ketika menggunakan uji t klasik adalah ", persen_klasik, "%. Ketika menggunakan uji Welch, persentasenya adalah ", persen_welch, "%.")
    
  })
  
  ## Plot perbandingan ----
  output$plot_perbandingan <- renderPlot({
    
    sig <- input$sig
    data_stat <- stat_set_sampel() %>% 
      mutate(p_signif = p <= sig)
    ringkasan <- data_stat %>% 
      group_by(uji_t) %>% 
      summarise(signif = mean(p_signif)) %>% 
      mutate(persen_signif = paste0(round(signif * 100, 2), "%"))
    
    x_maks <- max(ringkasan$signif, sig)
    x_min <- min(ringkasan$signif, sig)
    x_range <- x_maks - x_min
    plot_range <- c(x_min - x_range / 4,
                    x_maks + x_range / 4)
    
    slsh_persen <- round(abs(ringkasan$signif[1] - 
                               ringkasan$signif[2]) * 100, 2)
    pos_x <- min(ringkasan$signif[1], ringkasan$signif[2]) + 
      slsh_persen / 200
    
    ggplot(ringkasan) +
      geom_segment(aes(x = signif, xend = sig,
                       y = uji_t, yend = uji_t,
                       col = uji_t),
                   show.legend = FALSE, linewidth = 5,
                   alpha = .6) +
      geom_segment(aes(x = sig, xend = sig,
                       y = 0, yend = Inf),
                   linewidth = 3, alpha = .6) +
      geom_segment(aes(x = signif[1], xend = signif[2],
                       y = factor(1), yend = factor(1)),
                   show.legend = FALSE, linewidth = 5, color = "#7570b3",
                   alpha = .2) +
      geom_point(aes(x = signif, y = factor(1)),
                 size = 8, color = "#7570b3") +
      geom_point(aes(x = signif,
                     y = uji_t,
                     col = uji_t),
                 size = 8) +
      geom_label(aes(x = signif, y = uji_t,
                     label = persen_signif, col = uji_t),
                 fill = "white", fontface = "bold", size = 5,
                 nudge_y = .25, label.r = unit(0.1, "lines"),
                 show.legend = FALSE) +
      geom_label(aes(x = pos_x, y = factor(1),
                     label = paste0(slsh_persen, "%")),
                 color = "#7570b3", fill = "white", fontface = "bold",
                 size = 5, label.r = unit(0.1, "lines"),
                 show.legend = FALSE) +
      scale_x_continuous(labels = label_percent(scale = 100),
                         limits = plot_range) +
      scale_color_brewer(palette = "Dark2", name = "Distribusi sampling") +
      theme_bw(base_size = 16) +
      theme(legend.position = "top",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(title = "Selisih Persentase Penolakan Hipotesis Nol",
           x = "Persentase")
  })
  
  
  ## Teks perbandingan ----
  output$teks_perbandingan <- renderText({
    mu_1 <- input$mu_1
    mu_2 <- input$mu_2
    sig <- input$sig
    data_stat <- stat_set_sampel() %>% 
      mutate(p_signif = p <= sig)
    ringkasan <- data_stat %>% 
      group_by(uji_t) %>% 
      summarise(signif = mean(p_signif)) %>% 
      mutate(persen_signif = paste0(round(signif * 100, 2), "%"))
    
    slsh_persen <- round(abs(ringkasan$signif[1] - 
                               ringkasan$signif[2]) * 100, 2)
    
    if (mu_1 == mu_2) {
      if (ringkasan$signif[1] == ringkasan$signif[2]) {
        "Gambar 4.b: Selisih persentase penolakan hipotesis nol dari uji t klasik dan uji t Welch adalah 0%. Kedua uji ini memiliki persentase galat tipe I* yang sama."
      } else if (ringkasan$signif[1] < ringkasan$signif[2]) {
        paste0("Gambar 4.b: Persentase penolakan hipotesis nol dari uji t klasik ", slsh_persen, "% kurang dari uji t Welch. Dengan demikian, dalam simulasi ini uji t klasik lebih baik dibandingkan uji t Welch karena memiliki galat tipe I* yang lebih kecil.")
      } else {
        paste0("Gambar 4.b: Persentase penolakan hipotesis nol dari uji t Welch ", slsh_persen, "% lebih kecil dari uji t klasik. Dengan demikian, dalam simulasi ini uji t Welch lebih baik dibandingkan uji t klasik karena memiliki galat tipe I* yang lebih kecil.")
      }
    } else {
      if (ringkasan$signif[1] == ringkasan$signif[2]) {
        "Gambar 4.b: Selisih persentase penolakan hipotesis nol dari uji t klasik dan uji t Welch adalah 0%. Kedua uji ini memiliki kuasa* yang sama."
      } else if (ringkasan$signif[1] < ringkasan$signif[2]) {
        paste0("Gambar 4.b: Persentase penolakan hipotesis nol dari uji t Welch ", slsh_persen, "% lebih dari uji t klasik. Dengan demikian, dalam simulasi ini uji t Welch lebih baik dibandingkan uji t klasik karena memiliki kuasa* yang lebih besar.")
      } else {
        paste0("Gambar 4.b: Persentase penolakan hipotesis nol dari uji t klasik ", slsh_persen, "% lebih dari uji t Welch. Dengan demikian, dalam simulasi ini uji t klasik lebih baik dibandingkan uji t Welch karena memiliki kuasa* yang lebih besar.")
      }
    }
    
  })
  
  ## Teks informasi tambahan perbandingan ----
  output$teks_info_perbandingan <- renderText({
    mu_1 <- input$mu_1
    mu_2 <- input$mu_2
    if (mu_1 == mu_2) {
      "*Galat tipe I merupakan kekeliruan karena menolak hipotesis nol yang sebenarnya hipotesis tersebut benar."
    } else {
      "*Kuasa merupakan ketepatan dalam menolak hipotesis nol karena hipotesis tersebut sebenarnya memang salah."
    }
  })
  
}

# Membuat objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)
