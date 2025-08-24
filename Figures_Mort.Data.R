
N.of.Deaths <- readHMDweb("DNK", "Deaths_1x1", "ozerbakar@hacettepe.edu.tr", 
                          "Ozer19871905*", fixup = TRUE)
Exposures <- readHMDweb("DNK", "Exposures_1x1", "ozerbakar@hacettepe.edu.tr", 
                        "Ozer19871905*", fixup = TRUE)
Death.rate <- readHMDweb("DNK", "Mx_1x1", "ozerbakar@hacettepe.edu.tr", 
                         "Ozer19871905*", fixup = TRUE)

N.of.Deaths.2024 <- N.of.Deaths %>% 
  filter(Year == 2024) %>%
  select(Age, Female)

Exposures.2024 <- Exposures %>% 
  filter(Year == 2024) %>%
  select(Age, Female)

Death.rate.1950.2024 <- Death.rate %>% 
  select(Year, Age, Female) %>%
  filter(Female != 0) %>% 
  mutate(Female = log10(Female)) %>%
  filter(Year > 1949) 

Deaths.plot <- ggplot(N.of.Deaths.2024, aes(x = as.factor(Age), y = Female)) + 
  geom_bar(stat = "identity", fill = "skyblue", color = "black") + 
  labs(title = "Deaths",
       x = "Age",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_discrete(breaks = c("0", "20", "40", "60", "80", "100", "110"))

Exposures.plot <- ggplot(Exposures.2024, aes(x = as.factor(Age), y = Female)) + 
  geom_bar(stat = "identity", fill = "maroon3") + 
  labs(title = "Exposures",
       x = "Age",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_discrete(breaks = c("0", "20", "40", "60", "80", "100", "110"))

Death_rates.plot <- ggplot(Death.rate.1950.2024, aes(x = as.factor(Age), y = Female, color = Year, group = Year)) +
  geom_line() +
  scale_colour_viridis_c(option = "viridis") +
  labs(title = "Death Rates",
       x = "Age", y = "", color = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_discrete(breaks = c("0", "20", "40", "60", "80", "100", "110"))

Combined.plot <- Deaths.plot + Exposures.plot
print(Combined.plot)
print(Death_rates.plot)
