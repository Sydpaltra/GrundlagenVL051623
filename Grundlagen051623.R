library(tidyverse)
library(scales)
library(lubridate)
library(ggokabeito)

# Import ---------------------------------------------------
# Reminder: # benutzen wir, wenn wir Kommentare schreiben (Kommentare werden vom Compiler ignoriert)
# Reminder : <- benutzen wir, um Variablen/Vektoren/Dataframes zu deklarieren.

trips_data <- read_delim("https://github.com/Sydpaltra/GrundlagenVL051623/raw/main/kelheim-v2.0-25pct-av.output_trips.csv.gz", delim = ";")

trips_data <- data.frame(trips_data)

# Wir benutzen `glimpse()` um uns einen ersten Überblick über die Daten zu verschaffen.

glimpse(trips_data)

# Wir benutzen `head()`, um uns die ersten Zeilen eines Dataframes anzuschauen.

head(trips_data)

# Wir benutzen `tail()`, um uns die letzten Zeilen eines Dataframes anzuschauen.

tail(trips_data)

# Wir benutzen sample_n(), um uns 6 beliebige Zeilen ausgeben zu lassen.

sample_n(trips_data, size = 6)


# TIDY --------------------------------------------------------------------
# Nutze `colnames()`, um die Spaltennamen des Dataframes anzusehen.

colnames(trips_data)

# Mithilfe von `pivot_longer()`transformieren wir unser Dataframe in das tidy Format.

trips_data_tidy <- trips_data %>% select(trip_id, dep_time, trav_time, wait_time) %>% 
  pivot_longer(cols = c("dep_time", "trav_time", "wait_time"), values_to = "Duration", names_to = "Considered_Time")

#Erneutes nutzen von head(), um uns die ersten Zeilen anzusehen

head(trips_data_tidy)

# TRANSFORM ---------------------------------------------------------------
## select()
# Wir wählen nur die Spalten aus, für die wir uns interessieren
# Reminder : %>% ist der Pipe-Operator. Wird verwendet, um eine Sequenz von Operationen miteinander zu verknüpfen
trips_data <- trips_data %>% select("person", "dep_time", "trav_time", "wait_time", "traveled_distance", "main_mode", "longest_distance_mode", "modes")

##mutate()
## Fortgeschrittenes Beispiel: Wir verwandeln die verschiedenen Zeiten-Spalten in sogenannte date-time Objects.

#Transformieren der verschiedenen Zeiten
#Ersten 3 Zeilen: Runden der Zeiten auf 5 Minuten
#Nächsten 3 Zeilen: Transformation zu sog. "date-time" Objekten
#Letzten 2 Zeilen: Konverte "date-time" Objekte in Sekunden
trips_data <- trips_data %>% mutate(dep_time_round = hms::round_hms(hms::as_hms(dep_time), secs = 300)) %>%
               mutate(trav_time_round = hms::round_hms(hms::as_hms(trav_time), secs = 300)) %>%
               mutate(wait_time_round = hms::round_hms(hms::as_hms(wait_time), secs = 300)) %>%
               mutate(dep_time = hms(as.character(dep_time))) %>%
               mutate(trav_time = hms(as.character(trav_time))) %>%
               mutate(wait_time = hms(as.character(wait_time))) %>%
               mutate(dep_time_seconds = period_to_seconds(dep_time)) %>%
               mutate(wait_time_seconds = period_to_seconds(wait_time))

#Nutze head(), um die ersten Zeilen anzuschauen
head(trips_data$wait_time_round)


## Einfaches Beispiel: Wir nutzen mutate(), um eine Spalte für die Ankunftszeit zu unserem Dataframe hinzuzufügen.
trips_data %>% mutate(arrival_time = dep_time + wait_time + trav_time)

## arrange()
#Von Interesse sind beispielsweise sehr kurze und auch sehr lange Wartezeiten. Um uns einen Überblick über die kürzesten und längsten Wartezeiten zu verschaffen, nutzen wir `arrange()`.
trips_data %>% arrange(dep_time)

#Reminder : `desc()` nutzen wir, um die ausgewählte Spalte in absteigender Reihenfolge zu sortieren.
trips_data %>% arrange(desc(dep_time))

#Mit `slice()` schneiden wir die angegebenen Zeilen heraus.
trips_data %>% slice(5:10)


## Statistiken
sum(trips_data$wait_time_seconds) #Wie lang ist die Wartezeit insgesamt?
mean(trips_data$wait_time_seconds) #Was ist der Mittelwert der Variable "wait_time_seconds"?
median(trips_data$wait_time_seconds) #Was ist der Median der Variable "wait_time_seconds"? Unterscheidung Median/Mean?

max(trips_data$wait_time_seconds) #Findet das erste(!) Maxium der Spalte
which.max(trips_data$wait_time_seconds) #Findet die Position des ersten(!) Maximums der Spalte

min(trips_data$wait_time_seconds) #Findet das erste(!) Minimum der Spalte
which.min(trips_data$wait_time_seconds) #Findet die Position des ersten(!) Minimums der Spalte



# VISUALISE ---------------------------------------------------------------

## Lineplot
# Der folgende Code-Block erstellt einen Lineplot, mit dem wir die Frage beantworten wollen: "Wie verteilen sich die Trips über den Tag?"
# Reminder : `+` nutzen wir, um unserem Ggplot weitere Ebenen hinzuzufügen.
trips_data %>% group_by(dep_time_round) %>% count() %>% #Mit count() zählen wir die Anzahl der Trips
ggplot(aes(x = dep_time_round, y = n)) +
  geom_line() + 
  xlab("Depature Time") + #Beschriftung der x-Achse
  ylab("Anzahl Trips") + #Beschriftung der y-Achse
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) #Unsere Legende soll sich unten befinden und keinen Titel haben

## Histogramm
#1.  Andere Darstellung der Frage: "Wie verteilen sich die Trips über den Tag?"
ggplot(trips_data, aes(x=dep_time_seconds)) +
  geom_histogram() +
  theme_minimal() +
  xlab("Departure time in Sekunden") + #Beschriftung der x-Achse
  ylab("Anzahl") + #Beschriftung der y-Achse
  scale_x_continuous(labels=comma)

# 2. Darstellung der Verteilung der Wartezeit (in Sekunden) mithilfe eines Histograms
trips_data %>% 
  ggplot(aes(x = wait_time_seconds)) +
  geom_histogram() +
  theme_minimal() +
  ylab("abs. Häufigkeit")

#Erste Verbesserung des Plots:
trips_data %>% filter(wait_time_seconds > 0) %>% #Wir betrachten nur Trips, die eine Wartezeit größer als 0 haben.
ggplot(aes(x = wait_time_seconds)) +
  geom_histogram() +
  theme_minimal() +
  ylab("abs. Häufigkeit")

#Zweite Verbesserung des Plots:
trips_data %>% filter(main_mode == "pt" | main_mode == "drt") %>%
  ggplot(aes(x = wait_time_seconds, fill = main_mode)) + #Hier ordnen wir pt und drt jeweils eine Farbe zu!
  geom_histogram() +
  theme_minimal() +
  ylab("abs. Häufigkeit")


## Pie Chart
# Zur Darstellung des Modal Split
trips_data$main_mode <- factor(trips_data$main_mode, levels = rev(c("car", "ride", "walk", "bike", "pt", "freight", "drt", "av")))

trips_data %>% count(main_mode) %>% mutate(Anteil = n / sum(n)) %>% #Berechnung der Anzahl der Trips, pro main mode; dann: Berechnung des Anteil dieses main modes an der Gesamtheit der Trips
  ggplot(aes(x= "", y = Anteil, fill = main_mode)) +
  geom_col(color = "white") + #Sort für weiße Linien zwischen den Slices(deutsch?)
  xlab("") +
  ggtitle("Verteilung main mode") +
  coord_polar("y", start = 0) + 
  scale_y_continuous(labels = scales::percent) + #Da wir Anteile betrachten, soll die Achsenbeschriftung in Prozent erfolgen
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) + #Legende wird unten im Plot positioniert, kein Legendentitel
  scale_fill_viridis_d() #Farbskala, für die ich mich entschieden habe


## Lineplot (die 2.)
#Hier versuchen wir zu erörten, wie sich der Modal Split im Laufe des Zeitraums ändert.
trips_data %>% mutate(dep_time_hour = hour(dep_time)) %>%
  group_by(dep_time_hour, main_mode) %>% count() %>% ungroup() %>% group_by(dep_time_hour) %>%
  mutate(Anteil = n / sum(n)) %>%
  ggplot(aes(dep_time_hour, Anteil, color = main_mode)) +
  geom_line() +
  xlab("Departure time in Stunden") +
  theme_minimal() +
  scale_color_okabe_ito() +
  theme(legend.position = "bottom", legend.title = element_blank())