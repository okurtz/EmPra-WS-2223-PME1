library(dplyr)
library(lgr);
library(rstudioapi);
library(stringr);

# Diese Einstellungen dürfen angepasst werden
SOURCE_FILE_NAME = 'EmPraWS2223_final.csv';   # Name der Datei, die die Rohdaten enthält, inkl. Dateiendung.
LOG_PATH = './Log Datenverarbeitung.log';     # Relativer oder absoluter Pfad, erlaubt ist bspw. auch 'C:/Users/<Name>/Desktop/Log.log'
# ENABLE_DATA_PREPROCESSING = TRUE;             # TRUE: Daten werden vorverarbeitet. FALSE: Daten werden nicht vorverarbeitet - Noch nicht implementiert

# Ab hier Finger weg!
PROCESSED_DATA_FILE_NAME = paste(unlist(strsplit(SOURCE_FILE_NAME, '\\.'))[1], '.RDS', sep='');
SERIOUS_PARTICIPATION_VALUE = 1;
DECIMAL_PLACES_TO_SHOW = 2;
INTERAKTIONSBEREITSCHAFT_ITEMS = c('v_46', 'v_47', 'v_48', 'v_49', 'v_50', 'v_51');
AFFECTION_ITEMS = c('v_52', 'v_53', 'v_54');
ENTHUSIASM_ITEMS = c('v_55', 'v_56', 'v_57');
BEHAVIORAL_APPROACH_TENDENCIES_ITEMS = c('v_58', 'v_59', 'v_60');
ALLOPHILIA_ITEMS = c(AFFECTION_ITEMS, ENTHUSIASM_ITEMS, BEHAVIORAL_APPROACH_TENDENCIES_ITEMS);
VALID_VALUES_INTERAKTIONSBEREITSCHAFT = c(1, 2, 3, 4, 5, 6, 7);
VALID_VALUES_ALLOPHILIA = c(1, 2, 3, 4, 5, 6);
VALID_VALUES_GENDER = c(1, 2, 3, 6);
LABELS_GENDER = c('männlich', 'weiblich', 'divers', 'weiteres');
VALID_VALUES_GRADUATION = c(1, 2, 3, 4, 5, 6, 7, 8, 9);
LABELS_GRADUATION = c('Ohne Abschluss', 'Haupt-/Realschulabschluss', 'Fachhochschulreife/allgemeine Hochschulreife', 'Lehre/Berufsausbildung', 'Meister/Techniker', 'Bachelor', 'Master/Diplom', 'Promotion/Habilitation', 'Sonstiges');

setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
unlink(LOG_PATH);
try(lgr$remove_appender(pos = 'file appender'), silent = TRUE);
lgr$add_appender(AppenderFile$new(LOG_PATH), name = 'file appender');
lgr$info('Datenaufbereitungsskript EmPra WS 22/23 Gruppe 1 (Interaktionsbereitschaft, Allophilie)');

preprocessData = function(fileName) {
  rowsDeleted = 0;
  nonSeriousParticipations = 0;
  countInvalidValuesInteraktionsbereitschaft = 0;
  countInvalidValuesAllophilie = 0;
  isInteraktionsbereitschaftValid = c();
  isAllophilieValid = c();
  
  rawData = read.table(file = fileName, header = TRUE, sep=';');
  lgr$info('Beginne Vorverarbeitung der Daten. Der ursprüngliche Datensatz hat %i Zeilen.', nrow(rawData));
  
  lgr$info('Lösche die Spalten der anderen Praktikumsgruppen.');
  rawData[c('v_61', 'v_62', 'v_63', 'v_64', 'v_65', 'v_66', 'v_67', 'v_68')] = list(NULL);   # Toleranz, PME3
  rawData[c('v_38', 'v_39', 'v_40', 'v_41', 'v_42', 'v_43', 'v_44', 'v_45')] = list(NULL);   # Vorurteile, PME4
  rawData$v_24 = NULL;   # Feeling-Thermometer, PME5
  
  nonSeriousParticipations = nrow(rawData[rawData$v_11 != SERIOUS_PARTICIPATION_VALUE,]);
  if(nonSeriousParticipations == 0) {
    lgr$info('Alle Teilnehmer haben angegeben, ernsthaft teilgenommen zu haben. Lösche nichts.');
  } else {
    lgr$info('Es wurden %i Teilnehmer-Datensätze gefunden, die als nicht-ernst gekennzeichnet sind oder keine Ernsthaftigkeitsangabe haben. Diese werden gelöscht.', nonSeriousParticipations);
  }
  rawData = rawData[rawData$v_11 == SERIOUS_PARTICIPATION_VALUE,];
  rowsDeleted =+ nonSeriousParticipations;
  
  # Suche nach unvollständigen oder Quatsch-Werten bzgl. Interaktionsbereitschaft (v_46 bis v_51)
  isInteraktionsbereitschaftValid = apply(rawData[INTERAKTIONSBEREITSCHAFT_ITEMS], 1, function(row) {
    all(row %in% VALID_VALUES_INTERAKTIONSBEREITSCHAFT);
  });
  countInvalidValuesInteraktionsbereitschaft = sum(!isInteraktionsbereitschaftValid);
  if(countInvalidValuesInteraktionsbereitschaft == 0) {
    lgr$info('Alle Teilnehmer-Datensätze beinhalten ausschließlich gültige Werte für Interaktionsbereitschaft. Lösche nichts.');
  } else {
    invalidRows = rawData[!isAllophilieValid,];
    lgr$warn('Es wurden %i Teilnehmer-Datensätze gefunden, die in der Interaktionsbereitschaft-Skala mindestens einen ungültigen Wert beinhalten. Es handelt sich um die Datensätze mit folgenden Nummern (lfdn): %s. Beheben Sie dieses Problem manuell.', countInvalidValuesInteraktionsbereitschaft, toString(invalidRows));
  }
  
  # Suche nach unvollständigen oder Quatsch-Werten bzgl. Allophilie (v_52 bis v_60)
  isAllophilieValid = apply(rawData[ALLOPHILIA_ITEMS], 1, function(row) {
    all(row %in% VALID_VALUES_ALLOPHILIA);
  });
  countInvalidValuesAllophilie = sum(!isAllophilieValid);
  if(countInvalidValuesAllophilie == 0) {
    lgr$info('Alle Teilnehmer-Datensätze beinhalten ausschließlich gültige Werte für Allophilie. Lösche nichts.');
  } else {
    invalidRows = rawData[!isAllophilieValid,];
    lgr$warn('Es wurden %i Teilnehmer-Datensätze gefunden, die in der Allophilie-Skala mindestens einen ungültigen Wert beinhalten. Es handelt sich um die Datensätze mit folgender Nummer (lfdn): %s. Beheben Sie dieses Problem manuell und starten Sie das Skript erneut.', countInvalidValuesAllophilie, toString(invalidRows$lfdn));
  }
  
  # Ausreißer löschen
  
  lgr$info('Rekodiere das Geschlecht.');
  rawData$v_9 = factor(rawData$v_9, levels = VALID_VALUES_GENDER, labels = LABELS_GENDER);
  
  lgr$info('Rekodiere den Bildungsabschluss.');
  rawData$v_11 = factor(rawData$v_11, levels = VALID_VALUES_GRADUATION, labels = LABELS_GRADUATION);
  
  lgr$info('Sortiere alle Spalten ab der sechsten.');
  rawData = rawData[,c(colnames(rawData[1:5]), str_sort(colnames(rawData)[6:ncol(rawData)], numeric = TRUE))];
  
  lgr$info('Vorverarbeitung der Rohdaten abgeschlossen. Insgesamt wurden die Daten von %i Teilnehmern gelöscht.', rowsDeleted);
  saveRDS(rawData, file = paste(getwd(), PROCESSED_DATA_FILE_NAME, sep='/'));
}

if(!file.exists(PROCESSED_DATA_FILE_NAME)) {
  lgr$info('Konnte Datei %s nicht finden. Greife auf die Originaldaten zurück.', PROCESSED_DATA_FILE_NAME);
  preprocessData(SOURCE_FILE_NAME);
} else {
  lgr$info('Habe Datei %s gefunden und verwende sie nun.', PROCESSED_DATA_FILE_NAME);
}

# Händische Weiterverarbeitung:
# 1. Vorverarbeitete Daten in einer Tabellenansicht anschauen
# 2. Durch Draufschauen auffällige Datensätze ermitteln, die zu löschen sind (die bspw. immer nur den gleichen Messwert enthalten)
# 3. Diese Datensätze händisch löschen und im Weiteren nur mit den übrigen Datensätzen weitermachen

dataToAnalyze = readRDS(PROCESSED_DATA_FILE_NAME);
lgr$info('Der vorverarbeitete Datensatz enthält Daten von %i Teilnehmern.', nrow(dataToAnalyze));

# Mittleres Alter der Teilnehmer
lgr$info('Das mittlere Alter der Teilnehmer beträgt %.2f Jahre.', mean(dataToAnalyze$v_12));

# Anteile der Geschlechter
genderRatios = prop.table(table(dataToAnalyze$v_9)) * 100;
text = 'Geschlecht der Teilnehmer (%):';
for(i in 1:length(LABELS_GENDER)) {
  text = paste(text, round(genderRatios[i], digits = DECIMAL_PLACES_TO_SHOW), LABELS_GENDER[i], sep = ' ');
  if(i < length(LABELS_GENDER)) {
    text = paste(text, ',', sep = '');
  }
}
lgr$info(text);

# Anteile der Bildungsabschlüsse
gradRatios = prop.table(table(dataToAnalyze$v_10510)) * 100;
text = 'Bildungsabschlüsse der Teilnehmer (%):';
for(i in 1:length(LABELS_GRADUATION)) {
  text = paste(text, round(gradRatios[i], digits = DECIMAL_PLACES_TO_SHOW), LABELS_GRADUATION[i], sep = ' ');
  if(i < length(LABELS_GRADUATION)) {
    text = paste(text, ',', sep = '');
  }
}
lgr$info(text);

# Mittelwert und Standardabweichung von Interaktionsbereitschaft
means = colMeans(dataToAnalyze[INTERAKTIONSBEREITSCHAFT_ITEMS]);
lgr$info('Interaktionsbereitschaft: Mittelwert = %.4f, Standardabweichung = %.4f', mean(means), sd(means));

# Mittelwert und Standardabweichung der einzelnen ALLO-15-Subskalen
means = colMeans(dataToAnalyze[AFFECTION_ITEMS]);
lgr$info('Allophilie - Positive Affekte: Mittelwert = %.4f, Standardabweichung = %.4f', mean(means), sd(means));
means = colMeans(dataToAnalyze[ENTHUSIASM_ITEMS]);
lgr$info('Allophilie - Enthusiasmus: Mittelwert = %.4f, Standardabweichung = %.4f', mean(means), sd(means));
means = colMeans(dataToAnalyze[BEHAVIORAL_APPROACH_TENDENCIES_ITEMS]);
lgr$info('Allophilie - Verhaltenstendenzen: Mittelwert = %.4f, Standardabweichung = %.4f', mean(means), sd(means));
