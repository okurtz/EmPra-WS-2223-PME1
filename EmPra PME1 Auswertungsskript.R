library(dplyr)
library(lgr);
library(rstudioapi);
library(stringr);

# Diese Einstellungen dürfen angepasst werden
SOURCE_FILE_NAME = 'EmPraWS2223_final.csv';   # Name der Datei, die die Rohdaten enthält, inkl. Dateiendung.
LOG_PATH = './Log Datenverarbeitung.log';     # Relativer oder absoluter Pfad, erlaubt ist bspw. auch 'C:/Users/<Name>/Desktop/Log.log'
# ENABLE_DATA_PREPROCESSING = TRUE;             # TRUE: Daten werden vorverarbeitet. FALSE: Daten werden nicht vorverarbeitet - Noch nicht implementiert

# Technische Einstellungen
PROCESSED_DATA_FILE_NAME = paste(unlist(strsplit(SOURCE_FILE_NAME, '\\.'))[1], '.RDS', sep='');
DECIMAL_PLACES_TO_SHOW = 2;

# Item-Aliase
AFFECTION_ITEMS = c('v_52', 'v_53', 'v_54');
ENTHUSIASM_ITEMS = c('v_55', 'v_56', 'v_57');
GENDER_ITEM = 'v_9';
GRADUATION_ITEM = 'v_10510';
INTERAKTIONSBEREITSCHAFT_ITEMS = c('v_46', 'v_47', 'v_48', 'v_49', 'v_50', 'v_51');
BEHAVIORAL_APPROACH_TENDENCIES_ITEMS = c('v_58', 'v_59', 'v_60');
ALLOPHILIA_ITEMS = c(AFFECTION_ITEMS, ENTHUSIASM_ITEMS, BEHAVIORAL_APPROACH_TENDENCIES_ITEMS);
SERIOUS_PARTICIPATION_ITEM = 'v_11';
IMPAIRED_VISION_ITEM = 'v_110';
ROW_ID = 'lfdn';

# Wert-Aliase
INVALID_ANSWER_VALUE = -99;
INVALID_ANSWER_TEXT = 'Keine Angabe/ungültig';
VALID_VALUES_ALLOPHILIA = seq(1, 6, by = 0.5);  # Sequenz, weil bei fehlenden Werten Mittelwerte verwendet werden, die evtl. nicht-ganzzahlig sind
VALID_VALUES_GENDER = c(INVALID_ANSWER_VALUE, 1, 2, 3, 6);
VALID_VALUES_GRADUATION = c(INVALID_ANSWER_VALUE, 1, 2, 3, 4, 5, 6, 7, 8, 9);
VALID_VALUES_INTERAKTIONSBEREITSCHAFT = seq(1, 7, by = 0.5);
SERIOUS_PARTICIPATION_VALUE = 1;
NO_IMPAIRED_VISION_VALUE = 2;

# Labels
LABELS_GENDER = c(INVALID_ANSWER_TEXT, 'männlich', 'weiblich', 'divers', 'weiteres');
LABELS_GRADUATION = c(INVALID_ANSWER_TEXT, 'Ohne Abschluss', 'Haupt-/Realschulabschluss', 'Fachhochschulreife/allgemeine Hochschulreife', 'Lehre/Berufsausbildung', 'Meister/Techniker', 'Bachelor', 'Master/Diplom', 'Promotion/Habilitation', 'Sonstiges');

# Teilnehmer-Datensätze mit fehlenden/ungültigen Werten in der Allophilie-Skala
INVALID_DATASETS_AFFECTION = mapply(list, c(504, 501, 402, 231), c('v_52', 'v_53', 'v_53', 'v_54'));
INVALID_DATASETS_ENTHUSIASM = mapply(list, c(257, 297, 504), c('v_55', 'v_55', 'v_56'));

setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
unlink(LOG_PATH);
try(lgr$remove_appender(pos = 'file appender'), silent = TRUE);
lgr$add_appender(AppenderFile$new(LOG_PATH), name = 'file appender');
lgr$info('Datenaufbereitungsskript EmPra WS 22/23 Gruppe 1 (Interaktionsbereitschaft, Allophilie) v0.0.3, 16. Dezember 2022');

replaceInvalidValues = function(rawData, invalidItemList, totalItemList) {
  for(i in 1:length(invalidItemList[1,])) {
    invalidRow = invalidItemList[[1,i]];
    invalidCol = invalidItemList[[2,i]];
    invalidValue = rawData[rawData[ROW_ID] == invalidRow,invalidCol];
    otherItemsOfScale = setdiff(totalItemList, c(invalidCol));
    newValue = mean(unlist(rawData[rawData[ROW_ID] == invalidRow,otherItemsOfScale]));
    rawData[rawData[ROW_ID] == invalidRow,invalidCol] = newValue;
    lgr$info('Datensatz %i: Ersetze den ungültigen Wert \"%i\" von Item %s durch den Mittelwert \"%.1f\" der Items %s = %i, %s = %i.', 
             invalidRow, invalidValue, invalidCol, newValue, 
             otherItemsOfScale[1], rawData[rawData[ROW_ID] == invalidRow,otherItemsOfScale[1]],
             otherItemsOfScale[2], rawData[rawData[ROW_ID] == invalidRow,otherItemsOfScale[2]]);
  }
  return(rawData);
}

preprocessData = function(fileName) {
  rowsDeleted = 0;
  nonSeriousParticipations = 0;
  participantsWithImpairedVision = 0;
  countInvalidValuesInteraktionsbereitschaft = 0;
  countInvalidValuesAllophilie = 0;
  isInteraktionsbereitschaftValid = c();
  isAllophiliaValid = c();
  
  rawData = read.table(file = fileName, header = TRUE, sep=';');
  lgr$info('Beginne Vorverarbeitung der Daten. Der ursprüngliche Datensatz besteht aus %i Teilnehmer-Datensätzen.', nrow(rawData));
  
  
  ### Löschen von Spalten, die von Anfang an unnötig sind ###
  
  lgr$info('Lösche die Spalten der anderen Praktikumsgruppen.');
  rawData[c('v_61', 'v_62', 'v_63', 'v_64', 'v_65', 'v_66', 'v_67', 'v_68')] = list(NULL);   # Toleranz, PME3
  rawData[c('v_38', 'v_39', 'v_40', 'v_41', 'v_42', 'v_43', 'v_44', 'v_45')] = list(NULL);   # Vorurteile, PME4
  rawData$v_24 = NULL;   # Feeling-Thermometer, PME5
  
  lgr$info('Lösche die Spalten mit den Items der Kontrollgruppe.');
  rawData[c('v_140', 'v_141', 'v_142', 'v_143', 'v_144', 'v_145', 'v_146', 'v_147', 'v_148', 'v_149', 'v_150', 'v_151')] = list(NULL);
  
  
  ### Löschen von Items ###
  
  nonSeriousParticipations = nrow(rawData[rawData[SERIOUS_PARTICIPATION_ITEM] != SERIOUS_PARTICIPATION_VALUE,]);
  if(nonSeriousParticipations == 0) {
    lgr$info('Alle Teilnehmer haben angegeben, ernsthaft teilgenommen zu haben. Lösche nichts.');
  } else {
    lgr$info('Habe %i Teilnehmer-Datensätze gefunden, die als nicht-ernst gekennzeichnet sind oder keine Ernsthaftigkeitsangabe haben. Lösche die betroffenen Datensätze.', nonSeriousParticipations);
  }
  rawData = rawData[rawData[SERIOUS_PARTICIPATION_ITEM] == SERIOUS_PARTICIPATION_VALUE,];
  rowsDeleted = rowsDeleted + nonSeriousParticipations;
  lgr$info('Der Datensatz enthält jetzt nur noch Daten von Teilnehmern, die ernsthaft geantwortet haben. Lösche die Spalte \"Serious Participation\" (Item %s), da sie nun überflüssig ist.', SERIOUS_PARTICIPATION_ITEM);
  rawData[SERIOUS_PARTICIPATION_ITEM] = NULL;
  
  participantsWithImpairedVision = nrow(rawData[rawData[IMPAIRED_VISION_ITEM] != NO_IMPAIRED_VISION_VALUE,]);
  lgr$info('Habe folgende Werte für \"Einschränkungen Sehen\" (Item %s) gefunden: %s. %i Teilnehmer haben irgendetwas anderes als \"%i\" = \"Nein\" (keine Einschränkung beim Sehen) angegeben. Die Datensätze dieser Teilnehmer werden gelöscht.',
           IMPAIRED_VISION_ITEM, toString((sort(unique(data[,IMPAIRED_VISION_ITEM])))), participantsWithImpairedVision, NO_IMPAIRED_VISION_VALUE);
  rawData = rawData[rawData[IMPAIRED_VISION_ITEM] == NO_IMPAIRED_VISION_VALUE,];
  rowsDeleted = rowsDeleted + participantsWithImpairedVision;
  lgr$info('Der Datensatz enthält jetzt nur noch Daten von Teilnehmern ohne Seheinschränkung. Lösche die Spalte \"Einschränkungen Sehen\" (Item %s), da sie nun überflüssig ist.', IMPAIRED_VISION_ITEM);
  rawData[IMPAIRED_VISION_ITEM] = NULL;
  
  
  ### Behandlung von ungültigen Werten ###
  
  # Bekannte ungültige Werte ersetzen
  rawData = replaceInvalidValues(rawData, INVALID_DATASETS_AFFECTION, AFFECTION_ITEMS);
  rawData = replaceInvalidValues(rawData, INVALID_DATASETS_ENTHUSIASM, ENTHUSIASM_ITEMS);
  
  # Suche nach unvollständigen oder Quatsch-Werten bzgl. Interaktionsbereitschaft (v_46 bis v_51)
  isInteraktionsbereitschaftValid = apply(rawData[INTERAKTIONSBEREITSCHAFT_ITEMS], 1, function(row) {
    all(row %in% VALID_VALUES_INTERAKTIONSBEREITSCHAFT);
  });
  countInvalidValuesInteraktionsbereitschaft = sum(!isInteraktionsbereitschaftValid);
  if(countInvalidValuesInteraktionsbereitschaft == 0) {
    lgr$info('Alle Teilnehmer-Datensätze beinhalten (jetzt) ausschließlich gültige Werte für Interaktionsbereitschaft. Lösche nichts.');
  } else {
    invalidRows = rawData[!isInteraktionsbereitschaftValid,];
    lgr$warn('Es wurden %i Teilnehmer-Datensätze gefunden, die in der Interaktionsbereitschaft-Skala mindestens einen ungültigen Wert beinhalten. Es handelt sich um die Datensätze mit folgenden Nummern (lfdn): %s. Beheben Sie dieses Problem manuell.', countInvalidValuesInteraktionsbereitschaft, toString(invalidRows));
  }
  
  # Suche nach unvollständigen oder Quatsch-Werten bzgl. Allophilie (v_52 bis v_60)
  isAllophiliaValid = apply(rawData[ALLOPHILIA_ITEMS], 1, function(row) {
    all(row %in% VALID_VALUES_ALLOPHILIA);
  });
  countInvalidValuesAllophilie = sum(!isAllophiliaValid);
  if(countInvalidValuesAllophilie == 0) {
    lgr$info('Alle Teilnehmer-Datensätze beinhalten (jetzt) ausschließlich gültige Werte für Allophilie. Lösche nichts.');
  } else {
    invalidRows = rawData[!isAllophiliaValid,];
    lgr$warn('Es wurden %i Teilnehmer-Datensätze gefunden, die in der Allophilie-Skala mindestens einen ungültigen Wert beinhalten. Es handelt sich um die Datensätze mit folgender Nummer (lfdn): %s. Beheben Sie dieses Problem manuell und starten Sie das Skript erneut.', countInvalidValuesAllophilie, toString(invalidRows$lfdn));
  }
  
  # Ausreißer löschen
  
  
  ### Rekodieren ###
  
  lgr$info('Rekodiere das Geschlecht.');
  rawData[,GENDER_ITEM] = factor(rawData[,GENDER_ITEM], levels = VALID_VALUES_GENDER, labels = LABELS_GENDER);
  rawData[is.na(rawData[,GENDER_ITEM]),GENDER_ITEM] = INVALID_ANSWER_TEXT;
  
  lgr$info('Rekodiere den Bildungsabschluss.');
  rawData[,GRADUATION_ITEM] = factor(rawData[,GRADUATION_ITEM], levels = VALID_VALUES_GRADUATION, labels = LABELS_GRADUATION);
  rawData[is.na(rawData[,GRADUATION_ITEM]),GRADUATION_ITEM] = INVALID_ANSWER_TEXT;
  
  lgr$info('Sortiere alle Spalten ab der sechsten.');
  rawData = rawData[,c(colnames(rawData[1:5]), str_sort(colnames(rawData)[6:ncol(rawData)], numeric = TRUE))];
  
  ### Abschluss der Vorverarbeitung ###
  
  lgr$info('Vorverarbeitung der Rohdaten abgeschlossen. Insgesamt wurden die Daten von %i Teilnehmern gelöscht.', rowsDeleted);
  lgr$info('Speichere die vorverarbeiteten Daten in der Datei %s.', PROCESSED_DATA_FILE_NAME);
  saveRDS(rawData, file = paste(getwd(), PROCESSED_DATA_FILE_NAME, sep='/'));
}

if(!file.exists(PROCESSED_DATA_FILE_NAME)) {
  lgr$info('Konnte die Datei %s nicht finden. Greife auf die Originaldaten zurück.', PROCESSED_DATA_FILE_NAME);
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
genderRatios = prop.table(table(dataToAnalyze[GENDER_ITEM])) * 100;
text = 'Geschlecht der Teilnehmer (%):';
for(i in 1:length(LABELS_GENDER)) {
  text = paste(text, round(genderRatios[i], digits = DECIMAL_PLACES_TO_SHOW), LABELS_GENDER[i], sep = ' ');
  if(i < length(LABELS_GENDER)) {
    text = paste(text, ',', sep = '');
  }
}
lgr$info(text);

# Anteile der Bildungsabschlüsse
gradRatios = prop.table(table(dataToAnalyze[GRADUATION_ITEM])) * 100;
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
