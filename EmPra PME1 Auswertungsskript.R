library(dplyr);
library(lgr);
library(rstudioapi);
library(stringr);

# Diese Einstellungen dürfen angepasst werden. Erlaubt sind relative oder absolute Pfade, also bspw. auch "C:/Users/<Name>/Desktop/Log Datenverarbeitung.log". Wichtig: Der Pfad muss immer mit einem Dateinamen inkl. Dateiendung enden!
SOURCE_FILE_NAME = 'EmPraWS2223_final.csv';         # Pfad zur Datei, die die Rohdaten enthält
LOG_PATH = 'Log Datenverarbeitung.log';           # Pfad zur Datei, in die das Log geschrieben werden soll
OUTLIER_DUMP_PATH = 'Beseitigte Ausreißer.csv';   # Pfad zur Datei, in die die Ausreißer geschrieben werden sollen

# Technische Einstellungen
PROCESSED_DATA_FILE_NAME = paste(unlist(strsplit(SOURCE_FILE_NAME, '\\.'))[1], '.RDS', sep='');
VERSION_NUMBER = 'v0.0.5';
VERSION_DATE = '24. Dezember 2022';

# Item-Aliase
AFFECTION_ITEMS = c('v_52', 'v_53', 'v_54');
DATA_USAGE_AGREEMENT_ITEM = 'v_8';
DO_YOU_STUDY_ITEM = 'v_108';
ENTHUSIASM_ITEMS = c('v_55', 'v_56', 'v_57');
ALLOPHILIA_ITEMS = c(AFFECTION_ITEMS, ENTHUSIASM_ITEMS);
EXPERIMENTAL_CONDITION_ITEM = 'c_0001';
GENDER_ITEM = 'v_9';
GRADUATION_ITEM = 'v_10510';
IMPAIRED_VISION_ITEM = 'v_110';
INTERAKTIONSBEREITSCHAFT_ITEMS = c('v_46', 'v_47', 'v_48', 'v_49', 'v_50', 'v_51');
ROW_ID = 'lfdn';
SCREEN_ITEM = 'v_107';
STUDY_PARTICIPATION_AGREEMENT_ITEM = 'Einverst_Bedingungen';
SERIOUS_PARTICIPATION_ITEM = 'v_11';

# Wert-Aliase
I_AGREE_TO_MY_DATA_BEING_USED_VALUE = 1;
I_AGREE_TO_PARTICIPATE_IN_THE_STUDY_VALUE = 1;
INVALID_ANSWER_VALUES = c(-99, 0);
I_STUDY_PSYCHOLOGY_AT_FU_HAGEN_VALUE = 1;
NO_IMPAIRED_VISION_VALUE = 2;
SERIOUS_PARTICIPATION_VALUE = 1;
VALID_VALUES_ALLOPHILIA = seq(1, 6, by = 0.5);  # Sequenz, weil bei fehlenden Werten Me verwendet werden, die evtl. nicht-ganzzahlig sind
VALID_VALUES_EXPERIMENTAL_CONDITION = c(1, 2);
VALID_VALUES_GENDER = c(1, 2, 3, 6);
VALID_VALUES_GRADUATION = c(1, 2, 3, 4, 5, 6, 7, 8, 9);
VALID_VALUES_INTERAKTIONSBEREITSCHAFT = seq(1, 7, by = 0.5);
VALID_VALUES_SCREENS = c(1, 2, 3, 4, 5);

# Labels
LABELS_EXPERIMENTAL_CONDITION = c('Erst positiv, dann negativ', 'Erst negativ, dann positiv');
LABELS_GENDER = c('weiblich', 'männlich', 'divers', 'weiteres');
LABELS_GRADUATION = c('Ohne Abschluss', 'Haupt-/Realschulabschluss', 'Fachhochschulreife/allgemeine Hochschulreife', 'Lehre/Berufsausbildung', 'Meister/Techniker', 'Bachelor', 'Master/Diplom', 'Promotion/Habilitation', 'Sonstiges');
LABELS_SCREENS = c('Smartphone', 'Tablet', 'Laptop', 'Externer Bildschirm', 'Sonstiges');

# Teilnehmer-Datensätze mit fehlenden/ungültigen Werten in der Allophilie-Skala
INVALID_DATASETS_AFFECTION = mapply(list, c(504, 501, 402, 231), c('v_52', 'v_53', 'v_53', 'v_54'));
INVALID_DATASETS_ENTHUSIASM = mapply(list, c(257, 297, 504), c('v_55', 'v_55', 'v_56'));

replaceInvalidValues = function(rawData) {
  iterateItemList = function(rawData, invalidItemList, totalItemList) {
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
  
  checkForInvalidValues = function(scaleName, scaleItems, validValues) {
    scaleValidityVector = c();
    countInvalidValues = 0;
    
    scaleValidityVector = apply(rawData[scaleItems], 1, function(row) {
      all(row %in% validValues);
    });
    countInvalidValues = sum(!scaleValidityVector);
    if(countInvalidValues == 0) {
      lgr$info('Auf der Skala \"%s\" wurden keine (weiteren) ungültigen Werte gefunden.', scaleName);
    } else {
      invalidRows = rawData[!scaleValidityVector,];
      lgr$warn('Auf der Skala \"%s\" wurden %i Datensätze gefunden, die mindestens einen ungültigen Wert beinhalten. Es handelt sich um die Datensätze mit folgenden Nummern (lfdn): %s. Beheben Sie dieses Problem manuell.',
               scaleName, countInvalidValues, toString(invalidRows));
    }
  }
  
  newLogSection('Behandlung von fehlenden oder ungültigen Werten');
  
  rawData = iterateItemList(rawData, INVALID_DATASETS_AFFECTION, AFFECTION_ITEMS);
  rawData = iterateItemList(rawData, INVALID_DATASETS_ENTHUSIASM, ENTHUSIASM_ITEMS);
  checkForInvalidValues('Interaktionsbereitschaft', INTERAKTIONSBEREITSCHAFT_ITEMS, VALID_VALUES_INTERAKTIONSBEREITSCHAFT);
  checkForInvalidValues('Allophilie', ALLOPHILIA_ITEMS, VALID_VALUES_ALLOPHILIA);
  
  return(rawData);
}

newLogSection = function(title, newLine = TRUE) {
  delimiterChar = '*';
  borderWidth = 3;
  totalRowLength = 2*borderWidth + 2 + nchar(title);   # +2 zählt auch die Whitespaces
  
  if(newLine) {
    lgr$info('\n');
  }
  lgr$info(strrep(delimiterChar, totalRowLength));
  lgr$info(paste(strrep(delimiterChar, borderWidth), title, strrep(delimiterChar, borderWidth)));
  lgr$info(strrep(delimiterChar, totalRowLength));
}

deleteRows = function(rawData) {
  rowsDeleted = 0;
  nonSeriousParticipations = 0;
  countMissingParticipationAgreement = 0;
  countMissingDataUsageAgreement = 0;
  countParticipantsWithImpairedVision = 0;
  countParticipantsWhoStudyPsychologyAtFUHagen = 0;
  
  newLogSection('Items löschen');
  lgr$warn('Achtung: Es kann sein, dass ein Teilnehmer mehr als nur ein Ausschluss-Kriterium erfüllt. Die Anzahl der Teilnehmer, die aus einem bestimmten Grund ausgeschlossen werden, kann folglich von der Reihenfolge abhängen, in der die Datensätze gelöscht worden sind. Die Zahl der Datensätze, die in jedem Arbeitsschritt gelöscht werden, bezieht sich daher auf die zu diesem Punkt verbliebenen Datensätze, nicht auf die ursprünglich vorhandenen.');
  
  nonSeriousParticipations = nrow(rawData[rawData[SERIOUS_PARTICIPATION_ITEM] != SERIOUS_PARTICIPATION_VALUE,]);
  if(nonSeriousParticipations == 0) {
    lgr$info('Alle Teilnehmer haben angegeben, ernsthaft teilgenommen zu haben. Lösche nichts.');
  } else {
    lgr$info('Habe %i Teilnehmer-Datensätze gefunden, die als nicht-ernst gekennzeichnet sind oder keine Ernsthaftigkeitsangabe haben. Lösche die betroffenen Datensätze.', nonSeriousParticipations);
  }
  rawData = rawData[rawData[SERIOUS_PARTICIPATION_ITEM] == SERIOUS_PARTICIPATION_VALUE,];
  rowsDeleted = rowsDeleted + nonSeriousParticipations;
  
  countMissingParticipationAgreement = nrow(rawData[rawData[STUDY_PARTICIPATION_AGREEMENT_ITEM] != I_AGREE_TO_PARTICIPATE_IN_THE_STUDY_VALUE,]);
  if(countMissingParticipationAgreement == 0) {
    lgr$info('Alle Teilnehmer haben angegeben, die Teilnahmeerklärung gelesen und verstanden zu haben (ha.) und haben sich zur Teilnahme bereiterklärt. Lösche nichts.');
  } else {
    lgr$info('%i Teilnehmer haben die Teilnahmeerklärung nicht mit \"Ja\" beantwortet. Lösche die betroffenen Datensätze.', countMissingParticipationAgreement);
    rawData = rawData[rawData[STUDY_PARTICIPATION_AGREEMENT_ITEM] == I_AGREE_TO_PARTICIPATE_IN_THE_STUDY_VALUE,];
    rowsDeleted = rowsDeleted + countMissingParticipationAgreement;
  }
  
  countMissingDataUsageAgreement = nrow(rawData[rawData[DATA_USAGE_AGREEMENT_ITEM] != I_AGREE_TO_MY_DATA_BEING_USED_VALUE,]);
  if(countMissingDataUsageAgreement == 0) {
    lgr$info('Alle Teilnehmer haben ihre Einverständnis dazu abgegeben, dass ihre Daten verwendet werden. Lösche nichts.');
  } else {
    lgr$info('%i Teilnehmer haben keine Einverständnis dazu abgegeben, dass ihre Daten verwendet werden. Lösche die betroffenen Datensätze.', countMissingDataUsageAgreement);
    rawData = rawData[rawData[DATA_USAGE_AGREEMENT_ITEM] == I_AGREE_TO_MY_DATA_BEING_USED_VALUE,];
    rowsDeleted = rowsDeleted + countMissingDataUsageAgreement;
  }
  
  countParticipantsWithImpairedVision = nrow(rawData[rawData[IMPAIRED_VISION_ITEM] != NO_IMPAIRED_VISION_VALUE,]);
  if(countParticipantsWithImpairedVision == 0) {
    lgr$info('Alle Teilnehmer haben angegeben, keine Einschränkungen beim Sehen zu haben. Lösche nichts.');
  } else {
    lgr$info('Habe folgende Werte für \"Einschränkungen Sehen\" (Item %s) gefunden: %s. %i Teilnehmer haben irgendetwas anderes als \"%i\" = \"Nein\" (keine Einschränkung beim Sehen) angegeben. Die Datensätze dieser Teilnehmer werden gelöscht.',
             IMPAIRED_VISION_ITEM, toString((sort(unique(rawData[,IMPAIRED_VISION_ITEM])))), countParticipantsWithImpairedVision, NO_IMPAIRED_VISION_VALUE);
    rawData = rawData[rawData[IMPAIRED_VISION_ITEM] == NO_IMPAIRED_VISION_VALUE,];
    rowsDeleted = rowsDeleted + countParticipantsWithImpairedVision;
  }
  countParticipantsWhoStudyPsychologyAtFUHagen = nrow(rawData[rawData[DO_YOU_STUDY_ITEM] == I_STUDY_PSYCHOLOGY_AT_FU_HAGEN_VALUE,]);
  if(countParticipantsWhoStudyPsychologyAtFUHagen == 0) {
    lgr$info('Kein Teilnehmer hat angegeben, an der Fernuniversität in Hagen Psychologie zu studieren. Lösche nichts.');
  } else {
    lgr$info('%i Teilnehmer haben angegeben, Psychologie an der Fernuniversität in Hagen zu studieren. Lösche diese Datensätze.', countParticipantsWhoStudyPsychologyAtFUHagen);
    rawData = rawData[rawData[DO_YOU_STUDY_ITEM] != I_STUDY_PSYCHOLOGY_AT_FU_HAGEN_VALUE,];
    rowsDeleted = rowsDeleted + countParticipantsWhoStudyPsychologyAtFUHagen;
  }
  lgr$info('Insgesamt wurden die Daten von %i Teilnehmern gelöscht.', rowsDeleted);
  
  return(rawData);
}

deleteColumns = function(rawData) {
  newLogSection('Spalten löschen');
  
  lgr$info('Lösche die Spalten der anderen Praktikumsgruppen.');
  rawData[c('v_58', 'v_59', 'v_60')] = list(NULL);  # Engagement, PME2
  rawData[c('v_61', 'v_62', 'v_63', 'v_64', 'v_65', 'v_66', 'v_67', 'v_68')] = list(NULL);   # Toleranz, PME3
  rawData[c('v_38', 'v_39', 'v_40', 'v_41', 'v_42', 'v_43', 'v_44', 'v_45')] = list(NULL);   # Vorurteile, PME4
  rawData$v_24 = NULL;   # Feeling-Thermometer, PME5
  
  lgr$info('Lösche die Spalten mit den Items der Kontrollgruppe.');
  rawData[c('v_140', 'v_141', 'v_142', 'v_143', 'v_144', 'v_145', 'v_146', 'v_147', 'v_148', 'v_149', 'v_150', 'v_151')] = list(NULL);
  
  lgr$info('Der Datensatz enthält jetzt nur noch Daten von Teilnehmer, die der Teilnahme an der Studie zugestimmt haben. Lösche die Spalte mit der Teilnahmeerklärung (Item %s), da sie nun überflüssig ist.', STUDY_PARTICIPATION_AGREEMENT_ITEM)
  
  lgr$info('Der Datensatz enthält jetzt nur noch Daten von Teilnehmern, die mit der Verarbeitung ihrer Daten einverstanden sind. Lösche die Spalte mit der Einverständniserklärung zur Datenverarbeitung (Item %s), da sie nun überflüssig ist.', DATA_USAGE_AGREEMENT_ITEM);
  rawData[DATA_USAGE_AGREEMENT_ITEM] = NULL;
  
  lgr$info('Der Datensatz enthält jetzt nur noch Daten von Teilnehmern, die ernsthaft geantwortet haben. Lösche die Spalte \"Serious Participation\" (Item %s), da sie nun überflüssig ist.', SERIOUS_PARTICIPATION_ITEM);
  rawData[SERIOUS_PARTICIPATION_ITEM] = NULL;
  
  lgr$info('Der Datensatz enthält jetzt nur noch Daten von Teilnehmern ohne Seheinschränkung. Lösche die Spalte \"Einschränkungen Sehen\" (Item %s), da sie nun überflüssig ist.', IMPAIRED_VISION_ITEM);
  rawData[IMPAIRED_VISION_ITEM] = NULL;
  
  if(unique(rawData["v_3"]) == 1) {
    lgr$info('Item v_3 enthält für alle Zeilen nur einen einzigen Wert (%s). Zudem scheint dieses Item nicht bedeutungstragend zu sein. Lösche daher die Spalte.', unlist(unique(rawData["v_3"])));
    rawData["v_3"] = NULL;
  }
  
  return(rawData);
}

recodeDataset = function(rawData) {
  newLogSection('Rekodieren')
  
  lgr$info('Ersetze alle Vorkommen von %s durch NA.', toString(unlist(INVALID_ANSWER_VALUES)));
  rawData = data.frame(lapply(rawData, function(col){
    replace(col, col %in% INVALID_ANSWER_VALUES, NA);
  }));
  
  lgr$info('Rekodiere die Versuchsbedingung.');
  rawData[,EXPERIMENTAL_CONDITION_ITEM] = factor(rawData[,EXPERIMENTAL_CONDITION_ITEM], levels = VALID_VALUES_EXPERIMENTAL_CONDITION, labels = LABELS_EXPERIMENTAL_CONDITION, exclude = NULL);
  
  lgr$info('Rekodiere das Geschlecht.');
  rawData[,GENDER_ITEM] = factor(rawData[,GENDER_ITEM], levels = VALID_VALUES_GENDER, labels = LABELS_GENDER, exclude = NULL);
  
  lgr$info('Rekodiere den Bildungsabschluss.');
  rawData[,GRADUATION_ITEM] = addNA(factor(rawData[,GRADUATION_ITEM], levels = VALID_VALUES_GRADUATION, labels = LABELS_GRADUATION, exclude = NULL));
  
  lgr$info('Rekodiere den verwendeten Bildschirm.');
  rawData[,SCREEN_ITEM] = factor(rawData[,SCREEN_ITEM], levels = VALID_VALUES_SCREENS, labels = LABELS_SCREENS, exclude = NULL);
  
  lgr$info('Rekodieren abgeschlossen. Die rekodierten Spalten können nun nicht mehr über die numerischen Werte im Codebuch gefiltert werden, sondern nur noch über die Faktorwerte, also bspw. \"data[data[\"v_107\"] == \"Smartphone\",]\" anstatt \"data[data[\"v_107\"] == 1,]\".');
  
  return(rawData);
}

removeOutliers = function(rawData, sds = 2) {
  CUTOFF_MAX = sprintf('M_Skala + %i*SD', sds);
  CUTOFF_MIN = sprintf('M_Skala - %i*SD', sds);
  outlierItems = c(INTERAKTIONSBEREITSCHAFT_ITEMS, AFFECTION_ITEMS, ENTHUSIASM_ITEMS);
  additionalOutlierItems = c(ROW_ID, 'Skala', 'M_Skala', 'M_Teilnehmer', CUTOFF_MIN, CUTOFF_MAX);
  outliers = data.frame(matrix(ncol = length(additionalOutlierItems) + length(outlierItems)), nrow = 0) %>% mutate(ROW_ID = as.character(ROW_ID), .);
  
  removeOutliersInternal = function(scaleName, scaleItems) {
    means = c();
    scaleMean = 0;
    scaleSD = 0;
    upperThreshold = 0;
    lowerThreshold = 0;
    additionalOutlierData = data.frame(matrix(ncol = length(additionalOutlierItems), nrow = 0)) %>% mutate(ROW_ID = as.character(ROW_ID), .);
    
    lgr$info('Bereinige Skala \"%s\".', scaleName);
    means = rowMeans(rawData[scaleItems]);
    scaleMean = mean(means);
    scaleSD = sd(means);
    upperThreshold = scaleMean + sds*scaleSD;
    lowerThreshold = scaleMean - sds*scaleSD;
    lgr$info('Vor der Beseitigung der Ausreißer sind M = %.4f und SD = %.4f.', scaleMean, scaleSD);
    
    outliersInternal = rawData[means > upperThreshold | means < lowerThreshold,][c(ROW_ID, scaleItems)];
    if(nrow(outliersInternal) == 0) {
      lgr$info('Es wurden keine Ausreißer auf der Skala \"%s\" ermittelt.', scaleName);
    } else {
      lgr$info('Habe %i Teilnehmer gefunden, deren Mittelwert für \"%s\" größer als %.4f oder kleiner als %.4f ist. Die Antworten dieser Teilnehmer auf der Skala \"%s\" werden auf \"NA\" gesetzt.', 
               nrow(outliersInternal), scaleName, upperThreshold, lowerThreshold, scaleName);
      additionalOutlierData = data.frame(rawData[ROW_ID], c(scaleName), c(round(scaleMean, digits = 4)), round(means, digits = 4),
                                         c(round(lowerThreshold, digits = 4)), c(round(upperThreshold, digits = 4)))[row.names(outliersInternal[ROW_ID]),];
      colnames(additionalOutlierData) = additionalOutlierItems;
      outliersInternal = merge(outliersInternal, additionalOutlierData, by = ROW_ID);
      for(i in 1:nrow(outliersInternal)) {
        rawData[rawData[ROW_ID] == outliersInternal[i,ROW_ID],][scaleItems] <<- NA;
      }
      means = rowMeans(rawData[scaleItems]);
      lgr$info('Nach der Beseitigung der Ausreißer sind M = %.4f und SD = %.4f.', mean(means, na.rm = TRUE), sd(means, na.rm = TRUE));
    }
    return(outliersInternal);
  }
  
  colnames(outliers) = c(additionalOutlierItems, outlierItems);

  newLogSection('Ausreißer entfernen');
  lgr$info('Achtung: \"Entfernen\" von Ausreißern bedeutet nicht, dass der komplette Datensatz eines Teilnehmers gelöscht wird, sobald er auf einer Skala als Ausreißer erkennt worden ist (das wäre unsinnig), sondern dass Ausreißer-Antworten auf \"NA\" gesetzt werden.');
  outliers = (bind_rows(removeOutliersInternal('Positive Affekte', AFFECTION_ITEMS)) %>%
             bind_rows(removeOutliersInternal('Enthusiasmus', ENTHUSIASM_ITEMS)) %>%
             bind_rows(removeOutliersInternal('Interaktionsbereitschaft', INTERAKTIONSBEREITSCHAFT_ITEMS), .))[, c(additionalOutlierItems, outlierItems)];
  
  lgr$info('Entfernen von Ausreißern abgeschlossen. Es wurden bei insgesamt %i Teilnehmern Ausreißer erkannt. Die Antworten, aufgrund derer sie als Ausreißer erkannt worden sind, befinden sich in der Datei \"%s\".',
           nrow(outliers), OUTLIER_DUMP_PATH);
  unlink(OUTLIER_DUMP_PATH);
  write.csv(outliers, OUTLIER_DUMP_PATH, row.names = FALSE);
  return(rawData);
}

preprocessData = function(fileName) {
  rawData = read.table(file = fileName, header = TRUE, sep=';');
  lgr$info('Beginne Vorverarbeitung der Daten. Der ursprüngliche Datensatz besteht aus %i Teilnehmer-Datensätzen.', nrow(rawData));
  
  rawData = deleteRows(rawData);
  rawData = replaceInvalidValues(rawData)
  rawData = deleteColumns(rawData);
  rawData = recodeDataset(rawData);
  rawData = removeOutliers(rawData);
  
  newLogSection('Technischer Abschluss der Vorverarbeitung');
  lgr$info('Sortiere alle Spalten ab der sechsten.');
  rawData = rawData[,c(colnames(rawData[1:5]), str_sort(colnames(rawData)[6:ncol(rawData)], numeric = TRUE))];
  lgr$info('Vorverarbeitung der Rohdaten abgeschlossen. Speichere die vorverarbeiteten Daten in der Datei \"%s.\"', PROCESSED_DATA_FILE_NAME);
  saveRDS(rawData, file = paste(getwd(), PROCESSED_DATA_FILE_NAME, sep='/'));
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
unlink(LOG_PATH);
lgr$config(list(
  threshold = 'info',
  appenders = AppenderFile$new(
    LOG_PATH,
    layout = LayoutFormat$new('%m')
  )
));
newLogSection(sprintf('Datenaufbereitungsskript EmPra WS 22/23 Gruppe 1, %s, %s', VERSION_NUMBER, VERSION_DATE), FALSE);

if(!file.exists(PROCESSED_DATA_FILE_NAME)) {
  lgr$info('Konnte die Datei %s nicht finden. Greife auf die Originaldaten zurück.', PROCESSED_DATA_FILE_NAME);
  preprocessData(SOURCE_FILE_NAME);
} else {
  lgr$info('Habe Datei %s gefunden und verwende sie nun.', PROCESSED_DATA_FILE_NAME);
}

dataToAnalyze = readRDS(PROCESSED_DATA_FILE_NAME);

newLogSection('Statistiken');

# Separation beider Experimentalgruppen
pos_neg_group = dataToAnalyze[dataToAnalyze[EXPERIMENTAL_CONDITION_ITEM] == 'Erst positiv, dann negativ',];
neg_pos_group = dataToAnalyze[dataToAnalyze[EXPERIMENTAL_CONDITION_ITEM] == 'Erst negativ, dann positiv',];
lgr$info('Der vorverarbeitete Datensatz enthält Daten von %i Teilnehmern. Davon haben %i Teilnehmer an der Versuchsbedingung \"Erst positiv, dann negativ\" teilgenommen und %i Teilnehmer an der Versuchsbedingung \"Erst negativ, dann positiv\".',
         nrow(dataToAnalyze), nrow(pos_neg_group), nrow(neg_pos_group));

# Mittleres Alter der Teilnehmer
lgr$info('Das mittlere Alter aller Teilnehmer beträgt %.2f Jahre, SD = %.2f.', mean(dataToAnalyze$v_12), sd(dataToAnalyze$v_12));

# Anteile der Geschlechter
genderRatios = prop.table(table(dataToAnalyze[GENDER_ITEM])) * 100;
genderNames = names(genderRatios);
text = 'Geschlecht der Teilnehmer (%):';
for(i in 1:length(genderNames)) {
  text = paste(text, round(genderRatios[i], digits = 2), genderNames[i], sep = ' ');
  if(i < length(genderNames)) {
    text = paste(text, ',', sep = '');
  }
}
lgr$info(text);

# Anteile der Bildungsabschlüsse
gradRatios = prop.table(table(dataToAnalyze[GRADUATION_ITEM])) * 100;
graduationNames = names(gradRatios);
text = 'Bildungsabschlüsse der Teilnehmer (%):';
for(i in 1:length(graduationNames)) {
  text = paste(text, round(gradRatios[i], digits = 2), graduationNames[i], sep = ' ');
  if(i < length(graduationNames)) {
    text = paste(text, ',', sep = '');
  }
}
lgr$info(text);

# M und SD von Interaktionsbereitschaft
means_pos_neg = na.omit(rowMeans(pos_neg_group[INTERAKTIONSBEREITSCHAFT_ITEMS]));
means_neg_pos = na.omit(rowMeans(neg_pos_group[INTERAKTIONSBEREITSCHAFT_ITEMS]));
lgr$info('Interaktionsbereitschaft, Versuchsgruppe \"Erst positiv, dann negativ\": M = %.4f, SD = %.4f, n = %i',
         mean(means_pos_neg), sd(means_pos_neg), length(means_pos_neg));
lgr$info('Interaktionsbereitschaft, Versuchsgruppe \"Erst negativ, dann positiv\": M = %.4f, SD = %.4f, n = %i',
         mean(means_neg_pos), sd(means_neg_pos), length(means_neg_pos));

# M und SD der einzelnen ALLO-15-Subskalen
means_pos_neg = na.omit(rowMeans(pos_neg_group[AFFECTION_ITEMS]));
means_neg_pos = na.omit(rowMeans(neg_pos_group[AFFECTION_ITEMS]));
lgr$info('Allophilie - Positive Affekte, Versuchsgruppe \"Erst positiv, dann negativ\": M = %.4f, SD = %.4f, n = %i',
         mean(means_pos_neg), sd(means_pos_neg), length(means_pos_neg));
lgr$info('Allophilie - Positive Affekte, Versuchsgruppe \"Erst negativ, dann positiv\": M = %.4f, SD = %.4f, n = %i',
         mean(means_neg_pos), sd(means_neg_pos), length(means_neg_pos));

means_pos_neg = na.omit(rowMeans(pos_neg_group[ENTHUSIASM_ITEMS]));
means_neg_pos = na.omit(rowMeans(neg_pos_group[ENTHUSIASM_ITEMS]));
lgr$info('Allophilie - Enthusiasmus, Versuchsgruppe \"Erst positiv, dann negativ\": M = %.4f, SD = %.4f, n = %i',
         mean(means_pos_neg), sd(means_pos_neg), length(means_pos_neg));
lgr$info('Allophilie - Enthusiasmus, Versuchsgruppe \"Erst negativ, dann positiv\": M = %.4f, SD = %.4f, n = %i',
         mean(means_neg_pos), sd(means_neg_pos), length(means_neg_pos));

print('Skript wurde erfolgreich ausgeführt.');