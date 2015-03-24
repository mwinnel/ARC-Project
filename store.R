## now defunct - replaced by single line call in loop.R using lists()


if(dim(dataset.mix$TempC1)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TempC1")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TempC1[, 1:2]), as.double(dataset.mix$TempC1[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TempC")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TempC1 <- rbind(dataset.TempC1, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_TempC1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else{
    dataset.TempC1 <- cbind(as.data.frame(dataset.mix$TempC1[, 1:2]), as.double(dataset.mix$TempC1[, 3]))
    colnames(dataset.TempC1) <- c("Date", "Time", "TempC")
    dataset.TempC1 <-cbind(dataset.TempC1, MINUTES = HandleSuppliedTime2(dataset.TempC1, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TempC1[, 1], dataset.TempC1[, 2], dataset.TempC1[, 3], dataset.TempC1[, 4]), "dataset_TempC1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}

if(dim(dataset.mix$TempC2)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TempC2")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TempC2[, 1:2]), as.double(dataset.mix$TempC2[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TempC")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TempC2 <- rbind(dataset.TempC2, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_TempC2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else{
    dataset.TempC2 <- cbind(as.data.frame(dataset.mix$TempC2[, 1:2]), as.double(dataset.mix$TempC2[, 3]))
    colnames(dataset.TempC2) <- c("Date", "Time", "TempC")
    dataset.TempC2 <-cbind(dataset.TempC2, MINUTES = HandleSuppliedTime2(dataset.TempC2, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TempC2[, 1], dataset.TempC2[, 2], dataset.TempC2[, 3], dataset.TempC2[, 4]), "dataset_TempC2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


if(dim(dataset.mix$TempA1)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TempA1")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TempA1[, 1:2]), as.double(dataset.mix$TempA1[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TempA")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TempA1 <- rbind(dataset.TempA1, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3],returned_data[, 4]), "dataset_TempA1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else{
    dataset.TempA1 <- cbind(as.data.frame(dataset.mix$TempA1[, 1:2]), as.double(dataset.mix$TempA1[, 3]))
    colnames(dataset.TempA1) <- c("Date", "Time", "TempA")
    dataset.TempA1 <-cbind(dataset.TempA1, MINUTES = HandleSuppliedTime2(dataset.TempA1, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TempA1[, 1],dataset.TempA1[, 2],dataset.TempA1[, 3],dataset.TempA1[, 4]), "dataset_TempA1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}

if(dim(dataset.mix$TempA2)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TempA2")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TempA2[, 1:2]), as.double(dataset.mix$TempA2[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TempA")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TempA2 <- rbind(dataset.TempA2, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3],returned_data[, 4]), "dataset_TempA2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else{
    dataset.TempA2 <- cbind(as.data.frame(dataset.mix$TempA2[, 1:2]), as.double(dataset.mix$TempA2[, 3]))
    colnames(dataset.TempA2) <- c("Date", "Time", "TempA")
    dataset.TempA2 <-cbind(dataset.TempA2, MINUTES = HandleSuppliedTime2(dataset.TempA2, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TempA2[, 1],dataset.TempA2[, 2],dataset.TempA2[, 3],dataset.TempA2[, 4]), "dataset_TempA2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


if(dim(dataset.mix$TurbS1)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TurbS1")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TurbS1[, 1:2]), as.double(dataset.mix$TurbS1[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TurbS")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TurbS1 <- rbind(dataset.TurbS1, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_TurbS1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else {
    dataset.TurbS1 <- cbind(as.data.frame(dataset.mix$TurbS1[, 1:2]), as.double(dataset.mix$TurbS1[, 3]))
    colnames(dataset.TurbS1) <- c("Date", "Time", "TurbS")
    dataset.TurbS1 <-cbind(dataset.TurbS1, MINUTES = HandleSuppliedTime2(dataset.TurbS1, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TurbS1[, 1], dataset.TurbS1[, 2], dataset.TurbS1[, 3], dataset.TurbS1[, 4]), "dataset_TurbS1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}

if(dim(dataset.mix$TurbS2)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TurbS2")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TurbS2[, 1:2]), as.double(dataset.mix$TurbS2[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TurbS")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TurbS2 <- rbind(dataset.TurbS2, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_TurbS2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else {
    dataset.TurbS2 <- cbind(as.data.frame(dataset.mix$TurbS2[, 1:2]), as.double(dataset.mix$TurbS2[, 3]))
    colnames(dataset.TurbS2) <- c("Date", "Time", "TurbS")
    dataset.TurbS2 <-cbind(dataset.TurbS2, MINUTES = HandleSuppliedTime2(dataset.TurbS2, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TurbS2[, 1], dataset.TurbS2[, 2], dataset.TurbS2[, 3], dataset.TurbS2[, 4]), "dataset_TurbS2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


if(dim(dataset.mix$TurbA1)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TurbA1")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TurbA1[, 1:2]), as.double(dataset.mix$TurbA1[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TurbA")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TurbA1 <- rbind(dataset.TurbA1, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_TurbA1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else {
    dataset.TurbA1 <- cbind(as.data.frame(dataset.mix$TurbA1[, 1:2]), as.double(dataset.mix$TurbA1[, 3]))
    colnames(dataset.TurbA1) <- c("Date", "Time", "TurbA")
    dataset.TurbA1 <-cbind(dataset.TurbA1, MINUTES = HandleSuppliedTime2(dataset.TurbA1, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TurbA1[, 1], dataset.TurbA1[, 2], dataset.TurbA1[, 3], dataset.TurbA1[, 4]), "dataset_TurbA1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}

if(dim(dataset.mix$TurbA2)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.TurbA2")) {
    returned_data <- cbind(as.data.frame(dataset.mix$TurbA2[, 1:2]), as.double(dataset.mix$TurbA2[, 3]))
    colnames(returned_data) <- c("Date", "Time", "TurbA")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.TurbA2 <- rbind(dataset.TurbA2, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_TurbA2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else {
    dataset.TurbA2 <- cbind(as.data.frame(dataset.mix$TurbA2[, 1:2]), as.double(dataset.mix$TurbA2[, 3]))
    colnames(dataset.TurbA2) <- c("Date", "Time", "TurbA")
    dataset.TurbA2 <-cbind(dataset.TurbA2, MINUTES = HandleSuppliedTime2(dataset.TurbA2, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.TurbA2[, 1], dataset.TurbA2[, 2], dataset.TurbA2[, 3], dataset.TurbA2[, 4]), "dataset_TurbA2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


if(dim(dataset.mix$pH1)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.pH1")) {
    returned_data <- cbind(as.data.frame(dataset.mix$pH1[, 1:2]), as.double(dataset.mix$pH1[, 3]))
    colnames(returned_data) <- c("Date", "Time", "pH")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1,minstart = 4))
    dataset.pH1 <- rbind(dataset.pH1, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_pH1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else{
    dataset.pH1 <- cbind(as.data.frame(dataset.mix$pH1[, 1:2]), as.double(dataset.mix$pH1[, 3]))
    colnames(dataset.pH1) <- c("Date", "Time", "pH")
    dataset.pH1 <-cbind(dataset.pH1, MINUTES = HandleSuppliedTime2(dataset.pH1, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.pH1[, 1], dataset.pH1[, 2], dataset.pH1[, 3], dataset.pH1[, 4]), "dataset_pH1.dat",
                row.names = FALSE, append = TRUE,col.names = FALSE)
  }
}

if(dim(dataset.mix$pH2)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.pH2")) {
    returned_data <- cbind(as.data.frame(dataset.mix$pH2[, 1:2]), as.double(dataset.mix$pH2[, 3]))
    colnames(returned_data) <- c("Date", "Time", "pH")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1,minstart = 4))
    dataset.pH2 <- rbind(dataset.pH2, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_pH2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else{
    dataset.pH2 <- cbind(as.data.frame(dataset.mix$pH2[, 1:2]), as.double(dataset.mix$pH2[, 3]))
    colnames(dataset.pH2) <- c("Date", "Time", "pH")
    dataset.pH2 <-cbind(dataset.pH2, MINUTES = HandleSuppliedTime2(dataset.pH2, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.pH2[, 1], dataset.pH2[, 2], dataset.pH2[, 3], dataset.pH2[, 4]), "dataset_pH2.dat",
                row.names = FALSE, append = TRUE,col.names = FALSE)
  }
}

###redox, disoxy   conductivity

if(dim(dataset.mix$Cond1)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.Cond1")) {
    returned_data <- cbind(as.data.frame(dataset.mix$Cond1[, 1:2]), as.double(dataset.mix$Cond1[, 3]))
    colnames(returned_data) <- c("Date", "Time", "Cond")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.Cond1 <- rbind(dataset.Cond1, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_Cond1.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else {
    dataset.Cond1 <- cbind(as.data.frame(dataset.mix$Cond1[, 1:2]), as.double(dataset.mix$Cond1[, 3]))
    colnames(dataset.Cond1) <- c("Date", "Time", "Cond")
    dataset.Cond1 <-cbind(dataset.Cond1, MINUTES = HandleSuppliedTime2(dataset.Cond1, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.Cond1[, 1], dataset.Cond1[, 2], dataset.Cond1[, 3], dataset.Cond1[, 4]), "dataset_Cond1.dat",
                row.names = FALSE, append = TRUE,col.names = FALSE)
  }
}

if(dim(dataset.mix$Cond2)[1] > 0) {
  UPDATE <- TRUE
  if(exists("dataset.Cond2")) {
    returned_data <- cbind(as.data.frame(dataset.mix$Cond2[, 1:2]), as.double(dataset.mix$Cond2[, 3]))
    colnames(returned_data) <- c("Date", "Time", "Cond")
    returned_data <-cbind(returned_data, MINUTES = HandleSuppliedTime2(returned_data, "%d/%m/%Y", hourstart = 1, minstart = 4))
    dataset.Cond2 <- rbind(dataset.Cond2, returned_data)
    write.table(cbind(returned_data[, 1], returned_data[, 2], returned_data[, 3], returned_data[, 4]), "dataset_Cond2.dat",
                row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  else {
    dataset.Cond2 <- cbind(as.data.frame(dataset.mix$Cond2[, 1:2]), as.double(dataset.mix$Cond2[, 3]))
    colnames(dataset.Cond2) <- c("Date", "Time", "Cond")
    dataset.Cond2 <-cbind(dataset.Cond2, MINUTES = HandleSuppliedTime2(dataset.Cond2, "%d/%m/%Y", hourstart = 1, minstart = 4))
    write.table(cbind(dataset.Cond2[, 1], dataset.Cond2[, 2], dataset.Cond2[, 3], dataset.Cond2[, 4]), "dataset_Cond2.dat",
                row.names = FALSE, append = TRUE,col.names = FALSE)
  }
}