library(reshape)
library(plyr)
library(dplyr)
library(car)
#SET CORRECT DIRECTIORIES TO TOBII STUDIO SOURCE FILES
datafile <- "D:\\R\\MIPS\\0616\\06072016.tsv"
aoi_file <- "D:\\R\\MIPS\\0616\\06072016aois.tsv"
# function for plotting overlapping histograms by chrisamiller http://stackoverflow.com/questions/3485456/useful-little-functions-in-r
# slightly moddified to enable more breaks and true overlapping by me (PK)
plotOverlappingHist <- function(a, b, colors=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
                                breaks=20, xlim=NULL, ylim=NULL, mTitle = "Overlapping plot", xlabel = "a", ylabel = "Frequency"){
    
    ahist=NULL
    bhist=NULL
    
    ahist=hist(a, breaks = breaks, plot=F)
    bhist=hist(b,breaks = breaks, plot=F)
        
    dist = ahist$breaks[2]-ahist$breaks[1]
    breaks = seq(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks),dist)
    
    ahist=hist(a,breaks=breaks,plot=F)
    bhist=hist(b,breaks=breaks,plot=F)
        
    if(is.null(xlim)){
        xlim = c(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks))
    }
    
    if(is.null(ylim)){
        ylim = c(0,max(ahist$counts,bhist$counts))
    }
    
    plot(ahist, xlim=xlim, ylim=ylim, col=colors[1], main = mTitle, xlab = xlabel, ylab = ylabel)
    plot(bhist, xlim=xlim, ylim=ylim, col=colors[2], add=T)
}

sink("results\\results.txt", split = T)
if (file.exists(datafile) & file.exists("klucz.csv")){
    sprintf("Reading from %s", x = datafile)
} else{print("At least one of needed files is missing: MediaNames.csv or klucz.csv or datafile. Use setwd() and check datafile name.")
sink()}

raw_data <- read.table(file = datafile, sep = "\t", header = TRUE, fileEncoding = "UTF-8-BOM")
# removes columns with all NAs (for unknown reasons Tobii adds such a column in each tsv export file)
raw_data <- Filter(function(x)!all(is.na(x)), raw_data)
#one of participants used |\ (Oem5) key instead of Enter (Return)
raw_data$KeyPressEvent[raw_data$KeyPressEvent == "Oem5"] <- "Return"
#one of participants used Tab instead of CapsLock (Capital) key
raw_data$KeyPressEvent[raw_data$KeyPressEvent == "Tab"] <- "Capital"
raw_data <- droplevels(raw_data)
#mistakes in Participant Number
raw_data$ParticipantName[raw_data$RecordingName == "Rec 23"] <- "P23"
raw_data$ParticipantName[raw_data$RecordingName == "Rec 31"] <- "P31"
key <- read.csv("klucz.csv",sep=";")
# creates data-frame with correct order of images
media_names <- read.table(file = "MediaNames.csv", sep = ";", header = TRUE)
subj_no <- length(levels(raw_data$ParticipantName))
#creates answer key for the tests
ans_key <- as.character(unlist(list(rep(key$norm_pat, subj_no), rep(key$ct_mr, subj_no), rep(key$ich_stroke, subj_no))))
# compares participants' answers with the answer key
dataset <- cbind(raw_data, Correct = ans_key == raw_data$KeyPressEvent)

#change MediaName column of data set from filenames to names such as: ct_mr_01, ich_stroke_18, etc.
for (i in seq(length(levels(dataset$MediaName)))){
     levels(dataset$MediaName)[i] <- as.character(media_names$ImageName[media_names$FileName == levels(dataset$MediaName)[i]])
}
dataset$MediaName <- factor(dataset$MediaName, levels = c("ct_mr_01", "ct_mr_02", "ct_mr_03", "ct_mr_04", "ct_mr_05", "ct_mr_06", "ct_mr_07", "ct_mr_08", "ct_mr_09", "ct_mr_10",
                                                 "ct_mr_11", "ct_mr_12", "ct_mr_13", "ct_mr_14", "ct_mr_15", "ct_mr_16", "ct_mr_17", "ct_mr_18", "ct_mr_19", "ct_mr_20", 
                                                 "norm_pat_01", "norm_pat_02", "norm_pat_03", "norm_pat_04", "norm_pat_05", "norm_pat_06", "norm_pat_07", "norm_pat_08", "norm_pat_09", "norm_pat_10",
                                                 "norm_pat_11", "norm_pat_12", "norm_pat_13", "norm_pat_14", "norm_pat_15", "norm_pat_16", "norm_pat_17", "norm_pat_18", "norm_pat_19", "norm_pat_20",
                                                 "ich_stroke_01", "ich_stroke_02", "ich_stroke_03", "ich_stroke_04", "ich_stroke_05", "ich_stroke_06", "ich_stroke_07", "ich_stroke_08", "ich_stroke_09", "ich_stroke_10",
                                                 "ich_stroke_11", "ich_stroke_12", "ich_stroke_13", "ich_stroke_14", "ich_stroke_15", "ich_stroke_16", "ich_stroke_17", "ich_stroke_18", "ich_stroke_19", "ich_stroke_20"))

# creates first two columns of the data.frame (participants names as row.names and X.Images.Value as With_without)
participants = data.frame(Participant = levels(dataset$ParticipantName))
participants <- cbind(participants, With_without = NA)
for (participant in participants$Participant){
    participants[participants$Participant == participant,"With_without"] <- as.character(dataset[dataset$ParticipantName == participant,]$X.Images.Value[1])
}

# adds three columns for each of 60 images (time to answer, participant's answer and answer correctness)

for (image in levels(dataset$MediaName)){
    participants <- cbind(participants, subset(dataset, MediaName == image, select = c(SegmentDuration, KeyPressEvent, Correct)))
    num_names <- length(names(participants))
    names(participants)[(num_names-2):num_names] <- c(paste0(image, "_TimeToAns"), paste0(image, "_Ans"), 
                                                        paste0(image, "_Correct"))
}
# resets strange rownames
rownames(participants) <- NULL

num_with <- sum(participants$With_without == "with")
num_without <- sum(participants$With_without == "without")
sprintf("Number of participants in *with* group: %i", num_with)
sprintf("Number of participants in *without* group: %i", num_without)
#create three data.frames with number of correct answers for each image in each group (with and without images).
#WARNING: num_with doesn't have to be equal to num_without

answers_ct_mr <- grep("^ct_mr.*_Correct", names(participants))
correct_answers_ct_mr <- data.frame(row.names = c("with", "without"))
for (i in seq(length(answers_ct_mr))){
    correct_answers_ct_mr <- cbind(correct_answers_ct_mr, c(sum(participants[participants$With_without == "with",answers_ct_mr][,i]),
                                                            sum(participants[participants$With_without == "without",answers_ct_mr][,i])))
    names(correct_answers_ct_mr)[length(names(correct_answers_ct_mr))] <- paste0("ct_mr_", i)
}

answers_norm_pat <- grep("^norm_pat.*_Correct", names(participants))
correct_answers_norm_pat <- data.frame(row.names = c("with", "without"))
for (i in seq(length(answers_norm_pat))){
    correct_answers_norm_pat <- cbind(correct_answers_norm_pat, c(sum(participants[participants$With_without == "with",answers_norm_pat][,i]),
                                                            sum(participants[participants$With_without == "without",answers_norm_pat][,i])))
    names(correct_answers_norm_pat)[length(names(correct_answers_norm_pat))] <- paste0("norm_pat_", i)
}

answers_ich_stroke <- grep("^ich_stroke.*_Correct", names(participants))
correct_answers_ich_stroke <- data.frame(row.names = c("with", "without"))
for (i in seq(length(answers_norm_pat))){
    correct_answers_ich_stroke <- cbind(correct_answers_ich_stroke, c(sum(participants[participants$With_without == "with",answers_ich_stroke][,i]),
                                                                  sum(participants[participants$With_without == "without",answers_ich_stroke][,i])))
    names(correct_answers_ich_stroke)[length(names(correct_answers_ich_stroke))] <- paste0("ich_stroke_", i)
}

cat("Number of correct answers in *ct_mr* test:\n")
cat("\n")
print(correct_answers_ct_mr)
cat("\n")
cat("\n")
cat("Number of correct answers in *norm_pat* test:\n")
cat("\n")
print(correct_answers_norm_pat)
cat("\n")
cat("\n")
cat("Number of correct answers in *ich_stroke* test:\n")
cat("\n")
print(correct_answers_ich_stroke)
cat("\n")
cat("\n")

# create data.frame of number of correct answers for each participant in each of three tests
#participants_correct_answers <- data.frame(row.names = participants$row.names, With_without = participants$With_without, 
#                                           ct_mr = rowSums(participants[, answers_ct_mr]), norm_pat = rowSums(participants[, answers_norm_pat]),
#                                           ich_stroke = rowSums(participants[, answers_ich_stroke]))

#use reshape package to make it faster :)
temp <- melt(dataset, id.vars = c("StudioTestName", "ParticipantName", "X.Images.Value"), measure.vars = "Correct")
participants_correct_answers <- cast(temp, formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = sum)
rm(temp)

#correct order of columns
participants_correct_answers <- participants_correct_answers[c("ParticipantName","X.Images.Value", 
                                                               "ct_mr_Correct", "norm_pat_Correct", "ich_stroke_Correct")]

# create data.frame of total times to answer for each participant in each test

temp <- melt(dataset, id.vars = c("StudioTestName", "ParticipantName", "X.Images.Value"), measure.vars = "SegmentDuration")
participants_total_time <- cast(temp, formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = sum)
rm(temp)
names(participants_total_time)[names(participants_total_time) == "ct_mr_SegmentDuration"] <- "ct_mr_TotalTime"
names(participants_total_time)[names(participants_total_time) == "norm_pat_SegmentDuration"] <- "norm_pat_TotalTime"
names(participants_total_time)[names(participants_total_time) == "ich_stroke_SegmentDuration"] <- "ich_stroke_TotalTime"

#correct order of columns
participants_total_time <- participants_total_time[c("ParticipantName","X.Images.Value", 
                                                               "ct_mr_TotalTime", "norm_pat_TotalTime", "ich_stroke_TotalTime")]

cat("Number of correct answers by participant\n")
print(participants_correct_answers)
cat("\n\n")


cat("QUARTILES\n\n")
cat("CT_MR - number of correct answers in *with* group:\n")
print(quantile(participants_correct_answers[participants_correct_answers$X.Images.Value == "with",]$ct_mr_Correct))
cat("\n")

cat("CT_MR - number of correct answers in *without* group:\n")
print(quantile(participants_correct_answers[participants_correct_answers$X.Images.Value == "without",]$ct_mr_Correct))
cat("\n")

cat("NORM_PAT - number of correct answers in *with* group:\n")
print(quantile(participants_correct_answers[participants_correct_answers$X.Images.Value == "with",]$norm_pat_Correct))
cat("\n")

cat("NORM_PAT - number of correct answers in *without* group:\n")
print(quantile(participants_correct_answers[participants_correct_answers$X.Images.Value == "without",]$norm_pat_Correct))
cat("\n")

cat("ICH_STROKE - number of correct answers in *with* group:\n")
print(quantile(participants_correct_answers[participants_correct_answers$X.Images.Value == "with",]$ich_stroke_Correct))
cat("\n")

cat("ICH_STROKE - number of correct answers in *without* group:\n")
print(quantile(participants_correct_answers[participants_correct_answers$X.Images.Value == "without",]$ich_stroke_Correct))
cat("\n")


#means to correct answers

mean_corr_ans <- cast(melt(dataset[dataset$Correct == TRUE,], id.vars = c("StudioTestName", "X.Images.Value"), measure.vars = "SegmentDuration"),
                      formula = StudioTestName + X.Images.Value ~ ..., fun.aggregate = mean)

cat("Mean time to correct answer in each test")
print(mean_corr_ans)
cat("\n")


#creates three data.frames with mean times to correct answer in each of two groups (with and without images) - one for each of three tests
ct_mr_time_to_corr_ans <- data.frame(row.names = c("with", "without"))
for (i in seq(length(answers_ct_mr))){
    ct_mr_time_to_corr_ans <- cbind(ct_mr_time_to_corr_ans, c(mean(subset(participants, With_without == "with" & participants[,answers_ct_mr[i]] == TRUE)[,answers_ct_mr[i]-2]),
                                                              mean(subset(participants, With_without == "without" & participants[,answers_ct_mr[i]] == TRUE)[,answers_ct_mr[i]-2])))
    names(ct_mr_time_to_corr_ans)[length(names(ct_mr_time_to_corr_ans))] <- paste0("ct_mr_", i,"_MeanTimeToCorrAns")
    ct_mr_time_to_corr_ans <- cbind(ct_mr_time_to_corr_ans, c(sd(subset(participants, With_without == "with" & participants[,answers_ct_mr[i]] == TRUE)[,answers_ct_mr[i]-2]),
                                                              sd(subset(participants, With_without == "without" & participants[,answers_ct_mr[i]] == TRUE)[,answers_ct_mr[i]-2])))
    names(ct_mr_time_to_corr_ans)[length(names(ct_mr_time_to_corr_ans))] <- paste0("ct_mr_", i,"_SDofTimeToCorrAns")
}

norm_pat_time_to_corr_ans <- data.frame(row.names = c("with", "without"))
for (i in seq(length(answers_norm_pat))){
    norm_pat_time_to_corr_ans <- cbind(norm_pat_time_to_corr_ans, c(mean(subset(participants, With_without == "with" & participants[,answers_norm_pat[i]] == TRUE)[,answers_norm_pat[i]-2]),
                                                                    mean(subset(participants, With_without == "without" & participants[,answers_norm_pat[i]] == TRUE)[,answers_norm_pat[i]-2])))
    names(norm_pat_time_to_corr_ans)[length(names(norm_pat_time_to_corr_ans))] <- paste0("norm_pat_", i,"_MeanTimeToCorrAns")
    norm_pat_time_to_corr_ans <- cbind(norm_pat_time_to_corr_ans, c(sd(subset(participants, With_without == "with" & participants[,answers_norm_pat[i]] == TRUE)[,answers_norm_pat[i]-2]),
                                                                    sd(subset(participants, With_without == "without" & participants[,answers_norm_pat[i]] == TRUE)[,answers_norm_pat[i]-2])))
    names(norm_pat_time_to_corr_ans)[length(names(norm_pat_time_to_corr_ans))] <- paste0("norm_pat_", i,"_SDofTimeToCorrAns")
}

ich_stroke_time_to_corr_ans <- data.frame(row.names = c("with", "without"))
for (i in seq(length(answers_ich_stroke))){
    ich_stroke_time_to_corr_ans <- cbind(ich_stroke_time_to_corr_ans, c(mean(subset(participants, With_without == "with" & participants[,answers_ich_stroke[i]] == TRUE)[,answers_ich_stroke[i]-2]),
                                                                        mean(subset(participants, With_without == "without" & participants[,answers_ich_stroke[i]] == TRUE)[,answers_ich_stroke[i]-2])))
    names(ich_stroke_time_to_corr_ans)[length(names(ich_stroke_time_to_corr_ans))] <- paste0("ich_stroke_", i,"_MeanTimeToCorrAns")
    ich_stroke_time_to_corr_ans <- cbind(ich_stroke_time_to_corr_ans, c(sd(subset(participants, With_without == "with" & participants[,answers_ich_stroke[i]] == TRUE)[,answers_ich_stroke[i]-2]),
                                                                        sd(subset(participants, With_without == "without" & participants[,answers_ich_stroke[i]] == TRUE)[,answers_ich_stroke[i]-2])))
    names(ich_stroke_time_to_corr_ans)[length(names(ich_stroke_time_to_corr_ans))] <- paste0("ich_stroke_", i,"_SDofTimeToCorrAns")
}

cat("Mean time to correct answer in *ct_mr* test:\n")
cat("\n")
print(ct_mr_time_to_corr_ans)
cat("\n")
cat("\n")
cat("Mean time to correct answer in *norm_pat* test:\n")
cat("\n")
print(norm_pat_time_to_corr_ans)
cat("\n")
cat("\n")
cat("Mean time to correct answer in *ich_stroke* test:\n")
cat("\n")
print(ich_stroke_time_to_corr_ans)
cat("\n")
cat("\n")

sink()
print("Created results.txt in working directory")
write.csv2(participants, "participants_answers.csv")
print("Created participants_answers.csv in working directory")
#

time_to_ans <- grep("*_TimeToAns", names(participants))

#images summarizing the obtained data
#set plot grid dimensions
num_plots <- length(participants_correct_answers$ParticipantName)
x_dims <- round(sqrt(num_plots))
if (num_plots %% x_dims == 0){
    y_dims <- num_plots %/% x_dims 
} else {y_dims <- (num_plots%/% x_dims) + 1}

png("results\\participants_correct.png", width = 480 * x_dims, height = 480 * y_dims)
par(mfrow = c(y_dims,x_dims))
for (p in seq(1, num_plots)){
    if (participants_correct_answers$X.Images.Value[p] == "with"){
        colors = heat.colors(2)
    } else {
        colors = terrain.colors(2)
    }
    temp <- matrix(c(participants_correct_answers[p, 3:5], 20 - participants_correct_answers[p, 3:5]), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Correct", "Wrong"), c("ct_mr", "norm_pat", "ich_stroke")))
    barplot(temp, col = colors, width = 2, 
            main = sprintf("Answers of %s (group %s images)", participants_correct_answers$Participant[p],
                           participants_correct_answers$X.Images.Value[p]))
    legend("topright", fill = colors, legend = rownames(temp))
    
}
dev.off()

png("results\\any_ans.png", width = 960, height = 960)
par(mfrow = c(2, 2))
without <- dataset[dataset$X.Images.Value == "without",]$SegmentDuration
with <- dataset[dataset$X.Images.Value == "with",]$SegmentDuration
plotOverlappingHist(without, with, breaks = 30, mTitle = "Time to any answer - all tests", xlabel = "Time (ms)")
legend("topright", fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), legend = c("Without images", "With images"))
rm(with)
rm(without)

for(test in c("ct_mr", "norm_pat", "ich_stroke")){
    without <- dataset[dataset$X.Images.Value == "without" & dataset$StudioTestName == test,]$SegmentDuration
    with <- dataset[dataset$X.Images.Value == "with" & dataset$StudioTestName == test,]$SegmentDuration
    plotOverlappingHist(without, with, breaks = 30, mTitle = sprintf("Time to any answer - %s", test), xlabel = "Time (ms)")
    legend("topright", fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), legend = c("Without images", "With images"))
    rm(with)
    rm(without)
}
dev.off()


png("results\\correct_ans.png", width = 960, height = 960)
par(mfrow = c(2, 2))
without <- dataset[dataset$X.Images.Value == "without" & dataset$Correct == TRUE,]$SegmentDuration
with <- dataset[dataset$X.Images.Value == "with" & dataset$Correct == TRUE,]$SegmentDuration
plotOverlappingHist(without, with, breaks = 30, mTitle = "TIme to correct answer - all tests", xlabel = "Time (ms)")
legend("topright", fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), legend = c("Without images", "With images"))
rm(with)
rm(without)

for(test in c("ct_mr", "norm_pat", "ich_stroke")){
    without <- dataset[dataset$X.Images.Value == "without" & dataset$Correct == TRUE & dataset$StudioTestName == test,]$SegmentDuration
    with <- dataset[dataset$X.Images.Value == "with" & dataset$Correct == TRUE & dataset$StudioTestName == test,]$SegmentDuration
    plotOverlappingHist(without, with, breaks = 30, mTitle = sprintf("Time to correct answer - %s", test), xlabel = "Time (ms)")
    legend("topright", fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), legend = c("Without images", "With images"))
    rm(with)
    rm(without)
}
dev.off()

# time vs correctness
# create scatterplots for each test - total time to answer vs number of correct answers. Two participant groups
# (with and without images) have different color and markers
time_vs_ans <- cbind(participants_correct_answers, participants_total_time[c("ct_mr_TotalTime", "norm_pat_TotalTime", "ich_stroke_TotalTime")])
time_vs_ans$TotalCorrect <- time_vs_ans$ct_mr_Correct + time_vs_ans$norm_pat_Correct + time_vs_ans$ich_stroke_Correct
time_vs_ans$TotalTime <- time_vs_ans$ct_mr_TotalTime + time_vs_ans$norm_pat_TotalTime +  + time_vs_ans$ich_stroke_TotalTime

# time vs correct - CT_MR TEST
png("results\\time_vs_correct_ct_mr.png", width = 960, height = 960)
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
scatterplot(ct_mr_Correct ~ ct_mr_TotalTime | X.Images.Value, data = time_vs_ans, boxplots = "xy", main = "Time to ans vs. Correctness - ct_mr", xlab = "Total time to answer [ms]", ylab = "N of correct answers",
            legend.title = "Participant group", legend.coords = "topright")
dev.off()

# time vs correct - NORM_PAT TEST
png("results\\time_vs_correct_norm_pat.png", width = 960, height = 960)
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
scatterplot(norm_pat_Correct ~ norm_pat_TotalTime | X.Images.Value, data = time_vs_ans, boxplots = "xy", main = "Time to ans vs. Correctness - norm_pat", xlab = "Total time to answer [ms]", ylab = "N of correct answers",
            legend.title = "Participant group", legend.coords = "topright")
dev.off()

# time vs correct - ICH_STROKE TEST
png("results\\time_vs_correct_ich_stroke.png", width = 960, height = 960)
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
scatterplot(ich_stroke_Correct ~ ich_stroke_TotalTime | X.Images.Value, data = time_vs_ans, boxplots = "xy", main = "Time to ans vs. Correctness - ich_stroke", xlab = "Total time to answer [ms]", ylab = "N of correct answers",
            legend.title = "Participant group", legend.coords = "topright")
dev.off()

# time vs correct - ALL TESTS
png("results\\time_vs_correct_all.png", width = 960, height = 960)
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
scatterplot(TotalCorrect ~ TotalTime | X.Images.Value, data = time_vs_ans, boxplots = "xy", main = "Time to ans vs. Correctness - all tests", xlab = "Total time to answer [ms]", ylab = "N of correct answers",
            legend.title = "Participant group", legend.coords = "topright")
dev.off()


##### FIXATION AND EYE-TRACKING DATA
#read all data with AOIs etc. as full
full <- read.table(file = aoi_file, header = TRUE, sep = "\t", fileEncoding = "UTF-8-BOM")

#total number of fixations for each media for each participant
ct_mr_tot_fix <- cast(melt(full[full$StudioTestName == "ct_mr",], id.vars = c("ParticipantName", "X.Images.Value", "SegmentName"), measure.vars = "FixationIndex"), formula = ParticipantName + X.Images.Value ~ SegmentName + variable, fun.aggregate = max, na.rm = TRUE)
ct_mr_tot_fix[ct_mr_tot_fix == -Inf] <- NA

norm_pat_tot_fix <- cast(melt(full[full$StudioTestName == "norm_pat",], id.vars = c("ParticipantName", "X.Images.Value", "SegmentName"), measure.vars = "FixationIndex"), formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = max, na.rm = TRUE)
norm_pat_tot_fix[norm_pat_tot_fix == -Inf] <- NA

ich_stroke_tot_fix <- cast(melt(full[full$StudioTestName == "ich_stroke",], id.vars = c("ParticipantName", "X.Images.Value", "SegmentName"), measure.vars = "FixationIndex"), formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = max, na.rm = TRUE)
ich_stroke_tot_fix[ich_stroke_tot_fix == -Inf] <- NA

#number of fixations before

#get cols numbers of AOI.Hit variables for each test
ct_mr_AOIs_cols <- grep("AOI\\.ct_mr_\\d*\\.Hit", names(full))
norm_pat_AOIs_cols <- grep("AOI\\.norm_pat_\\d*.*\\.Hit", names(full))
ich_stroke_AOIs_cols <- grep("AOI\\.ich_stroke_\\d*.*\\.Hit", names(full))

#create first two columns for each data.frame
first_cols <- full[,c("ParticipantName", "X.Images.Value")][!duplicated(full[,c("ParticipantName", "X.Images.Value")]),]
row.names(first_cols) <- NULL

#create empty data.frame to collect "fixations before" counts
norm_pat_fix_before <- cbind(first_cols, as.data.frame(matrix(nrow = nrow(first_cols), 
                                                              ncol = length(norm_pat_AOIs_cols), dimnames = list(NULL, names(full)[norm_pat_AOIs_cols]))))
# "fixations before" count for each participant
for (participant_num in seq(1, nrow(norm_pat_fix_before))){
    #temporary data.frame of participant's fixations
    participant_AOIs <- full[(full$ParticipantName == norm_pat_fix_before$ParticipantName[participant_num] & full$StudioTestName == "norm_pat"),
                             c("ParticipantName", "FixationIndex",names(full)[norm_pat_AOIs_cols])]
    # "fixations" before for each AOI
    for(AOI_hit in names(norm_pat_fix_before)[3:ncol(norm_pat_fix_before)]){
        # check if participant fixated on media and AOI
        index <- which.max(participant_AOIs[,AOI_hit])
        # get fixation index of first fixation on AOI
        fix_num <- participant_AOIs$FixationIndex[index]
        # check if fixated on AOI
        if(participant_AOIs[,AOI_hit][index] == 1){
            if (is.na(fix_num)){
                norm_pat_fix_before[participant_num,AOI_hit] <- NA
            }else{
                #if fixated on AOI set fix_before
                norm_pat_fix_before[participant_num,AOI_hit] <- fix_num - 1
            }
        }else{
            norm_pat_fix_before[participant_num,AOI_hit] <- NA
        }
    }
    rm(AOI_hit)
}

ct_mr_fix_before <- cbind(first_cols, as.data.frame(matrix(nrow = nrow(first_cols), 
                                                           ncol = length(ct_mr_AOIs_cols), dimnames = list(NULL, names(full)[ct_mr_AOIs_cols]))))
for (participant_num in seq(1, nrow(ct_mr_fix_before))){
    participant_AOIs <- full[(full$ParticipantName == ct_mr_fix_before$ParticipantName[participant_num] & full$StudioTestName == "ct_mr"),
                             c("ParticipantName", "FixationIndex",names(full)[ct_mr_AOIs_cols])]
    for(AOI_hit in names(ct_mr_fix_before)[3:ncol(ct_mr_fix_before)]){
        index <- which.max(participant_AOIs[,AOI_hit])
        fix_num <- participant_AOIs$FixationIndex[index]
        if(participant_AOIs[,AOI_hit][index] == 1){
            if (is.na(fix_num)){
                ct_mr_fix_before[participant_num,AOI_hit] <- NA
            }else{
                ct_mr_fix_before[participant_num,AOI_hit] <- fix_num - 1
            }
        }else{
            ct_mr_fix_before[participant_num,AOI_hit] <- NA
        }
    }
    
}

ich_stroke_fix_before <- cbind(first_cols, as.data.frame(matrix(nrow = nrow(first_cols), 
                                                           ncol = length(ich_stroke_AOIs_cols), dimnames = list(NULL, names(full)[ich_stroke_AOIs_cols]))))
for (participant_num in seq(1, nrow(ich_stroke_fix_before))){
    participant_AOIs <- full[(full$ParticipantName == ich_stroke_fix_before$ParticipantName[participant_num] & full$StudioTestName == "ich_stroke"),
                             c("ParticipantName", "FixationIndex",names(full)[ich_stroke_AOIs_cols])]
    for(AOI_hit in names(ich_stroke_fix_before)[3:ncol(ich_stroke_fix_before)]){
        index <- which.max(participant_AOIs[,AOI_hit])
        fix_num <- participant_AOIs$FixationIndex[index]
        if(participant_AOIs[,AOI_hit][index] == 1){
            if (is.na(fix_num)){
                ich_stroke_fix_before[participant_num,AOI_hit] <- NA
            }else{
                ich_stroke_fix_before[participant_num,AOI_hit] <- fix_num - 1
            }
        }else{
            ich_stroke_fix_before[participant_num,AOI_hit] <- NA
        }
    }
    
}

#reduce number of variables by grouping single media AOIs in one column
norm_pat_fix_before2 <- first_cols
for (i in seq(1, 20)){
    
    temp <- as.data.frame(norm_pat_fix_before[,grep(paste0("AOI\\.norm_pat_",i,"(\\.Hit|_\\d*\\.Hit)"), names(norm_pat_fix_before))])
    if(dim(temp)[2] == 1){
        norm_pat_fix_before2 <- cbind(norm_pat_fix_before2, norm_pat_fix_before[,paste0("AOI.norm_pat_",i,".Hit")])
    }else{
        norm_pat_fix_before2 <- cbind(norm_pat_fix_before2, apply(temp, 1, min, na.rm = TRUE))
    }
    names(norm_pat_fix_before2)[length(names(norm_pat_fix_before2))] <- paste0("AOI.norm_pat_",i,".Hit")
}
norm_pat_fix_before2[norm_pat_fix_before2 == Inf] <- NA

ich_stroke_fix_before2 <- first_cols
for (i in seq(1, 20)){
    
    temp <- as.data.frame(ich_stroke_fix_before[,grep(paste0("AOI\\.ich_stroke_",i,"(\\.Hit|_\\d*\\.Hit)"), names(ich_stroke_fix_before))])
    if(dim(temp)[2] == 1){
        ich_stroke_fix_before2 <- cbind(ich_stroke_fix_before2, ich_stroke_fix_before[,paste0("AOI.ich_stroke_",i,".Hit")])
    }else{
        ich_stroke_fix_before2 <- cbind(ich_stroke_fix_before2, apply(temp, 1, min, na.rm = TRUE))
         }
    names(ich_stroke_fix_before2)[length(names(ich_stroke_fix_before2))] <- paste0("AOI.ich_stroke_",i,".Hit")
}
ich_stroke_fix_before2[ich_stroke_fix_before2 == Inf] <- NA


#images and plots - eyetracking data
png("results\\ct_mr_tot_fix.png", width = 2000, height = 2000)
par(mfrow = c(5, 4), cex.axis = 1.5, cex.lab = 1.5)
for (i in seq(1,20)){
    boxplot(ct_mr_tot_fix[ct_mr_tot_fix$X.Images.Value == "with",i + 2],ct_mr_tot_fix[ct_mr_tot_fix$X.Images.Value == "without",i + 2],
            names = c("with", "without"), main = paste0("ct_mr ", names(ct_mr_tot_fix)[i+2]), ylab = "N of fixations", ylim = c(0,80), varwidth = TRUE)
    
}
dev.off()

png("results\\norm_pat_tot_fix.png", width = 2000, height = 2000)
par(mfrow = c(5, 4), cex.axis = 1.5, cex.lab = 1.5)
for (i in seq(1,20)){
    boxplot(norm_pat_tot_fix[norm_pat_tot_fix$X.Images.Value == "with",i + 2],norm_pat_tot_fix[norm_pat_tot_fix$X.Images.Value == "without",i + 2],
            names = c("with", "without"), main = paste0("norm_pat ", names(norm_pat_tot_fix)[i+2]), ylab = "N of fixations", ylim = c(0,60), varwidth = TRUE)
            
}
dev.off()

png("results\\ich_stroke_tot_fix.png", width = 2000, height = 2000)
par(mfrow = c(5, 4), cex.axis = 1.5, cex.lab = 1.5)
for (i in seq(1,20)){
    boxplot(ich_stroke_tot_fix[ich_stroke_tot_fix$X.Images.Value == "with",i + 2],ich_stroke_tot_fix[ich_stroke_tot_fix$X.Images.Value == "without",i + 2],
            names = c("with", "without"), main = paste0("ich_stroke ", names(ich_stroke_tot_fix)[i+2]), ylab = "N of fixations", ylim = c(0, 80), varwidth = TRUE)
            
}
dev.off()

png("results\\norm_pat_fix_before.png", width = 2000, height = 2000)
par(mfrow = c(5, 4), cex.axis = 1.5, cex.lab = 1.5)
for (i in seq(1,20)){
    boxplot(norm_pat_fix_before2[norm_pat_fix_before2$X.Images.Value == "with",paste0("AOI.norm_pat_",i,".Hit")],norm_pat_fix_before2[norm_pat_fix_before2$X.Images.Value == "without",paste0("AOI.norm_pat_",i,".Hit")],
            names = c("with", "without"), main = sprintf("norm_pat_%i - FixBefore AOI Hit", i), ylab = "N of fixations", ylim = c(0,18), varwidth = TRUE)
    
}
dev.off()

png("results\\ich_stroke_fix_before.png", width = 2000, height = 2000)
par(mfrow = c(5, 4), cex.axis = 1.5, cex.lab = 1.5)
for (i in seq(1,20)){
    boxplot(ich_stroke_fix_before2[ich_stroke_fix_before2$X.Images.Value == "with",paste0("AOI.ich_stroke_",i,".Hit")],ich_stroke_fix_before2[ich_stroke_fix_before2$X.Images.Value == "without",paste0("AOI.ich_stroke_",i,".Hit")],
            names = c("with", "without"), main = sprintf("ich_stroke_%i - FixBefore AOI Hit", i), ylab = "N of fixations", ylim = c(0,65), varwidth = TRUE)

}
dev.off()

# time to answer
norm_pat_durations <- cast(melt(full[full$StudioTestName == "norm_pat",], id.vars = c("ParticipantName", "X.Images.Value", "SegmentName"), measure.vars = "SegmentDuration"),
                       formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = max, na.rm = TRUE)
norm_pat_fix_time <- data.frame(cbind(norm_pat_durations[,c(1,2)],(norm_pat_tot_fix[,3:22]/norm_pat_durations[,3:22])*1000))
names(norm_pat_fix_time) <- sapply(names(norm_pat_fix_time), gsub, pattern = "FixationIndex", replacement = "FixPerSec")

ct_mr_durations <- cast(melt(full[full$StudioTestName == "ct_mr",], id.vars = c("ParticipantName", "X.Images.Value", "SegmentName"), measure.vars = "SegmentDuration"),
                           formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = max, na.rm = TRUE)
ct_mr_fix_time <- data.frame(cbind(ct_mr_durations[,c(1,2)],(ct_mr_tot_fix[,3:22]/ct_mr_durations[,3:22])*1000))
names(ct_mr_fix_time) <- sapply(names(ct_mr_fix_time), gsub, pattern = "FixationIndex", replacement = "FixPerSec")

ich_stroke_durations <- cast(melt(full[full$StudioTestName == "ich_stroke",], id.vars = c("ParticipantName", "X.Images.Value", "SegmentName"), measure.vars = "SegmentDuration"),
                           formula = ParticipantName + X.Images.Value ~ ..., fun.aggregate = max, na.rm = TRUE)
ich_stroke_fix_time <- data.frame(cbind(ich_stroke_durations[,c(1,2)],(ich_stroke_tot_fix[,3:22]/ich_stroke_durations[,3:22])*1000))
names(ich_stroke_fix_time) <- sapply(names(ich_stroke_fix_time), gsub, pattern = "FixationIndex", replacement = "FixPerSec")


# list of correct answers in each trial
norm_pat_full_corr <- filter(dataset, StudioTestName == "norm_pat") %>% select(
    ParticipantName, X.Images.Value, MediaName, Correct) %>% cast(formula = ParticipantName + X.Images.Value ~ MediaName, value = "Correct")

ct_mr_full_corr <- filter(dataset, StudioTestName == "ct_mr") %>% select(
    ParticipantName, X.Images.Value, MediaName, Correct) %>% cast(formula = ParticipantName + X.Images.Value ~ MediaName, value = "Correct")

ich_stroke_full_corr <- filter(dataset, StudioTestName == "ich_stroke") %>% select(
    ParticipantName, X.Images.Value, MediaName, Correct) %>% cast(formula = ParticipantName + X.Images.Value ~ MediaName, value = "Correct")


#number of media answered correctly by all participants but one:
drop_participant <- data.frame(first_cols)

#ct_mr
x <- vector(mode = "numeric", length = participant_num)
for (participant in 1:participant_num){
    all_correct <- 0
    for (i in 3:length(names(ct_mr_full_corr))){
        if(sum(ct_mr_full_corr[-participant,i])/length(ct_mr_full_corr[-participant,i]) == 1){
            all_correct <- all_correct + 1
            print(all_correct)
        }
    }
    x[participant] <- all_correct
}
drop_participant <- cbind(drop_participant, ct_mr = x)
names(drop_participant)[length(names(drop_participant))] <- "ct_mr"

#norm_pat
x <- vector(mode = "numeric", length = participant_num)
for (participant in 1:participant_num){
    all_correct <- 0
    for (i in 3:length(names(norm_pat_full_corr))){
        if(sum(norm_pat_full_corr[-participant,i])/length(norm_pat_full_corr[-participant,i]) == 1){
            all_correct <- all_correct + 1
            print(all_correct)
        }
    }
    x[participant] <- all_correct
}
drop_participant <- cbind(drop_participant, norm_pat = x)
names(drop_participant)[length(names(drop_participant))] <- "norm_pat"

#ich_stroke
x <- vector(mode = "numeric", length = participant_num)
for (participant in 1:participant_num){
    all_correct <- 0
    for (i in 3:length(names(ich_stroke_full_corr))){
        if(sum(ich_stroke_full_corr[-participant,i])/length(ich_stroke_full_corr[-participant,i]) == 1){
            all_correct <- all_correct + 1
            print(all_correct)
        }
    }
    x[participant] <- all_correct
}
drop_participant <- cbind(drop_participant, ich_stroke = x)
names(drop_participant)[length(names(drop_participant))] <- "ich_stroke"

drop_participant <- data.frame(drop_participant, Sum = (drop_participant[,3] + drop_participant[,4] + drop_participant[,5]))

# number of media answered with at most 1 wrong answer by all participants but one
drop_participant_one_wrong <- data.frame(first_cols)

#ct_mr
x <- vector(mode = "numeric", length = participant_num)
for (participant in 1:participant_num){
    all_correct <- 0
    for (i in 3:length(names(ct_mr_full_corr))){
        if(sum(ct_mr_full_corr[-participant,i]) - length(ct_mr_full_corr[-participant,i]) >= -1){
            all_correct <- all_correct + 1
            print(all_correct)
        }
    }
    x[participant] <- all_correct
}
drop_participant_one_wrong <- cbind(drop_participant_one_wrong, ct_mr = x)
names(drop_participant_one_wrong)[length(names(drop_participant_one_wrong))] <- "ct_mr"

#norm_pat
x <- vector(mode = "numeric", length = participant_num)
for (participant in 1:participant_num){
    all_correct <- 0
    for (i in 3:length(names(norm_pat_full_corr))){
        if(sum(norm_pat_full_corr[-participant,i]) - length(norm_pat_full_corr[-participant,i]) >= -1){
            all_correct <- all_correct + 1
            print(all_correct)
        }
    }
    x[participant] <- all_correct
}
drop_participant_one_wrong <- cbind(drop_participant_one_wrong, norm_pat = x)
names(drop_participant_one_wrong)[length(names(drop_participant_one_wrong))] <- "norm_pat"

#ich_stroke
x <- vector(mode = "numeric", length = participant_num)
for (participant in 1:participant_num){
    all_correct <- 0
    for (i in 3:length(names(ich_stroke_full_corr))){
        if(sum(ich_stroke_full_corr[-participant,i]) - length(ich_stroke_full_corr[-participant,i]) >= -1){
            all_correct <- all_correct + 1
            print(all_correct)
        }
    }
    x[participant] <- all_correct
}
drop_participant_one_wrong <- cbind(drop_participant_one_wrong, ich_stroke = x)
names(drop_participant_one_wrong)[length(names(drop_participant_one_wrong))] <- "ich_stroke"

drop_participant_one_wrong <- data.frame(drop_participant_one_wrong, Sum = (drop_participant_one_wrong[,3] + drop_participant_one_wrong[,4] + drop_participant_one_wrong[,5]))



#tests
##correct answers

group_with <- filter(participants_correct_answers, X.Images.Value == "with") %>% select(ct_mr_Correct, ich_stroke_Correct, norm_pat_Correct)
group_without <- filter(participants_correct_answers, X.Images.Value == "without") %>% select(ct_mr_Correct, ich_stroke_Correct, norm_pat_Correct)

shapiro.test(group_with$ct_mr_Correct)
shapiro.test(group_without$ct_mr_Correct)

shapiro.test(group_with$norm_pat_Correct)
shapiro.test(group_without$norm_pat_Correct)

shapiro.test(group_with$ich_stroke_Correct)
shapiro.test(group_without$ich_stroke_Correct)

sink("results\\tests.txt")
cat("Number of correct answers in each test \n\n")
wilcox.test(group_with$ct_mr_Correct, group_without$ct_mr_Correct)
wilcox.test(group_with$norm_pat_Correct, group_without$norm_pat_Correct)
wilcox.test(group_with$ich_stroke_Correct, group_without$ich_stroke_Correct)

#t.test(group_with$ct_mr_Correct, group_without$ct_mr_Correct)
#t.test(group_with$norm_pat_Correct, group_without$norm_pat_Correct)
#t.test(group_with$ich_stroke_Correct, group_without$ich_stroke_Correct)

chisq_ct_mr <- matrix(data = c(sum(group_with$ct_mr_Correct),sum(group_without$ct_mr_Correct),
                nrow(group_with)*20-sum(group_with$ct_mr_Correct),nrow(group_without)*20-sum(group_without$ct_mr_Correct)), nrow= 2, ncol = 2)
   
chisq.test(chisq_ct_mr)

chisq_norm_pat <- matrix(data = c(sum(group_with$norm_pat_Correct),sum(group_without$norm_pat_Correct),
                               nrow(group_with)*20-sum(group_with$norm_pat_Correct),nrow(group_without)*20-sum(group_without$norm_pat_Correct)), nrow= 2, ncol = 2)

chisq.test(chisq_norm_pat)

chisq_ich_stroke <- matrix(data = c(sum(group_with$ich_stroke_Correct),sum(group_without$ich_stroke_Correct),
                               nrow(group_with)*20-sum(group_with$ich_stroke_Correct),nrow(group_without)*20-sum(group_without$ich_stroke_Correct)), nrow= 2, ncol = 2)

chisq.test(chisq_ich_stroke)

#time to answer

ct_mr_durations_with <- filter(ct_mr_durations, X.Images.Value == "with")[,3:22] %>% unlist()
ct_mr_durations_without <- filter(ct_mr_durations, X.Images.Value == "without")[,3:22] %>% unlist()

norm_pat_durations_with <- filter(norm_pat_durations, X.Images.Value == "with")[,3:22] %>% unlist()
norm_pat_durations_without <- filter(norm_pat_durations, X.Images.Value == "without")[,3:22] %>% unlist()

ich_stroke_durations_with <- filter(ich_stroke_durations, X.Images.Value == "with")[,3:22] %>% unlist()
ich_stroke_durations_without <- filter(ich_stroke_durations, X.Images.Value == "without")[,3:22] %>% unlist()


cat("Time to any answer in each test\n\n")
sprintf("Mean time to answer ct_mr *with*: %f", mean(ct_mr_durations_with))
sprintf("Mean time to answer ct_mr *without*: %f", mean(ct_mr_durations_without))
wilcox.test(ct_mr_durations_with, ct_mr_durations_without)
cat("\n\n")
sprintf("Mean time to answer norm_pat *with*: %f", mean(norm_pat_durations_with))
sprintf("Mean time to answer norm_pat *without*: %f", mean(norm_pat_durations_without))
wilcox.test(norm_pat_durations_with, norm_pat_durations_without)
cat("\n\n")
sprintf("Mean time to answer ich_stroke *with*: %f", mean(ich_stroke_durations_with))
sprintf("Mean time to answer ich_stroke *without*: %f", mean(ich_stroke_durations_without))
wilcox.test(ich_stroke_durations_with, ich_stroke_durations_without)

#time to correct answer

ct_mr_corr_time_with <- filter(dataset, X.Images.Value == "with" & Correct == T & StudioTestName == "ct_mr") %>% select(SegmentDuration) %>% unlist()
ct_mr_corr_time_without <- filter(dataset, X.Images.Value == "without" & Correct == T & StudioTestName == "ct_mr") %>% select(SegmentDuration) %>% unlist()

norm_pat_corr_time_with <- filter(dataset, X.Images.Value == "with" & Correct == T & StudioTestName == "norm_pat") %>% select(SegmentDuration) %>% unlist()
norm_pat_corr_time_without <- filter(dataset, X.Images.Value == "without" & Correct == T & StudioTestName == "norm_pat") %>% select(SegmentDuration) %>% unlist()

ich_stroke_corr_time_with <- filter(dataset, X.Images.Value == "with" & Correct == T & StudioTestName == "ich_stroke") %>% select(SegmentDuration) %>% unlist()
ich_stroke_corr_time_without <- filter(dataset, X.Images.Value == "without" & Correct == T & StudioTestName == "ich_stroke") %>% select(SegmentDuration) %>% unlist()

cat("Time to correct answer in each test \n\n")
sprintf("Mean time to correct answer ct_mr *with*: %f", mean(ct_mr_corr_time_with))
sprintf("Mean time to correct answer ct_mr *without*: %f", mean(ct_mr_corr_time_without))
wilcox.test(ct_mr_corr_time_with, ct_mr_corr_time_without)
cat("\n\n")
sprintf("Mean time to correct answer norm_pat *with*: %f", mean(norm_pat_corr_time_with))
sprintf("Mean time to correct answer norm_pat *without*: %f", mean(norm_pat_corr_time_without))
wilcox.test(norm_pat_corr_time_with, norm_pat_corr_time_without)
cat("\n\n")
sprintf("Mean time to correct answer ich_stroke *with*: %f", mean(ich_stroke_corr_time_with))
sprintf("Mean time to correct answer ich_stroke *without*: %f", mean(ich_stroke_corr_time_without))
wilcox.test(ich_stroke_corr_time_with, ich_stroke_corr_time_without)

sink()


hist(group_with$ct_mr_Correct)
hist(group_without$ct_mr_Correct)

hist(group_with$norm_pat_Correct)
hist(group_without$norm_pat_Correct)

hist(group_with$ich_stroke_Correct)
hist(group_without$ich_stroke_Correct)

(dataset[dataset$X.Images.Value == "with",])

image1 <- dataset[which(dataset$MediaName == '1_ct_norma_013_2.jpg'),]

image1 <- filter(dataset, MediaName == '1_ct_norma_013_2.jpg')
length(image1[image1$Correct == TRUE,1])/length(image1$Correct)
image1$Correct
#check how many timestamps do repeat (TODO: import to Ogama)
#x <- 0
#for (time_num in seq(2, length(full$RecordingTimestamp))){
#    if(full$RecordingTimestamp[time_num - 1] == full$RecordingTimestamp[time_num]){
#        x <- x+1
#        print(time_num)
#    }
#    
#}
#print ("x=", as.character(x))