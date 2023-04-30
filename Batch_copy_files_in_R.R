# Copy files in R: single file source, sequentially named copies 
# .. for peer review of seminar papers
#
# sample students
students <- c("Novak","Nemec","Niederle","Novy","Nejezchleba") 
#
# check source file & path
file.exists("~/Desktop/RWD/Hodnoceni_SP.docx") 
#
# generate named file copies to circulate among students
# .. each seminar paper reviewed by two peers 
# (distinct file names needed for feedback recirculation)
#
for(i in students) {
  File1 <- paste("~/Desktop/RWD/Hodnoceni_SP_",i,"_",1,".docx", sep = "")
  File2 <- paste("~/Desktop/RWD/Hodnoceni_SP_",i,"_",2,".docx", sep = "")
  file.copy("~/Desktop/RWD/Hodnoceni_SP.docx",File1)
  file.copy("~/Desktop/RWD/Hodnoceni_SP.docx",File2)
}
