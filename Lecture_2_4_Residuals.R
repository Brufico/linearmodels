#'---
#'title: "Linear models"
#'subtitle: "Code: Residuals ; Lecture 2_4"
#'author: "Bruno Fischer Colonimos"
#'date: '`r Sys.Date()`'
#'output:
#'      html_document:
#'          number_sections: yes
#'          theme: readable
#'          toc: yes
#'      word_document:
#'          default
#'      pdf_document:
#'          number_sections: yes
#'          theme: readable
#'          toc: yes
#' ---


#'
#'Data
#'====
data(swiss); par(mfrow = c(1, 1))


fit <- lm(Fertility ~ . , data = swiss);
plot(fit)


