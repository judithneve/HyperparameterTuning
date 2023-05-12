###### THIS FILE WAS CREATED USING GPT4 ######

# Install and load the pdftools package
# install.packages("pdftools")
library(pdftools)

# Specify the file paths of the PDFs to combine
pdf_file1 <- "Report/TitlePage.pdf"
pdf_file2 <- "Report/Report.pdf"
# pdf_file3 <- "Report/SupplementaryMaterials.pdf"

# Specify the output file path for the combined PDF
output_file <- "Report/RM_Thesis_JudithNeve_0070661.pdf"

# Combine the PDFs
pdf_combine(c(pdf_file1, pdf_file2#, pdf_file3
              ), output_file)

# Check if the combined PDF was created successfully
if (file.exists(output_file)) {
  print("PDF files combined successfully.")
} else {
  print("Failed to combine PDF files.")
}
