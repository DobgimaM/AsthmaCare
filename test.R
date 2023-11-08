
library(flextable)
library(officer)

use_df_printer()

#set_flextable_defaults(
#  padding.bottom = 5, 
#  padding.top = 5,
#  padding.left = 5,
#  padding.right = 5,
#  scroll = list(),
#  table.layout = "autofit",
#  theme_fun = theme_vanilla
#)


set_flextable_defaults(
  border.color = "#a9a9a9ff", font.family = "Arial",
  font.size = 9, padding = 2, line_spacing = 1,
  text.align = 'center', theme_fun = theme_vanilla
)



my_table <- flextable(tables_of_results1,cwidth =  0.6, cheight = 0.7)
my_table <- valign(my_table, i = 4, valign = 'center')

#line_spacing(my_table, space = 0.6)

#-------------------------------------------------------------------------------

#set_table_properties(
#  my_table,
#  layout = "autofit",
#  #  width = 0,
#  align = "center",
#  opts_html = list(),
#  opts_word = list(),
#  opts_pdf = list(),
#  word_title = NULL,
#  word_description = NULL
#)
#-------------------------------------------------------------------------------

dim_pretty(my_table, part = 'all')

my_table #%>% save_as_docx( path = "Report.docx")

#print(my_table, target = "Report.docx")

file.edit('Summary table.R')
