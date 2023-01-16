rmarkdown::render("Tim/Report.Rmd",
                  output_file = "wales-report-2017-18a.pdf",
                  output_dir = "/Tim",
                  params = list(current.year = current.year,
                                dp.text = dp.text,
                                dp.tables = dp.tables))



rmarkdown::render("Tim/wales-report-2017-18(No Styling).Rmd",
                  output_file = "wales-report-2017-18a.pdf",
                  output_dir = "/Tim",
                  params = list(current.year = current.year,
                                dp.text = dp.text,
                                dp.tables = dp.tables))





