
rmarkdown::render(input = './code/01-perform.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)
rmarkdown::render(input = './code/02-perform.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)

rmarkdown::render(input = './code/03.a-perform.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)
rmarkdown::render(input = './code/03.b-perform.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)

rmarkdown::render(input = './code/04.a-perform.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)
rmarkdown::render(input = './code/04.b-perform.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)

rmarkdown::render(input = './code/01-flow.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)
rmarkdown::render(input = './code/02-flow.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)

rmarkdown::render(input = './code/03.a-flow.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)
rmarkdown::render(input = './code/03.b-flow.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)

rmarkdown::render(input = './code/04.a-flow.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)
rmarkdown::render(input = './code/04.b-flow.Rmd', output_dir = './results',
                  clean = T, output_format = "all", quiet=F)

