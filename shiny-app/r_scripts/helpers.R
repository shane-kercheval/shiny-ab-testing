test_save_plot <- function(plot, file_name, size_inches=c(5, 8)) {

    stopifnot(!is.null(plot))

    if (file.exists(file_name)) file.remove(file_name)

    ggsave(filename=file_name, plot=plot, height=size_inches[1], width=size_inches[2], units='in')
    expect_true(file.exists(file_name))
}
