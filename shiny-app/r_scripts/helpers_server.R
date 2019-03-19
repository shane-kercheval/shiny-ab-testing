#' this function is a hack so that the progress bar works for ggplots that take a long time to render
#' because ggplot objects don't attempt to render under they are displayed
#' 
#' because it is meant to be used for non-blocking logic (like rendering ggplot UI), the user must insert 
#' the HTML element (with this function) before the progress bar is to be displayed. It takes advantage of
#' the `shiny-busy` class, so it is actually displayed any time the `shiny-busy` exists. 
#' 
#' Therefore, when the progress bar should not be automatically displayed when the `shiny-busy` is present
#' the HTML element should be removed via `remove_custom_progress_bar()` (e.g. when navigating away from the
#' page the custom progress bar is used on).
#' 
#' @param message the text to display under the progress bar
insert_custom_progress_bar <- function(message="Loading/Processing Data") {

    log_message_block_start(message)

    progress_bar_html <- HTML(paste0('"<div id="shiny-notification-panel"><div id="shiny-notification-42ec5661c2722d29" class="shiny-notification"><div class="shiny-notification-close">×</div><div class="shiny-notification-content"><div class="shiny-notification-content-text"><div id="shiny-progress-42ec5661c2722d29" class="shiny-progress-notification"><div class="progress progress-striped active" style=""><div class="progress-bar" style="width: 50%;"></div></div><div class="progress-text"><span class="progress-message">', message,'</span> <span class="progress-detail"></span></div></div></div><div class="shiny-notification-content-action"></div></div></div></div>"'))
    progress_html <- conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                      progress_bar_html,
                                      id='custom_progress_bar')
    remove_custom_progress_bar()
    insertUI(selector="#custom_progress_bar_placeholder", where = c("afterEnd"), progress_html, immediate=TRUE)
}

#' removes the UI element 
remove_custom_progress_bar <- function() {

    removeUI(selector="#custom_progress_bar", immediate = TRUE)
}

#' adds a tooltip via server
#' defualt placement is 'bottom', but I want the default to be 'top'
add_tooltip <- function(element, tooltip_text, placement='top', trigger='hover') {
    return ( tipify(element, title=tooltip_text, placement=placement, trigger=trigger) )
}

#' for renderUI code that returns multiple elements (e.g. custom menu options); a list is built up and 
#' passed to `tagList()` which is returned from the renderUI function
#' this function appends UI elements `ui_item` (e.g. radioButtons()) to the list `l` and, if applicable,
#' wraps the element in a div with a specified class
#' @param l the list to append to
#' @param ui_item the ui item to add ot the list
#' @param div_class if not NULL, the element is wrapped in a div with the specified class
ui_list_append <- function(l, ui_item, div_class=NULL) {

    if(is.null(div_class)) {

        return (c(l, list(ui_item)))

    } else {

        return (c(l, list(tags$div(class=div_class, ui_item))))
    }
}

red_font <- function(text) {
    return (paste0("<font color=\"#FF0000\"><b>", text, "</b></font>"))
}
