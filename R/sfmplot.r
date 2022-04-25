


sfmplot <- function(){
	  
	######################################################################################
	shiny::runApp( shinyApp(
		##################################################################################
		ui =  shinyUI(
			fluidPage(
				useShinyalert(),  # Set up shinyalert
				##########################################################################
				fluidRow(
					column(2, 
						HTML('<br><h3>Swisssfm STP map</h3>')
					),
					column(4, 
						HTML('<br>'),
						textInput("path_in_csv", "Path to wrap_vsa result file.csv", value = "F:/.../VSA/STP_result_Diclofenac.csv", width = '500px'),
						bsPopover("path_in_csv", 
							title = "",
							content = "", 
							placement = "right", trigger = "hover")
					),
					column(2, 
						HTML('<br>'),
						selectInput("csv_sep", "csv separator",
						choices = c("; (semicolon)" = ";", ", (comma)" = ",", "tab delimited" =  "\t", "white space" = ""), selected = ";")
					),				
					column(1, 
						HTML('<br><br>'),
						bsButton("open_path_in", "Load", style = "success")
					)
				),
				HTML('<hr noshade = "noshade" />'),
				##########################################################################				
				tabsetPanel(			
					tabPanel("Plot",				

						navbarPage("Plot options",
							tabPanel("Hide", HTML('<hr noshade="noshade" />')),
							tabPanel("STP", HTML('<br><br>')),
							tabPanel("Add", 
								selectInput("add_plot", "Add to plot:", "Link each STP to its ARANEXT", selected = NULL, multiple = TRUE,
									selectize = TRUE, width = NULL, size = NULL)
							
							)							
						),
						
						
						plotOutput("STP_plane",
							click = "STP_plane_click",
							dblclick = "STP_plane_dblclick",
							hover = "STP_plane_hover",
							brush = brushOpts(
								id = "STP_plane_brush",
								resetOnNew = TRUE,
								delay = 0
							),
							height = "700px",
							width  = "1300px"
						),					
						
						
						HTML('<br><br>')
					),
					tabPanel("STP table",	
						HTML('<hr noshade="noshade" />'),
						
						DT::dataTableOutput('STP_table_output'),
						
						
						HTML('<br><br>')
					)
				)
				##########################################################################	
			)
		),
		##################################################################################

		##################################################################################
		server = function(session, input, output) {

			plot_lim <- reactiveValues()
			plot_lim$xlim <- NULL
			plot_lim$ylim <- NULL

			if(any(objects(envir = as.environment(".GlobalEnv")) == "STP_table")) rm("STP_table", envir = as.environment(".GlobalEnv"))


#fliess <- read.shapefile("F:/VSA/topo/swissTLM3D_TLM_FLIESSGEWAESSER")


			##############################################################################
			observe({
				input$open_path_in
				if(isolate(input$open_path_in)){	
				
					path_in <- isolate(input$path_in_csv)
					if(grepl(".xlsx", path_in, fixed = TRUE)){
						STP_table_in <- try({
							openxlsx:::read.xlsx(xlsxFile = path_in, sheet = 1)
						})
					}else{
						STP_table_in <- try({
							read.csv(file = path_in, sep = isolate(input$csv_sep), stringsAsFactors = FALSE)
						})
					}
					
					if(class(STP_table_in) == "try-error"){
						shinyalert("Warning", STP_table_in[[1]], type = "error")
					}else{
					
						if(
							!any(STP_table_in[6, ] %in% c("STP_ID", "ARANEXTNR", "LageX", "LageY"))
						){
						
							shinyalert("Warning", "Missing columns in result table.", type = "error")
						
						}else{
						
							##############################################################
							names(STP_table_in) <- STP_table_in[6, ]
							STP_table_in <- STP_table_in[7:nrow(STP_table_in),, drop = FALSE]
							STP_table_in$LageX <- as.numeric(STP_table_in$LageX)
							STP_table_in$LageY <- as.numeric(STP_table_in$LageY)
							
							STP_table_in <<- STP_table_in

							##############################################################			
							output$STP_plane <- renderPlot({

								plot.new()
								plot.window(xlim = range(STP_table_in$LageX), ylim = range(STP_table_in$LageY))
								points(STP_table_in$LageX, STP_table_in$LageY, pch = 19, col = "black")



								box(col = "darkgrey")

							})
							##############################################################
							output$STP_table_output <- DT::renderDataTable({
								DT::datatable(STP_table_in)
							})
							##############################################################					
					
						}
					}
				
				}
			})
			##############################################################################
			

			
			##############################################################################	
							
			observeEvent(input$STP_plane_dblclick, {
				
				brush <- isolate(input$STP_plane_brush)
				if(!is.null(brush)) {
					plot_lim$xlim <- c(brush$xmin, brush$xmax)
					plot_lim$ylim <- c(brush$ymin, brush$ymax)
				}else{
					plot_lim$xlim <- NULL
					plot_lim$ylim <- NULL
				}
				
			}, ignoreInit = TRUE)
							
			##############################################################################				
			observe({		
			
				plot_lim$xlim #plot_lim$ylim
				input$add_plot
				
				if(any(objects(envir = as.environment(".GlobalEnv")) == "STP_table_in")){
					
					output$STP_plane <- renderPlot({
					
						
						if(is.null(isolate(plot_lim$xlim))) x_lim <- range(STP_table_in$LageX) else x_lim <- isolate(plot_lim$xlim)
						if(is.null(isolate(plot_lim$ylim))) y_lim <- range(STP_table_in$LageY) else y_lim <- isolate(plot_lim$ylim)				
						
						plot.new()
						plot.window(xlim = x_lim, ylim = y_lim)
						##################################################################
						if("Link each STP to its ARANEXT" %in% isolate(input$add_plot)){

							for(i in 1:nrow(STP_table_in)){

								if(is.na(STP_table_in[i, "ARANEXTNR"])) next
								
								to <- which(STP_table_in$STP_ID == STP_table_in[i, "ARANEXTNR"])
								
								lines(c(STP_table_in$LageX[i], STP_table_in$LageX[to]), c(STP_table_in$LageY[i], STP_table_in$LageY[to]))
								


							}

						}
						##################################################################						

						points(STP_table_in$LageX, STP_table_in$LageY, pch = 19, col = "black")

						##################################################################						
						box(col = "darkgrey")
						#axis(1);axis(2)


					})
					
				}else cat("\nSTP_table not yet loaded")

			})				
			##############################################################################	

		}
		##################################################################################
	))
	######################################################################################
	
}





