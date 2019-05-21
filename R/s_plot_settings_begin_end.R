#' Plot settings for all plots
#'
#' Settings:
#'
#' filenames
#'
#' devices settings
#'
#' par settings
#'
#' Begin:
#'
#' to interactive display
#'
#' png is default output since quick display on web and used to convert to other raster formats
#'
#' ps
#'
#' pdf
#'
#' bmp
#'
#' jpeg
#'
#' tiff
#'
#' par settings
#'
#' End:
#'
#' @param filename_prefix
#' @param plot_filename
#' @param plot_mode
#' @param plot_format
#'
#' @return NULL
#'
#' @examples
s_plot_settings_begin_end <-
function# Plot settings for all plots
###
(filename_prefix = "Prefix"
###
, plot_filename = "default_filename"
###
, plot_mode = "settings"
###
, plot_format = 1
###
)
#
#   if modifying plot file types, need to update:
#     Excel workbook, for input settings
#     assign_variables.R, for reading workbook input and setting plot_format_list
#     s_plot_settings_begin_end.R, this function for settings
#
{
  ##library("Cairo"); # for raster formats under unix

  # determine if running in windows or unix environment
  OS <- .Platform$OS.type;

  ##########
  ##details<<
  ## Settings:
  if (plot_mode == "settings")
  {
    ##details<<
    ## filenames
    #file_temp     <- paste(filename_prefix, "_", plot_filename, "_%02d", sep="");
    file_temp     <- paste(filename_prefix, "_", plot_filename, sep="");  # no "_%02d" for convert to work
    file_png      <- paste(file_temp, ".png" , sep="");
    file_eps      <- paste(file_temp, ".eps" , sep="");
    file_pdf      <- paste(file_temp, ".pdf" , sep="");
    file_bmp      <- paste(file_temp, ".bmp" , sep="");
    file_jpeg     <- paste(file_temp, ".jpeg", sep="");
    file_tiff     <- paste(file_temp, ".tiff", sep="");    #(added at R 2.7)
    # file_gif      <- paste(file_temp, ".gif" , sep="");  #(removed at R 2.7)

    convert_path  <- "/usr/bin/convert";  # for converting from png to other raster formats

    ##details<<
    ## devices settings
    onefile       <- TRUE;  # FALSE
    family        <- "ComputerModern";
    encoding      <- "TeXtext_enc";
    width_in      <- 8.0;
    height_in     <- 8.0;
    resolution    <- 300;
    width_px      <- 800;
    height_px     <- 800;
    #width_px      <- width_in *resolution; #800;
    #height_px     <- height_in*resolution; #800;
    horizontal    <- FALSE;
    paper         <- "letter";
    pagecentre    <- TRUE;
    print_it      <- FALSE;
    quality_jpeg  <- 100;
    compress_tiff <- "none";

    ##details<<
    ## par settings
    par_oma       <- c(5,4,4,4);
    par_mar       <- c(4,4,2,2);
    par_mfrow     <- c(1,1);
    par_bg        <- "transparent";
    par_xpd       <- TRUE;


    # list
    PLOT_SETTINGS <- as.list(new.env());  # create a list to return with all data

      # filenames
    PLOT_SETTINGS$file_png      <- file_png     ;
    PLOT_SETTINGS$file_eps      <- file_eps     ;
    PLOT_SETTINGS$file_pdf      <- file_pdf     ;
    PLOT_SETTINGS$file_bmp      <- file_bmp     ;
    PLOT_SETTINGS$file_jpeg     <- file_jpeg    ;
    # PLOT_SETTINGS$file_gif      <- file_gif     ;
    PLOT_SETTINGS$file_tiff     <- file_tiff    ;

    PLOT_SETTINGS$convert_path  <- convert_path ;

      # pdf and ps parameters
    PLOT_SETTINGS$onefile       <- onefile      ;
    PLOT_SETTINGS$family        <- family       ;
   #PLOT_SETTINGS$title         <- title        ;
   #PLOT_SETTINGS$fonts         <- fonts        ;
    PLOT_SETTINGS$encoding      <- encoding     ;
   #PLOT_SETTINGS$bg            <- bg           ;
   #PLOT_SETTINGS$fg            <- fg           ;
    PLOT_SETTINGS$width_in      <- width_in     ;
    PLOT_SETTINGS$height_in     <- height_in    ;
    PLOT_SETTINGS$horizontal    <- horizontal   ;
   #PLOT_SETTINGS$pointsize     <- pointsize    ;
    PLOT_SETTINGS$paper         <- paper        ;
    PLOT_SETTINGS$pagecentre    <- pagecentre   ;
    PLOT_SETTINGS$print_it      <- print_it     ;
   #PLOT_SETTINGS$command       <- command      ;
   #PLOT_SETTINGS$colormodel    <- colormodel   ;

      # rasters
    PLOT_SETTINGS$width_px      <- width_px     ;
    PLOT_SETTINGS$height_px     <- height_px    ;
    PLOT_SETTINGS$resolution    <- resolution;

      # jpeg
    PLOT_SETTINGS$quality_jpeg  <- quality_jpeg ;
      # tiff


    PLOT_SETTINGS$par_oma       <- par_oma      ;
    PLOT_SETTINGS$par_mar       <- par_mar      ;
    PLOT_SETTINGS$par_mfrow     <- par_mfrow    ;
    PLOT_SETTINGS$par_bg        <- par_bg       ;
    PLOT_SETTINGS$par_xpd       <- par_xpd      ;

    return( PLOT_SETTINGS );
  ### PLOT_SETTINGS
  } # settings

  ##########
  ##details<<
  ## Begin:
  if (plot_mode == "begin")
  {
    PLOT_SETTINGS <- s_plot_settings_begin_end(filename_prefix, plot_filename);

    ##details<<
    ## to interactive display
    if (plot_format == 0) {
      if (OS == "unix") { X11(); }
      if (OS == "windows") { windows(); }
    }

    ##details<<
    ## png is default output since quick display on web and used to convert to other raster formats
    if (plot_format == 1) {
      png       (filename   = PLOT_SETTINGS$file_png
               , width      = PLOT_SETTINGS$width_px
               , height     = PLOT_SETTINGS$height_px
      #         , res        = PLOT_SETTINGS$resolution
                 );
    }

    ##details<<
    ## ps
    if (plot_format == 2) {
      postscript(file       = PLOT_SETTINGS$file_eps   ,
                 onefile    = PLOT_SETTINGS$onefile    ,
      #           family     = PLOT_SETTINGS$family     ,
      #           encoding   = PLOT_SETTINGS$encoding   ,
                 width      = PLOT_SETTINGS$width_in   ,
                 height     = PLOT_SETTINGS$height_in  ,
                 horizontal = PLOT_SETTINGS$horizontal ,
                 paper      = PLOT_SETTINGS$paper      ,
                 pagecentre = PLOT_SETTINGS$pagecentre ,
                 print.it   = PLOT_SETTINGS$print_it
                 );
    }

    ##details<<
    ## pdf
    if (plot_format == 3) {
      pdf       (file       = PLOT_SETTINGS$file_pdf   ,
                 onefile    = PLOT_SETTINGS$onefile    ,
                 #family     = PLOT_SETTINGS$family     ,
                 #encoding   = PLOT_SETTINGS$encoding   ,
                 width      = PLOT_SETTINGS$width_in   ,
                 height     = PLOT_SETTINGS$height_in  ,
                # horizontal = PLOT_SETTINGS$horizontal ,
                 paper      = PLOT_SETTINGS$paper      ,
                 pagecentre = PLOT_SETTINGS$pagecentre
                # print.it   = PLOT_SETTINGS$print_it
                 );
    }

    ##details<<
    ## bmp
    if (plot_format == 4) {
      bmp       (filename   = PLOT_SETTINGS$file_bmp
               , width      = PLOT_SETTINGS$width_px
               , height     = PLOT_SETTINGS$height_px
      #         , res        = PLOT_SETTINGS$resolution
                 );

      ## Convert from png        #system("/usr/bin/convert test_png test.bmp")
      #convert_command <- paste(PLOT_SETTINGS$convert_path, PLOT_SETTINGS$file_png, PLOT_SETTINGS$file_bmp, sep=" ");
      #system(convert_command);
    }

    ##details<<
    ## jpeg
    if (plot_format == 5) {
      jpeg      (filename   = PLOT_SETTINGS$file_jpeg
               , width      = PLOT_SETTINGS$width_px
               , height     = PLOT_SETTINGS$height_px
               , quality    = PLOT_SETTINGS$quality_jpeg
      #         , res        = PLOT_SETTINGS$resolution
                 );
    }

    ## gif
    #if (plot_format == 6) {
    #  # Convert from png        #system("/usr/bin/convert test_png test.gif")
    #  convert_command <- paste(PLOT_SETTINGS$convert_path, PLOT_SETTINGS$file_png, PLOT_SETTINGS$file_gif, sep=" ");
    #  system(convert_command);
    #}

    ##details<<
    ## tiff
    if (plot_format == 6) {
      tiff      (filename    = PLOT_SETTINGS$file_tiff
               , width       = PLOT_SETTINGS$width_px
               , height      = PLOT_SETTINGS$height_px
               , compression = PLOT_SETTINGS$compress_tiff
      #         , res        = PLOT_SETTINGS$resolution
                 );
    }


    #dev.control(displaylist = "enable");
    #dev.set(which = dev.next());

    ##details<<
    ## par settings
    par(oma = PLOT_SETTINGS$par_oma,
        mar = PLOT_SETTINGS$par_mar,
      mfrow = PLOT_SETTINGS$par_mfrow,
         bg = PLOT_SETTINGS$par_bg,
        xpd = PLOT_SETTINGS$par_xpd);

  } # begin

  ##########
  ##details<<
  ## End:
  if (plot_mode == "end")
  {
    if (plot_format != 0) { dev.off(); };
    #graphics.off(); # don't use
  } # end

  invisible(NULL);
  ### NULL
} # s_plot_settings_begin_end()

