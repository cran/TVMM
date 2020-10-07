#' @title A graphical user interface (GUI) of the package TVMM
#' @description A graphical user interface (GUI) of the package TVMM to perform more general hypothesis tests on the vector of multivariate population means.
#'
#'
#' \url{https://rpubs.com/Henriqueufla/617206} for more details.
#'
#' @param gui Logical argument, \code{TRUE} or \code{FALSE}. The default is \code{TRUE}
#'
#'
#' @import tcltk
#' @import ggplot2
#' @importFrom robustbase covComed
#' @importFrom tkrplot tkrplot
#' @importFrom gridExtra grid.arrange
#' @importFrom stats quantile rchisq rf var runif
#' @importFrom utils browseURL edit read.table read.csv
#' @importFrom MASS mvrnorm ginv
#' @importFrom grDevices gray
#' @importFrom tcltk2 tk2text
#' @importFrom DescToolsAddIns dir.choose
#' @importFrom robustbase covComed
#'
#' @return \code{guiTVMM} A graphical user interface (GUI) for performing tests on the vector of multivariate population means.
#'
#' @references
#' Henrique J. P. Alves & Daniel F. Ferreira (2019): Proposition of new alternative tests adapted to the traditional T2 test, Communications in Statistics - Simulation and Computation, DOI: 10.1080/03610918.2019.1693596
#'
#'
#' @examples
#' library(TVMM)
#' if(interactive()){
#'   guiTVMM(gui=FALSE)
#' }
#'
#' @export
guiTVMM <- function(gui=TRUE) {
  if(gui==TRUE){
    color <- c("#FF9999", "#3366FF")
    Color <- gray(2:1 /2 )
    set.seed(0)
    options(digits = 4)
    T2help <- function() {
      browseURL("https://rpubs.com/Henriqueufla/617206")
    }

    #fun??o da op??o quit do menu file da GUI
    quit <- function(){
      dialog <- tkmessageBox(message = "Do you really want to end this GUI?" ,
                             icon = "question" , type = "yesno")
      if(as.character(dialog) == "yes"){
        l=tkdestroy(tt)
      }
    }

    X <- NULL
    mu0 <- NULL
    # funcoes auxiliares que carregam os arquivos de dados em txt
    DaTa <- function(){
      fileName <- tclvalue(tkgetOpenFile(filetypes=gettext('{"txt Files" {".txt"}} {{All files} {"*"}}')))
      if (fileName == "") return();
      if(exists("fileName")) {
        sapply(2:3, function(i) tkentryconfigure(topMenu, i, state = "normal"))
      }
      dat <- NULL
      X <<- dat <- dat <<- as.matrix(read.table(fileName, header = TRUE))
      pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
      u <- c(0, sort(runif(20, 0, 100)), 100)
      for(i in u) {
        Sys.sleep(0.1)
        info <- sprintf("%d%% done", round(i))
        setTkProgressBar(pb, i, sprintf("Data (%s)", info), info)
      }
      close(pb)
      sapply(0:5, function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
    }

    MeanVec <- function(){
      fileName <- tclvalue(tkgetOpenFile(filetypes=gettext('{"txt" {".txt"}} {{All files} {"*"}}')))
      if (fileName == "") return();
      if(exists("fileName")) {
        sapply(2:3, function(i) tkentryconfigure(topMenu, i, state = "normal"))
      }
      dat <- NULL
      mu0 <<- dat <- dat <<- as.matrix(read.table(fileName, header = TRUE))
      pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
      u <- c(0, sort(runif(20, 0, 100)), 100)
      for(i in u) {
        Sys.sleep(0.1)
        info <- sprintf("%d%% done", round(i))
        setTkProgressBar(pb, i, sprintf("Data (%s)", info), info)
      }
      close(pb)
      sapply(0:5, function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
    }

    DaTacsv <- function(){
      fileName <- tclvalue(tkgetOpenFile(filetypes=gettext('{"Excel Files" {".csv"}} {{All files} {"*"}}')))
      if (fileName == "") return();
      if(exists("fileName")) {
        sapply(2:3, function(i) tkentryconfigure(topMenu, i, state = "normal"))
      }
      dat <- NULL
      X <<- dat <- dat <<- as.matrix(read.csv(fileName, header = TRUE, sep=",", dec="."))
      pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
      u <- c(0, sort(runif(20, 0, 100)), 100)
      for(i in u) {
        Sys.sleep(0.1)
        info <- sprintf("%d%% done", round(i))
        setTkProgressBar(pb, i, sprintf("Data (%s)", info), info)
      }
      close(pb)
      sapply(0:5, function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
    }

    MeanVeccsv <- function(){
      fileName <- tclvalue(tkgetOpenFile(filetypes=gettext('{"Excel Files" {".csv"}} {{All files} {"*"}}')))
      if (fileName == "") return();
      if(exists("fileName")) {
        sapply(2:3, function(i) tkentryconfigure(topMenu, i, state = "normal"))
      }
      dat <- NULL
      mu0 <<- dat <- dat <<- as.matrix(read.csv(fileName,header = TRUE, sep=",", dec="."))
      pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
      u <- c(0, sort(runif(20, 0, 100)), 100)
      for(i in u) {
        Sys.sleep(0.1)
        info <- sprintf("%d%% done", round(i))
        setTkProgressBar(pb, i, sprintf("Data (%s)", info), info)
      }
      close(pb)
      sapply(0:5, function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
    }

    #fun??o que traz a op??o view/edit do menu file da GUI
    # X <- NULL
    editMatrixData <- function(){
      dat <- NULL
      X <<- dat <- dat <<- edit(X)
      return(X)
    }
    # mu0 <- NULL
    editMeanData <- function(){
      dat <- NULL
      mu0 <<- dat <- dat <<- edit(mu0)
      return(mu0)
    }

    #teste T2 original
    T2OO <- function(){
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        if(nrow(X)<=nrow(mu0)){
          tkmessageBox(message=paste("The T2O test cannot be calculated!!!!!!"))
        } else{
          win <- tktoplevel()
          tkwm.title(win,"Results of T2O test!!")
          win$env$txt <- tk2text(win, width = 60, height = 10)
          tkpack(win$env$txt, fill = "both", expand = TRUE)
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame, expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          calc.boot <- tkbutton(button_frame, text="T2O", background="#CCE5FF", command=function(){
            pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("T2 test (%s)", info), info)
            }
            close(pb)
            tkinsert(win$env$txt, "1.0", paste("The statistic value is:.\n", TVMM::T2O(X, mu0)$T2[1],".\n", "The p-value is:.\n", TVMM::T2O(X, mu0)$valor.p[1], ".\n"))
          })
          help.boot <- tkbutton(button_frame, text="Help", background="#CCE5FF", command= t2help)
          tkpack(calc.boot, padx = c(40,0),side="left")
          tkpack(help.boot, padx = c(40,0),side="right")
          sapply(0, function(i) tkentryconfigure(fileTestgraph, i, state = "normal"))
          sapply(1:5, function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
        }
      }
      tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
        response <- tkmessageBox(icon = "question",
                                 message = "Do you really want to close this window?" ,
                                 type = "yesno" ,
                                 parent = win)
        if (as.character(response) == "no") return()
        tkdestroy(win) # Caso contrario feche
      }
      )
    }

    #cria a janela de sa?da gr?fica na GUI
    FigplotT2O <- function(...) {
      gl1 <- as.numeric(nrow(X))
      gl2 <- as.numeric(nrow(mu0))
      vF <- rf(80, gl2,gl1-gl2)
      VF <- (gl1-1)*gl2*vF/(gl1-gl2)
      #OS GRUPOS
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1,ifelse(VF>=TVMM::T2O(X, mu0)$T2[1],2, 0))
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("T2O test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=TVMM::T2O(X, mu0)$T2[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("T2O test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    FigplotT200 <- function(...) {
      gl1 <- as.numeric(nrow(X))
      gl2 <- as.numeric(nrow(mu0))
      vF <- rf(80, gl2,gl1-gl2)
      VF <- (gl1-1)*gl2*vF/(gl1-gl2)
      #OS GRUPOS
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1,ifelse(VF>=TVMM::T2O(X, mu0)$T2[1],2, 0))
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("T2O test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=TVMM::T2O(X, mu0)$T2[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("T2O test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    plotT2O <- function() {
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        if(nrow(X)<=nrow(mu0)){
          tkmessageBox(message=paste("The T2O test cannot be calculated!!!!!!"))
        } else{
          dialog <- tkmessageBox (message = "Do you want to view this figure in color?" ,
                                  icon = "question" , type = "yesno")
          if(as.character(dialog) == "yes"){
            pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
            }
            close(pb)
            hscale <- 2    # Horizontal scaling
            vscale <- 1    # Vertical scaling
            win <- tktoplevel()
            # tktitle <- "T2 original test."
            tkwm.title(win,"T2 original test.")
            win$env$plot <- tkrplot(win, fun = FigplotT2O, hscale = hscale, vscale = vscale)
            tkpack(win$env$plot)
            #create button save
            frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
            tkpack(frame, expand = TRUE , fill = "both")
            ##
            nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
            #button
            button_frame <- ttkframe(frame)
            tkpack(button_frame , anchor = "ne")
            button1 <- ttkbutton (button_frame , text = "Save")
            tkpack(button1 , side = "right")
            rb1 <- tkradiobutton(button_frame)
            rb2 <- tkradiobutton(button_frame)
            rb3 <- tkradiobutton(button_frame)
            rb4 <- tkradiobutton(button_frame)
            rb5 <- tkradiobutton(button_frame)
            rb6 <- tkradiobutton(button_frame)
            rb7 <- tkradiobutton(button_frame)
            rbValue <- tclVar("all")
            tkconfigure(rb1, variable=rbValue, value="png")
            tkconfigure(rb3, variable=rbValue, value="jpeg")
            tkconfigure(rb4, variable=rbValue, value="pdf")
            tkconfigure(rb2, variable=rbValue, value="eps")
            tkconfigure(rb5, variable=rbValue, value="tiff")
            tkconfigure(rb6, variable=rbValue, value="bmp")
            tkconfigure(rb7, variable=rbValue, value="all")


            tkpack(tklabel(button_frame, text="png"), rb1, side="left")
            tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
            tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
            tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
            tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
            tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
            tkpack(tklabel(button_frame, text="all"), rb7, side="left")

            handler1 <- function () {
              dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                rbVal <- as.character(tclvalue(rbValue))
                if (rbVal=="png"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.png", path = folder, FigplotT2O(ml), width = 10, height = 7)
                }
                if (rbVal=="eps"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.eps", path = folder, FigplotT2O(ml), width = 10, height = 7)
                }
                if (rbVal=="jpeg"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.jpeg", path = folder, FigplotT2O(ml), width = 10, height = 7)
                }
                if (rbVal=="pdf"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.pdf", path = folder, FigplotT2O(ml),width = 10, height = 7)
                }
                if (rbVal=="tiff"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.tiff", path = folder, FigplotT2O(ml),width = 10, height = 7)
                }
                if (rbVal=="bmp"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.bmp", path = folder, FigplotT2O(ml),width = 10, height = 7)
                }
                if (rbVal=="all"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.png", path = folder, FigplotT2O(ml), width = 10, height = 7)
                  ggsave("T2O.eps", path = folder, FigplotT2O(ml), width = 10, height = 7)
                  ggsave("T2O.jpeg", path = folder, FigplotT2O(ml), width = 10, height = 7)
                  ggsave("T2O.pdf", path = folder, FigplotT2O(ml),width = 10, height = 7)
                  ggsave("T2O.tiff", path = folder, FigplotT2O(ml),width = 10, height = 7)
                  ggsave("T2O.bmp", path = folder, FigplotT2O(ml),width = 10, height = 7)
                }
              }
              else{
                dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                        icon = "question" , type = "yesno")
                if(as.character(dialog) == "yes"){
                  tkdestroy(win)
                }
              }
            }
            tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
              response <- tkmessageBox(icon = "question",
                                       message = "Do you really want to close this window?" ,
                                       type = "yesno" ,
                                       parent = win)
              if (as.character(response) == "no") return()
              tkdestroy(win) # Caso contrario feche
            }
            )
          } else{
            pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
            }
            close(pb)
            hscale <- 2    # Horizontal scaling
            vscale <- 1    # Vertical scaling
            win <- tktoplevel()
            # tktitle <- "T2 original test."
            tkwm.title(win,"T2 original test.")
            win$env$plot <- tkrplot(win, fun = FigplotT200, hscale = hscale, vscale = vscale)
            tkpack(win$env$plot)
            #create button save
            frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
            tkpack(frame, expand = TRUE , fill = "both")
            ##
            nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
            #button
            button_frame <- ttkframe(frame)
            tkpack(button_frame , anchor = "ne")
            button1 <- ttkbutton (button_frame , text = "Save")
            tkpack(button1 , side = "right")
            rb1 <- tkradiobutton(button_frame)
            rb2 <- tkradiobutton(button_frame)
            rb3 <- tkradiobutton(button_frame)
            rb4 <- tkradiobutton(button_frame)
            rb5 <- tkradiobutton(button_frame)
            rb6 <- tkradiobutton(button_frame)
            rb7 <- tkradiobutton(button_frame)
            rbValue <- tclVar("all")
            tkconfigure(rb1, variable=rbValue, value="png")
            tkconfigure(rb3, variable=rbValue, value="jpeg")
            tkconfigure(rb4, variable=rbValue, value="pdf")
            tkconfigure(rb2, variable=rbValue, value="eps")
            tkconfigure(rb5, variable=rbValue, value="tiff")
            tkconfigure(rb6, variable=rbValue, value="bmp")
            tkconfigure(rb7, variable=rbValue, value="all")


            tkpack(tklabel(button_frame, text="png"), rb1, side="left")
            tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
            tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
            tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
            tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
            tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
            tkpack(tklabel(button_frame, text="all"), rb7, side="left")


            handler1 <- function () {
              dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                rbVal <- as.character(tclvalue(rbValue))
                if (rbVal=="png"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.png", path = folder, FigplotT200(ml), width = 10, height = 7)
                }
                if (rbVal=="eps"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.eps", path = folder, FigplotT200(ml), width = 10, height = 7)
                }
                if (rbVal=="jpeg"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.jpeg", path = folder, FigplotT200(ml), width = 10, height = 7)
                }
                if (rbVal=="pdf"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.pdf", path = folder, FigplotT200(ml),width = 10, height = 7)
                }
                if (rbVal=="tiff"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.tiff", path = folder, FigplotT200(ml),width = 10, height = 7)
                }
                if (rbVal=="bmp"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.bmp", path = folder, FigplotT200(ml),width = 10, height = 7)
                }
                if (rbVal=="all"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.png", path = folder, FigplotT200(ml), width = 10, height = 7)
                  ggsave("T2O.eps", path = folder, FigplotT200(ml), width = 10, height = 7)
                  ggsave("T2O.jpeg", path = folder, FigplotT200(ml), width = 10, height = 7)
                  ggsave("T2O.pdf", path = folder, FigplotT200(ml),width = 10, height = 7)
                  ggsave("T2O.tiff", path = folder, FigplotT200(ml),width = 10, height = 7)
                  ggsave("T2O.bmp", path = folder, FigplotT200(ml),width = 10, height = 7)
                }
              }
              else{
                dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                        icon = "question" , type = "yesno")
                if(as.character(dialog) == "yes"){
                  tkdestroy(win)
                }
              }
            }
            tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
              response <- tkmessageBox(icon = "question",
                                       message = "Do you really want to close this window?" ,
                                       type = "yesno" ,
                                       parent = win)
              if (as.character(response) == "no") return()
              tkdestroy(win) # Caso contrario feche
            }
            )
          }
          tkconfigure(button1 , command = handler1)
        }
      }
    }

    #Teste T2Bootstrap
    T2BBoot <- function(){
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        if(nrow(X)<=nrow(mu0)){
          tkmessageBox(message=paste("The T2Boot test cannot be calculated!!!!!!"))
        } else{
          win <- tktoplevel()
          tkwm.title(win,"Results of T2Boot test!!")
          win$env$txt <- tk2text(win, width = 60, height = 10)
          tkpack(win$env$txt, fill = "both", expand = TRUE)
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame, expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          b <- tclVar(80)
          B.entry <- tkentry(button_frame, textvariable=b, width=7)
          calc <- function(){B <- as.numeric(tclvalue(b))
          if(B<=0 | B%%floor(B)!=0 | is.na(B)==TRUE){
            tkmessageBox(message=paste("B must be a positive integer!!!!!!"))
          } else{
            pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("T2Boot test (%s)", info), info)
            }
            close(pb)
            results <- TVMM::T2Boot(X, mu0, B)
            tkinsert(win$env$txt, "1.0", paste("The statistic value is:.\n", results$T2,".\n", "The p-value is:.\n", results$valor.p, ".\n"))
          }
          }
          calc.boot <- tkbutton(button_frame, text="T2Boot", background="#CCE5FF", command=calc)
          help.boot <- tkbutton(button_frame, text="Help", background="#CCE5FF", command= t2boothelp)
          tkpack(calc.boot, padx = c(40,0),side="left")
          tkpack(help.boot, padx = c(40,0),side="left")
          tkpack(tklabel(button_frame, text="B", background="#CCE5FF", width="7"), B.entry, padx = c(40,0),side="left")
          sapply(1, function(i) tkentryconfigure(fileTestgraph, i, state = "normal"))
          sapply(c(0,2:5), function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
        }
      }
      tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
        response <- tkmessageBox(icon = "question",
                                 message = "Do you really want to close this window?" ,
                                 type = "yesno" ,
                                 parent = win)
        if (as.character(response) == "no") return()
        tkdestroy(win) # Caso contrario feche
      }
      )
    }

    # fun??o que cria o gr?fico deste teste
    FigplotT2Boot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      XS <- apply(X,2,mean)
      SS <- var(X)
      T2 <- n * t(XS - mu0) %*% solve(SS) %*% (XS - mu0)
      T2v <- T2
      T2b <- numeric(length = 80)
      for (i in 1:(B=80))
      {
        Xb <- mvrnorm(n, mu0, SS)
        Xsb <- apply(Xb,2,mean)
        Ssb <- var(Xb)
        T2b[[i]] <- n * t(Xsb - mu0) %*% solve(Ssb) %*% (Xsb - mu0)
      }
      VF <- c(T2v,T2b)
      #OS GRUPOS
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1,ifelse(VF>=T2Boot(X,mu0, B=80)$T2[,1],2, 0))
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("T2Boot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=T2Boot(X,mu0, B=80)$T2[,1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("T2Boot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    FigplotT2BBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      XS <- apply(X,2,mean)
      SS <- var(X)
      T2 <- n * t(XS - mu0) %*% solve(SS) %*% (XS - mu0)
      T2v <- T2
      T2b <- numeric(length = 80)
      for (i in 1:(B=80))
      {
        Xb <- mvrnorm(n, mu0, SS)
        Xsb <- apply(Xb,2,mean)
        Ssb <- var(Xb)
        T2b[[i]] <- n * t(Xsb - mu0) %*% solve(Ssb) %*% (Xsb - mu0)
      }
      VF <- c(T2v,T2b)
      #OS GRUPOS
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1,ifelse(VF>=T2Boot(X,mu0, B=80)$T2[,1],2, 0))
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("T2Boot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=T2Boot(X,mu0, B=80)$T2[,1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)), colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("T2Boot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    #cria a janela de sa??da gr?fica na GUI
    plotT2Boot <- function() {
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        if(nrow(X)<=nrow(mu0)){
          tkmessageBox(message=paste("The T2Boot test cannot be calculated!!!!!!"))
        } else{
          dialog <- tkmessageBox (message = "Do you want to view this figure in color?" ,
                                  icon = "question" , type = "yesno")
          if(as.character(dialog) == "yes"){
            pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
            }
            close(pb)
            hscale <- 2    # Horizontal scaling
            vscale <- 1    # Vertical scaling
            win <- tktoplevel()
            tkwm.title(win,"T2 parametric bootstrap test.")
            win$env$plot <- tkrplot(win, fun = FigplotT2Boot, hscale = hscale, vscale = vscale)
            tkpack(win$env$plot)
            #create button save
            frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
            tkpack(frame , expand = TRUE , fill = "both")
            ##
            nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
            #button
            button_frame <- ttkframe(frame)
            tkpack(button_frame , anchor = "ne")
            button1 <- ttkbutton (button_frame , text = "Save")
            tkpack(button1 , side = "right")
            rb1 <- tkradiobutton(button_frame)
            rb2 <- tkradiobutton(button_frame)
            rb3 <- tkradiobutton(button_frame)
            rb4 <- tkradiobutton(button_frame)
            rb5 <- tkradiobutton(button_frame)
            rb6 <- tkradiobutton(button_frame)
            rb7 <- tkradiobutton(button_frame)
            rbValue <- tclVar("all")
            tkconfigure(rb1, variable=rbValue, value="png")
            tkconfigure(rb3, variable=rbValue, value="jpeg")
            tkconfigure(rb4, variable=rbValue, value="pdf")
            tkconfigure(rb2, variable=rbValue, value="eps")
            tkconfigure(rb5, variable=rbValue, value="tiff")
            tkconfigure(rb6, variable=rbValue, value="bmp")
            tkconfigure(rb7, variable=rbValue, value="all")


            tkpack(tklabel(button_frame, text="png"), rb1, side="left")
            tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
            tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
            tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
            tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
            tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
            tkpack(tklabel(button_frame, text="all"), rb7, side="left")


            handler1 <- function () {
              dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                rbVal <- as.character(tclvalue(rbValue))
                if (rbVal=="png"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.png", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="eps"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.eps", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="jpeg"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.jpeg", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="pdf"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.pdf", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="tiff"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.tiff", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="bmp"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.bmp", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="all"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.png", path = folder, FigplotT2Boot(ml), width = 10, height = 7)
                  ggsave("T2O.eps", path = folder, FigplotT2Boot(ml), width = 10, height = 7)
                  ggsave("T2O.jpeg", path = folder, FigplotT2Boot(ml), width = 10, height = 7)
                  ggsave("T2O.pdf", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                  ggsave("T2O.tiff", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                  ggsave("T2O.bmp", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
              }
              else{
                dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                        icon = "question" , type = "yesno")
                if(as.character(dialog) == "yes"){
                  tkdestroy(win)
                }
              }
            }
            tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
              response <- tkmessageBox(icon = "question",
                                       message = "Do you really want to close this window?" ,
                                       type = "yesno" ,
                                       parent = win)
              if (as.character(response) == "no") return()
              tkdestroy(win) # Caso contrario feche
            }
            )
          } else{
            pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
            }
            close(pb)
            hscale <- 2    # Horizontal scaling
            vscale <- 1    # Vertical scaling
            win <- tktoplevel()
            win$env$plot <- tkrplot(win, fun = FigplotT2BBoot, hscale = hscale, vscale = vscale)
            tkpack(win$env$plot)
            #create button save
            frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
            tkpack(frame , expand = TRUE , fill = "both")
            ##
            nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
            #button
            button_frame <- ttkframe(frame)
            tkpack(button_frame , anchor = "ne")
            button1 <- ttkbutton (button_frame , text = "Save")
            tkpack(button1 , side = "right")
            rb1 <- tkradiobutton(button_frame)
            rb2 <- tkradiobutton(button_frame)
            rb3 <- tkradiobutton(button_frame)
            rb4 <- tkradiobutton(button_frame)
            rb5 <- tkradiobutton(button_frame)
            rb6 <- tkradiobutton(button_frame)
            rb7 <- tkradiobutton(button_frame)
            rbValue <- tclVar("all")
            tkconfigure(rb1, variable=rbValue, value="png")
            tkconfigure(rb3, variable=rbValue, value="jpeg")
            tkconfigure(rb4, variable=rbValue, value="pdf")
            tkconfigure(rb2, variable=rbValue, value="eps")
            tkconfigure(rb5, variable=rbValue, value="tiff")
            tkconfigure(rb6, variable=rbValue, value="bmp")
            tkconfigure(rb7, variable=rbValue, value="all")


            tkpack(tklabel(button_frame, text="png"), rb1, side="left")
            tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
            tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
            tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
            tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
            tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
            tkpack(tklabel(button_frame, text="all"), rb7, side="left")


            handler1 <- function () {
              dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                rbVal <- as.character(tclvalue(rbValue))
                if (rbVal=="png"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.png", path = folder, FigplotT2Boot(ml),width = 10, height = 7)
                }
                if (rbVal=="eps"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.eps", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="jpeg"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.jpeg", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="pdf"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2Boot.pdf", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="tiff"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.tiff", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="bmp"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.bmp", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="all"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2O.png", path = folder, FigplotT2BBoot(ml), width = 10, height = 7)
                  ggsave("T2O.eps", path = folder, FigplotT2BBoot(ml), width = 10, height = 7)
                  ggsave("T2O.jpeg", path = folder, FigplotT2BBoot(ml), width = 10, height = 7)
                  ggsave("T2O.pdf", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                  ggsave("T2O.tiff", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                  ggsave("T2O.bmp", path = folder, FigplotT2BBoot(ml),width = 10, height = 7)
                }
              }
              else{
                dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                        icon = "question" , type = "yesno")
                if(as.character(dialog) == "yes"){
                  tkdestroy(win)
                }
              }
            }
            tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
              response <- tkmessageBox(icon = "question",
                                       message = "Do you really want to close this window?" ,
                                       type = "yesno" ,
                                       parent = win)
              if (as.character(response) == "no") return()
              tkdestroy(win) # Caso contrario feche
            }
            )
          }
          tkconfigure(button1 , command = handler1)
        }
      }
    }

    #teste T2 Robusto bootstrap
    T2RRBBoot <- function(){
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        if(nrow(X)<=nrow(mu0)){
          tkmessageBox(message=paste("The T2RBoot test cannot be calculated!!!!!!"))
        } else{
          win <- tktoplevel()
          tkwm.title(win,"Results of T2RBoot test!!")
          win$env$txt <- tk2text(win, width = 60, height = 10)
          tkpack(win$env$txt, fill = "both", expand = TRUE)
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame, expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          b <- tclVar(80)
          B.entry <- tkentry(button_frame, textvariable=b, width=7)
          calc <- function(){B <- as.numeric(tclvalue(b))
          if(B<=0 | B%%floor(B)!=0 | is.na(B)==TRUE){
            tkmessageBox(message=paste("B must be a positive integer!!!!!!"))
          } else{
            pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("T2Boot test (%s)", info), info)
            }
            close(pb)
            results <- TVMM::T2RobustBoot(X, mu0, B)
            tkinsert(win$env$txt, "1.0", paste("The statistic value is:.\n", results$T2,".\n", "The p-value is:.\n", results$valor.p, ".\n"))
          }
          }
          calc.boot <- tkbutton(button_frame, text="T2RBoot", background="#CCE5FF", command=calc)
          help.boot <- tkbutton(button_frame, text="Help", background="#CCE5FF", command= t2boothelp)
          tkpack(calc.boot, padx = c(40,0), side="left")
          tkpack(help.boot, padx = c(40,0), side="left")
          tkpack(tklabel(button_frame, text="B", background="#CCE5FF", width="5"), B.entry, padx = c(40,0),side="left")
          sapply(2, function(i) tkentryconfigure(fileTestgraph, i, state = "normal"))
          sapply(c(0:1,3:5), function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
        }
      }
      tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
        response <- tkmessageBox(icon = "question",
                                 message = "Do you really want to close this window?" ,
                                 type = "yesno" ,
                                 parent = win)
        if (as.character(response) == "no") return()
        tkdestroy(win) # Caso contrario feche
      }
      )
    }

    #cria seu gr?fico
    FigplotT2RBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      robust <- covComed(X)
      XS <- robust$raw.center
      SS <- robust$raw.cov
      T2 <- n * t(XS - mu0) %*% solve(SS) %*% (XS - mu0)
      T2v <- T2
      T2b <- numeric(length = 80)
      for (i in 1:(B=80))
      {
        Xb <- mvrnorm(n, mu0, SS)
        robust <- covComed(Xb)
        Xsb <- robust$raw.center
        Ssb <- robust$raw.cov
        T2b[[i]] <- n * t(Xsb - mu0) %*% ginv(Ssb) %*% (Xsb - mu0)
      }
      T2v <- c(T2v,T2b)
      VF <- c(T2v,T2b)
      #OS GRUPOS
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1,ifelse(VF>=T2RobustBoot(X,mu0, B=80)$T2[,1],2, 0))
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("T2RobustBoot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=T2RobustBoot(X,mu0, B=80)$T2[,1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("T2RobustBoot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    FigplotT2RRBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      robust <- covComed(X)
      XS <- robust$raw.center
      SS <- robust$raw.cov
      T2 <- n * t(XS - mu0) %*% solve(SS) %*% (XS - mu0)
      T2v <- T2
      T2b <- numeric(length = 80)
      for (i in 1:(B=80))
      {
        Xb <- mvrnorm(n, mu0, SS)
        robust <- covComed(Xb)
        Xsb <- robust$raw.center
        Ssb <- robust$raw.cov
        T2b[[i]] <- n * t(Xsb - mu0) %*% ginv(Ssb) %*% (Xsb - mu0)
      }
      T2v <- c(T2v,T2b)
      VF <- c(T2v,T2b)
      #OS GRUPOS
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1,ifelse(VF>=T2RobustBoot(X,mu0, B=80)$T2[,1],2, 0))
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("T2RobustBoot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=T2RobustBoot(X,mu0, B=80)$T2[,1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("T2RobustBoot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    plotT2RBoot <- function() {
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        if(nrow(X)<=nrow(mu0)){
          tkmessageBox(message=paste("The T2RBoot test cannot be calculated!!!!!!"))
        } else{
          dialog <- tkmessageBox (message = "Do you want to view this figure in color?" ,
                                  icon = "question" , type = "yesno")
          if(as.character(dialog) == "yes"){
            pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
            }
            close(pb)
            hscale <- 2    # Horizontal scaling
            vscale <- 1    # Vertical scaling
            win <- tktoplevel()
            tkwm.title(win,"T2 robust parametric bootstrap test.")
            win$env$plot <- tkrplot(win, fun = FigplotT2RBoot, hscale = hscale, vscale = vscale)
            tkpack(win$env$plot)
            #create button save
            frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
            tkpack(frame , expand = TRUE , fill = "both")
            ##
            nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
            #button
            button_frame <- ttkframe(frame)
            tkpack(button_frame , anchor = "ne")
            button1 <- ttkbutton (button_frame , text = "Save")
            tkpack(button1 , side = "right")
            rb1 <- tkradiobutton(button_frame)
            rb2 <- tkradiobutton(button_frame)
            rb3 <- tkradiobutton(button_frame)
            rb4 <- tkradiobutton(button_frame)
            rb5 <- tkradiobutton(button_frame)
            rb6 <- tkradiobutton(button_frame)
            rb7 <- tkradiobutton(button_frame)
            rbValue <- tclVar("all")
            tkconfigure(rb1, variable=rbValue, value="png")
            tkconfigure(rb3, variable=rbValue, value="jpeg")
            tkconfigure(rb4, variable=rbValue, value="pdf")
            tkconfigure(rb2, variable=rbValue, value="eps")
            tkconfigure(rb5, variable=rbValue, value="tiff")
            tkconfigure(rb6, variable=rbValue, value="bmp")
            tkconfigure(rb7, variable=rbValue, value="all")


            tkpack(tklabel(button_frame, text="png"), rb1, side="left")
            tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
            tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
            tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
            tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
            tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
            tkpack(tklabel(button_frame, text="all"), rb7, side="left")


            handler1 <- function () {
              dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                rbVal <- as.character(tclvalue(rbValue))
                if (rbVal=="png"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.png", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="eps"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.eps", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="jpeg"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.jpeg", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="pdf"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.pdf", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="tiff"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.tiff", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="bmp"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.bmp", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="all"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.png", path = folder, FigplotT2RBoot(ml), width = 10, height = 7)
                  ggsave("T2RBoot.eps", path = folder, FigplotT2RBoot(ml), width = 10, height = 7)
                  ggsave("T2RBoot.jpeg", path = folder, FigplotT2RBoot(ml), width = 10, height = 7)
                  ggsave("T2RBoot.pdf", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                  ggsave("T2RBoot.tiff", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                  ggsave("T2RBoot.bmp", path = folder, FigplotT2RBoot(ml),width = 10, height = 7)
                }
              }
              else{
                dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                        icon = "question" , type = "yesno")
                if(as.character(dialog) == "yes"){
                  tkdestroy(win)
                }
              }
            }
            tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
              response <- tkmessageBox(icon = "question",
                                       message = "Do you really want to close this window?" ,
                                       type = "yesno" ,
                                       parent = win)
              if (as.character(response) == "no") return()
              tkdestroy(win) # Caso contrario feche
            }
            )
          } else{
            pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
            u <- c(0, sort(runif(20, 0, 100)), 100)
            for(i in u) {
              Sys.sleep(0.1)
              info <- sprintf("%d%% done", round(i))
              setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
            }
            close(pb)
            hscale <- 2    # Horizontal scaling
            vscale <- 1    # Vertical scaling
            win <- tktoplevel()
            win$env$plot <- tkrplot(win, fun = FigplotT2RRBoot, hscale = hscale, vscale = vscale)
            tkpack(win$env$plot)
            #create button save
            frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
            tkpack(frame , expand = TRUE , fill = "both")
            ##
            nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
            #button
            button_frame <- ttkframe(frame)
            tkpack(button_frame , anchor = "ne")
            button1 <- ttkbutton (button_frame , text = "Save")
            tkpack(button1 , side = "right")
            rb1 <- tkradiobutton(button_frame)
            rb2 <- tkradiobutton(button_frame)
            rb3 <- tkradiobutton(button_frame)
            rb4 <- tkradiobutton(button_frame)
            rb5 <- tkradiobutton(button_frame)
            rb6 <- tkradiobutton(button_frame)
            rb7 <- tkradiobutton(button_frame)
            rbValue <- tclVar("all")
            tkconfigure(rb1, variable=rbValue, value="png")
            tkconfigure(rb3, variable=rbValue, value="jpeg")
            tkconfigure(rb4, variable=rbValue, value="pdf")
            tkconfigure(rb2, variable=rbValue, value="eps")
            tkconfigure(rb5, variable=rbValue, value="tiff")
            tkconfigure(rb6, variable=rbValue, value="bmp")
            tkconfigure(rb7, variable=rbValue, value="all")


            tkpack(tklabel(button_frame, text="png"), rb1, side="left")
            tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
            tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
            tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
            tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
            tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
            tkpack(tklabel(button_frame, text="all"), rb7, side="left")


            handler1 <- function () {
              dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                rbVal <- as.character(tclvalue(rbValue))
                if (rbVal=="png"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.png", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="eps"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.eps", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="jpeg"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.jpeg", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="pdf"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.pdf", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="tiff"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.tiff", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="bmp"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.bmp", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
                if (rbVal=="all"){
                  folder <- DescToolsAddIns::dir.choose()
                  ggsave("T2RBoot.png", path = folder, FigplotT2RRBoot(ml), width = 10, height = 7)
                  ggsave("T2RBoot.eps", path = folder, FigplotT2RRBoot(ml), width = 10, height = 7)
                  ggsave("T2RBoot.jpeg", path = folder, FigplotT2RRBoot(ml), width = 10, height = 7)
                  ggsave("T2RBoot.pdf", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                  ggsave("T2RBoot.tiff", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                  ggsave("T2RBoot.bmp", path = folder, FigplotT2RRBoot(ml),width = 10, height = 7)
                }
              }
              else{
                dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                        icon = "question" , type = "yesno")
                if(as.character(dialog) == "yes"){
                  tkdestroy(win)
                }
              }
            }
            tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
              response <- tkmessageBox(icon = "question",
                                       message = "Do you really want to close this window?" ,
                                       type = "yesno" ,
                                       parent = win)
              if (as.character(response) == "no") return()
              tkdestroy(win) # Caso contrario feche
            }
            )
          }
          tkconfigure(button1 , command = handler1)
        }
      }
    }


    #teste LRT com tra?o
    LRTTTrace <- function(){
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        win <- tktoplevel()
        tkwm.title(win,"Results of LRT test!!")
        win$env$txt <- tk2text(win, width = 60, height = 10)
        tkpack(win$env$txt, fill = "both", expand = TRUE)
        frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
        tkpack(frame, expand = TRUE , fill = "both")
        ##
        nested_frame <- ttkframe(frame) ; tkpack(nested_frame)
        #button
        button_frame <- ttkframe(frame)
        tkpack(button_frame , anchor = "ne")
        calc <- function(){
          pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("LRT test (%s)", info), info)
          }
          close(pb)
          results <- TVMM::LRTTrace(X, mu0)
          tkinsert(win$env$txt, "1.0", paste("The statistic value is:.\n", results$LRTT,".\n", "The p-value is:.\n", results$valor.p, ".\n"))
        }
        calc.boot <- tkbutton(button_frame, text="LRTT", background="#CCE5FF", command=calc)
        help.boot <- tkbutton(button_frame, text="Help", background="#CCE5FF", command= LRTThelp)
        tkpack(calc.boot, padx = c(40,0),side="left")
        tkpack(help.boot, padx = c(40,0),side="right")
        sapply(3, function(i) tkentryconfigure(fileTestgraph, i, state = "normal"))
        sapply(c(0:2,4:5), function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
      }
      tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
        response <- tkmessageBox(icon = "question",
                                 message = "Do you really want to close this window?" ,
                                 type = "yesno" ,
                                 parent = win)
        if (as.character(response) == "no") return()
        tkdestroy(win) # Caso contrario feche
      }
      )
    }

    #cria seu gr?fico (esse tem de olhar no caso de alta dimens?o)
    FigplotLRTTrace <- function(...) {
      p <- ncol(X)
      VF <- rchisq(2000, p)
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1, 0)
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("LRTTrace test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=LRTTrace(X,mu0)$LRTT[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("LRTTrace test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    FigplotLRTTTrace <- function(...) {
      p <- ncol(X)
      VF <- rchisq(2000, p)
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1, 0)
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("LRTTrace test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=LRTTrace(X,mu0)$LRTT[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("LRTTrace test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    #cria a janela de sa??da gr?fica na GUI
    LRTTracegraph <- function() {
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        dialog <- tkmessageBox (message = "Do you want to view this figure in color?" ,
                                icon = "question" , type = "yesno")
        if(as.character(dialog) == "yes"){
          pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
          }
          close(pb)
          hscale <- 2    # Horizontal scaling
          vscale <- 1    # Vertical scaling
          win <- tktoplevel()
          tkwm.title(win,"LRT with trace test.")
          win$env$plot <- tkrplot(win, fun = FigplotLRTTrace, hscale = hscale, vscale = vscale)
          tkpack(win$env$plot)
          #create button save
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame , expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          button1 <- ttkbutton (button_frame , text = "Save")
          tkpack(button1 , side = "right")
          rb1 <- tkradiobutton(button_frame)
          rb2 <- tkradiobutton(button_frame)
          rb3 <- tkradiobutton(button_frame)
          rb4 <- tkradiobutton(button_frame)
          rb5 <- tkradiobutton(button_frame)
          rb6 <- tkradiobutton(button_frame)
          rb7 <- tkradiobutton(button_frame)
          rbValue <- tclVar("all")
          tkconfigure(rb1, variable=rbValue, value="png")
          tkconfigure(rb3, variable=rbValue, value="jpeg")
          tkconfigure(rb4, variable=rbValue, value="pdf")
          tkconfigure(rb2, variable=rbValue, value="eps")
          tkconfigure(rb5, variable=rbValue, value="tiff")
          tkconfigure(rb6, variable=rbValue, value="bmp")
          tkconfigure(rb7, variable=rbValue, value="all")


          tkpack(tklabel(button_frame, text="png"), rb1, side="left")
          tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
          tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
          tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
          tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
          tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
          tkpack(tklabel(button_frame, text="all"), rb7, side="left")


          handler1 <- function () {
            dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                    icon = "question" , type = "yesno")
            if(as.character(dialog) == "yes"){
              rbVal <- as.character(tclvalue(rbValue))
              if (rbVal=="png"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.png", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="eps"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.eps", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="jpeg"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.jpeg", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="pdf"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.pdf", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="tiff"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.tiff", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="bmp"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.bmp", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="all"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.png", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.eps", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.jpeg", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.pdf", path = folder, FigplotLRTTrace(ml),width = 10, height = 7)
                ggsave("LRTTrace.tiff", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.bmp", path = folder, FigplotLRTTrace(ml), width = 10, height = 7)
              }
            }
            else{
              dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                tkdestroy(win)
              }
            }
          }
          tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
            response <- tkmessageBox(icon = "question",
                                     message = "Do you really want to close this window?" ,
                                     type = "yesno" ,
                                     parent = win)
            if (as.character(response) == "no") return()
            tkdestroy(win) # Caso contrario feche
          }
          )
        } else{
          pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
          }
          close(pb)
          hscale <- 2    # Horizontal scaling
          vscale <- 1    # Vertical scaling
          win <- tktoplevel()
          tkwm.title(win,"LRT with trace test.")
          win$env$plot <- tkrplot(win, fun = FigplotLRTTTrace, hscale = hscale, vscale = vscale)
          tkpack(win$env$plot)
          #create button save
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame , expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          button1 <- ttkbutton (button_frame , text = "Save")
          tkpack(button1 , side = "right")
          rb1 <- tkradiobutton(button_frame)
          rb2 <- tkradiobutton(button_frame)
          rb3 <- tkradiobutton(button_frame)
          rb4 <- tkradiobutton(button_frame)
          rb5 <- tkradiobutton(button_frame)
          rb6 <- tkradiobutton(button_frame)
          rb7 <- tkradiobutton(button_frame)
          rbValue <- tclVar("all")
          tkconfigure(rb1, variable=rbValue, value="png")
          tkconfigure(rb3, variable=rbValue, value="jpeg")
          tkconfigure(rb4, variable=rbValue, value="pdf")
          tkconfigure(rb2, variable=rbValue, value="eps")
          tkconfigure(rb5, variable=rbValue, value="tiff")
          tkconfigure(rb6, variable=rbValue, value="bmp")
          tkconfigure(rb7, variable=rbValue, value="all")


          tkpack(tklabel(button_frame, text="png"), rb1, side="left")
          tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
          tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
          tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
          tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
          tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
          tkpack(tklabel(button_frame, text="all"), rb7, side="left")


          handler1 <- function () {
            dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                    icon = "question" , type = "yesno")
            if(as.character(dialog) == "yes"){
              rbVal <- as.character(tclvalue(rbValue))
              if (rbVal=="png"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.png", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="eps"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.eps", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="jpeg"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.jpeg", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="pdf"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.pdf", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="tiff"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.tiff", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="bmp"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.bmp", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
              if (rbVal=="all"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTrace.png", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.eps", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.jpeg", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.pdf", path = folder, FigplotLRTTTrace(ml),width = 10, height = 7)
                ggsave("LRTTrace.tiff", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
                ggsave("LRTTrace.bmp", path = folder, FigplotLRTTTrace(ml), width = 10, height = 7)
              }
            }
            else{
              dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                tkdestroy(win)
              }
            }
          }
          tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
            response <- tkmessageBox(icon = "question",
                                     message = "Do you really want to close this window?" ,
                                     type = "yesno" ,
                                     parent = win)
            if (as.character(response) == "no") return()
            tkdestroy(win) # Caso contrario feche
          }
          )
        }
        tkconfigure(button1 , command = handler1)
      }
    }

    #Teste LRT bootstrap

    LRTTTBoot <- function(){
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        win <- tktoplevel()
        tkwm.title(win,"Results of LRTBoot test!!")
        win$env$txt <- tk2text(win, width = 60, height = 10)
        tkpack(win$env$txt, fill = "both", expand = TRUE)
        frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
        tkpack(frame, expand = TRUE , fill = "both")
        ##
        nested_frame <- ttkframe(frame) ; tkpack(nested_frame)
        #button
        button_frame <- ttkframe(frame)
        tkpack(button_frame , anchor = "ne")
        b <- tclVar(80)
        B.entry <- tkentry(button_frame,textvariable=b, width=7)
        calc <- function(){B <- as.numeric(tclvalue(b))
        if(B<=0 | B%%floor(B)!=0 | is.na(B)==TRUE){
          tkmessageBox(message=paste("B must be a positive integer!!!!!!"))
        } else{
          pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("LRTTBoot test (%s)", info), info)
          }
          close(pb)
          results <- TVMM::LRTTBoot(X, mu0, B)
          tkinsert(win$env$txt, "1.0", paste("The statistic value is:.\n", results$LRTT,".\n", "The p-value is:.\n", results$valor.p, ".\n"))
        }
        }
        calc.boot <- tkbutton(button_frame, text="LRTTBoot", background="#CCE5FF", command=calc)
        help.boot <- tkbutton(button_frame, text="Help", background="#CCE5FF", command= LRTTboothelp)
        tkpack(calc.boot, padx = c(40,0),side="left")
        tkpack(help.boot, padx = c(40,0),side="left")
        tkpack(tklabel(button_frame, text="B", background="#CCE5FF", width="7"), B.entry, padx = c(40,0),side="left")
        sapply(4, function(i) tkentryconfigure(fileTestgraph, i, state = "normal"))
        sapply(c(0:3,5), function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
      }
      tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
        response <- tkmessageBox(icon = "question",
                                 message = "Do you really want to close this window?" ,
                                 type = "yesno" ,
                                 parent = win)
        if (as.character(response) == "no") return()
        tkdestroy(win) # Caso contrario feche
      }
      )
    }

    #cria seu gr?fico
    FigplotLRTTBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      Xsrb <- apply(X, 2, mean)
      Ssrb <- var(X)
      H <- (Xsrb - mu0) %*% t(Xsrb - mu0)
      LRTTo <- n * (log(sum(diag(Ssrb + H))) - log(sum(diag(Ssrb))))
      LRTTv <- LRTTo
      LRTTb <- numeric(length = 80)
      for (i in 1:(B=80))
      {
        Xbb <- mvrnorm(n, mu0, Ssrb)
        Xsbb <- apply(Xbb, 2, mean)
        Ssbb <- var(Xbb)
        H <- (Xsbb - mu0) %*% t(Xsbb - mu0)
        LRTTb[i] <- n * (log(sum(diag(Ssbb + H))) - log(sum(diag(Ssbb))))
      }
      VF <- c(LRTTv, LRTTb)
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1, 0)
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("LRTTBoot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=LRTTBoot(X,mu0, B=80)$LRTT[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("LRTTBoot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    FigplotLRTTBBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      Xsrb <- apply(X, 2, mean)
      Ssrb <- var(X)
      H <- (Xsrb - mu0) %*% t(Xsrb - mu0)
      LRTTo <- n * (log(sum(diag(Ssrb + H))) - log(sum(diag(Ssrb))))
      LRTTv <- LRTTo
      LRTTb <- numeric(length = 80)
      for (i in 1:(B=80))
      {
        Xbb <- mvrnorm(n, mu0, Ssrb)
        Xsbb <- apply(Xbb, 2, mean)
        Ssbb <- var(Xbb)
        H <- (Xsbb - mu0) %*% t(Xsbb - mu0)
        LRTTb[i] <- n * (log(sum(diag(Ssbb + H))) - log(sum(diag(Ssbb))))
      }
      VF <- c(LRTTv, LRTTb)
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1, 0)
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("LRTTBoot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=LRTTBoot(X,mu0, B=80)$LRTT[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("LRTTBoot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    #cria a janela de sa??da gr?fica na GUI
    LRTTBootgraph <- function() {
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        dialog <- tkmessageBox (message = "Do you want to view this figure in color?" ,
                                icon = "question" , type = "yesno")
        if(as.character(dialog) == "yes"){
          pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
          }
          close(pb)
          hscale <- 2    # Horizontal scaling
          vscale <- 1    # Vertical scaling
          win <- tktoplevel()
          tkwm.title(win,"LRT parametric bootstrap with trace test.")
          win$env$plot <- tkrplot(win, fun = FigplotLRTTBoot, hscale = hscale, vscale = vscale)
          tkpack(win$env$plot)
          #create button save
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame , expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          button1 <- ttkbutton (button_frame , text = "Save")
          tkpack(button1 , side = "right")
          rb1 <- tkradiobutton(button_frame)
          rb2 <- tkradiobutton(button_frame)
          rb3 <- tkradiobutton(button_frame)
          rb4 <- tkradiobutton(button_frame)
          rb5 <- tkradiobutton(button_frame)
          rb6 <- tkradiobutton(button_frame)
          rb7 <- tkradiobutton(button_frame)
          rbValue <- tclVar("all")
          tkconfigure(rb1, variable=rbValue, value="png")
          tkconfigure(rb3, variable=rbValue, value="jpeg")
          tkconfigure(rb4, variable=rbValue, value="pdf")
          tkconfigure(rb2, variable=rbValue, value="eps")
          tkconfigure(rb5, variable=rbValue, value="tiff")
          tkconfigure(rb6, variable=rbValue, value="bmp")
          tkconfigure(rb7, variable=rbValue, value="all")


          tkpack(tklabel(button_frame, text="png"), rb1, side="left")
          tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
          tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
          tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
          tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
          tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
          tkpack(tklabel(button_frame, text="all"), rb7, side="left")


          handler1 <- function () {
            dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                    icon = "question" , type = "yesno")
            if(as.character(dialog) == "yes"){
              rbVal <- as.character(tclvalue(rbValue))
              if (rbVal=="png"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.png", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="eps"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.eps", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="jpeg"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.jpeg", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="pdf"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.pdf", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="tiff"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.tiff", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="bmp"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.bmp", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="all"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.png", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.eps", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.jpeg", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.pdf", path = folder, FigplotLRTTBoot(ml),width = 10, height = 7)
                ggsave("LRTTBoot.tiff", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.bmp", path = folder, FigplotLRTTBoot(ml), width = 10, height = 7)
              }
            }
            else{
              dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                tkdestroy(win)
              }
            }
          }
          tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
            response <- tkmessageBox(icon = "question",
                                     message = "Do you really want to close this window?" ,
                                     type = "yesno" ,
                                     parent = win)
            if (as.character(response) == "no") return()
            tkdestroy(win) # Caso contrario feche
          }
          )
        } else{
          pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
          }
          close(pb)
          hscale <- 2    # Horizontal scaling
          vscale <- 1    # Vertical scaling
          win <- tktoplevel()
          tkwm.title(win,"LRT parametric bootstrap with trace test.")
          win$env$plot <- tkrplot(win, fun = FigplotLRTTBBoot, hscale = hscale, vscale = vscale)
          tkpack(win$env$plot)
          #create button save
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame , expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          button1 <- ttkbutton (button_frame , text = "Save")
          tkpack(button1 , side = "right")
          rb1 <- tkradiobutton(button_frame)
          rb2 <- tkradiobutton(button_frame)
          rb3 <- tkradiobutton(button_frame)
          rb4 <- tkradiobutton(button_frame)
          rb5 <- tkradiobutton(button_frame)
          rb6 <- tkradiobutton(button_frame)
          rb7 <- tkradiobutton(button_frame)
          rbValue <- tclVar("all")
          tkconfigure(rb1, variable=rbValue, value="png")
          tkconfigure(rb3, variable=rbValue, value="jpeg")
          tkconfigure(rb4, variable=rbValue, value="pdf")
          tkconfigure(rb2, variable=rbValue, value="eps")
          tkconfigure(rb5, variable=rbValue, value="tiff")
          tkconfigure(rb6, variable=rbValue, value="bmp")
          tkconfigure(rb7, variable=rbValue, value="all")


          tkpack(tklabel(button_frame, text="png"), rb1, side="left")
          tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
          tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
          tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
          tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
          tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
          tkpack(tklabel(button_frame, text="all"), rb7, side="left")


          handler1 <- function () {
            dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                    icon = "question" , type = "yesno")
            if(as.character(dialog) == "yes"){
              rbVal <- as.character(tclvalue(rbValue))
              if (rbVal=="png"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.png", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="eps"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.eps", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="jpeg"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.jpeg", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="pdf"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.pdf", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="tiff"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.tiff", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="bmp"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.bmp", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="all"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTBoot.png", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.eps", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.jpeg", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.pdf", path = folder, FigplotLRTTBBoot(ml),width = 10, height = 7)
                ggsave("LRTTBoot.tiff", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
                ggsave("LRTTBoot.bmp", path = folder, FigplotLRTTBBoot(ml), width = 10, height = 7)
              }
            }
            else{
              dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                tkdestroy(win)
              }
            }
          }
          tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
            response <- tkmessageBox(icon = "question",
                                     message = "Do you really want to close this window?" ,
                                     type = "yesno" ,
                                     parent = win)
            if (as.character(response) == "no") return()
            tkdestroy(win) # Caso contrario feche
          }
          )
        }
        tkconfigure(button1 , command = handler1)
      }
    }

    # teste LRTT roobusto Bootstrap
    LRTTTRBoot<- function(){
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        win <- tktoplevel()
        tkwm.title(win,"Results of LRTRBoot test!!")
        win$env$txt <- tk2text(win, width = 60, height = 10)
        tkpack(win$env$txt, fill = "both", expand = TRUE)
        frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
        tkpack(frame, expand = TRUE , fill = "both")
        ##
        nested_frame <- ttkframe(frame) ; tkpack(nested_frame)
        #button
        button_frame <- ttkframe(frame)
        tkpack(button_frame , anchor = "ne")
        b <- tclVar(80)
        B.entry <- tkentry(button_frame,textvariable=b, width=7)
        calc <- function(){B <- as.numeric(tclvalue(b))
        if(B<=0 | B%%floor(B)!=0 | is.na(B)==TRUE){
          tkmessageBox(message=paste("B must be a positive integer!!!!!!"))
        } else{
          pb <- tkProgressBar("Test progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("LRTTRBoot test (%s)", info), info)
          }
          close(pb)
          results <- TVMM::LRTTRBoot(X, mu0, B)
          tkinsert(win$env$txt, "1.0", paste("The statistic value is:.\n", results$LRTT,".\n", "The p-value is:.\n", results$valor.p, ".\n"))
        }
        }
        calc.boot <- tkbutton(button_frame, text="LRTTRBoot", background="#CCE5FF", command=calc)
        help.boot <- tkbutton(button_frame, text="Help", background="#CCE5FF", command= LRTTRboothelp)
        tkpack(calc.boot, padx = c(40,0),side="left")
        tkpack(help.boot, padx = c(40,0),side="left")
        tkpack(tklabel(button_frame, text="B", background="#CCE5FF", width="7"), B.entry, padx = c(40,0),side="left")
        sapply(5, function(i) tkentryconfigure(fileTestgraph, i, state = "normal"))
        sapply(c(0:4), function(i) tkentryconfigure(fileTestgraph, i, state = "disabled"))
      }
      tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
        response <- tkmessageBox(icon = "question",
                                 message = "Do you really want to close this window?" ,
                                 type = "yesno" ,
                                 parent = win)
        if (as.character(response) == "no") return()
        tkdestroy(win) # Caso contrario feche
      }
      )
    }

    #cria seu gr?fico
    FigplotLRTTRBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      robust <- covComed(X)
      Xsrb <- robust$raw.center
      Ssrb <- robust$raw.cov
      H <- (Xsrb - mu0) %*% t(Xsrb - mu0)
      LRTTo <- n * (log(sum(diag(Ssrb + H))) - log(sum(diag(Ssrb))))
      LRTTv <- LRTTo
      LRTTb <- numeric(length = 80)
      for (i in 1:(B= 80))
      {
        Xrbb <- mvrnorm(n, mu0, Ssrb)
        robust <- covComed(Xrbb)
        Xsrbb <- robust$raw.center
        Ssrbb <- robust$raw.cov
        H <- (Xsrbb - mu0) %*% t(Xsrbb - mu0)
        LRTTb[i] <- n * (log(sum(diag(Ssrbb + H))) - log(sum(diag(Ssrbb))))
      }
      VF <- c(LRTTv, LRTTb)
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1, 0)
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("LRTTRboot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=LRTTRBoot(X,mu0, B=80)$LRTT[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=color,
                          name="",
                          labels=c("LRTTRboot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    FigplotLRTTRRBoot <- function(...) {
      n <- nrow(X)
      p <- ncol(X)
      robust <- covComed(X)
      Xsrb <- robust$raw.center
      Ssrb <- robust$raw.cov
      H <- (Xsrb - mu0) %*% t(Xsrb - mu0)
      LRTTo <- n * (log(sum(diag(Ssrb + H))) - log(sum(diag(Ssrb))))
      LRTTv <- LRTTo
      LRTTb <- numeric(length = 80)
      for (i in 1:(B= 80))
      {
        Xrbb <- mvrnorm(n, mu0, Ssrb)
        robust <- covComed(Xrbb)
        Xsrbb <- robust$raw.center
        Ssrbb <- robust$raw.cov
        H <- (Xsrbb - mu0) %*% t(Xsrbb - mu0)
        LRTTb[i] <- n * (log(sum(diag(Ssrbb + H))) - log(sum(diag(Ssrbb))))
      }
      VF <- c(LRTTv, LRTTb)
      group <- ifelse(VF>=as.numeric(quantile(VF,0.95)),1, 0)
      Dat <- as.data.frame(cbind(VF, group))
      p1 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group)))+
        geom_histogram(aes(x = VF, fill = as.factor(group)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("LRTTRboot test density.","Critical region (5%)."))
      ##########
      group1 <- ifelse(VF>=LRTTRBoot(X,mu0, B=80)$LRTT[1],1, 0)
      Dat1 <- as.data.frame(cbind(VF, group1))
      p2 <- ggplot(Dat,aes(x=VF,  y = (..count..)/sum(..count..), group=as.factor(group1)))+
        geom_histogram(aes(x = VF, fill = as.factor(group1)),colour="black",
                       bins = round(sqrt(length(VF)))+1)+
        xlab ("Quantiles")+
        ylab("Density")+
        theme_classic()+
        theme(legend.position="right",
              legend.box = "horizontal",legend.title=element_text(size=12),
              legend.text=element_text(size=12), axis.text=element_text(size=15, colour = "#000000"),
              axis.title=element_text(size=15, colour = "#000000"), axis.line = element_line(colour = "#000000"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        scale_fill_manual(values=Color,
                          name="",
                          labels=c("LRTTRboot test density.", "Statistic observed."))
      ml <- grid.arrange(p1, p2, ncol=1)
    }

    #cria a janela de sa??da gr?fica na GUI
    LRTTRBootgraph <- function() {
      if(is.null(X)==TRUE){
        tkmessageBox(message=paste("You haven't loaded the dataset yet!!!"))
      } else{
        dialog <- tkmessageBox (message = "Do you want to view this figure in color?" ,
                                icon = "question" , type = "yesno")
        if(as.character(dialog) == "yes"){
          pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
          }
          close(pb)
          hscale <- 2    # Horizontal scaling
          vscale <- 1    # Vertical scaling
          win <- tktoplevel()
          tkwm.title(win,"LRT robust parametric bootstrap with trace test.")
          win$env$plot <- tkrplot(win, fun = FigplotLRTTRBoot, hscale = hscale, vscale = vscale)
          tkpack(win$env$plot)
          #create button save
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame , expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          button1 <- ttkbutton (button_frame , text = "Save")
          tkpack(button1 , side = "right")
          rb1 <- tkradiobutton(button_frame)
          rb2 <- tkradiobutton(button_frame)
          rb3 <- tkradiobutton(button_frame)
          rb4 <- tkradiobutton(button_frame)
          rb5 <- tkradiobutton(button_frame)
          rb6 <- tkradiobutton(button_frame)
          rb7 <- tkradiobutton(button_frame)
          rbValue <- tclVar("all")
          tkconfigure(rb1, variable=rbValue, value="png")
          tkconfigure(rb3, variable=rbValue, value="jpeg")
          tkconfigure(rb4, variable=rbValue, value="pdf")
          tkconfigure(rb2, variable=rbValue, value="eps")
          tkconfigure(rb5, variable=rbValue, value="tiff")
          tkconfigure(rb6, variable=rbValue, value="bmp")
          tkconfigure(rb7, variable=rbValue, value="all")


          tkpack(tklabel(button_frame, text="png"), rb1, side="left")
          tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
          tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
          tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
          tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
          tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
          tkpack(tklabel(button_frame, text="all"), rb7, side="left")


          handler1 <- function () {
            dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                    icon = "question" , type = "yesno")
            if(as.character(dialog) == "yes"){
              rbVal <- as.character(tclvalue(rbValue))
              if (rbVal=="png"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.png", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="eps"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.eps", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="jpeg"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.jpeg", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="pdf"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.pdf", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="tiff"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.tiff", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="bmp"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.bmp", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="all"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.png", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.eps", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.jpeg", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.pdf", path = folder, FigplotLRTTRBoot(ml),width = 10, height = 7)
                ggsave("LRTTRBoot.tiff", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.bmp", path = folder, FigplotLRTTRBoot(ml), width = 10, height = 7)
              }
            }
            else{
              dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                tkdestroy(win)
              }
            }
          }
          tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
            response <- tkmessageBox(icon = "question",
                                     message = "Do you really want to close this window?" ,
                                     type = "yesno" ,
                                     parent = win)
            if (as.character(response) == "no") return()
            tkdestroy(win) # Caso contrario feche
          }
          )
        } else{
          pb <- tkProgressBar("Graph progress bar", "",0, 100, 0)
          u <- c(0, sort(runif(20, 0, 100)), 100)
          for(i in u) {
            Sys.sleep(0.1)
            info <- sprintf("%d%% done", round(i))
            setTkProgressBar(pb, i, sprintf("graph (%s)", info), info)
          }
          close(pb)
          hscale <- 2    # Horizontal scaling
          vscale <- 1    # Vertical scaling
          win <- tktoplevel()
          tkwm.title(win,"LRT robust parametric bootstrap with trace test.")
          win$env$plot <- tkrplot(win, fun = FigplotLRTTRRBoot, hscale = hscale, vscale = vscale)
          tkpack(win$env$plot)
          #create button save
          frame <- ttkframe(win , padding = c(3 ,3 ,12 ,12), relief="groove", borderwidth = 2)
          tkpack(frame , expand = TRUE , fill = "both")
          ##
          nested_frame <- ttkframe(frame ) ; tkpack(nested_frame)
          #button
          button_frame <- ttkframe(frame)
          tkpack(button_frame , anchor = "ne")
          button1 <- ttkbutton (button_frame , text = "Save")
          tkpack(button1 , side = "right")
          rb1 <- tkradiobutton(button_frame)
          rb2 <- tkradiobutton(button_frame)
          rb3 <- tkradiobutton(button_frame)
          rb4 <- tkradiobutton(button_frame)
          rb5 <- tkradiobutton(button_frame)
          rb6 <- tkradiobutton(button_frame)
          rb7 <- tkradiobutton(button_frame)
          rbValue <- tclVar("all")
          tkconfigure(rb1, variable=rbValue, value="png")
          tkconfigure(rb3, variable=rbValue, value="jpeg")
          tkconfigure(rb4, variable=rbValue, value="pdf")
          tkconfigure(rb2, variable=rbValue, value="eps")
          tkconfigure(rb5, variable=rbValue, value="tiff")
          tkconfigure(rb6, variable=rbValue, value="bmp")
          tkconfigure(rb7, variable=rbValue, value="all")


          tkpack(tklabel(button_frame, text="png"), rb1, side="left")
          tkpack(tklabel(button_frame, text="jpeg"), rb3, side="left")
          tkpack(tklabel(button_frame, text="pdf"), rb4, side="left")
          tkpack(tklabel(button_frame, text="eps"), rb2, side="left")
          tkpack(tklabel(button_frame, text="tiff"), rb5, side="left")
          tkpack(tklabel(button_frame, text="bmp"), rb6, side="left")
          tkpack(tklabel(button_frame, text="all"), rb7, side="left")


          handler1 <- function () {
            dialog <- tkmessageBox (message = "Do you want to save this figure?" ,
                                    icon = "question" , type = "yesno")
            if(as.character(dialog) == "yes"){
              rbVal <- as.character(tclvalue(rbValue))
              if (rbVal=="png"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.png", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="eps"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.eps", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="jpeg"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.jpeg", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="pdf"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.pdf", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="tiff"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.tiff", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="bmp"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.bmp", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
              if (rbVal=="all"){
                folder <- DescToolsAddIns::dir.choose()
                ggsave("LRTTRBoot.png", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.eps", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.jpeg", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.pdf", path = folder, FigplotLRTTRRBoot(ml),width = 10, height = 7)
                ggsave("LRTTRBoot.tiff", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
                ggsave("LRTTRBoot.bmp", path = folder, FigplotLRTTRRBoot(ml), width = 10, height = 7)
              }
            }
            else{
              dialog <- tkmessageBox (message = "Don't you really want to save this figure?" ,
                                      icon = "question" , type = "yesno")
              if(as.character(dialog) == "yes"){
                tkdestroy(win)
              }
            }
          }
          tkwm.protocol(win, "WM_DELETE_WINDOW", function(){
            response <- tkmessageBox(icon = "question",
                                     message = "Do you really want to close this window?" ,
                                     type = "yesno" ,
                                     parent = win)
            if (as.character(response) == "no") return()
            tkdestroy(win) # Caso contrario feche
          }
          )
        }
        tkconfigure(button1 , command = handler1)
      }
    }


    t2help <- function(){
      img <- tclVar()
      win <- tktoplevel()
      tkwm.title(win,"The T2 test!!")
      tclimg2 <- tkimage.create("photo", img, file = system.file("etc", "T2test.gif", package="TVMM"))
      button.widget <- ttklabel(win, text = "", image = as.character(tclimg2), compound = "none")
      tkgrid(button.widget)
    }

    t2boothelp <- function(){
      img <- tclVar()
      win <- tktoplevel()
      tkwm.title(win,"The parametric bootstrap T2 test!!")
      tclimg3 <- tkimage.create("photo", img, file = system.file("etc", "T2boot.gif", package="TVMM"))
      button.widget <- ttklabel(win, text = "", image = as.character(tclimg3), compound = "none")
      tkgrid(button.widget)
    }

    t2rboothelp <- function(){
      img <- tclVar()
      win <- tktoplevel()
      tkwm.title(win,"The robust parametric bootstrap T2 test!!")
      tclimg4 <- tkimage.create("photo", img, file = system.file("etc", "T2RBoot.gif", package="TVMM"))
      button.widget <- ttklabel(win, text = "", image = as.character(tclimg4), compound = "none")
      tkgrid(button.widget)
    }

    LRTThelp <- function(){
      img <- tclVar()
      win <- tktoplevel()
      tkwm.title(win,"The likelihood ratio test with trace!!")
      tclimg5 <- tkimage.create("photo", img, file = system.file("etc", "LRTT.gif", package="TVMM"))
      button.widget <- ttklabel(win, text = "", image = as.character(tclimg5), compound = "none")
      tkgrid(button.widget)
    }

    LRTTboothelp <- function(){
      img <- tclVar()
      win <- tktoplevel()
      tkwm.title(win,"The likelihood ratio parametric bootstrap test with trace!!")
      tclimg6 <- tkimage.create("photo", img, file = system.file("etc", "LRTTBoot.gif", package="TVMM"))
      button.widget <- ttklabel(win, text = "", image = as.character(tclimg6), compound = "none")
      tkgrid(button.widget)
    }

    LRTTRboothelp <- function(){
      img <- tclVar()
      win <- tktoplevel()
      tkwm.title(win,"The robust likelihood ratio parametric bootstrap test with trace!!")
      tclimg7 <- tkimage.create("photo", img , file = system.file("etc", "LRTTRBoot.gif", package="TVMM"))
      button.widget <- ttklabel(win, text = "", image = as.character(tclimg7), compound = "none")
      tkgrid(button.widget)
    }
    #inicio da GUI



    tt <- tktoplevel(width=800, height=450)
    tkwm.title(tt,"GUI TVMM package")
    img <- tclVar()
    tclimg <- tkimage.create("photo" , img , file = system.file("etc", "figura.gif", package="TVMM"))
    figura <- ttklabel(tt, text = "", image = as.character(tclimg), compound = "none")
    tkgrid(figura)
    topMenu <- tkmenu(tt)
    fontMenu <- tkfont.create(family="sans",size=8,weight="normal",slant="italic")
    tkconfigure(tt, menu = topMenu)
    # fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    fileMenu <- tkmenu(topMenu, tearoff = FALSE, font = fontMenu)
    openMenutxt <- tkmenu(topMenu, tearoff=FALSE, font = fontMenu)
    openMenucsv <- tkmenu(topMenu, tearoff=FALSE, font = fontMenu)
    openEdit <- tkmenu(topMenu, tearoff=FALSE, font = fontMenu)
    fileTest <- tkmenu(topMenu,tearoff=FALSE, font = fontMenu)
    openTest <- tkmenu(topMenu, tearoff=FALSE, font = fontMenu)
    fileTestgraph <- tkmenu(topMenu,tearoff=FALSE, font = fontMenu)
    openTestgraph <- tkmenu(topMenu, tearoff=FALSE, font = fontMenu)
    helpMenu <- tkmenu(topMenu,tearoff=FALSE, font = fontMenu)
    openHelp <- tkmenu(topMenu, tearoff=FALSE, font = fontMenu)
    #command head file
    tkadd(openMenutxt,"command",label="matrix data (txt)", command=DaTa, font = fontMenu)
    tkadd(openMenutxt,"command",label="mean vector (txt)", command=MeanVec, font = fontMenu)
    tkadd(openMenucsv,"command",label="matrix data (csv)", command=DaTacsv, font = fontMenu)
    tkadd(openMenucsv,"command",label="mean vector (csv)", command=MeanVeccsv, font = fontMenu)
    # tkadd(openMenu,"command",label="xlsx", command=getXlsx, font = fontMenu)
    # tkadd(openMenu,"command",label="xls", command=getXls, font = fontMenu)
    tkadd(openEdit,"command",label="Matrix Data", command=editMatrixData, font = fontMenu)
    tkadd(openEdit,"command",label="Mean vector", command=editMeanData, font = fontMenu)
    tkadd(fileMenu, "cascade",label="Data (txt)", menu=openMenutxt, font = fontMenu)
    tkadd(fileMenu, "cascade",label="Data (csv)", menu=openMenucsv, font = fontMenu)
    tkadd(fileMenu, "cascade",label="View/Edit", menu=openEdit, font = fontMenu)
    #command tests
    tkadd(fileTest,"command",label="T2 test", command=T2OO, font = fontMenu)
    tkadd(fileTest,"command",label="T2 bootstrap test", command=T2BBoot, font = fontMenu)
    tkadd(fileTest,"command",label="T2 robust bootstrap test", command=T2RRBBoot, font = fontMenu)
    tkadd(fileTest,"command",label="LRT trace test", command=LRTTTrace, font = fontMenu)
    tkadd(fileTest,"command",label="LRT bootstrap trace test", command=LRTTTBoot, font = fontMenu)
    tkadd(fileTest,"command",label="LRT robust bootstrap trace test", command=LRTTTRBoot, font = fontMenu)
    #command graphs
    tkadd(fileTestgraph,"command",label="T2 original", command=plotT2O, font = fontMenu)
    tkadd(fileTestgraph,"command",label="T2 parametric bootstrap", command=plotT2Boot, font = fontMenu)
    tkadd(fileTestgraph,"command",label="T2 robust parametric bootstrap", command=plotT2RBoot, font = fontMenu)
    tkadd(fileTestgraph,"command",label="LRT trace", command=LRTTracegraph, font = fontMenu)
    tkadd(fileTestgraph,"command",label="LRT parametric bootstrap trace", command=LRTTBootgraph, font = fontMenu)
    tkadd(fileTestgraph,"command",label="LRT robust parametric bootstrap trace", command=LRTTRBootgraph, font = fontMenu)
    #command help
    tkadd(openHelp,"command",label="Manual", command=T2help, font = fontMenu)
    tkadd(openHelp,"command",label="T2 test", command=t2help, font = fontMenu)
    tkadd(openHelp,"command",label="T2Boot test", command=t2boothelp, font = fontMenu)
    tkadd(openHelp,"command",label="T2RBoot test", command=t2rboothelp, font = fontMenu)
    tkadd(openHelp,"command",label="LRTTrace test", command=LRTThelp, font = fontMenu)
    tkadd(openHelp,"command",label="LRTTBoot test", command=LRTTboothelp, font = fontMenu)
    tkadd(openHelp,"command",label="LRTTRBoot test", command=LRTTRboothelp, font = fontMenu)
    #command quit
    tkadd(fileMenu,"cascade",label="Quit", command=quit, font = fontMenu)
    #create the menus
    tkadd(topMenu, "cascade", label="File", menu=fileMenu, font = fontMenu)
    tkadd(topMenu, "cascade", label="Tests", menu=fileTest, font = fontMenu)
    tkadd(topMenu, "cascade", label="Graphs", menu=fileTestgraph, font = fontMenu)
    tkadd(topMenu, "cascade",label="Help", menu=openHelp, font = fontMenu)
    sapply(2:3, function(i) tkentryconfigure(topMenu, i, state = "disabled"))
    #the end program
    tkwm.protocol(tt, "WM_DELETE_WINDOW", function(){
      response <- tkmessageBox(icon = "question",
                               message = "Do you really want to close this GUI?" ,
                               type = "yesno" ,
                               parent = tt)
      if (as.character(response) == "no") return()
      tkdestroy(tt) # Caso contrario feche
    }
    )
    tkfocus(tt)
  }
}
