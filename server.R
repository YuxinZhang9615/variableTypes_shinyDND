library(shiny)
library(shinyDND)
library(shinyjs)
library(shinyBS)
library(V8)

bank <- read.csv("questionBank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

bankB = read.csv("questionBankB.csv")
bankB = data.frame(lapply(bankB, as.character), stringsAsFactors = FALSE)


shinyServer(function(input, output, session) {
  ####################################Hide Menu bar###############################################
  observe({
    if(input$go == 0){
      hide(selector = "#navMain li a[data-value=b]")
    }
    else {
      show(selector = "#navMain li a[data-value=b]")
    }
  })
  observe({
    hide(selector = "#navMain li a[data-value=c]")
  })
  observeEvent(input$next2, {
    show(selector = "#navMain li a[data-value=c]")
  })
  observe({
    hide(selector = "#navMain li a[data-value=d]")
  })
  observeEvent(input$finish, {
    show(selector = "#navMain li a[data-value=d]")
  })
  
  #Write all instructions in the home page
  output$about <- renderUI(
    print("About")
  )
  output$about1 <- renderUI(
    print("Identify four different variable types: Quantitative (numeric) discrete variables, Quantitative continuous variables,
          Qualitative (categorical) nominal variables, and Qualitative ordinal variables")
    )
  output$about2 <- renderUI(
    img(src = "STAT.png", width = 380)
  )
  output$instruction <- renderUI(
    print("Instruction")
  )
  output$instruction1 <- renderUI(
    print("Click")
  )
  output$instruction1b <- renderUI(
    print("to start the game")
  )
  output$instruction2 <- renderUI(
    print("Drag the variable names to the boxes by the variable types.")
  )
  output$instruction3 <- renderUI(
    print("Submit your answer only after finishing all the questions")
  )
  output$instruction4 <- renderUI(
    print("You may go to the next level only when you correct your answers")
  )
  output$instruction5 <- renderUI(
    print("The score you get after the first trial and the revised score you get after correct all answers will be weighted to
          generate your final score. Keep in mind that both the final score and consumed time will determine whether you will be on the leaderboard!")
    )
  output$acknowledge <- renderUI(
    print("Acknowledgement and Credit")
  )
  output$acknowledge1 <- renderUI(
    print("This app was developed and coded by Yuxin Zhang. Special thanks to Robert P. Carey III and Alex Chen for help on some programming issues.")
  )
  
  #reload the app when the reset button is clicked 
  observeEvent(input$reset_button, {js$reset()}) 
  
  #Create six pagers
  observeEvent(input$go,{
    updateNavbarPage(session = session,"navMain", selected = "b")
  })
  observeEvent(input$next2,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  observeEvent(input$finish,{
    updateNavbarPage(session = session,"navMain", selected = "d")
  })
  observeEvent(input$previous1,{
    updateNavbarPage(session = session,"navMain", selected = "c")
  })
  observeEvent(input$previous2,{
    updateNavbarPage(session = session,"navMain", selected = "b")
  })
  observeEvent(input$previous3,{
    updateNavbarPage(session = session,"navMain", selected = "a")
  })
  
  ##Set timer with start, stop, restart, stop, and termination; and show the timer
  time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
  
  observeEvent(input$go, {time$started<-TRUE})
  observeEvent(input$submitA, {time$started <- FALSE})
  observeEvent(input$next2, {time$started <- TRUE})
  observeEvent(input$submitB, {time$started <- FALSE})
  observeEvent(input$finish, {time$timer<-reactiveTimer(Inf)})
  
  observe({
    time$timer()
    if(isolate(time$started))
      time$inc<-isolate(time$inc)+1
  })
  #show the timer
  observeEvent(input$bt1 == TRUE, {
    toggle('timer1h')
    output$timer1 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  
  observeEvent(input$bt2 == TRUE, {
    toggle('timer2h')
    output$timer2 <- renderPrint({
      cat("you have used:", time$inc, "secs")})
  })
  output$timer3 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
  
  
  
  ##################################################################################################  
  numbers <- reactiveValues(dis = c(), cont = c(), nom = c(), ord = c())
  
  observeEvent(input$go,{
    numbers$dis = sample(1:10,4)
    numbers$cont = sample(11:36,4)
    numbers$nom = sample(37:56,4)
    numbers$ord = sample(57:71,4)
  })
  
  output$disID1 <- renderText({
    bank[numbers$dis[1], 2]
  })
  output$disID2 <- renderText({
    bank[numbers$dis[2], 2]
  })
  output$disID3 <- renderText({
    bank[numbers$dis[3], 2]
  })
  output$disID4 <- renderText({
    bank[numbers$dis[4], 2]
  })
  
  output$disName1 <- renderText({
    bank[numbers$dis[1],3]
  })
  output$disName2 <- renderText({
    bank[numbers$dis[2],3]
  })
  output$disName3 <- renderText({
    bank[numbers$dis[3],3]
  })
  output$disName4 <- renderText({
    bank[numbers$dis[4],3]
  })
  
  
  output$contID1 <- renderText({
    bank[numbers$cont[1], 2]
  })
  output$contID2 <- renderText({
    bank[numbers$cont[2], 2]
  })
  output$contID3 <- renderText({
    bank[numbers$cont[3], 2]
  })
  output$contID4 <- renderText({
    bank[numbers$cont[4], 2]
  })
  
  output$contName1 <- renderText({
    bank[numbers$cont[1],3]
  })
  output$contName2 <- renderText({
    bank[numbers$cont[2],3]
  })
  output$contName3 <- renderText({
    bank[numbers$cont[3],3]
  })
  output$contName4 <- renderText({
    bank[numbers$cont[4],3]
  })
  
  
  
  output$nomID1 <- renderText({
    bank[numbers$nom[1], 2]
  })
  output$nomID2 <- renderText({
    bank[numbers$nom[2], 2]
  })
  output$nomID3 <- renderText({
    bank[numbers$nom[3], 2]
  })
  output$nomID4 <- renderText({
    bank[numbers$nom[4], 2]
  })
  
  output$nomName1 <- renderText({
    bank[numbers$nom[1],3]
  })
  output$nomName2 <- renderText({
    bank[numbers$nom[2],3]
  })
  output$nomName3 <- renderText({
    bank[numbers$nom[3],3]
  })
  output$nomName4 <- renderText({
    bank[numbers$nom[4],3]
  })
  
  
  
  output$ordID1 <- renderText({
    bank[numbers$ord[1], 2]
  })
  output$ordID2 <- renderText({
    bank[numbers$ord[2], 2]
  })
  output$ordID3 <- renderText({
    bank[numbers$ord[3], 2]
  })
  output$ordID4 <- renderText({
    bank[numbers$ord[4], 2]
  })
  
  output$ordName1 <- renderText({
    bank[numbers$ord[1],3]
  })
  output$ordName2 <- renderText({
    bank[numbers$ord[2],3]
  })
  output$ordName3 <- renderText({
    bank[numbers$ord[3],3]
  })
  output$ordName4 <- renderText({
    bank[numbers$ord[4],3]
  })
  ###################################################################################################
  numbersB <- reactiveValues(disB = c(), contB = c(), nomB = c(), ordB = c(), indexB = c(), questionB = data.frame())
  
  observeEvent(input$go,{
    numbersB$disB = sample(1:13,1)
    numbersB$contB = sample(14:39,1)
    numbersB$nomB = sample(40:58,1)
    numbersB$ordB = sample(59:74,1)
    
    numbersB$indexB = sample(c("A","B","C","D"),4)
    numbersB$questionB = cbind(bankB[c(numbersB$disB,numbersB$contB,numbersB$nomB,numbersB$ordB),],numbersB$indexB)
    
  })
  output$imgQ1 <- renderText({
    paste("A.",numbersB$questionB[numbersB$questionB[5] == "A",4])
  })
  
  output$image1 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "A",3], width = "95%", height = "95%", style = "text-align: center")
  })
  output$imgQ2 <- renderText({
    paste("B.",numbersB$questionB[numbersB$questionB[5] == "B",4])
  })
  
  output$image2 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "B",3], width = "95%", height = "95%")
  })
  output$imgQ3 <- renderText({
    paste("C.",numbersB$questionB[numbersB$questionB[5] == "C",4])
  })
  output$image3 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "C",3], width = "95%", height = "95%")
  })
  output$imgQ4 <- renderText({
    paste("D.",numbersB$questionB[numbersB$questionB[5] == "D",4])
  })
  output$image4 <- renderUI({
    img(src = numbersB$questionB[numbersB$questionB[5] == "D",3], width = "95%", height = "95%")
  })
  
  
  
  
  
  
  
  
  
  ################################################################################################  
  
  observeEvent(input$submitA,{
    updateButton(session,"submitA",disabled = TRUE)
  })
  observeEvent(input$clear,{
    updateButton(session,"submitA",disabled = FALSE)
  })
  observeEvent(input$submitB,{
    updateButton(session,"submitB",disabled = TRUE)
  })
  observeEvent(input$clearB,{
    updateButton(session,"submitB",disabled = FALSE)
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer1 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer1 <- renderUI({
        if (!is.null(input$drp1)){
          if (any(input$drp1 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer2 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer2 <- renderUI({
        if (!is.null(input$drp2)){
          if (any(input$drp2 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer3 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer3 <- renderUI({
        if (!is.null(input$drp3)){
          if (any(input$drp3 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer4 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer4 <- renderUI({
        if (!is.null(input$drp4)){
          if (any(input$drp4 == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer5 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer5 <- renderUI({
        if (!is.null(input$drp5)){
          if (any(input$drp5 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer6 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer6 <- renderUI({
        if (!is.null(input$drp6)){
          if (any(input$drp6 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer7 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer7 <- renderUI({
        if (!is.null(input$drp7)){
          if (any(input$drp7 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer8 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer8 <- renderUI({
        if (!is.null(input$drp8)){
          if (any(input$drp8 == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer9 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer9 <- renderUI({
        if (!is.null(input$drp9)){
          if (any(input$drp9 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer10 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer10 <- renderUI({
        if (!is.null(input$drp10)){
          if (any(input$drp10 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer11 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer11 <- renderUI({
        if (!is.null(input$drp11)){
          if (any(input$drp11 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer12 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer12 <- renderUI({
        if (!is.null(input$drp12)){
          if (any(input$drp12 == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer13 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer13 <- renderUI({
        if (!is.null(input$drp13)){
          if (any(input$drp13 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer14 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer14 <- renderUI({
        if (!is.null(input$drp14)){
          if (any(input$drp14 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer15 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer15 <- renderUI({
        if (!is.null(input$drp15)){
          if (any(input$drp15 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitA,{  
    observeEvent(input$clear,{
      output$answer16 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer16 <- renderUI({
        if (!is.null(input$drp16)){
          if (any(input$drp16 == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer17 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer17 <- renderUI({
        if (!is.null(input$drop1)){
          if (input$drop1 == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete",5]){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer18 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer18 <- renderUI({
        if (!is.null(input$drop2)){
          if (input$drop2 == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous",5]){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer19 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer19 <- renderUI({
        if (!is.null(input$drop3)){
          if (input$drop3 == numbersB$questionB[numbersB$questionB[1] == "QualNominal",5]){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  observeEvent(input$submitB,{  
    observeEvent(input$clearB,{
      output$answer20 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer20 <- renderUI({
        if (!is.null(input$drop4)){
          if (input$drop4 == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal",5]){
            img(src = "check.png",width = 30)
          }else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  
  
  ####################################################################
  summation <- reactiveValues(summationA = c(rep(0,20)), summationB = c(rep(0,20)),summationScore = c(rep(0,20)))
  
  observeEvent(input$submitA,{
    score1 = c()
    score2 = c()
    score3 = c()
    score4 = c()
    for (i in c(input$drp1,input$drp2,input$drp3,input$drp4)){
      if (any(i == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
        score1 = c(score1,5)
      }else{
        score1 = c(score1,-3)
      }
    }
    for (i in c(input$drp5,input$drp6,input$drp7,input$drp8)){
      if (any(i == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
        score2 = c(score2, 5)}else{
          score2 = c(score2, -3)}
    }
    for (i in c(input$drp9,input$drp10,input$drp11,input$drp12)){
      if (any(i == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
        score3 = c(score3, 5)}else{
          score3 = c(score3, -3)}
    }
    for (i in c(input$drp13,input$drp14,input$drp15,input$drp16)){
      if (any(i == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
        score4 = c(score4, 5)}else{
          score4 = c(score4, -3)}
    }
    
    # summation$summationA <- c(summation$summationA, sum(c(score1,score2,score3,score4))) 
    summation$summationA[input$submitA] <- sum(c(score1,score2,score3,score4))
  })
  
  observeEvent(input$submitB,{
    score5 = c()
    
    for (i in input$drop1){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop2){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop3){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualNominal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop4){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    
    #summation$summationB <- c(summation$summationB, sum(score5)) 
    summation$summationB[input$submitB] <- sum(score5)
  })
  values = reactiveValues(
    count = 0
  )
  observeEvent(input$submitA,{
    if(summation$summationA[input$submitA] == 80){
      updateButton(session, "next2",disabled = FALSE)
      #values$count = values$count + 80
    }
    
  })
  observeEvent(input$submitB,{
    if(summation$summationB[input$submitB] == 20){
      updateButton(session, "finish",disabled = FALSE)
      #values$count = values$count + 20
    }
    else{
      updateButton(session, "finish", disabled = TRUE)
    }
    
    
  })
  
  
  output$scoreA <- renderPrint({
    cat("Score",summation$summationA[input$submitA])
  })
  output$scoreB <- renderPrint({
    cat("Score",max(summation$summationB))
  })
  
  
  
  observeEvent(input$finish,{
    summation$summationA[which(summation$summationA == 0)] = summation$summationA[input$submitA]
    summation$summationB[which(summation$summationB == 0)] = summation$summationB[input$submitB]
    summation$summationScore = summation$summationA + summation$summationB
  })
  
  output$init <- renderPrint({
    
    # summation$summationA[which(summation$summationA == 0)] = summation$summationA[input$submitA]
    # summation$summationB[which(summation$summationB == 0)] = summation$summationB[input$submitB]
    # summation$summationScore = summation$summationA + summation$summationB
    if (any(summation$summationA != 0) & any(summation$summationB != 0) ){
      initialScore = summation$summationScore[which(summation$summationScore != 0)][1]
    }else{
      initialScore = 0
    }
    
    cat("Initial","\n",initialScore)
  })
  
  final <- reactiveValues(final = 0)
  observeEvent(input$finish,{
    score1 = c()
    score2 = c()
    score3 = c()
    score4 = c()
    score5 = c()
    for (i in c(input$drp1,input$drp2,input$drp3,input$drp4)){
      if (any(i == paste("\n                  ",bank[c(1:10),3],"\n                ", sep = ""))){
        score1 = c(score1,5)
      }else{
        score1 = c(score1,-3)
      }
    }
    for (i in c(input$drp5,input$drp6,input$drp7,input$drp8)){
      if (any(i == paste("\n                  ",bank[c(11:36),3],"\n                ", sep = ""))){
        score2 = c(score2, 5)}else{
          score2 = c(score2, -3)}
    }
    for (i in c(input$drp9,input$drp10,input$drp11,input$drp12)){
      if (any(i == paste("\n                  ",bank[c(37:56),3],"\n                ", sep = ""))){
        score3 = c(score3, 5)}else{
          score3 = c(score3, -3)}
    }
    for (i in c(input$drp13,input$drp14,input$drp15,input$drp16)){
      if (any(i == paste("\n                  ",bank[c(57:71),3],"\n                ", sep = ""))){
        score4 = c(score4, 5)}else{
          score4 = c(score4, -3)}
    }
    for (i in input$drop1){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanDiscrete",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop2){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QuanContinuous",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop3){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualNominal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    for (i in input$drop4){
      if (i == numbersB$questionB[numbersB$questionB[1] == "QualOrdinal",5]){
        score5 = c(score5,5)
      }else{
        score5 = c(score5,-3)
      }
    }
    final$final = sum(c(score1,score2,score3,score4,score5))
  })
  
  output$end <- renderPrint({
    cat("Improved","\n", final$final)
  })
  
  output$totalScore <- renderPrint({
    cat("Total","\n",round(as.numeric(summation$summationScore[1]) * (2/3) + as.numeric(final$final) * (1/3), digits = 1))
  })
  ################################################LeaderBoard################
  
  output$checkName <- renderText({
    df = data.frame(data())
    df = df[,1]
    if (input$name %in% df){
      updateButton(session,"check",disabled = TRUE)
      print("The nickname has already been used. Please try another one.")
    }else{
      updateButton(session,"check",disabled = FALSE)
      print("The nickname is usable.")
    }
  })
  
  
  outputDir = "scores"
  
  # options(warn = -1)
  values = reactiveValues()
  
  update = reactive({
    value = data.frame("Name" = as.character(input$name),
                       "Score1" = as.numeric(summation$summationScore[1]),
                       "Score2" = as.numeric(final$final),
                       "TotalScore" = as.numeric(summation$summationScore[1]) * (2/3) + as.numeric(final$final) * (1/3),
                       "TimeTaken" = as.numeric(time$inc))
    
    
  })
  values$df = data.frame()
  
  
  saveQuestions <- function(data) {
    # data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.Date()), digest::digest(data))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(outputDir, fileName), 
      row.names = FALSE, quote = TRUE
    )
  }
  
  loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  }
  # First pattern try
  # paste0("^", Sys.Date(), sep = '')
  x = as.character(as.numeric(Sys.Date()))
  y = as.character(as.numeric(Sys.Date() - 7))
  if(substring(x, 5, 5) >= 7){
    pattern = paste0(substring(x, 1, 4), "[", substring(y, 5, 5), "-", substring(x, 5, 5), "]", sep = '')
  }
  else{
    pattern = paste0(substring(y, 1, 4), "[",substring(y, 5, 5), "-9]|", substring(x,1,4), "[0-", substring(x, 5, 5), "]")
  }
  
  
  loadDataWeek <- function() {
    files = list.files(outputDir, pattern = pattern, full.names = TRUE)
    data = lapply(files, read.csv, stringsAsFactors = FALSE)
    data = do.call(rbind, data)
    data
  }
  
  data = reactive({
    data = loadData()
    data = data[order(-data[,"TotalScore"], data[,"TimeTaken"]),]
    # data
  })
  
  data2 = reactive({
    data = loadDataWeek()
    data = data[order(-data[,"TotalScore"], data[,"TimeTaken"]),]
  })
  
  output$total = renderText({
    as.numeric(summation$summationScore[1]) * (2/3) + as.numeric(final$final) * (1/3)
  })
  
  observeEvent(input$check, {
    scores = update()
    values$df = rbind(values$df, scores)
    saveQuestions(values$df)
  })
  observeEvent(input$weekhigh, {
    output$highscore = renderDataTable({
      # head(data(), 5)
      if(is.null(loadDataWeek()) == TRUE){
        "No Highscores This week"
      }
      else{
        data2()  
      } 
      
    })
  })
  
  observeEvent(input$totalhigh, {
    output$highscore = renderDataTable({
      if(is.null(loadData()) == TRUE){
        "No Highscores"
      }
      else{
        data()
      }
    })
  })
  ########################################################################################
  
  
  observeEvent(input$check,{
    updateButton(session,"check",disabled = TRUE)
  })
  # 
  # output$badge = renderUI({
  #   score = round(summation$summationScore[1] * (2/3) + final$final * (1/3))
  #   if(is.null(input$name) == T){
  #     place = 0
  #   }
  #   else{
  #     # for(i in 1:3)
  #     #   {
  #     #   if(data2()[i, 1] == input$initials){
  #     #     place = i
  #     #   }
  #     # }
  #     if(score > data2()[3,4]){
  #       
  #       if(score > data2()[2,4]){
  #         if(score > data2()[1,4]){
  #           place = 1
  #         }
  #         else if (score == data2()[1,4]){
  #           if (time$inc < data2()[1,5]){
  #             place = 1
  #           }else if (time$inc >= data2()[1,5]){
  #             place = 2
  #           }
  #         }else{
  #           place = 2
  #         }
  #       }
  #       else if(score == data2()[2,4]){
  #         if(time$inc > data2()[2,5]){
  #           place = 3
  #         }
  #         else{
  #           place = 2
  #         }
  #       }
  #       else{
  #         place = 3
  #       }
  #     }
  #     else if(score == data2()[3,4]){
  #       if(time$inc > data2()[3,5]){
  #         place = 0
  #       }
  #       else{
  #         if(time$inc == data2()[3,5]){
  #           place = 3
  #         }
  #         else{
  #           if(time$inc < data2()[2,5]){
  #             if(time$inc < data2()[1.5]){
  #               place = 1
  #             }
  #             else{
  #               place = 2
  #             }
  #           }
  #           else if(time$inc == data2()[2,5]){
  #             place = 2
  #           }
  #           else{
  #             place = 3
  #           }
  #         }
  #         
  #       }
  #     }
  #     else{
  #       place = 0
  #     }
  #   }
  #   
  #   
  #   
  #   
  #   if(place == 1){
  #     # 1st place image
  #     img(src = "trophy1st.png",width = 600)
  #   }
  #   
  #   else if(place == 2){
  #     # 2nd place
  #     img(src = "trophy2nd.png",width = 600)
  #   }
  #   
  #   else if(place == 3){
  #     # 3rd place
  #     img(src = "trophy3rd.png",width = 600)
  #   }
  #   else{
  #     ""
  #   }
  #   #else if(values$count == data2()[1,2] && time$inc <= )
  # })

  
})