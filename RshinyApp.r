{
library(lifecontingencies)
library(gt)
library(dplyr)
# ELT15 (Male and Female)
elt15m = read.csv("elt15m.csv")
elt15f = read.csv("elt15f.csv")
colnames(elt15m) = c("x","lx","dx")
colnames(elt15f) = c("x","lx","dx")
elt15m.act = new("actuarialtable",lx=elt15m$lx, x=elt15m$x, interest=0.04, name="elt15m")
elt15f.act = new("actuarialtable",lx=elt15f$lx, x=elt15f$x, interest=0.04, name="elt15f")
# PMA92C20 - PFA92C20
pma = read.csv("pma.csv")
pfa = read.csv("pfa.csv")
pma.act = new("actuarialtable",lx=pma$lx, x=pma$x, interest=0.04, name="pma")
pfa.act = new("actuarialtable",lx=pfa$lx, x=pfa$x, interest=0.04, name="pfa")
# United Kingdom
am92 = read.csv("am92.csv")
af92 = read.csv("af92.csv")
am92.lt  = probs2lifetable(probs = am92$qx, radix=100000, type="qx", name="AM92")
af92.lt  = probs2lifetable(probs = af92$qx, radix=100000, type="qx", name="AF92")
am92.act = new("actuarialtable", lx=am92.lt@lx, x=am92.lt@x, interest = 0.04, name="AM92")
af92.act = new("actuarialtable", lx=af92.lt@lx, x=af92.lt@x, interest = 0.04, name="AF92")
# Society of Actuaries
data("soaLt")
soa08.act = with(soaLt, new("actuarialtable",interest=0.04,x=x,lx=Ix,name="SOA2008"))
# CANADA
data("demoCanada")
up94mlt =   probs2lifetable(probs = demoCanada$up94M, radix = 100000, "qx",name="UP94m")
up94flt =   probs2lifetable(probs = demoCanada$up94F, radix = 100000, "qx",name="UP94f")
up94m.act = new("actuarialtable", lx=up94mlt@lx, x=up94mlt@x, interest=0.04)
up94f.act = new("actuarialtable", lx=up94mlt@lx, x=up94mlt@x, interest=0.04)
# CHINA
data("demoChina")
china.m.lt =  probs2lifetable(probs = demoChina$CL5, radix=100000, type="qx", name="China.Male")
china.f.lt =  probs2lifetable(probs = demoChina$CL6, radix=1000, type="qx", name="China.Female")
china.m.act = new("actuarialtable", lx=china.m.lt@lx, x=china.m.lt@x, interest=0.04)
china.f.act = new("actuarialtable", lx=china.f.lt@lx, x=china.f.lt@x, interest=0.04)
# FRANCE
data("demoFrance")
france.m.act = new("actuarialtable", lx=demoFrance$TH00_02, x=demoFrance$age, interest=0.04)
france.f.act = new("actuarialtable", lx=demoFrance$TF00_02, x=demoFrance$age, interest=0.04)
# GERMANY
data("demoGermany")
germany.m.lt =  probs2lifetable(probs = demoGermany$qxMale, radix=100000, type="qx", name="Germany.Male")
germany.f.lt =  probs2lifetable(probs = demoGermany$qxFemale, radix=100000, type="qx", name="Germany.Female")
germany.m.act = new("actuarialtable", lx=germany.m.lt@lx, x=germany.m.lt@x, interest=0.04)
germany.f.act = new("actuarialtable", lx=germany.f.lt@lx, x=germany.f.lt@x, interest=0.04)
# ITALY
data("demoIta")
italy.m.act = new("actuarialtable", lx=demoIta$SIM02, x=demoIta$X, interest=0.04)
italy.f.act = new("actuarialtable", lx=demoIta$SIF02, x=demoIta$X, interest=0.04)
# Japan
data("demoJapan")
japan.m.lt =  probs2lifetable(probs = na.omit(demoJapan$JP8587M), radix=100000, type="qx", name="Japan.Male")
japan.f.lt =  probs2lifetable(probs = demoJapan$JP8587F, radix=100000, type="qx", name="Japan.Female")
japan.m.act = new("actuarialtable", lx=japan.m.lt@lx, x=japan.m.lt@x, interest=0.04)
japan.f.act = new("actuarialtable", lx=japan.f.lt@lx, x=japan.f.lt@x, interest=0.04)
# USA
data("demoUsa")
usa.m.act = new("actuarialtable", lx=demoUsa$USSS2007M, x=demoUsa$age, interest=0.04)
usa.f.act = new("actuarialtable", lx=demoUsa$USSS2007F, x=demoUsa$age, interest=0.04)
}

library(shiny)
library(shinythemes)

ui = shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  titlePanel(title = h2("CT5 Calculator", align="center")),
                  sidebarLayout(
                    sidebarPanel(tags$label(h3('Input parameters')),
                                 selectInput("table.x", label = "Table for X",choices = list("SOA","ELT15 Females","ELT15 Males",
                                                                                             "PMA92C20","PFA92C20","AM92","AF92")),
                                 selectInput("table.y", label = "Table for Y",choices = list("SOA","ELT15 Females","ELT15 Males",
                                                                                             "PMA92C20","PFA92C20","AM92","AF92")),
                                 numericInput("x", label = "Age x",   value = 45, min = 0),
                                 numericInput("y", label = "Age y",   value = 50, min = 0),
                                 numericInput("n", label = "Term",    value = 15, min = 0),
                                 numericInput("i", label = "Interest",value = 0.04, step = 0.01),
                                 numericInput("m", label = "m thly",  value = 12, min = 0),
                                 radioButtons("option", "Single or Multiple Life?", list("Single", "Multiple"), "Single"),
                                 actionButton("submitbutton", "Submit", class = "btn btn-primary") ),
                    mainPanel(tags$label(h3('Output')),
                              verbatimTextOutput('contents'),
                              gt_output('gttable')) )))

server = shinyServer( function(input, output, session){
  single.life = function(table.name, x, n, i, m){
    
    table = if(table.name=="SOA"){soa08.act}else 
      if(table.name=="ELT15 Females"){elt15f.act}else 
        if(table.name=="ELT15 Males"){elt15m.act}else 
          if(table.name=="PMA92C20"){pma.act}else 
            if(table.name=="PFA92C20"){pfa.act}else 
              if(table.name=="AM92"){am92.act}else 
                if(table.name=="AF92"){af92.act}else NA
    
    mm = (m-1)/(2*m)
    ########## Assurance Factor
    Ax1n = Axn(actuarialtable = table, x = x, n = n, i = i)
    aexn = Exn(actuarialtable = table, x = x, n = n, i = i)
    
    assurance = rbind( round(Axn(table,x=x,i=i),6),
                       round((1+i)^0.5*Axn(table,x=x,i=i),6),
                       round(Ax1n,6),
                       round((1+i)^0.5*Ax1n,6),
                       round(aexn,6),
                       round(AExn(table,x= x, n = n, i = i),6),
                       round(aexn + (1+i)^0.5*Ax1n ,6))
    Assurance.Factor = c("Whole.Life EOYOD", "Whole.Life  IOD", "Term EOYOD", "Term IOD",
                         "Endowment Pure", "Endowment EOYOD", "Endowment IOD")
    
    ########## Simple Increasing Assurance
    IAx = IAxn(actuarialtable = table, x = x, i = i)
    IAX1n = IAxn(actuarialtable = table, x = x, n = n, i = i)
    SIA = rbind( round(IAx,6),
                 round((1+i)^0.5*IAx,6),
                 round(IAX1n,6),
                 round((1+i)^0.5*IAX1n,6),
                 round(IAX1n + aexn*n,6),
                 round((1+i)^0.5*IAX1n + aexn*n ,6))
    Simple.Increasing.Assurance = c("Whole.Life EOYOD","Whole.Life IOD","Term EOYOD",
                                    "Term IOD","Endowment EOYOD","Endowment IOD")
    
    ########## Annuity Factor
    ax = axn(actuarialtable = table, x = x, i = i)
    ax.n = axn(actuarialtable = table, x = x+n, i = i)
    ax1n = axn(actuarialtable = table, x = x, n = n, i = i)
    annuity = rbind( round(ax,3), round(ax - mm,3), round(ax-0.5,3), round(ax1n,3),
                     round(ax1n - mm*(1 - aexn),3), round((ax-0.5) - aexn*(ax.n-0.5),3) )
    Annuity.Factor = c("Whole.Life Advance", "Whole.Life m.thly", "Whole.Life Continuous",
                       "Temporary Advance", "Temporary m.thly", "Temporary Continuous")
    
    ########## Simple Increasing Annuity
    Iax = Iaxn(actuarialtable = table, x = x, i = i)
    Iax.n = Iaxn(actuarialtable = table, x = x+n, i = i)
    SIa = rbind( round(Iax,3), round(Iax - ax,3), round(Iax - 0.5*ax,3),
                 round(Iaxn(table,x=x,n=n,i=i),3), round(Iax-ax-aexn*(n*(ax.n-1)+Iax.n-ax.n),3),
                 round(Iax-0.5*ax-aexn*(n*(ax.n-0.5)+Iax.n-0.5*ax.n),3) )
    Simple.Increasing.Annuity = c("Whole.Life Advance", "Whole.Life Arrear", "Whole.Life Continuous",
                                  "Temporary Advance", "Temporary Arrear", "Temporary Continuous")
    
    df1 = as.data.frame(cbind(Assurance.Factor, assurance))
    colnames(df1) = c("Type", "Value")
    table1 =  df1 %>% gt()
    
    df2 = as.data.frame(cbind(Simple.Increasing.Assurance, SIA))  
    colnames(df2) = c("Type", "Value")
    table2 =  df2 %>% gt()
    
    df3 = as.data.frame(cbind(Annuity.Factor, annuity))
    colnames(df3) = c("Type", "Value")
    table3 =  df3 %>% gt()
    
    df4 = as.data.frame(cbind(Simple.Increasing.Annuity, SIa))
    colnames(df4) = c("Type", "Value")
    table4 =  df4 %>% gt()
    
    final = rbind(table1$"_data", table2$"_data", table3$"_data", table4$"_data")
    
    
    gttable = gt(final) %>% 
      tab_row_group(group = md("**Assurance Factor**"), rows = 1:7) %>% 
      tab_row_group(group = md("**Simple Increasing Assurance**"), rows = 8:13) %>% 
      tab_row_group(group = md("**Annuity Factor**"), rows = 14:19) %>% 
      tab_row_group(group = md("**Simple Increasing Annuity**"), rows = 20:25) %>% 
      cols_label(Type=md("**Type**"),Value=md("**Value**")) %>% 
      tab_header(title = md("**Single Life Annuity/Assurance Factors**")) %>% 
      tab_style(style = list(cell_fill(color = "#a7d0db")), locations = cells_row_groups())
    
    return(gttable)
    
  }
  multiple.life = function(table.x, table.y, x, y, n, i,m){  
    xy = c(x,y)
    mm = (m-1)/(2*m)
    tabx = if(table.x=="SOA"){soa08.act}else 
      if(table.x=="ELT15 Females"){elt15f.act}else 
        if(table.x=="ELT15 Males"){elt15m.act}else 
          if(table.x=="PMA92C20"){pma.act}else 
            if(table.x=="PFA92C20"){pfa.act}else 
              if(table.x=="AM92"){am92.act}else 
                if(table.x=="AF92"){af92.act}else NA
    taby = if(table.y=="SOA"){soa08.act}else 
      if(table.y=="ELT15 Females"){elt15f.act}else 
        if(table.y=="ELT15 Males"){elt15m.act}else 
          if(table.y=="PMA92C20"){pma.act}else 
            if(table.y=="PFA92C20"){pfa.act}else 
              if(table.y=="AM92"){am92.act}else 
                if(table.y=="AF92"){af92.act}else NA
    list.of.table = list(tabx, taby)
    ########## Multiple Life Annuity
    a.xy     = axyzn(list.of.table, x=xy,   i=i, status = "j")
    a.xyn    = axyzn(list.of.table, x=xy+n, i=i, status = "j")
    a.xybar  = axyzn(list.of.table, x=xy,   i=i, status = "l")
    a.xynbar = axyzn(list.of.table, x=xy+n, i=i, status = "l")
    a1 = axn(list.of.table[[1]], x=xy[1], n=n, i=i)- mm*(1-((1+i)^-n)*pxt(list.of.table[[1]],x=xy[1],t=n)) 
    a2 = axn(list.of.table[[2]], x=xy[2], n=n, i=i)- mm*(1-((1+i)^-n)*pxt(list.of.table[[2]],x=xy[2],t=n))
    a3 = axyzn(list.of.table,x=xy,i=i,status="j")- mm - ((1+i)^-n)*pxyzt(list.of.table,x=xy,t=n)*(axyzn(list.of.table,x=xy+n,i=i,status="j")- mm)
    
    annuity.xy = rbind(round(a.xy,3), round(a.xybar,3), round(a.xy-mm,3), round(a.xybar-mm,3),
                       round(axyzn(list.of.table,x=xy,n=n,i=i,status="j"),3), 
                       round(axyzn(list.of.table,x=xy,n=n,i=i,status="l"),3), 
                       round(a3,3), round(a1+a2-a3,3),
                       round(axn(list.of.table[[2]],x=xy[2],i=i)-a.xy,3),
                       round(axn(list.of.table[[2]],x=xy[2]+n,i=i) - a.xyn,3))
    Multiple.Life.Advance.Annuity = c("Whole Life (Joint Life)", "Whole Life (Last Survivor)", 
                                      "Whole Life (Joint Life - m.thly)", "Whole Life (Last Survivor - m.thly)",
                                      "Temporary (Joint Life)", "Temporary (Last Survivor)", 
                                      "Temporary (Joint Life - m.thly)", "Temporary (Last Survivor - m.thly)", 
                                      "Simple Reversionary (x|y)", "Simple Reversionary (x+n|y+n)")
    
    ########## Multiple Life Assurance
    assurance.xy = rbind( round(Axyzn(list.of.table,x=xy,    i=i,status="j"),6),
                          round(Axyzn(list.of.table,x=xy,    i=i,status="l"),6),
                          round(Axyzn(list.of.table,x=xy,n=n,i=i,status="j"),6),
                          round(Axyzn(list.of.table,x=xy,n=n,i=i,status="l"),6) )
    Multiple.Life.Assurance.Factor = c("Whole Life (Joint)", 
                                       "Whole Life (Last Survivor)",
                                       "Temporary (Joint)", 
                                       "Temporary (Last Survivor)")
    df5 = as.data.frame(cbind(Multiple.Life.Advance.Annuity, annuity.xy))
    colnames(df5) = c("Type", "Value")
    table5 =  df5 %>% gt()
    
    df6 = as.data.frame(cbind(Multiple.Life.Assurance.Factor, assurance.xy))
    colnames(df6) = c("Type", "Value")
    table6 =  df6 %>% gt()
    
    final.m = rbind(table5$"_data", table6$"_data")
    
    gttable.m = gt(final.m) %>% 
      tab_row_group(group = md("**Multiple Life Advance Annuity Factor**"), rows = 1:10) %>% 
      tab_row_group(group = md("**Multiple Life Assurance Factor**"), rows = 11:14) %>% 
      cols_label(Type=md("**Type**"),Value=md("**Value**")) %>% 
      tab_header(title = md("**Multiple Life Annuity/Assurance Factors**")) %>% 
      tab_style(style = list(cell_fill(color = "#a7d0db")), locations = cells_row_groups())
    
    return(gttable.m)
  }
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { isolate("Calculation complete.") 
    } else { return("Server is ready for calculation.") }})
  
  output$gttable <- render_gt({
    if(input$submitbutton>0){
    if(input$option == "Single") { single.life(table.name = as.character(input$table.x),
                                                 x = as.numeric(input$x),
                                                 n = as.numeric(input$n),
                                                 i = as.numeric(input$i),
                                                 m = as.numeric(input$m)) }
    else multiple.life(table.x = as.character(input$table.x),
                       table.y = as.character(input$table.y),
                       x = as.numeric(input$x),
                       y = as.numeric(input$y),
                       n = as.numeric(input$n), 
                       i = as.numeric(input$i), 
                       m = as.numeric(input$m))
  }
    })
  
} 
                      )

shinyApp(ui = ui,server = server)
