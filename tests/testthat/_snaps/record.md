# record

    Code
      cast$output$data
    Output
       [1] "type: prompt"                "> "                         
       [3] "print(\"Hello world!\")\r\n" "type: input"                
       [5] "print(\"Hello world!\")\r\n" "busy: 1"                    
       [7] "type: stdout"                "[1]"                        
       [9] "type: stdout"                " \"Hello world!\""          
      [11] "type: stdout"                "\r\n"                       
      [13] "busy: 0"                     "type: read"                 
      [15] "type: wait"                  ""                           

# errors

    Code
      cast1$output$data
    Output
       [1] "type: prompt"                             
       [2] "> "                                       
       [3] "foo12313\r\n"                             
       [4] "type: input"                              
       [5] "foo12313\r\n"                             
       [6] "busy: 1"                                  
       [7] "type: stderr"                             
       [8] "Error: object 'foo12313' not found\r\n"   
       [9] "busy: 0"                                  
      [10] "type: read"                               
      [11] "type: prompt"                             
      [12] "> "                                       
      [13] "barsdsdfsdf\r\n"                          
      [14] "type: input"                              
      [15] "barsdsdfsdf\r\n"                          
      [16] "busy: 1"                                  
      [17] "type: stderr"                             
      [18] "Error: object 'barsdsdfsdf' not found\r\n"
      [19] "busy: 0"                                  
      [20] "type: read"                               
      [21] "type: wait"                               
      [22] ""                                         

# R quits

    Code
      cast$output$data
    Output
      [1] "type: prompt"   "> "             "quit('no')\r\n" "type: input"   
      [5] "quit('no')\r\n" "busy: 1"        "type: wait"     ""              

# R crashes

    Code
      cast$output$data
    Output
       [1] "type: prompt"                                                       
       [2] "> "                                                                 
       [3] "callr:::crash()\r\n"                                                
       [4] "type: input"                                                        
       [5] "callr:::crash()\r\n"                                                
       [6] "busy: 1"                                                            
       [7] "type: stderr"                                                       
       [8] "\r\n *** caught segfault ***\r\n"                                   
       [9] "type: stderr"                                                       
      [10] "address 0x50, cause 'invalid permissions'\r\n"                      
      [11] "type: stderr"                                                       
      [12] "\r\nTraceback:\r\n"                                                 
      [13] "type: stderr"                                                       
      [14] " 1: "                                                               
      [15] "type: stderr"                                                       
      [16] "get(\"attach\")(structure(list(), class = \"UserDefinedDatabase\"))"
      [17] "type: stderr"                                                       
      [18] "\r\n"                                                               
      [19] "type: stderr"                                                       
      [20] " 2: "                                                               
      [21] "type: stderr"                                                       
      [22] "callr:::crash()"                                                    
      [23] "type: stderr"                                                       
      [24] "\r\n"                                                               
      [25] "type: stderr"                                                       
      [26] "An irrecoverable exception occurred. R is aborting now ...\r\n"     
      [27] "type: wait"                                                         
      [28] ""                                                                   

# echo = FALSE

    Code
      cast$output$data
    Output
       [1] "print(\"Hello world!\")\r\n" "busy: 1"                    
       [3] "type: stdout"                "[1]"                        
       [5] "type: stdout"                " \"Hello world!\""          
       [7] "type: stdout"                "\r\n"                       
       [9] "busy: 0"                     "type: read"                 
      [11] "type: wait"                  ""                           

