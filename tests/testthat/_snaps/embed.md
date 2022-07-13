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

# forced pause

    Code
      cmds
    Output
       [1] "type: prompt" "type: input"  "type: stdout" "type: stdout" "type: stdout"
       [6] "type: read"   "type: prompt" "type: wait"   "type: input"  "type: stdout"
      [11] "type: stdout" "type: stdout" "type: read"   "type: wait"  

# edge case with no wait

    Code
      cmds
    Output
      [1] "type: prompt" "type: input"  "type: stdout" "type: stdout" "type: stdout"
      [6] "type: read"  

