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
      [10] "address 0x50, cause 'memory not mapped'\r\n"                        
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

