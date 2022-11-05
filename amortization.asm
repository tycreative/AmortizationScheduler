//****************************************************************
//ASM    EXEC   PROC=HLASMCLG                                     
//SYSIN  DD     *                                                 
         TITLE  'AMORTIZATION SCHEDULER'               
         PRINT  ON,NODATA,NOGEN 
******************************************************************
*   STANDARD BEGINNING FOR ASSEMBLER PROGRAMS                    *
******************************************************************
PROG6    CSECT                                                    
         STM   R14,R12,12(R13)         STORE EXISTING REGISTERS   
         LR    R12,R15                 ESTABLISH 1ST BASE REG     
         USING PROG6,R12 ,R11,R10      DEFINING BASE REGISTER 'C' 
*        LAY   R11,PROG6+4096          SECOND BASE REG + 4K       
*        LAY   R10,PROG6+8192          THIRD BASE REG + 8K        
         ST    R13,SAVEAREA+4          BACKWARD CHAIN CALLER'S    
         LA    R13,SAVEAREA            ADDRESS OF MY SAVE AREA    
******************************************************************
* BEGIN THE PROGRAM LOGIC. FIRST OPEN THE INPUT AND OUTPUT FILES *
******************************************************************
         OPEN  (FILEIN1,(INPUT))       OPEN INPUT FILE            
         OPEN  (FILEOUT1,(OUTPUT))     OPEN OUTPUT FILE           
         GET   FILEIN1,RECIN1          GET THE FIRST RECORD, IF ON
******************************************************************
*        MAIN PROGRAM LOOP THAT EXECUTES SUBROUTINES             *
******************************************************************
LOOP     EQU   *                                                  
         BAS   R8,RESET                RESET VALUES FOR NEXT CALC 
         BAS   R8,SETUP                SETUP VALUES FOR CALC      
         CLC   ERRFLAG,NOERR           CHECK IF VALUES ARE VALID  
         BNE   NEXTIN                  GET NEXT INPUT IF NOT      
         BAS   R8,PRTHEAD              DISPLAY HEADERS            
CALCNUM  EQU   *                                                  
         BAS   R8,CALCINT              CALCULATE INTEREST VALUE   
         BAS   R8,CALCPMT              CALCULATE PAYMENT VALUE    
         BAS   R8,CALCPAID             CALCULATE LOAN PAID VALUE  
         BAS   R8,PRTNUMS              PRINT THESE NUMBERS        
         AP    PMTNUM,PK1              INCREMENT PAYMENT NUMBER   
         CLC   ERRFLAG,NOERR           CHECK IF PMT HAS ENDED     
         BE    CALCNUM                 OTHERWISE CONTINUE LOOP    
         BAS   R8,PRTTOTS              PRINT TOTALS ONCE DONE     
NEXTIN   EQU   *                                                  
         GET   FILEIN1,RECIN1          GET THE NEXT RECORD        
         B     LOOP                    RESTART MAIN LOOP          
******************************************************************
*        END OF INPUT PROCESSING                                 *
******************************************************************
EOF1     EQU   *                                                  
         CLOSE (FILEIN1)               CLOSE INPUT FILE           
         CLOSE (FILEOUT1)              CLOSE OUTPUT FILE          
         L     R13,SAVEAREA+4          POINT AT OLD SAVE AREA     
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS      
         LA    R15,0                   RETURN CODE = 0            
         BR    R14                     RETURN TO OPERATING SYSTEM 
******************************************************************
*        RESET SUBROUTINE TO SET VARIABLES TO DEFAULT            *
******************************************************************
RESET    EQU   *                                                  
         MVC   ERRFLAG,NOERR           RESET ERROR FLAG TO 'N'    
         ZAP   PMTNUM,PK1              RESET PAYMENT NUMBER TO 1  
         ZAP   TOTPMT,=PL2'0'          RESET TOTAL PAYMENTS TO 0  
         ZAP   TOTINT,TOTPMT           RESET TOTAL INTEREST TO 0  
         ZAP   TOTPAID,TOTPMT          RESET TOTAL LOANS PAID TO 0
******************************************************************
*        SETUP SUBROUTINE TO VALIDATE INPUTS                     *
******************************************************************
SETUP    EQU   *                                                  
         PACK  PKAMT,INAMT             TRY TO PACK AMOUNT INPUT   
         TP    PKAMT                   TEST IF VALID NUMBER       
         BNZ   BADINPUT                IF NOT, RAISE ERROR FLAG   
         PACK  PKPMT,INPMT             TRY TO PACK PAYMENT INPUT  
         TP    PKPMT                   TEST IF VALID NUMBER       
         BNZ   BADINPUT                IF NOT, RAISE ERROR FLAG   
         PACK  PKRATE,INRATE           TRY TO PACK RATE INPUT     
         TP    PKRATE                  TEST IF VALID NUMBER       
         BNZ   BADINPUT                IF NOT, RAISE ERROR FLAG   
         PACK  PKTERM,INTERM           TRY TO PACK TERM INPUT     
         TP    PKTERM                  TEST IF VALID NUMBER       
         BNZ   BADINPUT                IF NOT, RAISE ERROR FLAG   
         BR    R8                      RETURN TO LOOP             
BADINPUT EQU   *                                                  
         MVC   ERRFLAG,YESERR          SET ERROR FLAG TO 'Y'      
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        PRINT HEADERS SUBROUTINE TO DISPLAY HEADERS             *
******************************************************************
PRTHEAD  EQU   *                                                  
         PUT   FILEOUT1,PRHEAD1        DISPLAY THE FIRST HEADER   
         MVC   OUTAMT,EDMK9            MOVE EDIT MASK FOR AMOUNT  
         ED    OUTAMT,PKAMT            EDIT OUT PACKED AMOUNT     
         MVC   OUTPMT,EDMK7            MOVE EDIT MASK FOR PAYMENT 
         ED    OUTPMT,PKPMT            EDIT OUT PACKED PAYMENT    
         MVC   OUTRATE,EDMK5           MOVE EDIT MASK FOR RATE    
         ED    OUTRATE,PKRATE          EDIT OUT PACKED RATE       
         MVC   OUTTERM,EDMK3           MOVE EDIT MASK FOR TERM    
         ED    OUTTERM,PKTERM          EDIT OUT PACKED TERM       
         PUT   FILEOUT1,PRHEAD2        DISPLAY THE SECOND HEADER  
         PUT   FILEOUT1,PRHEAD3        DISPLAY THE THIRD HEADER   
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        SUBROUTINE TO CALCULATE THE INTEREST & ADD TO TOTAL     *
******************************************************************
CALCINT  EQU   *                                                  
         ZAP   PKBAL,PKAMT             SET BALANCE FOR MULTIPLY   
         MP    PKBAL,PKRATE            MULTIPLY BALANCE BY RATE   
         DP    PKBAL,=PL2'12'          DIVIDE BALANCE BY 12       
         SRP   DIVRES,64-5,5           SHIFT RIGHT 5 AND ROUND    
         ZAP   PKINT,DIVRES            SET INTEREST TO DIV RESULT 
         AP    TOTINT,PKINT            ADD INTEREST TO TOTAL INT  
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        SUBROUTINE TO CALCULATE PAYMENT & CHECK FOR END         *
******************************************************************
CALCPMT  EQU   *                                                  
         CP    PKPMT,PKAMT             CHECK IF PAYMENT >= BAL    
         BNL   ENDPMT                  GET END PAYMENT IF NOT     
         CP    PKTERM,PMTNUM           CHECK IF ON LAST TERM      
         BE    ENDPMT                  GET END PAYMENT IF ON      
         BR    R8                      RETURN TO LOOP             
ENDPMT   EQU   *                                                  
         MVC   ERRFLAG,YESERR          RAISE ERROR FLAG FOR LOOP  
         ZAP   PKPMT,PKINT             SET PAYMENT FOR ADDITION   
         AP    PKPMT,PKAMT             ADD AMOUNT TO INTEREST     
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        SUBROUTINE TO CALCULATE LOAN PAID & ADD TO TOTAL        *
******************************************************************
CALCPAID EQU   *                                                  
         AP    TOTPMT,PKPMT            ADD PAYMENT TO TOTAL       
         ZAP   PKPAID,PKPMT            SET PAID FOR SUBTRACTION   
         SP    PKPAID,PKINT            PAID = PAYMENT - INTEREST  
         AP    TOTPAID,PKPAID          UPDATE TOTAL PAID          
         SP    PKAMT,PKPAID            UPDATE BALANCE AMOUNT      
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        SUBROUTINE TO DISPLAY CALCULATED NUMBERS                *
******************************************************************
PRTNUMS  EQU   *                                                  
         MVC   NUMBER,EDMK3            MOVE EDIT MASK FOR NUMBER  
         ED    NUMBER,PMTNUM           EDIT OUT PAYMENT NUMBER    
         MVC   PAYMENT,EDMK7           MOVE EDIT MASK FOR PAYMENT 
         ED    PAYMENT,PKPMT           EDIT OUT PAYMENT AMOUNT    
         MVC   INTAMT,EDMK7            MOVE EDIT MASK FOR INTEREST
         ED    INTAMT,PKINT            EDIT OUT INTEREST AMOUNT   
         MVC   PAIDAMT,EDMK7           MOVE EDIT MASK 4 LOAN PAID 
         ED    PAIDAMT,PKPAID          EDIT OUT LOAN PAID AMOUNT  
         MVC   BALANCE,EDMK9           MOVE EDIT MASK FOR BALANCE 
         ED    BALANCE,PKAMT           EDIT OUT BALANCE AMOUNT    
         PUT   FILEOUT1,RECOUT1        DISPLAY THE CALC NUMBERS   
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        SUBROUTINE TO DISPLAY THE FINAL TOTALS                  *
******************************************************************
PRTTOTS  EQU   *                                                  
         MVC   TOTALP,EDMK11           MOVE EDIT MASK FOR PAYMENT 
         ED    TOTALP,TOTPMT           EDIT OUT TOTAL PAYMENTS    
         MVC   TOTALI,EDMK11           MOVE EDIT MASK FOR INTEREST
         ED    TOTALI,TOTINT           EDIT OUT TOTAL INTEREST    
         MVC   TOTALLP,EDMK9           MOVE EDIT MASK 4 LOAN PAID 
         ED    TOTALLP,TOTPAID         EDIT OUT TOTAL LOANS PAID  
         PUT   FILEOUT1,TOTALS         DISPLAY TOTAL AMOUNTS      
         BR    R8                      RETURN TO LOOP             
******************************************************************
*        AREA FOR VARIABLES AND CONSTANTS                        *
******************************************************************
* INPUT HOLDERS                                                   
PKAMT    DC    PL5'0'            PACKED LOAN AMOUNT               
PKPMT    DC    PL4'0'            PACKED LOAN PAYMENT AMOUNT       
PKRATE   DC    PL3'0'            PACKED LOAN RATE                 
PKTERM   DC    PL2'0'            PACKED LOAN TERMS                
* OTHER HOLDERS                                                   
PMTNUM   DC    PL2'0'            PACKED CURRENT PAYMENT NUMBER    
PKINT    DC    PL4'0'            PACKED CALC LOAN INTEREST        
PKPAID   DC    PL4'0'            PACKED CALC LOAN PAID AMOUNT     
TOTPMT   DC    PL6'0'            PACKED LOAN TOTAL PAYMENTS       
TOTINT   DC    PL6'0'            PACKED LOAN TOTAL INTEREST       
TOTPAID  DC    PL5'0'            PACKED TOTAL LOAN PAYMENTS       
* CONSTANTS & ERROR FLAG                                          
PK1      DC    PL2'1'            PACKED CONSTANT OF 1             
NOERR    DC    CL1'N'            NO ERROR CONSTANT                
YESERR   DC    CL1'Y'            YES ERROR CONSTANT               
ERRFLAG  DC    CL1'N'            USED TO RAISE ERRORS             
* DIVISION                                                        
PKBAL    DS   0PL9'0'            PACKED BALANCE FOR DIVISION      
DIVRES   DC    PL7'0'            DIVISION RESULT HOLDER           
DIVREM   DC    PL2'0'            DIVISION REMAINDER HOLDER        
******************************************************************
*    EDIT MASKS TO FORMAT PRICE AND COST VALUES                  *
******************************************************************
EDMK3    DC    XL05'4020202060'                                   
EDMK5    DC    XL08'4021204B20202060'                             
EDMK7    DC    XL11'4020206B2021204B202060'                       
EDMK9    DC    XL14'40206B2020206B2021204B202060'                 
EDMK11   DC    XL16'402020206B2020206B2021204B202060'             
******************************************************************
*     INPUT FILE - DATA CONTROL BLOCK                            *
******************************************************************
FILEIN1  DCB   DSORG=PS,                                          
               MACRF=(GM),                                        
               DEVD=DA,                                           
               DDNAME=FILEIN1,                                    
               EODAD=EOF1,                                        
               RECFM=FB,                                          
               LRECL=80                                           
******************************************************************
*    INPUT RECORD AREA                                           *
******************************************************************
RECIN1   DS   0CL80           FULL RECORD DEFINITION (80 BYTES)   
INAMT    DS    CL09                                               
         DS    CL01                                               
INRATE   DS    CL05                                               
         DS    CL05                                               
INPMT    DS    CL07                                               
         DS    CL03                                               
INTERM   DS    CL03                                               
         DS    CL47           FILLER                              
******************************************************************
*     OUTPUT FILE - DATA CONTROL BLOCK                           *
******************************************************************
FILEOUT1 DCB   DSORG=PS,                                          
               MACRF=(PM),                                        
               DEVD=DA,                                           
               DDNAME=FILEOUT1,                                   
               RECFM=FM,                                          
               LRECL=80                                           
******************************************************************
*    OUTPUT RECORD AREAS                                         *
******************************************************************
*      HERE IS THE HEADER FOR ** C S U **                         
******************************************************************
PRHEAD1  DS   0CL80                                               
PRC1     DC    CL01'1'           PRINT CONTROL - START NEW PAGE   
         DC    CL02' '           SPACING OF 18 CHARACTERS         
         DC    CL39'*** CSU AMORTIZATION SCHEDULE - SPRING '      
         DC    CL28'2021 *** - TYLER WENNDT (35)'                 
         DC    CL11' '                                            
******************************************************************
*        THIS IS THE HEADER FOR INPUT VALUES                     *
******************************************************************
PRHEAD2  DS   0CL80                                               
PRC2     DC    CL01' '                                            
         DC    CL08' AMOUNT:'                                     
OUTAMT   DC    CL13' '                                            
         DC    CL11'   PAYMENT:'                                  
OUTPMT   DC    CL10' '                                            
         DC    CL08'   RATE:'                                     
OUTRATE  DC    CL07' '                                            
         DC    CL08'   TERM:'                                     
OUTTERM  DC    CL04' '                                            
         DC    CL13' '           FILLER                           
******************************************************************
*        THIS IS THE HEADER FOR CALCULATED VALUES                *
******************************************************************
PRHEAD3  DS   0CL80                                               
PRC3     DC    CL01' '           PRINT CONTROL - SINGLE SPACE     
         DC    CL37'  PMT #         PAYMENT         INTER'        
         DC    CL33'EST      LOAN PAID        BALANCE'            
         DC    CL09' '                                            
******************************************************************
*        THIS IS THE CALCULATED NUMBERS OUT AREA                 *
******************************************************************
RECOUT1  DS   0CL80              PRINT AREA                       
PRC4     DC    CL01' '           PRINT CONTROL CHARACTER          
         DC    CL02' '                                            
NUMBER   DC    CL04' '                                            
         DC    CL07' '                                            
PAYMENT  DC    CL10' '                                            
         DC    CL07' '                                            
INTAMT   DC    CL10' '                                            
         DC    CL05' '                                            
PAIDAMT  DC    CL10' '                                            
         DC    CL02' '                                            
BALANCE  DC    CL13' '                                            
         DC    CL09' '                                            
******************************************************************
*        THIS IS THE FOOTER FOR TOTAL AMOUNTS                    *
******************************************************************
TOTALS   DS   0CL80                                               
PRC5     DC    CL01' '                                            
         DC    CL08' TOTALS '                                     
TOTALP   DC    CL15' '                                            
         DC    CL02' '                                            
TOTALI   DC    CL15' '                                            
         DC    CL02' '                                            
TOTALLP  DC    CL13' '                                            
         DC    CL24' '                                            
******************************************************************
*    REGISTER SAVE AREA                                          *
******************************************************************
SAVEAREA DS  18F                 ROOM FOR STORAGE OF REGISTERS    
******************************************************************
*     REGISTER EQUATES                                           *
******************************************************************
R0       EQU   0                                                  
R1       EQU   1                                                  
R2       EQU   2                                                  
R3       EQU   3                                                  
R4       EQU   4                                                  
R5       EQU   5                                                  
R6       EQU   6                                                  
R7       EQU   7                                                  
R8       EQU   8                                                  
R9       EQU   9                                                  
R10      EQU   10                                                 
R11      EQU   11                                                 
R12      EQU   12                                                 
R13      EQU   13                                                 
R14      EQU   14                                                 
R15      EQU   15                                                 
******************************************************************
*    LITERAL POOL - THIS PROGRAM MAY USE LITERALS.                
******************************************************************
         LTORG *                                                  
         END   PROG6                                              
/*                                                                
//G.SYSABOUT DD SYSOUT=*                                          
//G.SYSUDUMP DD SYSOUT=*                                          
//G.FILEOUT1 DD SYSOUT=*,OUTLIM=2500                              
//*.FILEIN1  DD DSN=CSU0&CICS&ID..C3121.ASM(DATAPRG&PROG),DISP=SHR
//G.FILEIN1  DD DSN=CSU.PUBLIC.DATA(DATAPRG&PROG),DISP=SHR        
//                                                                