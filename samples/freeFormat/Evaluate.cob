 ID DIVISION.                                                     
 PROGRAM-ID. EVAL.                                             
 ENVIRONMENT DIVISION.                                            
 CONFIGURATION SECTION.                                           
 SPECIAL-NAMES.                                                   
     C02 IS LCP-CH2
     C03 IS LCP-CH3
     C04 IS LCP-CH4
     C05 IS LCP-CH5
     C06 IS LCP-CH6
     C07 IS LCP-CH7
     C08 IS LCP-CH8
     C09 IS LCP-CH9
     C10 IS LCP-CH10
     C11 IS LCP-CH11
     C12 IS LCP-CH12
     S01 IS LCP-P01
     S02 IS LCP-P02
     DECIMAL-POINT IS COMMA.
 INPUT-OUTPUT SECTION.                                            
 DATA DIVISION.                                                   
 WORKING-STORAGE SECTION.                                         
 77  LCP-ASA                       PIC X.
 PROCEDURE DIVISION.                                              
 LCP-WRITE-POS-LIN   SECTION.
     EVALUATE LCP-ASA
         WHEN '+'
             WRITE LIN   AFTER 0 LINE
         WHEN ' '
             WRITE LIN   AFTER ADVANCING 1 LINE
         WHEN '0'
             WRITE LIN   AFTER ADVANCING 2 LINE
         WHEN '-'
             WRITE LIN   AFTER ADVANCING 3 LINE
         WHEN '1'
             WRITE LIN   AFTER ADVANCING PAGE
         WHEN '2'
             WRITE LIN   AFTER ADVANCING LCP-CH2
         WHEN '3'
             WRITE LIN   AFTER ADVANCING LCP-CH3
         WHEN '4'
             WRITE LIN   AFTER ADVANCING LCP-CH4
         WHEN '5'
             WRITE LIN   AFTER ADVANCING LCP-CH5
         WHEN '6'
             WRITE LIN   AFTER ADVANCING LCP-CH6
         WHEN '7'
             WRITE LIN   AFTER ADVANCING LCP-CH7
         WHEN '8'
             WRITE LIN   AFTER ADVANCING LCP-CH8
         WHEN '9'
             WRITE LIN   AFTER ADVANCING LCP-CH9
         WHEN 'A'
             WRITE LIN   AFTER ADVANCING LCP-CH10
         WHEN 'B'
             WRITE LIN   AFTER ADVANCING LCP-CH11
         WHEN 'C'
             WRITE LIN   AFTER ADVANCING LCP-CH12
         WHEN 'V'
             WRITE LIN   AFTER ADVANCING LCP-P01
         WHEN 'W'
             WRITE LIN   AFTER ADVANCING LCP-P02
         WHEN OTHER
             DISPLAY 'ASA CODE ERROR'
     END-EVALUATE.
 LCP-WRITE-END-LIN  .
     EXIT.

