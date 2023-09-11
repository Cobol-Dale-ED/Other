       IDENTIFICATION DIVISION.
       PROGRAM-ID. practice.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  LOANAMT    PIC S9(9)V9(2)   VALUE 0.
       01  INTRATE    PIC S9V9(2)      VALUE 0.
       01  NUMMONTHS  PIC S9(3)        VALUE 0.
       
       01  ws-Balance   PIC S9(9)V9(2)   VALUE 0.
       01  ws-Interest  PIC S9(9)V9(2)   VALUE 0.
       01  ws-Principal PIC S9(9)V9(2)   VALUE 0.

       01  col-hdr.
          05               pic x(15) value "Month".
          05               pic x(15) value 'Balance'.
          05               pic x(15) value 'Interest'.
          05               pic x(15) value 'Principal'.

       01  Detail-Line.
            05                Pic X(2) Value Spaces.
            05  DL-MONTH      Pic 999  VALUE 1.
            05                Pic X(5) Value Spaces.
            05  DL-BALANCE    Pic $$$$,$$$,$$$.99.
            05                Pic X(4) Value Spaces.
            05  DL-INTEREST   Pic $$$$,$$$,$$$.99.
            05                Pic X(4) Value Spaces.
            05  DL-PRINCIPAL  Pic $$$$,$$$,$$$.99.


       PROCEDURE DIVISION.
       000-MAIN SECTION.
           DISPLAY "Enter Loan Amount: " WITH NO ADVANCING
           ACCEPT LOANAMT
           IF 0 > LOANAMT
           PERFORM UNTIL LOANAMT > 0
            DISPLAY "Loan Amount must be positive"
            DISPLAY "Enter Loan Amount: " WITH NO ADVANCING
            ACCEPT LOANAMT
           end-PERFORM
           END-IF.

           DISPLAY "Enter Annual Interest Rate: " WITH NO ADVANCING
           ACCEPT INTRATE
           IF 0 > INTRATE
              PERFORM UNTIL INTRATE > 0
                 DISPLAY "Annual Interest Rate must be positive"
              DISPLAY "Enter Annual Interest Rate: "  wiTH  NO ADVANCING
            ACCEPT INTRATE
           end-PERFORM
           END-IF.
		   
		   COMPUTE INTRATE = INTRATE / 100

           DISPLAY "Enter Number of Months: " WITH NO ADVANCING
           ACCEPT NUMMONTHS
           IF 0 > NUMMONTHS
           PERFORM UNTIL NUMMONTHS > 0
            DISPLAY "Number of Months must be positive"
            DISPLAY "Enter Number of Months: " WITH NO
            ADVANCING
            ACCEPT NUMMONTHS
           end-PERFORM
           END-IF.

           DISPLAY SPACE
		   
		   DISPLAY LOANAMT
		   DISPLAY INTRATE
		   DISPLAY NUMMONTHS
		   DISPLAY WS-Balance
		   DISPLAY ws-Interest
		   DISPLAY ws-Principal

           DISPLAY col-hdr.

       100-init.
              move LOANAMT to ws-BALANCE 
              compute ws-INTEREST = (INTRATE/NUMMONTHS) * ws-Balance
              compute ws-PRINCIPAL = LOANAMT - ws-INTEREST
              
              move ws-Balance to DL-BALANCE
              move ws-Principal to DL-PRINCIPAL
              move ws-Interest to DL-INTEREST
			  
		   DISPLAY LOANAMT
		   DISPLAY INTRATE
		   DISPLAY NUMMONTHS
		   DISPLAY WS-Balance
		   DISPLAY ws-Interest
		   DISPLAY ws-Principal			  
              
              DISPLAY DETAIL-LINE
              PERFORM 200-ADDMONTH UNTIL NUMMONTHS = DL-MONTH.
			  STOP RUN.

       200-ADDMONTH.
              ADD 1 TO DL-MONTH.
      *       move dl-Balance to ws-BALANCE
      *       move dl-Principal to ws-PRINCIPAL
      *       move dl-Interest to ws-INTEREST
              
              compute ws-BALANCE =  ws-BALANCE - ws-PRINCIPAL
              compute ws-INTEREST = (INTRATE/NUMMONTHS) * WS-Balance
              compute ws-PRINCIPAL = LOANAMT - ws-INTEREST
              
              move ws-Balance to DL-BALANCE
              move ws-Principal to DL-PRINCIPAL
              move ws-Interest to DL-INTEREST

              DISPLAY DETAIL-LINE.



