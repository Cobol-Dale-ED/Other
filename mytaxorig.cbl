  IDENTIFICATION DIVISION.
   PROGRAM-ID. practice.
   DATA DIVISION.
   WORKING-STORAGE SECTION.

   01  LOANFMT    PIC $$$$,$$$,$$$.$$.
   01  LOANAMT    PIC S9(9)V9(2)   VALUE 0.
   01  INTRATE    PIC S9V9(2)      VALUE 0.
   01  INTFMT     PIC 9.999.
   01  NUMMONTHS  PIC S9(3)        VALUE 0.
   01  MONFMT     PIC ZZ9.
   01  MONCNT     PIC S999         VALUE 1.
   01  PMT        PIC S9(9)V9(2)   VALUE 0. 
   01  PMTFMT     PIC $$$$,$$$,$$$.$9.
   01  TOTPMT     PIC S9(9)V9(2)   VALUE 0. 
   01  TOTFMT     PIC $$$$,$$$,$$$.$9.          

   01  col-hdr.
        05                     pic x(15) value "Month".
        05                     pic x(15) value "Balance".
        05                     pic x(15) value "Interest".
        05                     pic x(15) value "Principal".      

   01  Detail-Line.
        05                Pic X(2) Value Spaces.
        05  DL-MONTH      Pic X(999) VALUE 1.
        05                Pic X(5) Value Spaces.
        05  DL-BALANCE    Pic $$$$,$$$,$$$.$9.
        05                Pic X(4) Value Spaces.
        05  DL-INTEREST   Pic $$$$,$$$,$$$.$9.
        05                Pic X(4) Value Spaces.
        05  DL-PRINCIPAL  Pic $$$$,$$$,$$$.$9.            


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
       END-IF           


       DISPLAY "Enter Annual Interest Rate: " WITH NO ADVANCING
       ACCEPT INTRATE
       IF 0 > INTRATE 
          PERFORM UNTIL INTRATE > 0
             DISPLAY "Annual Interest Rate must be positive"
             DISPLAY "Enter Annual Interest Rate: "  WITH 
             NO ADVANCING
        ACCEPT INTRATE
       end-PERFORM
       END-IF

       DISPLAY "Enter Number of Months: " WITH NO ADVANCING
       ACCEPT NUMMONTHS
       IF 0 > NUMMONTHS
       PERFORM UNTIL NUMMONTHS > 0
        DISPLAY "Number of Months must be positive"
        DISPLAY "Enter Number of Months: " WITH NO 
        ADVANCING
        ACCEPT NUMMONTHS
       end-PERFORM
       END-IF

       DISPLAY SPACE     

       move LOANAMT TO LOANFMT
       move INTRATE TO INTFMT
       MOVE NUMMONTHS TO MONFMT
       MOVE PMT TO PMTFMT
       MOVE TOTPMT TO TOTFMT

       DISPLAY col-hdr

       100-init.
          DL-BALANCE = LOANAMT
          DL-INTEREST = LOAN * (INTRATE/NUMMONTHS)
          DL-PRINCIPAL = LOANAMT - DL-INTEREST
          DISPLAY DETAIL-LINE
          PERFORM 200-ADDMONTH UNTIL NUMMONTHS = DL-MONTH

       200-ADDMONTH.
          ADD 1 TO DL-MONTH
          DL-BALANCE = DL-BALANCE - DL-PRINCIPAL
          DL-INTEREST = LOAN * (INTRATE/NUMMONTHS)
          DL-PRINCIPAL = LOANAMT - DL-INTEREST              
          DISPLAY DETAIL-LINE.




       STOP RUN.