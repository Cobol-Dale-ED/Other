       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CALCULADORA.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.


       01  LOANFMT    PIC $$$$,$$$,$$$.99.
       01  LOANAMT    PIC S9(9)V9(2)   VALUE 0.
       01  INTRATE    PIC S9V9(2)      VALUE 0.
       01  INTFMT     PIC 9.999.
       01  NUMMONTHS  PIC S9(3)        VALUE 0.
       01  MONFMT     PIC ZZ9.
       01  MONCNT     PIC S999         VALUE 1.
       01  PMT        PIC S9(9)V9(2)   VALUE 0.
       01  PMTFMT     PIC $$$$,$$$,$$$.99.
       01  TOTPMT     PIC S9(9)V9(2)   VALUE 0.
       01  TOTFMT     PIC $$$$,$$$,$$$.99.

       
       
       PROCEDURE DIVISION.
       INPUT-PROCEDURE.
       000-MAIN SECTION.

           IF 0 > INTRATE
              PERFORM UNTIL INTRATE > 0
                 DISPLAY "Annual Interest Rate must be positive"
                DISPLAY "Enter Annual Interest Rate: " WITH NO ADVANCING
           DISPLAY "Enter Annual Interest Rate: " WITH NO ADVANCING
	  
                 DISPLAY "Enter Annual Interest Rate: " 
				 
           END-PERFORM
           END-IF.
	   
    
       END PROGRAM CALCULADORA.
