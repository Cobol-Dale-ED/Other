       IDENTIFICATION DIVISION.
       PROGRAM-ID. tax.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Get user constants.
       01 Principal               pic 9(6)v9(15).
       01 AnnualInterestRate      pic 9v9(15).
       01 NumberOfMonths          pic 99.

      * More constants.
       01 MonthlyInterestRate     pic 9v9(15).
       01 MonthlyPayment          pic 9(6)v9(15).

      * Variables.
       01 CurrentMonth            pic 99.
       01 StartBalance            pic 9(6)v9(15).
       01 MonthlyInterest         pic 9(6)v9(15).
       01 MonthlyPrincipal        pic 9(6)v9(15).
       01 EndBalance              pic 9(6)v9(15).

      * Boolean for test output with 9's in every column (except Month)
       01 filler pic 9 usage is comp-5 value 0.
           88 NinesSet value 1 when set to false is 0.
      *-----------------------------------------------------------------
       01  ColumnHeader1.
            05                    pic a(5)     value Spaces.
            05                    pic a(13)    value "    Beginning".
            05                    pic a(13)    value "      Monthly".
            05                    pic a(13)    value "      Monthly".
            05                    pic a(13)    value "      Monthly".
            05                    pic a(13)    value "       Ending".
      *-----------------------------------------------------------------
       01  ColumnHeader2.
            05                    pic a(5)     value "Month".
            05                    pic a(13)    value "      Balance".
            05                    pic a(13)    value "      Payment".
            05                    pic a(13)    value "    Principal".
            05                    pic a(13)    value "     Interest".
            05                    pic a(13)    value "      Balance".
      *-----------------------------------------------------------------
       01  DetailLine.
            05                    pic x(3)     value Spaces.
            05  DL-Month          pic z(2).
            05                    pic x(2)     value Spaces.
            05  DL-StartBalance   pic $$$$,$$9.99.
            05                    pic x(2)     value Spaces.
            05  DL-MonthlyPayment pic $$$$,$$9.99.
            05                    pic x(2)     value Spaces.
            05  DL-Principal      pic $$$$,$$9.99.
            05                    pic x(2)     value Spaces.
            05  DL-Interest       pic $$$$,$$9.99.
            05                    pic x(2)     value Spaces.
            05  DL-EndBalance     pic $$$$,$$9.99.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       perform Initialization
       perform ProduceReport
       goback.
      *-----------------------------------------------------------------
       Initialization.
           perform GetConstantsFromUser
           perform InitializeConstants
      *    perform DisplayValues
       .
      *-----------------------------------------------------------------
       ProduceReport.
           perform DisplayHeaders
           perform DisplayMonths

      *    Let's leave the code that writes 9's so that we will
      *    still be able to gauge column widths should a situation
      *    arise where we need to modify the report.
           perform DisplayHeaders
           perform DisplayMonthsAsNines
       .
      *-----------------------------------------------------------------
       DisplayMonths.
           perform varying CurrentMonth from 1
                   by 1 until CurrentMonth = NumberOfMonths + 1
               if CurrentMonth = 1 then
                   move Principal to StartBalance
               else
                   move EndBalance to StartBalance
               end-if
               
               compute MonthlyInterest =
                   StartBalance * MonthlyInterestRate

               compute MonthlyPrincipal =
                   MonthlyPayment - MonthlyInterest

               compute EndBalance = StartBalance - MonthlyPrincipal

               perform LoadDetailValues
               display DetailLine
           end-perform
           display " ".
      *-----------------------------------------------------------------
       DisplayMonthsAsNines.
           perform varying CurrentMonth from 1
                   by 1 until CurrentMonth = NumberOfMonths + 1
               perform LoadDetailValuesNines
               display DetailLine
           end-perform
           display " ".
      *-----------------------------------------------------------------
      *------------------------Initialization---------------------------
       GetConstantsFromUser.
           perform GetPrincipal
           perform GetInterestRate
           perform GetMonths.
      *-----------------------------------------------------------------
       GetPrincipal.
           display "Enter Loan Amount: " with no advancing
           accept Principal
      *    move 1000.0 to Principal

              if Principal <= 0
                  perform until Principal > 0
                      display "Loan Amount must be greater than zero."
                      display "Enter Loan Amount: "
                          with no advancing
                      accept Principal
                  end-perform
              end-if.
      *-----------------------------------------------------------------
       GetInterestRate.
           display "Enter Annual Interest Rate: " with no advancing
           accept AnnualInterestRate
      *    move 0.05 to AnnualInterestRate

           if 0 > AnnualInterestRate 
               perform until AnnualInterestRate > 0
                   display "Annual Interest Rate must be positive"
                   display "Enter Annual Interest Rate: "  
                       with no advancing
                   accept AnnualInterestRate
               end-perform
           end-if.
      *-----------------------------------------------------------------
       GetMonths.
           display "Enter Number of Months: " with no advancing
           accept NumberOfMonths
      *    move 12 to NumberOfMonths

           if 0 > NumberOfMonths
               perform until NumberOfMonths > 0
                   display "Number of Months must be positive"
                   display "Enter Number of Months: " 
                       with no advancing
                   accept NumberOfMonths
               end-perform
           end-if.
      *-----------------------------------------------------------------
       InitializeConstants.
      *    Monthly interest rate
      *    Monthly payment

      *    Make sure that this is computed prior to computing
      *    the monthly payment (it's computed below).
           compute MonthlyInterestRate =
               AnnualInterestRate / NumberOfMonths

      *    compute MonthlyPayment rounded =
           compute MonthlyPayment =
               (Principal * MonthlyInterestRate) /
               (1 - ((1 + MonthlyInterestRate) **
                   (-1 * NumberOfMonths))).
      *    Because the monthly payment is always the same,
      *    set the display value for it here.
           move MonthlyPayment to DL-MonthlyPayment
       .
      *-----------------------------------------------------------------
      *-------------------------ProduceReport---------------------------
       DisplayHeaders.
           display ColumnHeader1
           display ColumnHeader2.
      *-----------------------------------------------------------------
       LoadDetailValues.
           move CurrentMonth to DL-Month
           move StartBalance to DL-StartBalance
      *                         DL-MonthlyPayment
           move MonthlyPrincipal to DL-Principal
           move MonthlyInterest to DL-Interest
           move EndBalance to DL-EndBalance.
      *-----------------------------------------------------------------
       LoadDetailValuesNines.
           move CurrentMonth to DL-Month

           if not NinesSet
               perform LoadNines.
      *-----------------------------------------------------------------
       LoadNines.
           move 999999999.99 to DL-StartBalance
           move 999999999.99 to DL-MonthlyPayment
           move 999999999.99 to DL-Principal
           move 999999999.99 to DL-Interest
           move 999999999.99 to DL-EndBalance
           set NinesSet to true
       .
      *-----------------------------------------------------------------


