      *================================================================*
      * CUSTOMER RECORD COPYBOOK - Shared customer data structure
      *================================================================*
       01  CUSTOMER-RECORD.
           05  CUST-KEY.
               10  CUST-ID                 PIC 9(8).
               10  CUST-TYPE               PIC X(2).
                   88  CUST-INDIVIDUAL     VALUE 'IN'.
                   88  CUST-CORPORATE      VALUE 'CO'.
                   88  CUST-GOVERNMENT     VALUE 'GV'.
           05  CUST-NAME-DATA.
               10  CUST-FIRST-NAME         PIC X(25).
               10  CUST-LAST-NAME          PIC X(30).
               10  CUST-MIDDLE-INIT        PIC X(1).
           05  CUST-NAME-FULL REDEFINES CUST-NAME-DATA.
               10  CUST-FULL-NAME          PIC X(56).
           05  CUST-ADDRESS.
               10  CUST-STREET             PIC X(40).
               10  CUST-CITY               PIC X(25).
               10  CUST-STATE              PIC X(2).
               10  CUST-ZIP                PIC 9(5).
               10  CUST-ZIP-EXT            PIC 9(4).
           05  CUST-CONTACT.
               10  CUST-PHONE              PIC 9(10).
               10  CUST-EMAIL              PIC X(50).
           05  CUST-FINANCIAL.
               10  CUST-CREDIT-LIMIT       PIC 9(7)V99.
               10  CUST-BALANCE            PIC S9(7)V99.
               10  CUST-LAST-PAYMENT       PIC 9(7)V99.
               10  CUST-PAYMENT-DATE       PIC 9(8).
