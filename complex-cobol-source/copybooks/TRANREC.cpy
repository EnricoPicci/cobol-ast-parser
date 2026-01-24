      *================================================================*
      * TRANSACTION RECORD COPYBOOK - Multiple transaction formats
      *================================================================*
       01  TRANSACTION-RECORD.
           05  TRAN-HEADER.
               10  TRAN-ID                 PIC 9(12).
               10  TRAN-DATE               PIC 9(8).
               10  TRAN-TIME               PIC 9(6).
               10  TRAN-TYPE               PIC X(2).
                   88  TRAN-PAYMENT        VALUE 'PY'.
                   88  TRAN-PURCHASE       VALUE 'PU'.
                   88  TRAN-REFUND         VALUE 'RF'.
                   88  TRAN-ADJUSTMENT     VALUE 'AJ'.
           05  TRAN-CUSTOMER-ID            PIC 9(8).
           05  TRAN-AMOUNT                 PIC S9(7)V99.
           05  TRAN-DETAIL-AREA            PIC X(100).

      *----------------------------------------------------------------*
      * REDEFINES for Payment transactions
      *----------------------------------------------------------------*
       01  PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD.
           05  FILLER                      PIC X(37).
           05  PAY-METHOD                  PIC X(2).
               88  PAY-CASH                VALUE 'CA'.
               88  PAY-CHECK               VALUE 'CK'.
               88  PAY-CARD                VALUE 'CC'.
               88  PAY-ACH                 VALUE 'AC'.
           05  PAY-REFERENCE               PIC X(20).
           05  PAY-BANK-CODE               PIC X(10).
           05  PAY-AUTH-CODE               PIC X(12).
           05  FILLER                      PIC X(56).

      *----------------------------------------------------------------*
      * REDEFINES for Purchase transactions
      *----------------------------------------------------------------*
       01  PURCHASE-DETAIL REDEFINES TRANSACTION-RECORD.
           05  FILLER                      PIC X(37).
           05  PUR-ITEM-COUNT              PIC 9(3).
           05  PUR-SUBTOTAL                PIC 9(7)V99.
           05  PUR-TAX-AMOUNT              PIC 9(5)V99.
           05  PUR-DISCOUNT                PIC 9(5)V99.
           05  PUR-SHIP-METHOD             PIC X(2).
           05  PUR-SHIP-COST               PIC 9(5)V99.
           05  FILLER                      PIC X(61).

      *----------------------------------------------------------------*
      * REDEFINES for Refund transactions
      *----------------------------------------------------------------*
       01  REFUND-DETAIL REDEFINES TRANSACTION-RECORD.
           05  FILLER                      PIC X(37).
           05  REF-ORIGINAL-TRAN           PIC 9(12).
           05  REF-REASON-CODE             PIC X(4).
           05  REF-APPROVED-BY             PIC X(10).
           05  REF-APPROVAL-DATE           PIC 9(8).
           05  FILLER                      PIC X(66).
