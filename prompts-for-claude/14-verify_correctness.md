# Question 1

Calculate how many bytes are taken by this Cobol record definition and which is the starting posizion of field TRAN-CUSTOMER-ID

**record definition**
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

# Question 2
Look at the **record descriptions** below. If I change field TRAN-CUSTOMER-ID which are the fields of PAYMENT-DETAIL that are changed?
Explain why they change.

**Record Descriptions**
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

# Bug description
If I run the following command "cobol-analyzer paragraph-variables-map complex-cobol-source/TRANPROC.cbl -c complex-cobol-source/copybooks -o output-1" the file TRANPROC-paragraph-variables.json tells that fields "PAY-AUTH-CODE" "PAY-BANK-CODE" and "PAYMENT-DETAIL" are affected by the change of "TRAN-CUSTOMER-ID" which occurs in line 132 of file "complex-cobol-source/TRANPROC.cbl" (see **json snippet** below). Is this a bug given that TRANPROC-paragraph-variables.json should highlight all fields that may change in each PARAGRAPH or SECTION?

**json snippet** 
    "2000-PROCESS": {
      "PAY-AUTH-CODE": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PAYMENT-DETAIL"
      },
      "PAY-BANK-CODE": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PAYMENT-DETAIL"
      },
      "PAYMENT-DETAIL": {
        "77-level-var": true,
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PAYMENT-DETAIL"
      },
      "PUR-SHIP-COST": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PURCHASE-DETAIL"
      },
      "PUR-SHIP-METHOD": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PURCHASE-DETAIL"
      },
      "PURCHASE-DETAIL": {
        "77-level-var": true,
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PURCHASE-DETAIL"
      },
      "REF-APPROVAL-DATE": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "REFUND-DETAIL"
      },
      "REFUND-DETAIL": {
        "77-level-var": true,
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "REFUND-DETAIL"
      },
      "RPT-ERROR-COUNT": {
        "base_record": "REPORT-COUNTERS",
        "defined_in_record": "REPORT-COUNTERS"
      },
      "RPT-RECORD-COUNT": {
        "base_record": "REPORT-COUNTERS",
        "defined_in_record": "REPORT-COUNTERS"
      },
      "TRAN-CUSTOMER-ID": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "TRANSACTION-RECORD"
      },
      "WS-ERROR-COUNT": {
        "base_record": "WS-COUNTS",
        "defined_in_record": "WS-COUNTS"
      }
    },