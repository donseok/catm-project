      ******************************************************************
      * CPYQCRS - 품질검사 결과 레코드
      *
      * 사용 프로그램: PGM003 (품질검사 실시간 처리)
      * 용도: QC-RESULT-FILE의 레코드 레이아웃
      ******************************************************************
       05 QR-KEY.
           10 QR-PLANT-CD        PIC X(04).
           10 QR-LINE-CD         PIC X(10).
           10 QR-INSPECT-DT      PIC 9(08).
           10 QR-INSPECT-ID      PIC 9(08).
       05 QR-DATA.
           10 QR-PRODUCT-CD      PIC X(15).
           10 QR-LOT-NO          PIC X(15).
           10 QR-JUDGE-CD        PIC X(01).
               88 QR-PASS        VALUE 'P'.
               88 QR-FAIL        VALUE 'F'.
               88 QR-REWORK      VALUE 'R'.
           10 QR-DEFECT-TYPE     PIC X(01).
               88 QR-CRITICAL    VALUE 'A'.
               88 QR-MAJOR       VALUE 'B'.
               88 QR-MINOR       VALUE 'C'.
           10 QR-DEFECT-CD       PIC X(05).
           10 QR-MEASURE-VAL     PIC S9(7)V99 COMP-3.
           10 QR-UPPER-LIMIT     PIC S9(7)V99 COMP-3.
           10 QR-LOWER-LIMIT     PIC S9(7)V99 COMP-3.
           10 QR-REWORK-CNT      PIC 9(02).
           10 QR-INSPECTOR-ID    PIC X(10).
           10 QR-REG-TIME        PIC 9(06).
           10 FILLER             PIC X(10).
