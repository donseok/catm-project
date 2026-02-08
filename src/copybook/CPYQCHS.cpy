      ******************************************************************
      * CPYQCHS - 품질검사 이력 레코드
      *
      * 사용 프로그램: PGM003 (품질검사 실시간 처리)
      * 용도: QC-HISTORY-FILE의 레코드 레이아웃
      ******************************************************************
       05 QH-INSPECT-ID      PIC 9(08).
       05 QH-LINE-CD         PIC X(10).
       05 QH-PRODUCT-CD      PIC X(15).
       05 QH-JUDGE-CD        PIC X(01).
       05 QH-INSPECT-DT      PIC 9(08).
       05 QH-FINAL-RESULT    PIC X(02).
       05 FILLER             PIC X(20).
