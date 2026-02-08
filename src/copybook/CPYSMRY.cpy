      ******************************************************************
      * CPYSMRY - 일일 생산집계 요약 레코드
      *
      * 사용 프로그램: PGM001 (생산실적 일일집계)
      * 용도: DAILY-SUMMARY-FILE의 레코드 레이아웃
      ******************************************************************
       05 DS-SUMMARY-KEY.
           10 DS-PLANT-CD        PIC X(04).
           10 DS-PROD-DATE       PIC 9(08).
       05 DS-SUMMARY-DATA.
           10 DS-TOTAL-QTY       PIC S9(9)V99 COMP-3.
           10 DS-TOTAL-COUNT     PIC 9(07).
           10 DS-ERROR-COUNT     PIC 9(05).
           10 DS-PROCESS-TIME    PIC 9(06).
           10 DS-STATUS-CD       PIC X(02).
               88 DS-NORMAL      VALUE '00'.
               88 DS-WARNING     VALUE '01'.
               88 DS-ERROR       VALUE '99'.
           10 FILLER             PIC X(20).
