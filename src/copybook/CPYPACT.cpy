000100* =========================================================CPYPACT
000200* COPYBOOK: CPYPACT                                       CPYPACT
000300* 용도    : 월별 생산실적 레코드 구조                     CPYPACT
000400* 사용    : PGM004 (생산계획 대비 실적 분석)              CPYPACT
000500* =========================================================CPYPACT
000600     05 PA-KEY.                                           CPYPACT
000700         10 PA-PRODUCT-CD     PIC X(15).                  CPYPACT
000800         10 PA-YYYYMM         PIC 9(06).                  CPYPACT
000900     05 PA-DATA.                                          CPYPACT
001000         10 PA-ACTUAL-QTY     PIC S9(9)V99 COMP-3.        CPYPACT
001100         10 PA-WORK-DAYS      PIC 9(03).                  CPYPACT
001200         10 PA-LINE-CNT       PIC 9(02).                  CPYPACT
001300         10 PA-DEFECT-QTY     PIC S9(7)V99 COMP-3.        CPYPACT
001400         10 PA-YIELD-RATE     PIC 9(3)V99.                CPYPACT
001500         10 PA-LAST-UPD       PIC 9(08).                  CPYPACT
001600         10 FILLER            PIC X(20).                  CPYPACT
