000100* =========================================================CPYPPLAN
000200* COPYBOOK: CPYPPLAN                                      CPYPPLAN
000300* 용도    : 월별 생산계획 레코드 구조                     CPYPPLAN
000400* 사용    : PGM004 (생산계획 대비 실적 분석)              CPYPPLAN
000500* =========================================================CPYPPLAN
000600     05 PP-KEY.                                           CPYPPLAN
000700         10 PP-PLANT-CD       PIC X(04).                  CPYPPLAN
000800         10 PP-PRODUCT-CD     PIC X(15).                  CPYPPLAN
000900         10 PP-YYYYMM         PIC 9(06).                  CPYPPLAN
001000     05 PP-DATA.                                          CPYPPLAN
001100         10 PP-PLAN-QTY       PIC S9(9)V99 COMP-3.        CPYPPLAN
001200         10 PP-UNIT-CD        PIC X(03).                  CPYPPLAN
001300         10 PP-LINE-CD        PIC X(10).                  CPYPPLAN
001400         10 PP-PRIORITY       PIC 9(01).                  CPYPPLAN
001500             88 PP-URGENT     VALUE 1.                    CPYPPLAN
001600             88 PP-NORMAL     VALUE 2.                    CPYPPLAN
001700             88 PP-LOW        VALUE 3.                    CPYPPLAN
001800         10 PP-PLAN-TYPE      PIC X(01).                  CPYPPLAN
001900             88 PP-REGULAR    VALUE 'R'.                  CPYPPLAN
002000             88 PP-EXTRA      VALUE 'E'.                  CPYPPLAN
002100             88 PP-TRIAL      VALUE 'T'.                  CPYPPLAN
002200         10 PP-REG-DATE       PIC 9(08).                  CPYPPLAN
002300         10 PP-REG-USER       PIC X(10).                  CPYPPLAN
002400         10 FILLER            PIC X(15).                  CPYPPLAN
