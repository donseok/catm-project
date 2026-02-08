000100* =========================================================CPYTRANS
000200* COPYBOOK: CPYTRANS                                      CPYTRANS
000300* 용도    : 생산실적 트랜잭션 레코드 구조                 CPYTRANS
000400* =========================================================CPYTRANS
000500     05 PT-KEY.                                           CPYTRANS
000600         10 PT-PLANT-CD       PIC X(04).                  CPYTRANS
000700         10 PT-LINE-CD        PIC X(10).                  CPYTRANS
000800         10 PT-PROD-DATE      PIC 9(08).                  CPYTRANS
000900         10 PT-SEQ-NO         PIC 9(05).                  CPYTRANS
001000     05 PT-DATA.                                          CPYTRANS
001100         10 PT-PRODUCT-CD     PIC X(15).                  CPYTRANS
001200         10 PT-QTY            PIC S9(9)V99 COMP-3.        CPYTRANS
001300         10 PT-UNIT-CD        PIC X(03).                  CPYTRANS
001400         10 PT-WORKER-ID      PIC X(10).                  CPYTRANS
001500         10 PT-SHIFT-CD       PIC X(01).                  CPYTRANS
001600             88 PT-DAY-SHIFT  VALUE '1'.                  CPYTRANS
001700             88 PT-EVE-SHIFT  VALUE '2'.                  CPYTRANS
001800             88 PT-NGT-SHIFT  VALUE '3'.                  CPYTRANS
001900         10 PT-STATUS-CD      PIC X(02).                  CPYTRANS
002000             88 PT-NORMAL     VALUE '00'.                 CPYTRANS
002100             88 PT-DEFECT     VALUE '01'.                 CPYTRANS
002200             88 PT-REWORK     VALUE '02'.                 CPYTRANS
002300         10 PT-REG-TIME       PIC 9(06).                  CPYTRANS
002400         10 PT-UPD-TIME       PIC 9(06).                  CPYTRANS
002500         10 FILLER            PIC X(20).                  CPYTRANS
