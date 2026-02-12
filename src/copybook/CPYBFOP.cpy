000100* =========================================================CPYBFOP
000200* COPYBOOK: CPYBFOP                                       CPYBFOP
000300* 용도    : 고로 조업 데이터 레코드 구조                  CPYBFOP
000400* 사용    : PGM005 (고로 조업일보 처리)                   CPYBFOP
000500* =========================================================CPYBFOP
000600     05 BO-KEY.                                           CPYBFOP
000700         10 BO-BF-NO          PIC X(03).                  CPYBFOP
000800         10 BO-OPER-DATE      PIC 9(08).                  CPYBFOP
000900         10 BO-OPER-SEQ       PIC 9(05).                  CPYBFOP
001000     05 BO-DATA.                                          CPYBFOP
001100         10 BO-OPER-TYPE      PIC X(01).                  CPYBFOP
001200             88 BO-CHARGING   VALUE 'C'.                  CPYBFOP
001300             88 BO-TAPPING    VALUE 'T'.                  CPYBFOP
001400             88 BO-BLOWING    VALUE 'B'.                  CPYBFOP
001500         10 BO-SHIFT-CD       PIC X(01).                  CPYBFOP
001600             88 BO-SHIFT-A    VALUE 'A'.                  CPYBFOP
001700             88 BO-SHIFT-B    VALUE 'B'.                  CPYBFOP
001800             88 BO-SHIFT-C    VALUE 'C'.                  CPYBFOP
001900         10 BO-PRESSURE       PIC S9(3)V99 COMP-3.        CPYBFOP
002000         10 BO-WIND-VOL       PIC S9(7)V9 COMP-3.         CPYBFOP
002100         10 BO-COKE-RATE      PIC 9(3)V99.                CPYBFOP
002200         10 BO-TAP-QTY        PIC S9(9)V99 COMP-3.        CPYBFOP
002300         10 BO-SLAG-QTY       PIC S9(7)V99 COMP-3.        CPYBFOP
002400         10 BO-REMARK-CD      PIC X(03).                  CPYBFOP
002500         10 BO-REG-TIME       PIC 9(06).                  CPYBFOP
002600         10 BO-WORKER-ID      PIC X(10).                  CPYBFOP
002700         10 FILLER            PIC X(10).                  CPYBFOP
