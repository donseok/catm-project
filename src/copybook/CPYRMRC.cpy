000100* =========================================================CPYRMRC
000200* COPYBOOK: CPYRMRC                                       CPYRMRC
000300* 용도    : 원자재 입고 전표 레코드 구조                  CPYRMRC
000400* 사용    : PGM006 (원자재 입고 및 검수 처리)             CPYRMRC
000500* =========================================================CPYRMRC
000600     05 RR-KEY.                                           CPYRMRC
000700         10 RR-RECEIPT-NO     PIC 9(10).                  CPYRMRC
000800         10 RR-RECEIPT-DT     PIC 9(08).                  CPYRMRC
000900     05 RR-DATA.                                          CPYRMRC
001000         10 RR-MATL-CD        PIC X(12).                  CPYRMRC
001100         10 RR-MATL-NM        PIC X(30).                  CPYRMRC
001200         10 RR-VENDOR-CD      PIC X(10).                  CPYRMRC
001300         10 RR-RECEIPT-QTY    PIC S9(9)V99 COMP-3.        CPYRMRC
001400         10 RR-UNIT-CD        PIC X(03).                  CPYRMRC
001500         10 RR-INSPECT-CD     PIC X(01).                  CPYRMRC
001600             88 RR-ACCEPTED   VALUE 'A'.                  CPYRMRC
001700             88 RR-PARTIAL    VALUE 'P'.                  CPYRMRC
001800             88 RR-REJECTED   VALUE 'R'.                  CPYRMRC
001900         10 RR-ACCEPT-RATE    PIC 9(3)V99.                CPYRMRC
002000         10 RR-REASON-CD      PIC X(03).                  CPYRMRC
002100             88 RR-QUALITY    VALUE 'QL '.                CPYRMRC
002200             88 RR-DAMAGE     VALUE 'DM '.                CPYRMRC
002300             88 RR-SPEC       VALUE 'SP '.                CPYRMRC
002400             88 RR-MOISTURE   VALUE 'MS '.                CPYRMRC
002500         10 RR-WAREHOUSE-CD   PIC X(04).                  CPYRMRC
002600         10 RR-PO-NO          PIC X(12).                  CPYRMRC
002700         10 RR-REG-USER       PIC X(10).                  CPYRMRC
002800         10 FILLER            PIC X(10).                  CPYRMRC
