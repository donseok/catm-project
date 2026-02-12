000100* =========================================================CPYRMMS
000200* COPYBOOK: CPYRMMS                                       CPYRMMS
000300* 용도    : 원자재 마스터 레코드 구조                     CPYRMMS
000400* 사용    : PGM006 (원자재 입고 및 검수 처리)             CPYRMMS
000500* =========================================================CPYRMMS
000600     05 RM-MATL-CD            PIC X(12).                  CPYRMMS
000700     05 RM-DATA.                                          CPYRMMS
000800         10 RM-MATL-NM        PIC X(30).                  CPYRMMS
000900         10 RM-CATEGORY       PIC X(02).                  CPYRMMS
001000             88 RM-IRON-ORE   VALUE 'IO'.                 CPYRMMS
001100             88 RM-COAL       VALUE 'CL'.                 CPYRMMS
001200             88 RM-LIMESTONE  VALUE 'LS'.                 CPYRMMS
001300             88 RM-FERRO      VALUE 'FA'.                 CPYRMMS
001400             88 RM-SCRAP      VALUE 'SC'.                 CPYRMMS
001500         10 RM-UNIT-CD        PIC X(03).                  CPYRMMS
001600         10 RM-UNIT-PRICE     PIC S9(9)V99 COMP-3.        CPYRMMS
001700         10 RM-STOCK-QTY      PIC S9(9)V99 COMP-3.        CPYRMMS
001800         10 RM-SAFETY-QTY     PIC S9(9)V99 COMP-3.        CPYRMMS
001900         10 RM-LEAD-DAYS      PIC 9(03).                  CPYRMMS
002000         10 RM-MAIN-VENDOR    PIC X(10).                  CPYRMMS
002100         10 RM-LAST-IN-DT     PIC 9(08).                  CPYRMMS
002200         10 RM-LAST-OUT-DT    PIC 9(08).                  CPYRMMS
002300         10 FILLER            PIC X(15).                  CPYRMMS
