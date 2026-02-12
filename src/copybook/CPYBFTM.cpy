000100* =========================================================CPYBFTM
000200* COPYBOOK: CPYBFTM                                       CPYBFTM
000300* 용도    : 고로 온도 측정 레코드 구조                    CPYBFTM
000400* 사용    : PGM005 (고로 조업일보 처리)                   CPYBFTM
000500* =========================================================CPYBFTM
000600     05 BT-KEY.                                           CPYBFTM
000700         10 BT-BF-NO          PIC X(03).                  CPYBFTM
000800         10 BT-MEASURE-DT     PIC 9(08).                  CPYBFTM
000900         10 BT-MEASURE-SEQ    PIC 9(05).                  CPYBFTM
001000     05 BT-DATA.                                          CPYBFTM
001100         10 BT-TEMP-VAL       PIC S9(5)V9 COMP-3.         CPYBFTM
001200         10 BT-SENSOR-ID      PIC X(08).                  CPYBFTM
001300         10 BT-LOCATION-CD    PIC X(04).                  CPYBFTM
001400             88 BT-HEARTH     VALUE 'HTH '.               CPYBFTM
001500             88 BT-BOSH       VALUE 'BOSH'.               CPYBFTM
001600             88 BT-SHAFT      VALUE 'SHFT'.               CPYBFTM
001700             88 BT-TOP        VALUE 'TOP '.               CPYBFTM
001800         10 BT-MEASURE-TYPE   PIC X(01).                  CPYBFTM
001900             88 BT-AUTO       VALUE 'A'.                  CPYBFTM
002000             88 BT-MANUAL     VALUE 'M'.                  CPYBFTM
002100         10 BT-STATUS-CD      PIC X(02).                  CPYBFTM
002200             88 BT-NORMAL     VALUE 'OK'.                 CPYBFTM
002300             88 BT-WARNING    VALUE 'WN'.                 CPYBFTM
002400             88 BT-CRITICAL   VALUE 'CR'.                 CPYBFTM
002500         10 BT-REG-TIME       PIC 9(06).                  CPYBFTM
002600         10 FILLER            PIC X(15).                  CPYBFTM
