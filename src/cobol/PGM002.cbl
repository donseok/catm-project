000100 IDENTIFICATION DIVISION.                                 PGM002
000200 PROGRAM-ID.    PGM002.                                   PGM002
000300 AUTHOR.        DONGKUK-SYSTEMS.                          PGM002
000400*                                                         PGM002
000500* =========================================================PGM002
000600* 프로그램명: 재고수불 일일처리                            PGM002
000700* 처리내용  : 입출고 트랜잭션을 처리하여                   PGM002
000800*            재고 마스터를 갱신하고 수불대장을 생성한다.   PGM002
000900* =========================================================PGM002
001000                                                          PGM002
001100 ENVIRONMENT DIVISION.                                    PGM002
001200 CONFIGURATION SECTION.                                   PGM002
001300 INPUT-OUTPUT SECTION.                                    PGM002
001400 FILE-CONTROL.                                            PGM002
001500     SELECT INV-TRANS-FILE                                PGM002
001600         ASSIGN TO INVTRAN                                PGM002
001700         ORGANIZATION IS INDEXED                          PGM002
001800         ACCESS MODE IS SEQUENTIAL                        PGM002
001900         RECORD KEY IS IT-KEY                             PGM002
002000         FILE STATUS IS WS-FILE-STATUS.                   PGM002
002100     SELECT INV-MASTER-FILE                               PGM002
002200         ASSIGN TO INVMAST                                PGM002
002300         ORGANIZATION IS INDEXED                          PGM002
002400         ACCESS MODE IS RANDOM                            PGM002
002500         RECORD KEY IS IM-ITEM-CD                         PGM002
002600         FILE STATUS IS WS-FILE-STATUS2.                  PGM002
002700     SELECT LEDGER-FILE                                   PGM002
002800         ASSIGN TO INVLEDG                                PGM002
002900         ORGANIZATION IS SEQUENTIAL                       PGM002
003000         FILE STATUS IS WS-FILE-STATUS3.                  PGM002
003100                                                          PGM002
003200 DATA DIVISION.                                           PGM002
003300 FILE SECTION.                                            PGM002
003400 FD  INV-TRANS-FILE.                                      PGM002
003500 01  INV-TRANS-REC.                                       PGM002
003600     COPY CPYINVTR.                                       PGM002
003700 FD  INV-MASTER-FILE.                                     PGM002
003800 01  INV-MASTER-REC.                                      PGM002
003900     COPY CPYINVMS.                                       PGM002
004000 FD  LEDGER-FILE.                                         PGM002
004100 01  LEDGER-REC.                                          PGM002
004200     COPY CPYLEDGR.                                       PGM002
004300                                                          PGM002
004400 WORKING-STORAGE SECTION.                                 PGM002
004500 01  WS-FILE-STATUS         PIC XX.                       PGM002
004600 01  WS-FILE-STATUS2        PIC XX.                       PGM002
004700 01  WS-FILE-STATUS3        PIC XX.                       PGM002
004800 01  WS-EOF-FLAG            PIC X VALUE 'N'.              PGM002
004900     88 WS-EOF              VALUE 'Y'.                    PGM002
005000 01  WS-PROCESS-COUNT       PIC 9(7) VALUE ZEROS.         PGM002
005100 01  WS-IN-COUNT            PIC 9(7) VALUE ZEROS.         PGM002
005200 01  WS-OUT-COUNT           PIC 9(7) VALUE ZEROS.         PGM002
005300 01  WS-ERROR-COUNT         PIC 9(5) VALUE ZEROS.         PGM002
005400 01  WS-PREV-QTY            PIC S9(9) VALUE ZEROS.        PGM002
005500 01  WS-NEW-QTY             PIC S9(9) VALUE ZEROS.        PGM002
005600                                                          PGM002
005700     EXEC SQL INCLUDE SQLCA END-EXEC.                     PGM002
005800     EXEC SQL INCLUDE DCLTBINV END-EXEC.                  PGM002
005900                                                          PGM002
006000 PROCEDURE DIVISION.                                      PGM002
006100*                                                         PGM002
006200 0000-MAIN-PROCESS.                                       PGM002
006300     PERFORM 1000-INITIALIZE                              PGM002
006400     PERFORM 2000-PROCESS-TRANSACTION                     PGM002
006500         UNTIL WS-EOF                                     PGM002
006600     PERFORM 3000-UPDATE-SUMMARY                          PGM002
006700     PERFORM 9000-FINALIZE                                PGM002
006800     STOP RUN.                                            PGM002
006900*                                                         PGM002
007000 1000-INITIALIZE.                                         PGM002
007100     OPEN INPUT  INV-TRANS-FILE                           PGM002
007200     OPEN I-O    INV-MASTER-FILE                          PGM002
007300     OPEN OUTPUT LEDGER-FILE                              PGM002
007400     IF WS-FILE-STATUS NOT = '00'                         PGM002
007500         DISPLAY 'TRANS FILE OPEN ERROR: ' WS-FILE-STATUS PGM002
007600         PERFORM 9900-ABNORMAL-END                        PGM002
007700     END-IF                                               PGM002
007800     IF WS-FILE-STATUS2 NOT = '00'                        PGM002
007900         DISPLAY 'MASTER FILE OPEN ERROR: ' WS-FILE-STATUS2PGM002
008000         PERFORM 9900-ABNORMAL-END                        PGM002
008100     END-IF                                               PGM002
008200     PERFORM 1100-READ-FIRST-RECORD.                      PGM002
008300*                                                         PGM002
008400 1100-READ-FIRST-RECORD.                                  PGM002
008500     READ INV-TRANS-FILE                                  PGM002
008600     AT END SET WS-EOF TO TRUE                            PGM002
008700     END-READ.                                            PGM002
008800*                                                         PGM002
008900 2000-PROCESS-TRANSACTION.                                PGM002
009000     ADD 1 TO WS-PROCESS-COUNT                            PGM002
009100     MOVE IT-ITEM-CD TO IM-ITEM-CD                        PGM002
009200     READ INV-MASTER-FILE                                 PGM002
009300         INVALID KEY                                      PGM002
009400             PERFORM 2100-HANDLE-NEW-ITEM                 PGM002
009500         NOT INVALID KEY                                  PGM002
009600             PERFORM 2200-UPDATE-INVENTORY                PGM002
009700     END-READ                                             PGM002
009800     PERFORM 2300-WRITE-LEDGER                            PGM002
009900     READ INV-TRANS-FILE                                  PGM002
010000     AT END SET WS-EOF TO TRUE                            PGM002
010100     END-READ.                                            PGM002
010200*                                                         PGM002
010300 2100-HANDLE-NEW-ITEM.                                    PGM002
010400     INITIALIZE INV-MASTER-REC                            PGM002
010500     MOVE IT-ITEM-CD TO IM-ITEM-CD                        PGM002
010600     MOVE IT-QTY TO IM-CURR-QTY                           PGM002
010700     MOVE IT-TRANS-DATE TO IM-LAST-DATE                   PGM002
010800     WRITE INV-MASTER-REC                                 PGM002
010900     ADD 1 TO WS-IN-COUNT.                                PGM002
011000*                                                         PGM002
011100 2200-UPDATE-INVENTORY.                                   PGM002
011200     MOVE IM-CURR-QTY TO WS-PREV-QTY                      PGM002
011300     EVALUATE IT-TRANS-TYPE                               PGM002
011400         WHEN 'I'                                         PGM002
011500             ADD IT-QTY TO IM-CURR-QTY                    PGM002
011600             ADD 1 TO WS-IN-COUNT                         PGM002
011700         WHEN 'O'                                         PGM002
011800             SUBTRACT IT-QTY FROM IM-CURR-QTY             PGM002
011900             ADD 1 TO WS-OUT-COUNT                        PGM002
012000             IF IM-CURR-QTY < 0                           PGM002
012100                 CALL 'STOCKERR' USING IT-ITEM-CD         PGM002
012200                                       IM-CURR-QTY        PGM002
012300                 MOVE WS-PREV-QTY TO IM-CURR-QTY          PGM002
012400                 ADD 1 TO WS-ERROR-COUNT                  PGM002
012500             END-IF                                       PGM002
012600         WHEN OTHER                                       PGM002
012700             CALL 'ERRLOG' USING IT-TRANS-TYPE IT-ITEM-CD PGM002
012800             ADD 1 TO WS-ERROR-COUNT                      PGM002
012900     END-EVALUATE                                         PGM002
013000     MOVE IT-TRANS-DATE TO IM-LAST-DATE                   PGM002
013100     REWRITE INV-MASTER-REC.                              PGM002
013200*                                                         PGM002
013300 2300-WRITE-LEDGER.                                       PGM002
013400     INITIALIZE LEDGER-REC                                PGM002
013500     MOVE IT-ITEM-CD TO LG-ITEM-CD                        PGM002
013600     MOVE IT-TRANS-TYPE TO LG-TRANS-TYPE                  PGM002
013700     MOVE IT-QTY TO LG-QTY                                PGM002
013800     MOVE WS-PREV-QTY TO LG-PREV-QTY                      PGM002
013900     MOVE IM-CURR-QTY TO LG-CURR-QTY                      PGM002
014000     MOVE IT-TRANS-DATE TO LG-TRANS-DATE                  PGM002
014100     WRITE LEDGER-REC.                                    PGM002
014200*                                                         PGM002
014300 3000-UPDATE-SUMMARY.                                     PGM002
014400     EXEC SQL                                             PGM002
014500         INSERT INTO TB_INV_SUMMARY                       PGM002
014600         (PROC_DATE, IN_CNT, OUT_CNT, ERR_CNT)            PGM002
014700         VALUES                                           PGM002
014800         (CURRENT DATE,                                   PGM002
014900          :WS-IN-COUNT, :WS-OUT-COUNT, :WS-ERROR-COUNT)   PGM002
015000     END-EXEC                                             PGM002
015100     IF SQLCODE NOT = 0                                   PGM002
015200         CALL 'SQLERR' USING SQLCODE                      PGM002
015300     END-IF.                                              PGM002
015400*                                                         PGM002
015500 9000-FINALIZE.                                           PGM002
015600     CLOSE INV-TRANS-FILE                                 PGM002
015700     CLOSE INV-MASTER-FILE                                PGM002
015800     CLOSE LEDGER-FILE                                    PGM002
015900     DISPLAY 'PGM002 COMPLETED: ' WS-PROCESS-COUNT        PGM002
016000             ' TRANS, IN:' WS-IN-COUNT                    PGM002
016100             ' OUT:' WS-OUT-COUNT.                        PGM002
016200*                                                         PGM002
016300 9900-ABNORMAL-END.                                       PGM002
016400     DISPLAY 'PGM002 ABEND'                               PGM002
016500     CALL 'ABNDPGM' USING WS-FILE-STATUS                  PGM002
016600     STOP RUN.                                            PGM002
