000100 IDENTIFICATION DIVISION.                                 PGM001
000200 PROGRAM-ID.    PGM001.                                   PGM001
000300 AUTHOR.        LEGACY-DEVELOPER.                         PGM001
000400*                                                         PGM001
000500* =========================================================PGM001
000600* 프로그램명: 생산실적 일일집계                            PGM001
000700* 처리내용  : 전일 생산라인별 실적을 집계하여              PGM001
000800*            일일 생산보고서를 생성한다.                   PGM001
000900* =========================================================PGM001
001000                                                          PGM001
001100 ENVIRONMENT DIVISION.                                    PGM001
001200 CONFIGURATION SECTION.                                   PGM001
001300 INPUT-OUTPUT SECTION.                                    PGM001
001400 FILE-CONTROL.                                            PGM001
001500     SELECT PROD-TRANS-FILE                               PGM001
001600         ASSIGN TO PRODTRAN                               PGM001
001700         ORGANIZATION IS INDEXED                          PGM001
001800         ACCESS MODE IS SEQUENTIAL                        PGM001
001900         RECORD KEY IS PT-KEY                             PGM001
002000         FILE STATUS IS WS-FILE-STATUS.                   PGM001
002100     SELECT DAILY-SUMMARY-FILE                            PGM001
002200         ASSIGN TO DLYSMRY                                PGM001
002300         ORGANIZATION IS SEQUENTIAL                       PGM001
002400         FILE STATUS IS WS-FILE-STATUS2.                  PGM001
002500                                                          PGM001
002600 DATA DIVISION.                                           PGM001
002700 FILE SECTION.                                            PGM001
002800 FD  PROD-TRANS-FILE.                                     PGM001
002900 01  PROD-TRANS-REC.                                      PGM001
003000     COPY CPYTRANS.                                       PGM001
003100 FD  DAILY-SUMMARY-FILE.                                  PGM001
003200 01  DAILY-SUMMARY-REC.                                   PGM001
003300     COPY CPYSMRY.                                        PGM001
003400                                                          PGM001
003500 WORKING-STORAGE SECTION.                                 PGM001
003600 01  WS-FILE-STATUS         PIC XX.                       PGM001
003700 01  WS-FILE-STATUS2        PIC XX.                       PGM001
003800 01  WS-EOF-FLAG            PIC X VALUE 'N'.              PGM001
003900     88 WS-EOF              VALUE 'Y'.                    PGM001
004000 01  WS-LINE-TOTAL          PIC 9(9)V99 VALUE ZEROS.      PGM001
004100 01  WS-DAILY-COUNT         PIC 9(7) VALUE ZEROS.         PGM001
004200 01  WS-ERROR-COUNT         PIC 9(5) VALUE ZEROS.         PGM001
004300 01  WS-CURRENT-LINE        PIC X(10).                    PGM001
004400                                                          PGM001
004500     EXEC SQL INCLUDE SQLCA END-EXEC.                     PGM001
004600     EXEC SQL INCLUDE DCLTBPROD END-EXEC.                 PGM001
004700                                                          PGM001
004800 PROCEDURE DIVISION.                                      PGM001
004900*                                                         PGM001
005000 0000-MAIN-PROCESS.                                       PGM001
005100     PERFORM 1000-INITIALIZE                              PGM001
005200     PERFORM 2000-PROCESS-RECORDS                         PGM001
005300         UNTIL WS-EOF                                     PGM001
005400     PERFORM 3000-WRITE-SUMMARY                           PGM001
005500     PERFORM 4000-UPDATE-DB2                              PGM001
005600     PERFORM 9000-FINALIZE                                PGM001
005700     STOP RUN.                                            PGM001
005800*                                                         PGM001
005900 1000-INITIALIZE.                                         PGM001
006000     OPEN INPUT  PROD-TRANS-FILE                          PGM001
006100     OPEN OUTPUT DAILY-SUMMARY-FILE                       PGM001
006200     IF WS-FILE-STATUS NOT = '00'                         PGM001
006300         DISPLAY 'OPEN ERROR: ' WS-FILE-STATUS            PGM001
006400         PERFORM 9900-ABNORMAL-END                        PGM001
006500     END-IF                                               PGM001
006600     PERFORM 1100-READ-FIRST-RECORD.                      PGM001
006700*                                                         PGM001
006800 1100-READ-FIRST-RECORD.                                  PGM001
006900     READ PROD-TRANS-FILE                                 PGM001
007000     AT END SET WS-EOF TO TRUE                            PGM001
007100     END-READ.                                            PGM001
007200*                                                         PGM001
007300 2000-PROCESS-RECORDS.                                    PGM001
007400     EVALUATE TRUE                                        PGM001
007500         WHEN PT-QTY > 0                                  PGM001
007600             ADD PT-QTY TO WS-LINE-TOTAL                  PGM001
007700             ADD 1 TO WS-DAILY-COUNT                      PGM001
007800         WHEN PT-QTY = 0                                  PGM001
007900             ADD 1 TO WS-ERROR-COUNT                      PGM001
008000         WHEN OTHER                                       PGM001
008100             CALL 'ERRLOG' USING PT-LINE-CD PT-QTY        PGM001
008200             ADD 1 TO WS-ERROR-COUNT                      PGM001
008300     END-EVALUATE                                         PGM001
008400     READ PROD-TRANS-FILE                                 PGM001
008500     AT END SET WS-EOF TO TRUE                            PGM001
008600     END-READ.                                            PGM001
008700*                                                         PGM001
008800 3000-WRITE-SUMMARY.                                      PGM001
008900     MOVE WS-LINE-TOTAL  TO DS-TOTAL-QTY                  PGM001
009000     MOVE WS-DAILY-COUNT TO DS-TOTAL-COUNT                PGM001
009100     MOVE WS-ERROR-COUNT TO DS-ERROR-COUNT                PGM001
009200     WRITE DAILY-SUMMARY-REC.                             PGM001
009300*                                                         PGM001
009400 4000-UPDATE-DB2.                                         PGM001
009500     EXEC SQL                                             PGM001
009600         UPDATE TB_DAILY_PROD                              PGM001
009700         SET TOTAL_QTY   = :WS-LINE-TOTAL                 PGM001
009800           , TOTAL_COUNT = :WS-DAILY-COUNT                PGM001
009900           , ERROR_COUNT = :WS-ERROR-COUNT                PGM001
010000         WHERE PROD_DATE = CURRENT DATE                   PGM001
010100     END-EXEC                                             PGM001
010200     IF SQLCODE NOT = 0                                   PGM001
010300         CALL 'SQLERR' USING SQLCODE                      PGM001
010400     END-IF.                                              PGM001
010500*                                                         PGM001
010600 9000-FINALIZE.                                           PGM001
010700     CLOSE PROD-TRANS-FILE                                PGM001
010800     CLOSE DAILY-SUMMARY-FILE                             PGM001
010900     DISPLAY 'PGM001 COMPLETED: ' WS-DAILY-COUNT          PGM001
011000             ' RECORDS PROCESSED'.                        PGM001
011100*                                                         PGM001
011200 9900-ABNORMAL-END.                                       PGM001
011300     DISPLAY 'PGM001 ABEND - FILE STATUS: ' WS-FILE-STATUSPGM001
011400     CALL 'ABNDPGM' USING WS-FILE-STATUS                 PGM001
011500     STOP RUN.                                            PGM001
