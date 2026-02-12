000100 IDENTIFICATION DIVISION.                                 PGM004
000200 PROGRAM-ID.    PGM004.                                   PGM004
000300 AUTHOR.        DONGKUK-SYSTEMS.                          PGM004
000400*                                                         PGM004
000500* =========================================================PGM004
000600* 프로그램명: 생산계획 대비 실적 분석                      PGM004
000700* 처리내용  : 월별 생산계획 대비 실적을 비교하여           PGM004
000800*            달성율을 산출하고 미달 품목에 대해             PGM004
000900*            생산지시 알림을 발행한다.                     PGM004
001000* =========================================================PGM004
001100                                                          PGM004
001200 ENVIRONMENT DIVISION.                                    PGM004
001300 CONFIGURATION SECTION.                                   PGM004
001400 INPUT-OUTPUT SECTION.                                    PGM004
001500 FILE-CONTROL.                                            PGM004
001600     SELECT PROD-PLAN-FILE                                PGM004
001700         ASSIGN TO PRDPLAN                                PGM004
001800         ORGANIZATION IS INDEXED                          PGM004
001900         ACCESS MODE IS SEQUENTIAL                        PGM004
002000         RECORD KEY IS PP-KEY                             PGM004
002100         FILE STATUS IS WS-FILE-STATUS.                   PGM004
002200     SELECT PROD-ACTUAL-FILE                              PGM004
002300         ASSIGN TO PRDACT                                 PGM004
002400         ORGANIZATION IS INDEXED                          PGM004
002500         ACCESS MODE IS RANDOM                            PGM004
002600         RECORD KEY IS PA-KEY                             PGM004
002700         FILE STATUS IS WS-FILE-STATUS2.                  PGM004
002800     SELECT PLAN-RESULT-FILE                              PGM004
002900         ASSIGN TO PLNRSLT                                PGM004
003000         ORGANIZATION IS SEQUENTIAL                       PGM004
003100         FILE STATUS IS WS-FILE-STATUS3.                  PGM004
003200                                                          PGM004
003300 DATA DIVISION.                                           PGM004
003400 FILE SECTION.                                            PGM004
003500 FD  PROD-PLAN-FILE.                                      PGM004
003600 01  PROD-PLAN-REC.                                       PGM004
003700     COPY CPYPPLAN.                                       PGM004
003800 FD  PROD-ACTUAL-FILE.                                    PGM004
003900 01  PROD-ACTUAL-REC.                                     PGM004
004000     COPY CPYPACT.                                        PGM004
004100 FD  PLAN-RESULT-FILE.                                    PGM004
004200 01  PLAN-RESULT-REC.                                     PGM004
004300     05 PR-PRODUCT-CD        PIC X(15).                   PGM004
004400     05 PR-PLAN-QTY          PIC S9(9)V99.                PGM004
004500     05 PR-ACTUAL-QTY        PIC S9(9)V99.                PGM004
004600     05 PR-ACHIEVE-RATE      PIC 9(3)V99.                 PGM004
004700     05 PR-GAP-QTY           PIC S9(9)V99.                PGM004
004800     05 PR-JUDGE-CD          PIC X(01).                   PGM004
004900     05 FILLER               PIC X(20).                   PGM004
005000                                                          PGM004
005100 WORKING-STORAGE SECTION.                                 PGM004
005200 01  WS-FILE-STATUS          PIC XX.                      PGM004
005300 01  WS-FILE-STATUS2         PIC XX.                      PGM004
005400 01  WS-FILE-STATUS3         PIC XX.                      PGM004
005500 01  WS-EOF-FLAG             PIC X VALUE 'N'.             PGM004
005600     88 WS-EOF               VALUE 'Y'.                   PGM004
005700 01  WS-PLAN-COUNT           PIC 9(7) VALUE ZEROS.        PGM004
005800 01  WS-ACHIEVE-COUNT        PIC 9(7) VALUE ZEROS.        PGM004
005900 01  WS-SHORT-COUNT          PIC 9(7) VALUE ZEROS.        PGM004
006000 01  WS-OVER-COUNT           PIC 9(7) VALUE ZEROS.        PGM004
006100 01  WS-ERROR-COUNT          PIC 9(5) VALUE ZEROS.        PGM004
006200 01  WS-ACHIEVE-RATE         PIC 9(3)V99 VALUE ZEROS.     PGM004
006300 01  WS-TOTAL-PLAN           PIC S9(11)V99 VALUE ZEROS.   PGM004
006400 01  WS-TOTAL-ACTUAL         PIC S9(11)V99 VALUE ZEROS.   PGM004
006500 01  WS-ACHIEVE-THRESHOLD    PIC 9(3) VALUE 090.          PGM004
006600 01  WS-OVER-THRESHOLD       PIC 9(3) VALUE 120.          PGM004
006700                                                          PGM004
006800     EXEC SQL INCLUDE SQLCA END-EXEC.                     PGM004
006900     EXEC SQL INCLUDE DCLTBPPRF END-EXEC.                 PGM004
007000                                                          PGM004
007100 PROCEDURE DIVISION.                                      PGM004
007200*                                                         PGM004
007300 0000-MAIN-PROCESS.                                       PGM004
007400     PERFORM 1000-INITIALIZE                              PGM004
007500     PERFORM 2000-COMPARE-PLAN-ACTUAL                     PGM004
007600         UNTIL WS-EOF                                     PGM004
007700     PERFORM 3000-CALC-TOTAL-RATE                         PGM004
007800     PERFORM 4000-UPDATE-DB2-SUMMARY                      PGM004
007900     PERFORM 5000-NOTIFY-SHORTFALL                        PGM004
008000     PERFORM 9000-FINALIZE                                PGM004
008100     STOP RUN.                                            PGM004
008200*                                                         PGM004
008300 1000-INITIALIZE.                                         PGM004
008400     OPEN INPUT  PROD-PLAN-FILE                           PGM004
008500     OPEN INPUT  PROD-ACTUAL-FILE                         PGM004
008600     OPEN OUTPUT PLAN-RESULT-FILE                         PGM004
008700     IF WS-FILE-STATUS NOT = '00'                         PGM004
008800         DISPLAY 'PLAN FILE OPEN ERROR: '                 PGM004
008900                 WS-FILE-STATUS                           PGM004
009000         PERFORM 9900-ABNORMAL-END                        PGM004
009100     END-IF                                               PGM004
009200     IF WS-FILE-STATUS2 NOT = '00'                        PGM004
009300         DISPLAY 'ACTUAL FILE OPEN ERROR: '               PGM004
009400                 WS-FILE-STATUS2                          PGM004
009500         PERFORM 9900-ABNORMAL-END                        PGM004
009600     END-IF                                               PGM004
009700     PERFORM 1100-READ-FIRST-RECORD.                      PGM004
009800*                                                         PGM004
009900 1100-READ-FIRST-RECORD.                                  PGM004
010000     READ PROD-PLAN-FILE                                  PGM004
010100     AT END SET WS-EOF TO TRUE                            PGM004
010200     END-READ.                                            PGM004
010300*                                                         PGM004
010400 2000-COMPARE-PLAN-ACTUAL.                                PGM004
010500     ADD 1 TO WS-PLAN-COUNT                               PGM004
010600     MOVE PP-PRODUCT-CD TO PA-PRODUCT-CD                  PGM004
010700     MOVE PP-YYYYMM     TO PA-YYYYMM                     PGM004
010800     READ PROD-ACTUAL-FILE                                PGM004
010900         INVALID KEY                                      PGM004
011000             PERFORM 2100-NO-ACTUAL-DATA                  PGM004
011100         NOT INVALID KEY                                  PGM004
011200             PERFORM 2200-CALC-ACHIEVEMENT                PGM004
011300     END-READ                                             PGM004
011400     READ PROD-PLAN-FILE                                  PGM004
011500     AT END SET WS-EOF TO TRUE                            PGM004
011600     END-READ.                                            PGM004
011700*                                                         PGM004
011800 2100-NO-ACTUAL-DATA.                                     PGM004
011900     MOVE ZEROS TO PR-ACTUAL-QTY                          PGM004
012000     MOVE ZEROS TO PR-ACHIEVE-RATE                        PGM004
012100     MOVE PP-PLAN-QTY TO PR-GAP-QTY                       PGM004
012200     MOVE 'S' TO PR-JUDGE-CD                              PGM004
012300     ADD 1 TO WS-SHORT-COUNT                              PGM004
012400     ADD PP-PLAN-QTY TO WS-TOTAL-PLAN                     PGM004
012500     MOVE PP-PRODUCT-CD TO PR-PRODUCT-CD                  PGM004
012600     MOVE PP-PLAN-QTY   TO PR-PLAN-QTY                    PGM004
012700     WRITE PLAN-RESULT-REC                                PGM004
012800     CALL 'ERRLOG' USING PP-PRODUCT-CD PP-YYYYMM.         PGM004
012900*                                                         PGM004
013000 2200-CALC-ACHIEVEMENT.                                   PGM004
013100     ADD PP-PLAN-QTY   TO WS-TOTAL-PLAN                   PGM004
013200     ADD PA-ACTUAL-QTY TO WS-TOTAL-ACTUAL                 PGM004
013300     MOVE PP-PRODUCT-CD TO PR-PRODUCT-CD                  PGM004
013400     MOVE PP-PLAN-QTY   TO PR-PLAN-QTY                    PGM004
013500     MOVE PA-ACTUAL-QTY TO PR-ACTUAL-QTY                  PGM004
013600     IF PP-PLAN-QTY > 0                                   PGM004
013700         COMPUTE WS-ACHIEVE-RATE =                        PGM004
013800             (PA-ACTUAL-QTY / PP-PLAN-QTY) * 100          PGM004
013900     ELSE                                                 PGM004
014000         MOVE 100.00 TO WS-ACHIEVE-RATE                   PGM004
014100     END-IF                                               PGM004
014200     MOVE WS-ACHIEVE-RATE TO PR-ACHIEVE-RATE              PGM004
014300     COMPUTE PR-GAP-QTY =                                 PGM004
014400         PP-PLAN-QTY - PA-ACTUAL-QTY                      PGM004
014500     EVALUATE TRUE                                        PGM004
014600         WHEN WS-ACHIEVE-RATE >= WS-OVER-THRESHOLD        PGM004
014700             MOVE 'O' TO PR-JUDGE-CD                      PGM004
014800             ADD 1 TO WS-OVER-COUNT                       PGM004
014900         WHEN WS-ACHIEVE-RATE >= WS-ACHIEVE-THRESHOLD     PGM004
015000             MOVE 'A' TO PR-JUDGE-CD                      PGM004
015100             ADD 1 TO WS-ACHIEVE-COUNT                    PGM004
015200         WHEN OTHER                                       PGM004
015300             MOVE 'S' TO PR-JUDGE-CD                      PGM004
015400             ADD 1 TO WS-SHORT-COUNT                      PGM004
015500     END-EVALUATE                                         PGM004
015600     WRITE PLAN-RESULT-REC.                               PGM004
015700*                                                         PGM004
015800 3000-CALC-TOTAL-RATE.                                    PGM004
015900     IF WS-TOTAL-PLAN > 0                                 PGM004
016000         COMPUTE WS-ACHIEVE-RATE =                        PGM004
016100             (WS-TOTAL-ACTUAL / WS-TOTAL-PLAN) * 100      PGM004
016200     ELSE                                                 PGM004
016300         MOVE ZEROS TO WS-ACHIEVE-RATE                    PGM004
016400     END-IF.                                              PGM004
016500*                                                         PGM004
016600 4000-UPDATE-DB2-SUMMARY.                                 PGM004
016700     EXEC SQL                                             PGM004
016800         INSERT INTO TB_PROD_PLAN_RESULT                  PGM004
016900         (PLAN_MONTH, TOTAL_PLAN, TOTAL_ACTUAL,           PGM004
017000          ACHIEVE_RATE, ACHIEVE_CNT, SHORT_CNT,           PGM004
017100          OVER_CNT)                                       PGM004
017200         VALUES                                           PGM004
017300         (:PP-YYYYMM, :WS-TOTAL-PLAN,                    PGM004
017400          :WS-TOTAL-ACTUAL, :WS-ACHIEVE-RATE,             PGM004
017500          :WS-ACHIEVE-COUNT, :WS-SHORT-COUNT,             PGM004
017600          :WS-OVER-COUNT)                                 PGM004
017700     END-EXEC                                             PGM004
017800     IF SQLCODE NOT = 0                                   PGM004
017900         CALL 'SQLERR' USING SQLCODE                      PGM004
018000     END-IF.                                              PGM004
018100*                                                         PGM004
018200 5000-NOTIFY-SHORTFALL.                                   PGM004
018300     IF WS-ACHIEVE-RATE < WS-ACHIEVE-THRESHOLD            PGM004
018400         DISPLAY 'SHORTFALL ALERT: RATE='                 PGM004
018500                 WS-ACHIEVE-RATE '%'                      PGM004
018600         CALL 'PRDNOTI' USING WS-ACHIEVE-RATE             PGM004
018700                              WS-SHORT-COUNT              PGM004
018800     END-IF.                                              PGM004
018900*                                                         PGM004
019000 9000-FINALIZE.                                           PGM004
019100     CLOSE PROD-PLAN-FILE                                 PGM004
019200     CLOSE PROD-ACTUAL-FILE                               PGM004
019300     CLOSE PLAN-RESULT-FILE                               PGM004
019400     DISPLAY 'PGM004 COMPLETED: '                         PGM004
019500             WS-PLAN-COUNT ' ITEMS, RATE='                PGM004
019600             WS-ACHIEVE-RATE '%'                          PGM004
019700             ' SHORT:' WS-SHORT-COUNT                     PGM004
019800             ' OVER:' WS-OVER-COUNT.                      PGM004
019900*                                                         PGM004
020000 9900-ABNORMAL-END.                                       PGM004
020100     DISPLAY 'PGM004 ABEND - STATUS: '                    PGM004
020200             WS-FILE-STATUS                               PGM004
020300     CALL 'ABNDPGM' USING WS-FILE-STATUS                 PGM004
020400     STOP RUN.                                            PGM004
