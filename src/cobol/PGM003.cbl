000100 IDENTIFICATION DIVISION.                                 PGM003
000200 PROGRAM-ID.    PGM003.                                   PGM003
000300 AUTHOR.        LEGACY-DEVELOPER.                         PGM003
000400*                                                         PGM003
000500* =========================================================PGM003
000600* 프로그램명: 품질검사 실시간 처리                         PGM003
000700* 처리내용  : 생산라인에서 발생하는 품질검사 결과를        PGM003
000800*            실시간으로 수신하여 불량 판정 및              PGM003
000900*            품질이력을 관리한다.                          PGM003
001000* =========================================================PGM003
001100                                                          PGM003
001200 ENVIRONMENT DIVISION.                                    PGM003
001300 CONFIGURATION SECTION.                                   PGM003
001400 INPUT-OUTPUT SECTION.                                    PGM003
001500 FILE-CONTROL.                                            PGM003
001600     SELECT QC-RESULT-FILE                                PGM003
001700         ASSIGN TO QCRSLT                                 PGM003
001800         ORGANIZATION IS INDEXED                          PGM003
001900         ACCESS MODE IS DYNAMIC                           PGM003
002000         RECORD KEY IS QR-KEY                             PGM003
002100         FILE STATUS IS WS-FILE-STATUS.                   PGM003
002200     SELECT QC-HISTORY-FILE                               PGM003
002300         ASSIGN TO QCHIST                                 PGM003
002400         ORGANIZATION IS SEQUENTIAL                       PGM003
002500         FILE STATUS IS WS-FILE-STATUS2.                  PGM003
002600                                                          PGM003
002700 DATA DIVISION.                                           PGM003
002800 FILE SECTION.                                            PGM003
002900 FD  QC-RESULT-FILE.                                      PGM003
003000 01  QC-RESULT-REC.                                       PGM003
003100     COPY CPYQCRS.                                        PGM003
003200 FD  QC-HISTORY-FILE.                                     PGM003
003300 01  QC-HISTORY-REC.                                      PGM003
003400     COPY CPYQCHS.                                        PGM003
003500                                                          PGM003
003600 WORKING-STORAGE SECTION.                                 PGM003
003700 01  WS-FILE-STATUS         PIC XX.                       PGM003
003800 01  WS-FILE-STATUS2        PIC XX.                       PGM003
003900 01  WS-EOF-FLAG            PIC X VALUE 'N'.              PGM003
004000     88 WS-EOF              VALUE 'Y'.                    PGM003
004100 01  WS-INSPECT-COUNT       PIC 9(7) VALUE ZEROS.         PGM003
004200 01  WS-DEFECT-COUNT        PIC 9(5) VALUE ZEROS.         PGM003
004300 01  WS-PASS-COUNT          PIC 9(7) VALUE ZEROS.         PGM003
004400 01  WS-DEFECT-RATE         PIC 9(3)V99 VALUE ZEROS.      PGM003
004500 01  WS-CRITICAL-LIMIT      PIC 9(3)V99 VALUE 5.00.       PGM003
004600 01  WS-WARNING-LIMIT       PIC 9(3)V99 VALUE 2.50.       PGM003
004700 01  WS-CURRENT-LOT         PIC X(15).                    PGM003
004800 01  WS-PREV-RESULT         PIC X(02).                    PGM003
004900                                                          PGM003
005000     EXEC SQL INCLUDE SQLCA END-EXEC.                     PGM003
005100     EXEC SQL INCLUDE DCLTBQC END-EXEC.                   PGM003
005200                                                          PGM003
005300 PROCEDURE DIVISION.                                      PGM003
005400*                                                         PGM003
005500 0000-MAIN-PROCESS.                                       PGM003
005600     PERFORM 1000-INITIALIZE                              PGM003
005700     PERFORM 2000-PROCESS-INSPECTION                      PGM003
005800         UNTIL WS-EOF                                     PGM003
005900     PERFORM 3000-CALCULATE-RATE                          PGM003
006000     PERFORM 4000-UPDATE-QC-SUMMARY                       PGM003
006100     PERFORM 5000-CHECK-ALERT                             PGM003
006200     PERFORM 9000-FINALIZE                                PGM003
006300     STOP RUN.                                            PGM003
006400*                                                         PGM003
006500 1000-INITIALIZE.                                         PGM003
006600     OPEN INPUT  QC-RESULT-FILE                           PGM003
006700     OPEN OUTPUT QC-HISTORY-FILE                          PGM003
006800     IF WS-FILE-STATUS NOT = '00'                         PGM003
006900         DISPLAY 'OPEN ERROR: ' WS-FILE-STATUS            PGM003
007000         PERFORM 9900-ABNORMAL-END                        PGM003
007100     END-IF                                               PGM003
007200     IF WS-FILE-STATUS2 NOT = '00'                        PGM003
007300         DISPLAY 'HISTORY FILE OPEN ERROR: '              PGM003
007400                 WS-FILE-STATUS2                          PGM003
007500         PERFORM 9900-ABNORMAL-END                        PGM003
007600     END-IF                                               PGM003
007700     PERFORM 1100-READ-FIRST-RECORD.                      PGM003
007800*                                                         PGM003
007900 1100-READ-FIRST-RECORD.                                  PGM003
008000     READ QC-RESULT-FILE                                  PGM003
008100     AT END SET WS-EOF TO TRUE                            PGM003
008200     END-READ.                                            PGM003
008300*                                                         PGM003
008400 2000-PROCESS-INSPECTION.                                 PGM003
008500     ADD 1 TO WS-INSPECT-COUNT                            PGM003
008600     EVALUATE QR-JUDGE-CD                                 PGM003
008700         WHEN 'P'                                         PGM003
008800             ADD 1 TO WS-PASS-COUNT                       PGM003
008900             PERFORM 2100-RECORD-PASS                     PGM003
009000         WHEN 'F'                                         PGM003
009100             ADD 1 TO WS-DEFECT-COUNT                     PGM003
009200             PERFORM 2200-RECORD-DEFECT                   PGM003
009300         WHEN 'R'                                         PGM003
009400             PERFORM 2300-RECORD-REWORK                   PGM003
009500         WHEN OTHER                                       PGM003
009600             CALL 'ERRLOG' USING QR-INSPECT-ID            PGM003
009700                                 QR-JUDGE-CD              PGM003
009800     END-EVALUATE                                         PGM003
009900     PERFORM 2400-WRITE-HISTORY                           PGM003
010000     READ QC-RESULT-FILE                                  PGM003
010100     AT END SET WS-EOF TO TRUE                            PGM003
010200     END-READ.                                            PGM003
010300*                                                         PGM003
010400 2100-RECORD-PASS.                                        PGM003
010500     MOVE 'OK' TO WS-PREV-RESULT.                        PGM003
010600*                                                         PGM003
010700 2200-RECORD-DEFECT.                                      PGM003
010800     MOVE 'NG' TO WS-PREV-RESULT                         PGM003
010900     IF QR-DEFECT-TYPE = 'A'                              PGM003
011000         CALL 'QCALERT' USING QR-LINE-CD                 PGM003
011100                              QR-PRODUCT-CD               PGM003
011200                              QR-DEFECT-TYPE              PGM003
011300     END-IF.                                              PGM003
011400*                                                         PGM003
011500 2300-RECORD-REWORK.                                      PGM003
011600     MOVE 'RW' TO WS-PREV-RESULT                         PGM003
011700     IF QR-REWORK-CNT > 3                                 PGM003
011800         MOVE 'F' TO QR-JUDGE-CD                          PGM003
011900         ADD 1 TO WS-DEFECT-COUNT                         PGM003
012000         CALL 'ERRLOG' USING QR-INSPECT-ID                PGM003
012100                             QR-REWORK-CNT                PGM003
012200     END-IF.                                              PGM003
012300*                                                         PGM003
012400 2400-WRITE-HISTORY.                                      PGM003
012500     MOVE QR-INSPECT-ID  TO QH-INSPECT-ID                PGM003
012600     MOVE QR-LINE-CD     TO QH-LINE-CD                   PGM003
012700     MOVE QR-PRODUCT-CD  TO QH-PRODUCT-CD                PGM003
012800     MOVE QR-JUDGE-CD    TO QH-JUDGE-CD                  PGM003
012900     MOVE QR-INSPECT-DT  TO QH-INSPECT-DT               PGM003
013000     MOVE WS-PREV-RESULT TO QH-FINAL-RESULT              PGM003
013100     WRITE QC-HISTORY-REC.                                PGM003
013200*                                                         PGM003
013300 3000-CALCULATE-RATE.                                     PGM003
013400     IF WS-INSPECT-COUNT > 0                              PGM003
013500         COMPUTE WS-DEFECT-RATE =                         PGM003
013600             (WS-DEFECT-COUNT / WS-INSPECT-COUNT) * 100   PGM003
013700     ELSE                                                 PGM003
013800         MOVE ZEROS TO WS-DEFECT-RATE                     PGM003
013900     END-IF.                                              PGM003
014000*                                                         PGM003
014100 4000-UPDATE-QC-SUMMARY.                                  PGM003
014200     EXEC SQL                                             PGM003
014300         INSERT INTO TB_QC_DAILY_SUMMARY                  PGM003
014400         (INSPECT_DATE, LINE_CD, TOTAL_CNT,               PGM003
014500          PASS_CNT, DEFECT_CNT, DEFECT_RATE)              PGM003
014600         VALUES                                           PGM003
014700         (CURRENT DATE, :QR-LINE-CD,                      PGM003
014800          :WS-INSPECT-COUNT, :WS-PASS-COUNT,              PGM003
014900          :WS-DEFECT-COUNT, :WS-DEFECT-RATE)              PGM003
015000     END-EXEC                                             PGM003
015100     IF SQLCODE NOT = 0                                   PGM003
015200         CALL 'SQLERR' USING SQLCODE                      PGM003
015300     END-IF.                                              PGM003
015400*                                                         PGM003
015500 5000-CHECK-ALERT.                                        PGM003
015600     IF WS-DEFECT-RATE > WS-CRITICAL-LIMIT                PGM003
015700         DISPLAY 'CRITICAL: DEFECT RATE ' WS-DEFECT-RATE  PGM003
015800         CALL 'QCALERT' USING QR-LINE-CD                 PGM003
015900                              QR-PRODUCT-CD               PGM003
016000                              WS-DEFECT-RATE              PGM003
016100     ELSE                                                 PGM003
016200     IF WS-DEFECT-RATE > WS-WARNING-LIMIT                 PGM003
016300         DISPLAY 'WARNING: DEFECT RATE ' WS-DEFECT-RATE   PGM003
016400     END-IF                                               PGM003
016500     END-IF.                                              PGM003
016600*                                                         PGM003
016700 9000-FINALIZE.                                           PGM003
016800     CLOSE QC-RESULT-FILE                                 PGM003
016900     CLOSE QC-HISTORY-FILE                                PGM003
017000     DISPLAY 'PGM003 COMPLETED: '                         PGM003
017100             WS-INSPECT-COUNT ' INSPECTED, '              PGM003
017200             WS-DEFECT-COUNT  ' DEFECTS'.                 PGM003
017300*                                                         PGM003
017400 9900-ABNORMAL-END.                                       PGM003
017500     DISPLAY 'PGM003 ABEND - FILE STATUS: '              PGM003
017600             WS-FILE-STATUS                               PGM003
017700     CALL 'ABNDPGM' USING WS-FILE-STATUS                 PGM003
017800     STOP RUN.                                            PGM003
