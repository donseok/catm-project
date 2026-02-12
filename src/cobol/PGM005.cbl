000100 IDENTIFICATION DIVISION.                                 PGM005
000200 PROGRAM-ID.    PGM005.                                   PGM005
000300 AUTHOR.        DONGKUK-SYSTEMS.                          PGM005
000400*                                                         PGM005
000500* =========================================================PGM005
000600* 프로그램명: 고로 조업일보 처리                           PGM005
000700* 처리내용  : 고로(용광로)의 일일 조업 데이터를 수집하여   PGM005
000800*            온도/압력/출선량 등을 분석하고                PGM005
000900*            이상치 발생 시 경보를 발행한다.               PGM005
001000* =========================================================PGM005
001100                                                          PGM005
001200 ENVIRONMENT DIVISION.                                    PGM005
001300 CONFIGURATION SECTION.                                   PGM005
001400 INPUT-OUTPUT SECTION.                                    PGM005
001500 FILE-CONTROL.                                            PGM005
001600     SELECT BF-OPER-FILE                                  PGM005
001700         ASSIGN TO BFOPER                                 PGM005
001800         ORGANIZATION IS INDEXED                          PGM005
001900         ACCESS MODE IS SEQUENTIAL                        PGM005
002000         RECORD KEY IS BO-KEY                             PGM005
002100         FILE STATUS IS WS-FILE-STATUS.                   PGM005
002200     SELECT BF-TEMP-FILE                                  PGM005
002300         ASSIGN TO BFTEMP                                 PGM005
002400         ORGANIZATION IS INDEXED                          PGM005
002500         ACCESS MODE IS RANDOM                            PGM005
002600         RECORD KEY IS BT-KEY                             PGM005
002700         FILE STATUS IS WS-FILE-STATUS2.                  PGM005
002800     SELECT DAILY-REPORT-FILE                             PGM005
002900         ASSIGN TO BFDAILY                                PGM005
003000         ORGANIZATION IS SEQUENTIAL                       PGM005
003100         FILE STATUS IS WS-FILE-STATUS3.                  PGM005
003200                                                          PGM005
003300 DATA DIVISION.                                           PGM005
003400 FILE SECTION.                                            PGM005
003500 FD  BF-OPER-FILE.                                        PGM005
003600 01  BF-OPER-REC.                                         PGM005
003700     COPY CPYBFOP.                                        PGM005
003800 FD  BF-TEMP-FILE.                                        PGM005
003900 01  BF-TEMP-REC.                                         PGM005
004000     COPY CPYBFTM.                                        PGM005
004100 FD  DAILY-REPORT-FILE.                                   PGM005
004200 01  DAILY-REPORT-REC.                                    PGM005
004300     05 DR-BF-NO             PIC X(03).                   PGM005
004400     05 DR-OPER-DATE         PIC 9(08).                   PGM005
004500     05 DR-AVG-TEMP          PIC S9(5)V9 COMP-3.          PGM005
004600     05 DR-MAX-TEMP          PIC S9(5)V9 COMP-3.          PGM005
004700     05 DR-MIN-TEMP          PIC S9(5)V9 COMP-3.          PGM005
004800     05 DR-AVG-PRESS         PIC S9(3)V99 COMP-3.         PGM005
004900     05 DR-TAPPING-QTY       PIC S9(9)V99 COMP-3.         PGM005
005000     05 DR-COKE-RATE         PIC 9(3)V99.                 PGM005
005100     05 DR-STATUS-CD         PIC X(02).                   PGM005
005200     05 DR-ALERT-CNT         PIC 9(03).                   PGM005
005300     05 FILLER               PIC X(20).                   PGM005
005400                                                          PGM005
005500 WORKING-STORAGE SECTION.                                 PGM005
005600 01  WS-FILE-STATUS          PIC XX.                      PGM005
005700 01  WS-FILE-STATUS2         PIC XX.                      PGM005
005800 01  WS-FILE-STATUS3         PIC XX.                      PGM005
005900 01  WS-EOF-FLAG             PIC X VALUE 'N'.             PGM005
006000     88 WS-EOF               VALUE 'Y'.                   PGM005
006100 01  WS-READ-COUNT           PIC 9(7) VALUE ZEROS.        PGM005
006200 01  WS-ALERT-COUNT          PIC 9(5) VALUE ZEROS.        PGM005
006300 01  WS-ERROR-COUNT          PIC 9(5) VALUE ZEROS.        PGM005
006400 01  WS-TEMP-SUM             PIC S9(9)V9 VALUE ZEROS.     PGM005
006500 01  WS-TEMP-MAX             PIC S9(5)V9 VALUE ZEROS.     PGM005
006600 01  WS-TEMP-MIN             PIC S9(5)V9 VALUE 9999.9.    PGM005
006700 01  WS-TEMP-AVG             PIC S9(5)V9 VALUE ZEROS.     PGM005
006800 01  WS-PRESS-SUM            PIC S9(7)V99 VALUE ZEROS.    PGM005
006900 01  WS-TAPPING-TOTAL        PIC S9(11)V99 VALUE ZEROS.   PGM005
007000 01  WS-TEMP-HIGH-LIMIT      PIC S9(5)V9 VALUE 1650.0.   PGM005
007100 01  WS-TEMP-LOW-LIMIT       PIC S9(5)V9 VALUE 1400.0.   PGM005
007200 01  WS-PRESS-HIGH-LIMIT     PIC S9(3)V99 VALUE 3.50.    PGM005
007300 01  WS-CURRENT-BF           PIC X(03).                   PGM005
007400                                                          PGM005
007500     EXEC SQL INCLUDE SQLCA END-EXEC.                     PGM005
007600     EXEC SQL INCLUDE DCLTBBFOP END-EXEC.                 PGM005
007700                                                          PGM005
007800 PROCEDURE DIVISION.                                      PGM005
007900*                                                         PGM005
008000 0000-MAIN-PROCESS.                                       PGM005
008100     PERFORM 1000-INITIALIZE                              PGM005
008200     PERFORM 2000-PROCESS-OPER-DATA                       PGM005
008300         UNTIL WS-EOF                                     PGM005
008400     PERFORM 3000-CALC-DAILY-STATS                        PGM005
008500     PERFORM 4000-CHECK-ABNORMAL                          PGM005
008600     PERFORM 5000-WRITE-DAILY-REPORT                      PGM005
008700     PERFORM 6000-UPDATE-DB2                              PGM005
008800     PERFORM 9000-FINALIZE                                PGM005
008900     STOP RUN.                                            PGM005
009000*                                                         PGM005
009100 1000-INITIALIZE.                                         PGM005
009200     OPEN INPUT  BF-OPER-FILE                             PGM005
009300     OPEN INPUT  BF-TEMP-FILE                             PGM005
009400     OPEN OUTPUT DAILY-REPORT-FILE                        PGM005
009500     IF WS-FILE-STATUS NOT = '00'                         PGM005
009600         DISPLAY 'OPER FILE OPEN ERROR: '                 PGM005
009700                 WS-FILE-STATUS                           PGM005
009800         PERFORM 9900-ABNORMAL-END                        PGM005
009900     END-IF                                               PGM005
010000     IF WS-FILE-STATUS2 NOT = '00'                        PGM005
010100         DISPLAY 'TEMP FILE OPEN ERROR: '                 PGM005
010200                 WS-FILE-STATUS2                          PGM005
010300         PERFORM 9900-ABNORMAL-END                        PGM005
010400     END-IF                                               PGM005
010500     PERFORM 1100-READ-FIRST-RECORD.                      PGM005
010600*                                                         PGM005
010700 1100-READ-FIRST-RECORD.                                  PGM005
010800     READ BF-OPER-FILE                                    PGM005
010900     AT END SET WS-EOF TO TRUE                            PGM005
011000     END-READ.                                            PGM005
011100*                                                         PGM005
011200 2000-PROCESS-OPER-DATA.                                  PGM005
011300     ADD 1 TO WS-READ-COUNT                               PGM005
011400     MOVE BO-BF-NO TO WS-CURRENT-BF                       PGM005
011500     PERFORM 2100-GET-TEMPERATURE                         PGM005
011600     PERFORM 2200-ACCUMULATE-DATA                         PGM005
011700     IF BO-OPER-TYPE = 'T'                                PGM005
011800         PERFORM 2300-PROCESS-TAPPING                     PGM005
011900     END-IF                                               PGM005
012000     READ BF-OPER-FILE                                    PGM005
012100     AT END SET WS-EOF TO TRUE                            PGM005
012200     END-READ.                                            PGM005
012300*                                                         PGM005
012400 2100-GET-TEMPERATURE.                                    PGM005
012500     MOVE BO-BF-NO     TO BT-BF-NO                       PGM005
012600     MOVE BO-OPER-DATE TO BT-MEASURE-DT                  PGM005
012700     MOVE BO-OPER-SEQ  TO BT-MEASURE-SEQ                 PGM005
012800     READ BF-TEMP-FILE                                    PGM005
012900         INVALID KEY                                      PGM005
013000             ADD 1 TO WS-ERROR-COUNT                      PGM005
013100             CALL 'ERRLOG' USING BO-BF-NO BO-OPER-DATE   PGM005
013200         NOT INVALID KEY                                  PGM005
013300             PERFORM 2110-CHECK-TEMP-RANGE                PGM005
013400     END-READ.                                            PGM005
013500*                                                         PGM005
013600 2110-CHECK-TEMP-RANGE.                                   PGM005
013700     ADD BT-TEMP-VAL TO WS-TEMP-SUM                       PGM005
013800     IF BT-TEMP-VAL > WS-TEMP-MAX                         PGM005
013900         MOVE BT-TEMP-VAL TO WS-TEMP-MAX                  PGM005
014000     END-IF                                               PGM005
014100     IF BT-TEMP-VAL < WS-TEMP-MIN                         PGM005
014200         MOVE BT-TEMP-VAL TO WS-TEMP-MIN                  PGM005
014300     END-IF                                               PGM005
014400     IF BT-TEMP-VAL > WS-TEMP-HIGH-LIMIT                  PGM005
014500         ADD 1 TO WS-ALERT-COUNT                          PGM005
014600         CALL 'BFALERT' USING BO-BF-NO                    PGM005
014700                              BT-TEMP-VAL                 PGM005
014800                              WS-TEMP-HIGH-LIMIT          PGM005
014900     END-IF                                               PGM005
015000     IF BT-TEMP-VAL < WS-TEMP-LOW-LIMIT                   PGM005
015100         ADD 1 TO WS-ALERT-COUNT                          PGM005
015200         CALL 'BFALERT' USING BO-BF-NO                    PGM005
015300                              BT-TEMP-VAL                 PGM005
015400                              WS-TEMP-LOW-LIMIT           PGM005
015500     END-IF.                                              PGM005
015600*                                                         PGM005
015700 2200-ACCUMULATE-DATA.                                    PGM005
015800     ADD BO-PRESSURE TO WS-PRESS-SUM                      PGM005
015900     IF BO-PRESSURE > WS-PRESS-HIGH-LIMIT                 PGM005
016000         ADD 1 TO WS-ALERT-COUNT                          PGM005
016100         CALL 'BFALERT' USING BO-BF-NO                    PGM005
016200                              BO-PRESSURE                 PGM005
016300                              WS-PRESS-HIGH-LIMIT         PGM005
016400     END-IF.                                              PGM005
016500*                                                         PGM005
016600 2300-PROCESS-TAPPING.                                    PGM005
016700     ADD BO-TAP-QTY TO WS-TAPPING-TOTAL.                  PGM005
016800*                                                         PGM005
016900 3000-CALC-DAILY-STATS.                                   PGM005
017000     IF WS-READ-COUNT > 0                                 PGM005
017100         COMPUTE WS-TEMP-AVG =                            PGM005
017200             WS-TEMP-SUM / WS-READ-COUNT                  PGM005
017300     END-IF.                                              PGM005
017400*                                                         PGM005
017500 4000-CHECK-ABNORMAL.                                     PGM005
017600     IF WS-ALERT-COUNT > 10                               PGM005
017700         DISPLAY 'CRITICAL: BF=' WS-CURRENT-BF            PGM005
017800                 ' ALERTS=' WS-ALERT-COUNT                PGM005
017900         CALL 'BFALERT' USING WS-CURRENT-BF              PGM005
018000                              WS-ALERT-COUNT              PGM005
018100                              WS-TEMP-MAX                 PGM005
018200     END-IF.                                              PGM005
018300*                                                         PGM005
018400 5000-WRITE-DAILY-REPORT.                                 PGM005
018500     MOVE WS-CURRENT-BF  TO DR-BF-NO                      PGM005
018600     MOVE BO-OPER-DATE   TO DR-OPER-DATE                  PGM005
018700     MOVE WS-TEMP-AVG    TO DR-AVG-TEMP                   PGM005
018800     MOVE WS-TEMP-MAX    TO DR-MAX-TEMP                   PGM005
018900     MOVE WS-TEMP-MIN    TO DR-MIN-TEMP                   PGM005
019000     IF WS-READ-COUNT > 0                                 PGM005
019100         COMPUTE DR-AVG-PRESS =                           PGM005
019200             WS-PRESS-SUM / WS-READ-COUNT                 PGM005
019300     END-IF                                               PGM005
019400     MOVE WS-TAPPING-TOTAL TO DR-TAPPING-QTY              PGM005
019500     MOVE WS-ALERT-COUNT   TO DR-ALERT-CNT                PGM005
019600     IF WS-ALERT-COUNT > 10                               PGM005
019700         MOVE 'CR' TO DR-STATUS-CD                        PGM005
019800     ELSE IF WS-ALERT-COUNT > 0                           PGM005
019900         MOVE 'WN' TO DR-STATUS-CD                        PGM005
020000     ELSE                                                 PGM005
020100         MOVE 'OK' TO DR-STATUS-CD                        PGM005
020200     END-IF                                               PGM005
020300     END-IF                                               PGM005
020400     WRITE DAILY-REPORT-REC.                              PGM005
020500*                                                         PGM005
020600 6000-UPDATE-DB2.                                         PGM005
020700     EXEC SQL                                             PGM005
020800         INSERT INTO TB_BF_DAILY_OPER                     PGM005
020900         (BF_NO, OPER_DATE, AVG_TEMP, MAX_TEMP,           PGM005
021000          MIN_TEMP, TAPPING_QTY, ALERT_CNT,               PGM005
021100          STATUS_CD)                                      PGM005
021200         VALUES                                           PGM005
021300         (:WS-CURRENT-BF, :BO-OPER-DATE,                  PGM005
021400          :WS-TEMP-AVG, :WS-TEMP-MAX,                     PGM005
021500          :WS-TEMP-MIN, :WS-TAPPING-TOTAL,                PGM005
021600          :WS-ALERT-COUNT, :DR-STATUS-CD)                  PGM005
021700     END-EXEC                                             PGM005
021800     IF SQLCODE NOT = 0                                   PGM005
021900         CALL 'SQLERR' USING SQLCODE                      PGM005
022000     END-IF.                                              PGM005
022100*                                                         PGM005
022200 9000-FINALIZE.                                           PGM005
022300     CLOSE BF-OPER-FILE                                   PGM005
022400     CLOSE BF-TEMP-FILE                                   PGM005
022500     CLOSE DAILY-REPORT-FILE                              PGM005
022600     DISPLAY 'PGM005 COMPLETED: BF=' WS-CURRENT-BF        PGM005
022700             ' RECORDS=' WS-READ-COUNT                    PGM005
022800             ' ALERTS=' WS-ALERT-COUNT.                   PGM005
022900*                                                         PGM005
023000 9900-ABNORMAL-END.                                       PGM005
023100     DISPLAY 'PGM005 ABEND - STATUS: '                    PGM005
023200             WS-FILE-STATUS                               PGM005
023300     CALL 'ABNDPGM' USING WS-FILE-STATUS                 PGM005
023400     STOP RUN.                                            PGM005
