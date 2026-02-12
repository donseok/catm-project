000100 IDENTIFICATION DIVISION.                                 PGM006
000200 PROGRAM-ID.    PGM006.                                   PGM006
000300 AUTHOR.        LEGACY-DEVELOPER.                         PGM006
000400*                                                         PGM006
000500* =========================================================PGM006
000600* 프로그램명: 원자재 입고 및 검수 처리                     PGM006
000700* 처리내용  : 원자재 입고 데이터를 처리하여                PGM006
000800*            검수 결과에 따라 재고를 반영하고              PGM006
000900*            불합격 자재는 반품 처리한다.                  PGM006
001000* =========================================================PGM006
001100                                                          PGM006
001200 ENVIRONMENT DIVISION.                                    PGM006
001300 CONFIGURATION SECTION.                                   PGM006
001400 INPUT-OUTPUT SECTION.                                    PGM006
001500 FILE-CONTROL.                                            PGM006
001600     SELECT RM-RECEIPT-FILE                               PGM006
001700         ASSIGN TO RMRCPT                                 PGM006
001800         ORGANIZATION IS INDEXED                          PGM006
001900         ACCESS MODE IS SEQUENTIAL                        PGM006
002000         RECORD KEY IS RR-KEY                             PGM006
002100         FILE STATUS IS WS-FILE-STATUS.                   PGM006
002200     SELECT RM-MASTER-FILE                                PGM006
002300         ASSIGN TO RMMAST                                 PGM006
002400         ORGANIZATION IS INDEXED                          PGM006
002500         ACCESS MODE IS RANDOM                            PGM006
002600         RECORD KEY IS RM-MATL-CD                         PGM006
002700         FILE STATUS IS WS-FILE-STATUS2.                  PGM006
002800     SELECT RETURN-FILE                                   PGM006
002900         ASSIGN TO RMRETN                                 PGM006
003000         ORGANIZATION IS SEQUENTIAL                       PGM006
003100         FILE STATUS IS WS-FILE-STATUS3.                  PGM006
003200                                                          PGM006
003300 DATA DIVISION.                                           PGM006
003400 FILE SECTION.                                            PGM006
003500 FD  RM-RECEIPT-FILE.                                     PGM006
003600 01  RM-RECEIPT-REC.                                      PGM006
003700     COPY CPYRMRC.                                        PGM006
003800 FD  RM-MASTER-FILE.                                      PGM006
003900 01  RM-MASTER-REC.                                       PGM006
004000     COPY CPYRMMS.                                        PGM006
004100 FD  RETURN-FILE.                                         PGM006
004200 01  RETURN-REC.                                          PGM006
004300     05 RT-MATL-CD           PIC X(12).                   PGM006
004400     05 RT-RECEIPT-NO        PIC 9(10).                   PGM006
004500     05 RT-RETURN-QTY        PIC S9(9)V99 COMP-3.         PGM006
004600     05 RT-REASON-CD         PIC X(03).                   PGM006
004700     05 RT-VENDOR-CD         PIC X(10).                   PGM006
004800     05 RT-RETURN-DATE       PIC 9(08).                   PGM006
004900     05 FILLER               PIC X(20).                   PGM006
005000                                                          PGM006
005100 WORKING-STORAGE SECTION.                                 PGM006
005200 01  WS-FILE-STATUS          PIC XX.                      PGM006
005300 01  WS-FILE-STATUS2         PIC XX.                      PGM006
005400 01  WS-FILE-STATUS3         PIC XX.                      PGM006
005500 01  WS-EOF-FLAG             PIC X VALUE 'N'.             PGM006
005600     88 WS-EOF               VALUE 'Y'.                   PGM006
005700 01  WS-RECEIPT-COUNT        PIC 9(7) VALUE ZEROS.        PGM006
005800 01  WS-ACCEPT-COUNT         PIC 9(7) VALUE ZEROS.        PGM006
005900 01  WS-REJECT-COUNT         PIC 9(5) VALUE ZEROS.        PGM006
006000 01  WS-PARTIAL-COUNT        PIC 9(5) VALUE ZEROS.        PGM006
006100 01  WS-ERROR-COUNT          PIC 9(5) VALUE ZEROS.        PGM006
006200 01  WS-TOTAL-RECEIPT-AMT    PIC S9(11)V99 VALUE ZEROS.   PGM006
006300 01  WS-TOTAL-ACCEPT-AMT     PIC S9(11)V99 VALUE ZEROS.   PGM006
006400 01  WS-TOTAL-RETURN-AMT     PIC S9(11)V99 VALUE ZEROS.   PGM006
006500 01  WS-UNIT-PRICE           PIC S9(9)V99 VALUE ZEROS.    PGM006
006600 01  WS-ACCEPT-QTY           PIC S9(9)V99 VALUE ZEROS.    PGM006
006700 01  WS-REJECT-QTY           PIC S9(9)V99 VALUE ZEROS.    PGM006
006800                                                          PGM006
006900     EXEC SQL INCLUDE SQLCA END-EXEC.                     PGM006
007000     EXEC SQL INCLUDE DCLTBRM END-EXEC.                   PGM006
007100                                                          PGM006
007200 PROCEDURE DIVISION.                                      PGM006
007300*                                                         PGM006
007400 0000-MAIN-PROCESS.                                       PGM006
007500     PERFORM 1000-INITIALIZE                              PGM006
007600     PERFORM 2000-PROCESS-RECEIPT                         PGM006
007700         UNTIL WS-EOF                                     PGM006
007800     PERFORM 3000-UPDATE-SUMMARY                          PGM006
007900     PERFORM 9000-FINALIZE                                PGM006
008000     STOP RUN.                                            PGM006
008100*                                                         PGM006
008200 1000-INITIALIZE.                                         PGM006
008300     OPEN INPUT  RM-RECEIPT-FILE                           PGM006
008400     OPEN I-O    RM-MASTER-FILE                            PGM006
008500     OPEN OUTPUT RETURN-FILE                              PGM006
008600     IF WS-FILE-STATUS NOT = '00'                         PGM006
008700         DISPLAY 'RECEIPT FILE OPEN ERROR: '              PGM006
008800                 WS-FILE-STATUS                           PGM006
008900         PERFORM 9900-ABNORMAL-END                        PGM006
009000     END-IF                                               PGM006
009100     IF WS-FILE-STATUS2 NOT = '00'                        PGM006
009200         DISPLAY 'MASTER FILE OPEN ERROR: '               PGM006
009300                 WS-FILE-STATUS2                          PGM006
009400         PERFORM 9900-ABNORMAL-END                        PGM006
009500     END-IF                                               PGM006
009600     PERFORM 1100-READ-FIRST-RECORD.                      PGM006
009700*                                                         PGM006
009800 1100-READ-FIRST-RECORD.                                  PGM006
009900     READ RM-RECEIPT-FILE                                 PGM006
010000     AT END SET WS-EOF TO TRUE                            PGM006
010100     END-READ.                                            PGM006
010200*                                                         PGM006
010300 2000-PROCESS-RECEIPT.                                    PGM006
010400     ADD 1 TO WS-RECEIPT-COUNT                            PGM006
010500     MOVE RR-MATL-CD TO RM-MATL-CD                        PGM006
010600     READ RM-MASTER-FILE                                  PGM006
010700         INVALID KEY                                      PGM006
010800             CALL 'ERRLOG' USING RR-MATL-CD               PGM006
010900                                 RR-RECEIPT-NO            PGM006
011000             ADD 1 TO WS-ERROR-COUNT                      PGM006
011100         NOT INVALID KEY                                  PGM006
011200             PERFORM 2100-INSPECT-MATERIAL                PGM006
011300     END-READ                                             PGM006
011400     READ RM-RECEIPT-FILE                                 PGM006
011500     AT END SET WS-EOF TO TRUE                            PGM006
011600     END-READ.                                            PGM006
011700*                                                         PGM006
011800 2100-INSPECT-MATERIAL.                                   PGM006
011900     MOVE RM-UNIT-PRICE TO WS-UNIT-PRICE                  PGM006
012000     EVALUATE RR-INSPECT-CD                               PGM006
012100         WHEN 'A'                                         PGM006
012200             PERFORM 2200-ACCEPT-FULL                     PGM006
012300         WHEN 'P'                                         PGM006
012400             PERFORM 2300-ACCEPT-PARTIAL                  PGM006
012500         WHEN 'R'                                         PGM006
012600             PERFORM 2400-REJECT-MATERIAL                 PGM006
012700         WHEN OTHER                                       PGM006
012800             CALL 'ERRLOG' USING RR-MATL-CD               PGM006
012900                                 RR-INSPECT-CD            PGM006
013000             ADD 1 TO WS-ERROR-COUNT                      PGM006
013100     END-EVALUATE.                                        PGM006
013200*                                                         PGM006
013300 2200-ACCEPT-FULL.                                        PGM006
013400     ADD RR-RECEIPT-QTY TO RM-STOCK-QTY                   PGM006
013500     MOVE RR-RECEIPT-DT TO RM-LAST-IN-DT                  PGM006
013600     COMPUTE WS-ACCEPT-QTY = RR-RECEIPT-QTY               PGM006
013700     COMPUTE WS-TOTAL-ACCEPT-AMT =                        PGM006
013800         WS-TOTAL-ACCEPT-AMT +                            PGM006
013900         (RR-RECEIPT-QTY * WS-UNIT-PRICE)                 PGM006
014000     COMPUTE WS-TOTAL-RECEIPT-AMT =                       PGM006
014100         WS-TOTAL-RECEIPT-AMT +                           PGM006
014200         (RR-RECEIPT-QTY * WS-UNIT-PRICE)                 PGM006
014300     ADD 1 TO WS-ACCEPT-COUNT                             PGM006
014400     REWRITE RM-MASTER-REC.                               PGM006
014500*                                                         PGM006
014600 2300-ACCEPT-PARTIAL.                                     PGM006
014700     COMPUTE WS-ACCEPT-QTY =                              PGM006
014800         RR-RECEIPT-QTY * RR-ACCEPT-RATE / 100            PGM006
014900     COMPUTE WS-REJECT-QTY =                              PGM006
015000         RR-RECEIPT-QTY - WS-ACCEPT-QTY                   PGM006
015100     ADD WS-ACCEPT-QTY TO RM-STOCK-QTY                    PGM006
015200     MOVE RR-RECEIPT-DT TO RM-LAST-IN-DT                  PGM006
015300     COMPUTE WS-TOTAL-ACCEPT-AMT =                        PGM006
015400         WS-TOTAL-ACCEPT-AMT +                            PGM006
015500         (WS-ACCEPT-QTY * WS-UNIT-PRICE)                 PGM006
015600     COMPUTE WS-TOTAL-RECEIPT-AMT =                       PGM006
015700         WS-TOTAL-RECEIPT-AMT +                           PGM006
015800         (RR-RECEIPT-QTY * WS-UNIT-PRICE)                 PGM006
015900     ADD 1 TO WS-PARTIAL-COUNT                            PGM006
016000     REWRITE RM-MASTER-REC                                PGM006
016100     PERFORM 2410-WRITE-RETURN                            PGM006
016200         WITH WS-REJECT-QTY RR-REASON-CD.                 PGM006
016300*                                                         PGM006
016400 2400-REJECT-MATERIAL.                                    PGM006
016500     MOVE RR-RECEIPT-QTY TO WS-REJECT-QTY                 PGM006
016600     COMPUTE WS-TOTAL-RETURN-AMT =                        PGM006
016700         WS-TOTAL-RETURN-AMT +                            PGM006
016800         (RR-RECEIPT-QTY * WS-UNIT-PRICE)                 PGM006
016900     ADD 1 TO WS-REJECT-COUNT                             PGM006
017000     CALL 'MATRETN' USING RR-MATL-CD                     PGM006
017100                          RR-VENDOR-CD                    PGM006
017200                          RR-RECEIPT-QTY                  PGM006
017300     PERFORM 2410-WRITE-RETURN                            PGM006
017400         WITH RR-RECEIPT-QTY RR-REASON-CD.                PGM006
017500*                                                         PGM006
017600 2410-WRITE-RETURN.                                       PGM006
017700     INITIALIZE RETURN-REC                                PGM006
017800     MOVE RR-MATL-CD     TO RT-MATL-CD                    PGM006
017900     MOVE RR-RECEIPT-NO  TO RT-RECEIPT-NO                 PGM006
018000     MOVE WS-REJECT-QTY  TO RT-RETURN-QTY                 PGM006
018100     MOVE RR-REASON-CD   TO RT-REASON-CD                  PGM006
018200     MOVE RR-VENDOR-CD   TO RT-VENDOR-CD                  PGM006
018300     MOVE RR-RECEIPT-DT  TO RT-RETURN-DATE                PGM006
018400     WRITE RETURN-REC.                                    PGM006
018500*                                                         PGM006
018600 3000-UPDATE-SUMMARY.                                     PGM006
018700     EXEC SQL                                             PGM006
018800         INSERT INTO TB_RM_DAILY_RECEIPT                  PGM006
018900         (RECEIPT_DATE, RECEIPT_CNT, ACCEPT_CNT,          PGM006
019000          REJECT_CNT, PARTIAL_CNT,                        PGM006
019100          TOTAL_AMT, ACCEPT_AMT, RETURN_AMT)              PGM006
019200         VALUES                                           PGM006
019300         (CURRENT DATE,                                   PGM006
019400          :WS-RECEIPT-COUNT, :WS-ACCEPT-COUNT,            PGM006
019500          :WS-REJECT-COUNT, :WS-PARTIAL-COUNT,            PGM006
019600          :WS-TOTAL-RECEIPT-AMT,                          PGM006
019700          :WS-TOTAL-ACCEPT-AMT,                           PGM006
019800          :WS-TOTAL-RETURN-AMT)                           PGM006
019900     END-EXEC                                             PGM006
020000     IF SQLCODE NOT = 0                                   PGM006
020100         CALL 'SQLERR' USING SQLCODE                      PGM006
020200     END-IF.                                              PGM006
020300*                                                         PGM006
020400 9000-FINALIZE.                                           PGM006
020500     CLOSE RM-RECEIPT-FILE                                PGM006
020600     CLOSE RM-MASTER-FILE                                 PGM006
020700     CLOSE RETURN-FILE                                    PGM006
020800     DISPLAY 'PGM006 COMPLETED: '                         PGM006
020900             WS-RECEIPT-COUNT ' RECEIPTS, '               PGM006
021000             WS-ACCEPT-COUNT ' ACCEPTED, '                PGM006
021100             WS-REJECT-COUNT ' REJECTED'.                 PGM006
021200*                                                         PGM006
021300 9900-ABNORMAL-END.                                       PGM006
021400     DISPLAY 'PGM006 ABEND - STATUS: '                    PGM006
021500             WS-FILE-STATUS                               PGM006
021600     CALL 'ABNDPGM' USING WS-FILE-STATUS                 PGM006
021700     STOP RUN.                                            PGM006
