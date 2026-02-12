# 데이터 사전: CPYSMRY

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | DS-SUMMARY-KEY | `` | GROUP |  |  |  |
| 10 | 　DS-PLANT-CD | `X(04)` | 문자열(4) |  |  |  |
| 10 | 　DS-PROD-DATE | `9(08)` | 숫자(8) |  |  |  |
| 05 | DS-SUMMARY-DATA | `` | GROUP |  |  |  |
| 10 | 　DS-TOTAL-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　DS-TOTAL-COUNT | `9(07)` | 숫자(7) |  |  |  |
| 10 | 　DS-ERROR-COUNT | `9(05)` | 숫자(5) |  |  |  |
| 10 | 　DS-PROCESS-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　DS-STATUS-CD | `X(02)` | 문자열(2) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　DS-NORMAL | `` | GROUP |  |  | 00 |
| 88 | 　　　　　　　　　　　　　　　　　DS-WARNING | `` | GROUP |  |  | 01 |
| 88 | 　　　　　　　　　　　　　　　　　DS-ERROR | `` | GROUP |  |  | 99 |
| 10 | 　FILLER | `X(20)` | 문자열(20) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 5 |
| 기본 항목 (ELEMENTARY) | 8 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 13 |
