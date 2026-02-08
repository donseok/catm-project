# 데이터 사전: CPYINVTR

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | IT-KEY | `` | GROUP |  |  |  |
| 10 | 　IT-PLANT-CD | `X(04)` | 문자열(4) |  |  |  |
| 10 | 　IT-ITEM-CD | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　IT-TRANS-DATE | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　IT-TRANS-SEQ | `9(05)` | 숫자(5) |  |  |  |
| 05 | IT-DATA | `` | GROUP |  |  |  |
| 10 | 　IT-TRANS-TYPE | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　IT-IN | `` | GROUP |  |  | I |
| 88 | 　　　　　　　　　　　　　　　　　IT-OUT | `` | GROUP |  |  | O |
| 88 | 　　　　　　　　　　　　　　　　　IT-ADJ | `` | GROUP |  |  | A |
| 10 | 　IT-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 10 | 　IT-UNIT-PRICE | `S9(9)V99` | 부호숫자(9.2) |  |  |  |
| 10 | 　IT-WAREHOUSE-CD | `X(05)` | 문자열(5) |  |  |  |
| 10 | 　IT-LOCATION-CD | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　IT-VENDOR-CD | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　IT-PO-NO | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　IT-USER-ID | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　IT-REG-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　FILLER | `X(10)` | 문자열(10) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 5 |
| 기본 항목 (ELEMENTARY) | 14 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 19 |
