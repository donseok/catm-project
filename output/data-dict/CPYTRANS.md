# 데이터 사전: CPYTRANS

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | PT-KEY | `` | GROUP |  |  |  |
| 10 | 　PT-PLANT-CD | `X(04)` | 문자열(4) |  |  |  |
| 10 | 　PT-LINE-CD | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　PT-PROD-DATE | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　PT-SEQ-NO | `9(05)` | 숫자(5) |  |  |  |
| 05 | PT-DATA | `` | GROUP |  |  |  |
| 10 | 　PT-PRODUCT-CD | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　PT-QTY | `S9(9)V99` | 부호숫자(9.2) |  |  |  |
| 10 | 　PT-UNIT-CD | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　PT-WORKER-ID | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　PT-SHIFT-CD | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　PT-DAY-SHIFT | `` | GROUP |  |  | 1 |
| 88 | 　　　　　　　　　　　　　　　　　PT-EVE-SHIFT | `` | GROUP |  |  | 2 |
| 88 | 　　　　　　　　　　　　　　　　　PT-NGT-SHIFT | `` | GROUP |  |  | 3 |
| 10 | 　PT-STATUS-CD | `X(02)` | 문자열(2) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　PT-NORMAL | `` | GROUP |  |  | 00 |
| 88 | 　　　　　　　　　　　　　　　　　PT-DEFECT | `` | GROUP |  |  | 01 |
| 88 | 　　　　　　　　　　　　　　　　　PT-REWORK | `` | GROUP |  |  | 02 |
| 10 | 　PT-REG-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　PT-UPD-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　FILLER | `X(20)` | 문자열(20) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 8 |
| 기본 항목 (ELEMENTARY) | 13 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 21 |
