# 데이터 사전: CPYPPLAN

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | PP-KEY | `` | GROUP |  |  |  |
| 10 | 　PP-PLANT-CD | `X(04)` | 문자열(4) |  |  |  |
| 10 | 　PP-PRODUCT-CD | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　PP-YYYYMM | `9(06)` | 숫자(6) |  |  |  |
| 05 | PP-DATA | `` | GROUP |  |  |  |
| 10 | 　PP-PLAN-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　PP-UNIT-CD | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　PP-LINE-CD | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　PP-PRIORITY | `9(01)` | 숫자(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　PP-URGENT | `` | GROUP |  |  | 1 |
| 88 | 　　　　　　　　　　　　　　　　　PP-NORMAL | `` | GROUP |  |  | 2 |
| 88 | 　　　　　　　　　　　　　　　　　PP-LOW | `` | GROUP |  |  | 3 |
| 10 | 　PP-PLAN-TYPE | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　PP-REGULAR | `` | GROUP |  |  | R |
| 88 | 　　　　　　　　　　　　　　　　　PP-EXTRA | `` | GROUP |  |  | E |
| 88 | 　　　　　　　　　　　　　　　　　PP-TRIAL | `` | GROUP |  |  | T |
| 10 | 　PP-REG-DATE | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　PP-REG-USER | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　FILLER | `X(15)` | 문자열(15) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 8 |
| 기본 항목 (ELEMENTARY) | 11 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 19 |
