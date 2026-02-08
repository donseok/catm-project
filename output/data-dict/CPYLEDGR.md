# 데이터 사전: CPYLEDGR

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | LG-ITEM-CD | `X(15)` | 문자열(15) |  |  |  |
| 05 | LG-TRANS-DATE | `9(08)` | 숫자(8) |  |  |  |
| 05 | LG-TRANS-TYPE | `X(01)` | 문자열(1) |  |  |  |
| 05 | LG-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 05 | LG-PREV-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 05 | LG-CURR-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 05 | LG-UNIT-PRICE | `S9(9)V99` | 부호숫자(9.2) |  |  |  |
| 05 | LG-AMOUNT | `S9(11)V99` | 부호숫자(11.2) |  |  |  |
| 05 | FILLER | `X(20)` | 문자열(20) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 0 |
| 기본 항목 (ELEMENTARY) | 9 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 9 |
