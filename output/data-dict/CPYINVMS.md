# 데이터 사전: CPYINVMS

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | IM-ITEM-CD | `X(15)` | 문자열(15) |  |  |  |
| 05 | IM-ITEM-NAME | `X(50)` | 문자열(50) |  |  |  |
| 05 | IM-CATEGORY | `X(10)` | 문자열(10) |  |  |  |
| 05 | IM-UNIT-CD | `X(05)` | 문자열(5) |  |  |  |
| 05 | IM-CURR-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 05 | IM-MIN-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 05 | IM-MAX-QTY | `S9(9)` | 숫자(9) |  |  |  |
| 05 | IM-UNIT-COST | `S9(9)V99` | 부호숫자(9.2) |  |  |  |
| 05 | IM-LAST-DATE | `9(08)` | 숫자(8) |  |  |  |
| 05 | IM-STATUS | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　IM-ACTIVE | `` | GROUP |  |  | A |
| 88 | 　　　　　　　　　　　　　　　　　IM-INACTIVE | `` | GROUP |  |  | I |
| 88 | 　　　　　　　　　　　　　　　　　IM-DISCONTINUED | `` | GROUP |  |  | D |
| 05 | FILLER | `X(20)` | 문자열(20) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 3 |
| 기본 항목 (ELEMENTARY) | 11 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 14 |
