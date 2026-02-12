# 데이터 사전: CPYRMMS

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | RM-MATL-CD | `X(12)` | 문자열(12) |  |  |  |
| 05 | RM-DATA | `` | GROUP |  |  |  |
| 10 | 　RM-MATL-NM | `X(30)` | 문자열(30) |  |  |  |
| 10 | 　RM-CATEGORY | `X(02)` | 문자열(2) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　RM-IRON-ORE | `` | GROUP |  |  | IO |
| 88 | 　　　　　　　　　　　　　　　　　RM-COAL | `` | GROUP |  |  | CL |
| 88 | 　　　　　　　　　　　　　　　　　RM-LIMESTONE | `` | GROUP |  |  | LS |
| 88 | 　　　　　　　　　　　　　　　　　RM-FERRO | `` | GROUP |  |  | FA |
| 88 | 　　　　　　　　　　　　　　　　　RM-SCRAP | `` | GROUP |  |  | SC |
| 10 | 　RM-UNIT-CD | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　RM-UNIT-PRICE | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　RM-STOCK-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　RM-SAFETY-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　RM-LEAD-DAYS | `9(03)` | 숫자(3) |  |  |  |
| 10 | 　RM-MAIN-VENDOR | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　RM-LAST-IN-DT | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　RM-LAST-OUT-DT | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　FILLER | `X(15)` | 문자열(15) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 6 |
| 기본 항목 (ELEMENTARY) | 12 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 18 |
