# 데이터 사전: CPYBFOP

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | BO-KEY | `` | GROUP |  |  |  |
| 10 | 　BO-BF-NO | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　BO-OPER-DATE | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　BO-OPER-SEQ | `9(05)` | 숫자(5) |  |  |  |
| 05 | BO-DATA | `` | GROUP |  |  |  |
| 10 | 　BO-OPER-TYPE | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　BO-CHARGING | `` | GROUP |  |  | C |
| 88 | 　　　　　　　　　　　　　　　　　BO-TAPPING | `` | GROUP |  |  | T |
| 88 | 　　　　　　　　　　　　　　　　　BO-BLOWING | `` | GROUP |  |  | B |
| 10 | 　BO-SHIFT-CD | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　BO-SHIFT-A | `` | GROUP |  |  | A |
| 88 | 　　　　　　　　　　　　　　　　　BO-SHIFT-B | `` | GROUP |  |  | B |
| 88 | 　　　　　　　　　　　　　　　　　BO-SHIFT-C | `` | GROUP |  |  | C |
| 10 | 　BO-PRESSURE | `S9(3)V99` | 부호숫자(3.2) 패킹십진수 |  |  |  |
| 10 | 　BO-WIND-VOL | `S9(7)V9` | 부호숫자(7.1) 패킹십진수 |  |  |  |
| 10 | 　BO-COKE-RATE | `9(3)V99` | 숫자(3.2) |  |  |  |
| 10 | 　BO-TAP-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　BO-SLAG-QTY | `S9(7)V99` | 부호숫자(7.2) 패킹십진수 |  |  |  |
| 10 | 　BO-REMARK-CD | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　BO-REG-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　BO-WORKER-ID | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　FILLER | `X(10)` | 문자열(10) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 8 |
| 기본 항목 (ELEMENTARY) | 14 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 22 |
