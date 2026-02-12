# 데이터 사전: CPYBFTM

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | BT-KEY | `` | GROUP |  |  |  |
| 10 | 　BT-BF-NO | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　BT-MEASURE-DT | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　BT-MEASURE-SEQ | `9(05)` | 숫자(5) |  |  |  |
| 05 | BT-DATA | `` | GROUP |  |  |  |
| 10 | 　BT-TEMP-VAL | `S9(5)V9` | 부호숫자(5.1) 패킹십진수 |  |  |  |
| 10 | 　BT-SENSOR-ID | `X(08)` | 문자열(8) |  |  |  |
| 10 | 　BT-LOCATION-CD | `X(04)` | 문자열(4) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　BT-HEARTH | `` | GROUP |  |  | HTH  |
| 88 | 　　　　　　　　　　　　　　　　　BT-BOSH | `` | GROUP |  |  | BOSH |
| 88 | 　　　　　　　　　　　　　　　　　BT-SHAFT | `` | GROUP |  |  | SHFT |
| 88 | 　　　　　　　　　　　　　　　　　BT-TOP | `` | GROUP |  |  | TOP  |
| 10 | 　BT-MEASURE-TYPE | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　BT-AUTO | `` | GROUP |  |  | A |
| 88 | 　　　　　　　　　　　　　　　　　BT-MANUAL | `` | GROUP |  |  | M |
| 10 | 　BT-STATUS-CD | `X(02)` | 문자열(2) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　BT-NORMAL | `` | GROUP |  |  | OK |
| 88 | 　　　　　　　　　　　　　　　　　BT-WARNING | `` | GROUP |  |  | WN |
| 88 | 　　　　　　　　　　　　　　　　　BT-CRITICAL | `` | GROUP |  |  | CR |
| 10 | 　BT-REG-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　FILLER | `X(15)` | 문자열(15) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 11 |
| 기본 항목 (ELEMENTARY) | 10 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 21 |
