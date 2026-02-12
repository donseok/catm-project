# 데이터 사전: CPYQCRS

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | QR-KEY | `` | GROUP |  |  |  |
| 10 | 　QR-PLANT-CD | `X(04)` | 문자열(4) |  |  |  |
| 10 | 　QR-LINE-CD | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　QR-INSPECT-DT | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　QR-INSPECT-ID | `9(08)` | 숫자(8) |  |  |  |
| 05 | QR-DATA | `` | GROUP |  |  |  |
| 10 | 　QR-PRODUCT-CD | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　QR-LOT-NO | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　QR-JUDGE-CD | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　QR-PASS | `` | GROUP |  |  | P |
| 88 | 　　　　　　　　　　　　　　　　　QR-FAIL | `` | GROUP |  |  | F |
| 88 | 　　　　　　　　　　　　　　　　　QR-REWORK | `` | GROUP |  |  | R |
| 10 | 　QR-DEFECT-TYPE | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　QR-CRITICAL | `` | GROUP |  |  | A |
| 88 | 　　　　　　　　　　　　　　　　　QR-MAJOR | `` | GROUP |  |  | B |
| 88 | 　　　　　　　　　　　　　　　　　QR-MINOR | `` | GROUP |  |  | C |
| 10 | 　QR-DEFECT-CD | `X(05)` | 문자열(5) |  |  |  |
| 10 | 　QR-MEASURE-VAL | `S9(7)V99` | 부호숫자(7.2) 패킹십진수 |  |  |  |
| 10 | 　QR-UPPER-LIMIT | `S9(7)V99` | 부호숫자(7.2) 패킹십진수 |  |  |  |
| 10 | 　QR-LOWER-LIMIT | `S9(7)V99` | 부호숫자(7.2) 패킹십진수 |  |  |  |
| 10 | 　QR-REWORK-CNT | `9(02)` | 숫자(2) |  |  |  |
| 10 | 　QR-INSPECTOR-ID | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　QR-REG-TIME | `9(06)` | 숫자(6) |  |  |  |
| 10 | 　FILLER | `X(10)` | 문자열(10) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 8 |
| 기본 항목 (ELEMENTARY) | 16 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 24 |
