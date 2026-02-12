# 데이터 사전: CPYPACT

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | PA-KEY | `` | GROUP |  |  |  |
| 10 | 　PA-PRODUCT-CD | `X(15)` | 문자열(15) |  |  |  |
| 10 | 　PA-YYYYMM | `9(06)` | 숫자(6) |  |  |  |
| 05 | PA-DATA | `` | GROUP |  |  |  |
| 10 | 　PA-ACTUAL-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　PA-WORK-DAYS | `9(03)` | 숫자(3) |  |  |  |
| 10 | 　PA-LINE-CNT | `9(02)` | 숫자(2) |  |  |  |
| 10 | 　PA-DEFECT-QTY | `S9(7)V99` | 부호숫자(7.2) 패킹십진수 |  |  |  |
| 10 | 　PA-YIELD-RATE | `9(3)V99` | 숫자(3.2) |  |  |  |
| 10 | 　PA-LAST-UPD | `9(08)` | 숫자(8) |  |  |  |
| 10 | 　FILLER | `X(20)` | 문자열(20) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 2 |
| 기본 항목 (ELEMENTARY) | 9 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 11 |
