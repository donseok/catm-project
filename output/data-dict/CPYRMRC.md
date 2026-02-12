# 데이터 사전: CPYRMRC

> 자동 생성 by CATM (COBOL Analysis Task Manager)

## 필드 정의

| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
|----|--------|-----|------------|--------|-----------|--------|
| 05 | RR-KEY | `` | GROUP |  |  |  |
| 10 | 　RR-RECEIPT-NO | `9(10)` | 숫자(10) |  |  |  |
| 10 | 　RR-RECEIPT-DT | `9(08)` | 숫자(8) |  |  |  |
| 05 | RR-DATA | `` | GROUP |  |  |  |
| 10 | 　RR-MATL-CD | `X(12)` | 문자열(12) |  |  |  |
| 10 | 　RR-MATL-NM | `X(30)` | 문자열(30) |  |  |  |
| 10 | 　RR-VENDOR-CD | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　RR-RECEIPT-QTY | `S9(9)V99` | 부호숫자(9.2) 패킹십진수 |  |  |  |
| 10 | 　RR-UNIT-CD | `X(03)` | 문자열(3) |  |  |  |
| 10 | 　RR-INSPECT-CD | `X(01)` | 문자열(1) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　RR-ACCEPTED | `` | GROUP |  |  | A |
| 88 | 　　　　　　　　　　　　　　　　　RR-PARTIAL | `` | GROUP |  |  | P |
| 88 | 　　　　　　　　　　　　　　　　　RR-REJECTED | `` | GROUP |  |  | R |
| 10 | 　RR-ACCEPT-RATE | `9(3)V99` | 숫자(3.2) |  |  |  |
| 10 | 　RR-REASON-CD | `X(03)` | 문자열(3) |  |  |  |
| 88 | 　　　　　　　　　　　　　　　　　RR-QUALITY | `` | GROUP |  |  | QL  |
| 88 | 　　　　　　　　　　　　　　　　　RR-DAMAGE | `` | GROUP |  |  | DM  |
| 88 | 　　　　　　　　　　　　　　　　　RR-SPEC | `` | GROUP |  |  | SP  |
| 88 | 　　　　　　　　　　　　　　　　　RR-MOISTURE | `` | GROUP |  |  | MS  |
| 10 | 　RR-WAREHOUSE-CD | `X(04)` | 문자열(4) |  |  |  |
| 10 | 　RR-PO-NO | `X(12)` | 문자열(12) |  |  |  |
| 10 | 　RR-REG-USER | `X(10)` | 문자열(10) |  |  |  |
| 10 | 　FILLER | `X(10)` | 문자열(10) |  |  |  |

## 요약

| 항목 | 수 |
|------|-----|
| 그룹 항목 (GROUP) | 9 |
| 기본 항목 (ELEMENTARY) | 14 |
| 배열 (OCCURS) | 0 |
| REDEFINES | 0 |
| 총 필드 수 | 23 |
