# 데이터 구조 분석: CPYTRANS

## 1. 구조 개요
- **용도**: 생산실적 트랜잭션 레코드 — 공장 생산라인에서 발생하는 개별 생산 실적(수량, 작업자, 교대근무, 품질상태)을 기록
- **타입**: 트랜잭션
- **관련 업무**: 생산관리(MES), 품질관리, 실적집계

## 2. 필드 분석

### 키 영역 (PT-KEY)

| 필드명 | PIC | 한글 설명 | 업무 의미 | MES 매핑 후보 |
|--------|-----|----------|----------|--------------|
| PT-PLANT-CD | X(04) | 공장코드 | 생산 공장 식별자 | `plant_code VARCHAR(4)` |
| PT-LINE-CD | X(10) | 라인코드 | 생산라인(설비라인) 식별자 | `line_code VARCHAR(10)` |
| PT-PROD-DATE | 9(08) | 생산일자 | YYYYMMDD 형식 생산일 | `prod_date DATE` |
| PT-SEQ-NO | 9(05) | 일련번호 | 동일 공장/라인/일자 내 순번 | `seq_no INT` |

> 복합키: (공장 + 라인 + 생산일자 + 순번)으로 유일한 트랜잭션 식별

### 데이터 영역 (PT-DATA)

| 필드명 | PIC | 한글 설명 | 업무 의미 | MES 매핑 후보 |
|--------|-----|----------|----------|--------------|
| PT-PRODUCT-CD | X(15) | 제품코드 | 생산 대상 제품(품목) 식별자 | `product_code VARCHAR(15)` |
| PT-QTY | S9(9)V99 COMP-3 | 생산수량 | 부호 있는 소수 2자리 수량 (COMP-3 패킹) | `qty DECIMAL(11,2)` |
| PT-UNIT-CD | X(03) | 단위코드 | 수량 단위 (EA, KG, M 등) | `unit_code VARCHAR(3)` |
| PT-WORKER-ID | X(10) | 작업자ID | 생산 실적 등록 작업자 | `worker_id VARCHAR(10)` |
| PT-SHIFT-CD | X(01) | 교대근무코드 | 1=주간, 2=야간(오후), 3=심야 | `shift_code CHAR(1)` |
| PT-STATUS-CD | X(02) | 상태코드 | 00=정상, 01=불량, 02=재작업 | `status_code CHAR(2)` |
| PT-REG-TIME | 9(06) | 등록시각 | HHmmss 형식, 최초 등록 시각 | `reg_time TIME` |
| PT-UPD-TIME | 9(06) | 수정시각 | HHmmss 형식, 최종 수정 시각 | `upd_time TIME` |
| FILLER | X(20) | 예비영역 | 향후 확장용 | (삭제 가능) |

### 88-레벨 조건명 (코드 값 정의)

| 조건명 | 소속 필드 | 값 | 의미 |
|--------|----------|-----|------|
| PT-DAY-SHIFT | PT-SHIFT-CD | '1' | 주간 근무 |
| PT-EVE-SHIFT | PT-SHIFT-CD | '2' | 오후(야간) 근무 |
| PT-NGT-SHIFT | PT-SHIFT-CD | '3' | 심야 근무 |
| PT-NORMAL | PT-STATUS-CD | '00' | 정상 생산 |
| PT-DEFECT | PT-STATUS-CD | '01' | 불량 |
| PT-REWORK | PT-STATUS-CD | '02' | 재작업 |

## 3. MES 데이터 모델 매핑 제안

### 3-1. 테이블 설계

```sql
CREATE TABLE MES_PROD_TRANSACTION (
    -- PK (복합키 → 서로게이트 키 도입 권장)
    txn_id          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    
    -- 원래 복합키 (유니크 제약)
    plant_code      VARCHAR(4)      NOT NULL,
    line_code       VARCHAR(10)     NOT NULL,
    prod_date       DATE            NOT NULL,
    seq_no          INT             NOT NULL,
    
    -- 데이터
    product_code    VARCHAR(15)     NOT NULL,
    qty             DECIMAL(11,2)   NOT NULL,
    unit_code       VARCHAR(3),
    worker_id       VARCHAR(10),
    shift_code      CHAR(1)         CHECK (shift_code IN ('1','2','3')),
    status_code     CHAR(2)         DEFAULT '00' CHECK (status_code IN ('00','01','02')),
    reg_time        TIME,
    upd_time        TIME,
    
    -- 감사 컬럼 (MES 추가)
    created_at      TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    updated_at      TIMESTAMP,
    
    CONSTRAINT uq_prod_txn UNIQUE (plant_code, line_code, prod_date, seq_no)
);
```

### 3-2. 정규화 필요 사항

| 항목 | 현재 | 제안 |
|------|------|------|
| 공장코드 | 트랜잭션 내 하드코딩 | `MST_PLANT` 마스터 테이블 분리, FK 참조 |
| 라인코드 | 트랜잭션 내 하드코딩 | `MST_LINE` 마스터 테이블 분리, FK 참조 |
| 제품코드 | 트랜잭션 내 하드코딩 | `MST_PRODUCT` 마스터 테이블 분리, FK 참조 |
| 작업자ID | 트랜잭션 내 하드코딩 | `MST_WORKER` 마스터 테이블 분리, FK 참조 |
| 교대근무코드 | 88-레벨 조건명 | `MST_SHIFT_CODE` 코드 테이블 또는 ENUM |
| 상태코드 | 88-레벨 조건명 | `MST_STATUS_CODE` 코드 테이블 또는 ENUM |

### 3-3. 데이터 타입 변환 가이드

| COBOL PIC | COBOL 크기 | SQL 타입 | Java 타입 | 비고 |
|-----------|-----------|----------|-----------|------|
| X(nn) | nn bytes | VARCHAR(nn) | String | 문자열 그대로 |
| 9(08) | 8 bytes | DATE | LocalDate | YYYYMMDD → DATE 변환 필요 |
| 9(05) | 5 bytes | INT | int | 숫자 순번 |
| 9(06) | 6 bytes | TIME | LocalTime | HHmmss → TIME 변환 필요 |
| S9(9)V99 COMP-3 | 6 bytes | DECIMAL(11,2) | BigDecimal | 패킹 해제 필요, 부호 처리 주의 |

## 4. 데이터 품질 이슈

### 날짜/시각 형식
- **PT-PROD-DATE**: `9(08)` → YYYYMMDD 형식. 유효 날짜 검증 필요 (예: 20260230 같은 무효 날짜 가능성)
- **PT-REG-TIME / PT-UPD-TIME**: `9(06)` → HHmmss 형식. 24시간제 검증 필요. 날짜 정보 없이 시각만 있으므로, 일자 전환(자정 넘김) 시 주의

### COMP-3 (패킹 소수)
- **PT-QTY**: `S9(9)V99 COMP-3`은 물리적으로 6바이트 차지. 데이터 마이그레이션 시 바이트 단위 패킹 해제(unpack) 로직 필수
- 부호(S)가 있으므로 음수 수량(반품, 수정분) 가능 — MES에서도 음수 허용 정책 결정 필요

### 하드코딩된 코드 값
- 교대근무: '1', '2', '3'만 정의 — 향후 4조 2교대 등 근무 체계 변경 시 확장 필요
- 상태코드: '00', '01', '02'만 정의 — '03'(폐기), '04'(보류) 등 추가 상태가 실제 데이터에 존재할 가능성 있으므로 마이그레이션 전 실데이터 분포 조사 필수

### FILLER 영역
- `FILLER X(20)`이 20바이트 예비 영역으로 잡혀 있으나, 실제 운영 시스템에서 비공식적으로 데이터를 넣어두는 경우가 빈번함 → 마이그레이션 전 FILLER 영역에 유의미한 데이터가 있는지 샘플링 확인 필요

### 레코드 길이
- 전체 레코드 크기: 키(27) + 데이터(PT-PRODUCT-CD 15 + PT-QTY 6 + PT-UNIT-CD 3 + PT-WORKER-ID 10 + PT-SHIFT-CD 1 + PT-STATUS-CD 2 + PT-REG-TIME 6 + PT-UPD-TIME 6 + FILLER 20) = **96바이트**
- 고정 길이 레코드이므로, 파일 기반 마이그레이션 시 레코드 경계를 정확히 맞춰야 함