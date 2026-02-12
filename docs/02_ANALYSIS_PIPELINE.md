# 02. 6-Phase 분석 파이프라인

> 대상: **분석가, 기획자** 필독 / 설계자, 개발자 참고

---

## 전체 프로세스 흐름

```
┌─────────────────────────────────────────────────────────────────┐
│                    CATM 6-Phase 분석 파이프라인                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────┐   ┌─────────┐   ┌─────────┐                      │
│  │ Phase 1 │──▶│ Phase 2 │──▶│ Phase 3 │                      │
│  │ 인벤토리 │   │ 의존성   │   │ COPYBOOK│                      │
│  │ 스캔    │   │ 추출    │   │ 파싱    │                       │
│  └─────────┘   └────┬────┘   └─────────┘                      │
│                      │                                          │
│                      ▼                                          │
│  ┌─────────┐   ┌─────────┐   ┌─────────┐                      │
│  │ Phase 6 │◀──│ Phase 5 │◀──│ Phase 4 │                      │
│  │ 우선순위 │   │ Claude  │   │ 다이어   │                      │
│  │ 산정    │   │ AI 분석 │   │ 그램    │                       │
│  └─────────┘   └─────────┘   └─────────┘                      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Phase 간 의존 관계

```
Phase 1 (독립)     → Phase 2에 입력 제공
Phase 2 (Phase 1)  → Phase 4, 5, 6에 입력 제공 (핵심)
Phase 3 (독립)     → Phase 4에 ERD 데이터 제공
Phase 4 (Phase 2)  → 독립 산출물 (후속 의존 없음)
Phase 5 (Phase 2)  → Phase 6에 비즈니스 로직 문서 제공
Phase 6 (Phase 2+5)→ 최종 우선순위 보고서 생성
```

**핵심 의존성**: Phase 2가 생성하는 `dependency-scan.json`이 Phase 4, 5, 6의 필수 입력

---

## Phase 1: 소스코드 인벤토리 스캔

### 목적
메인프레임에서 추출한 소스코드의 **전체 규모를 정량적으로 파악**합니다.

### 스크립트
`catm/scripts/01_scan_inventory.py`

### 실행
```bash
python catm/scripts/01_scan_inventory.py
# 또는
./catm.sh --phase 1
```

### 스캔 대상

| 카테고리 | 디렉토리 | 확장자 |
|---------|---------|--------|
| COBOL 프로그램 | `src/cobol/` | `.cbl`, `.cob`, `.cobol`, (없음) |
| COPYBOOK | `src/copybook/` | `.cpy`, `.copy`, (없음) |
| JCL | `src/jcl/` | `.jcl`, (없음) |
| PROC | `src/proc/` | `.prc`, `.proc`, (없음) |
| DCLGEN | `src/dclgen/` | (없음), `.dclgen` |
| DB2 DDL | `src/ddl/` | (없음), `.ddl`, `.sql` |

### 처리 내용
1. 카테고리별 디렉토리를 순회하며 파일 목록 수집
2. 각 파일의 라인 수, 파일 크기(KB) 측정
3. 카테고리별/전체 통계 계산 (합계, 평균, 최대, 최소)

### 입력 → 출력

```
입력:
  src/ 디렉토리 전체

출력:
  output/reports/inventory.json          ← 구조화된 인벤토리 데이터
  output/reports/01_inventory_report.md  ← 사람이 읽을 수 있는 마크다운 보고서
```

### 출력 예시 (inventory.json 구조)
```json
{
  "categories": {
    "COBOL 프로그램": {
      "files": [
        {"name": "PGM001", "file": "src/cobol/PGM001.cbl", "lines": 115, "size_kb": 8.1},
        ...
      ]
    },
    "COPYBOOK": { ... },
    "JCL": { ... }
  },
  "scan_date": "2026-02-12T20:54:14"
}
```

---

## Phase 2: 의존성 추출 (정적 분석)

### 목적
COBOL 소스코드를 정적 분석하여 **프로그램 간 의존 관계, 복잡도, 기술 특성**을 자동 추출합니다. 이 Phase의 결과가 이후 모든 Phase의 기반 데이터입니다.

### 스크립트
`catm/scripts/02_extract_dependencies.py`

### 실행
```bash
python catm/scripts/02_extract_dependencies.py
# 또는
./catm.sh --phase 2
```

### 추출 항목

| 항목 | 추출 방법 | 예시 |
|------|----------|------|
| CALL 관계 | `CALL 'PGM명'` 패턴 매칭 | PGM001 → SQLERR, ERRLOG |
| COPY 참조 | `COPY CPYBOOK명` 패턴 매칭 | PGM001 → CPYTRANS, CPYSMRY |
| DB2 테이블 | `EXEC SQL` 블록 내 FROM/UPDATE/INSERT 추출 | PGM001 → TB_DAILY_PROD |
| VSAM 파일 | `ASSIGN TO` 패턴 매칭 | PGM001 → PRODTRAN, DLYSMRY |
| CICS 맵 | `SEND MAP/RECEIVE MAP` 추출 | (현재 해당 없음) |
| McCabe 복잡도 | IF/EVALUATE/PERFORM UNTIL 등 분기 키워드 카운트 | PGM003 = 27 |
| PARAGRAPH 수 | PROCEDURE DIVISION 내 패라그래프 카운트 | PGM003 = 13 |
| 업무 카테고리 | catm_config.yaml의 program_categories 매핑 | PGM001 → 조업업무 |

### 추가 분석 기능

| 기능 | 설명 |
|------|------|
| **교차 참조** | 순방향(A가 B를 호출) + 역방향(B가 A에 의해 호출됨) 매핑 |
| **영향도 점수** | 특정 리소스 변경 시 영향받는 프로그램 비율 (0.0~1.0) |
| **데드 코드 탐지** | 어디에서도 호출/실행되지 않는 고아 프로그램 식별 |
| **카테고리 요약** | 업무 카테고리별 프로그램 수, 라인 수, 평균 복잡도 집계 |

### 입력 → 출력

```
입력:
  src/cobol/*.cbl          ← COBOL 소스 파일
  src/jcl/*.jcl            ← JCL 파일 (있는 경우)
  catm_config.yaml         ← 카테고리 매핑 설정

출력:
  output/reports/dependency-scan.json   ← ★ 핵심 산출물 (Phase 4, 5, 6이 의존)
```

### 출력 구조 (dependency-scan.json)
```json
{
  "programs": [
    {
      "name": "PGM001",
      "line_count": 116,
      "calls": ["ABNDPGM", "ERRLOG", "SQLERR"],
      "copies": ["CPYSMRY", "CPYTRANS"],
      "db2_tables": ["TB_DAILY_PROD"],
      "vsam_files": ["DLYSMRY", "PRODTRAN"],
      "complexity": 12,
      "paragraph_count": 8,
      "has_cics": false,
      "has_db2": true,
      "has_vsam": true,
      "category": "조업업무"
    },
    ...
  ],
  "jcl_jobs": [...],
  "summary": {
    "total_programs": 6,
    "total_lines": 1120,
    "avg_complexity": 22.2,
    "unique_copybooks": 13,
    "unique_db2_tables": 6
  },
  "categories_summary": [...],
  "cross_reference": { ... },
  "impact_scores": { ... },
  "dead_code": { ... }
}
```

---

## Phase 3: COPYBOOK 파싱 → 데이터 사전

### 목적
COPYBOOK(.cpy) 파일을 파싱하여 **필드별 데이터 사전**과 **ERD 다이어그램**을 자동 생성합니다.

### 스크립트
`catm/scripts/03_parse_copybook.py`

### 실행
```bash
python catm/scripts/03_parse_copybook.py
# 또는
./catm.sh --phase 3
```

### 파싱 대상 항목

| COBOL 요소 | 파싱 내용 |
|-----------|----------|
| 레벨 번호 (01~49, 66, 77) | 데이터 계층 구조 파악 |
| PIC 절 | 데이터 타입, 길이 추출 (X=문자, 9=숫자, V=소수점) |
| USAGE 절 | COMP, COMP-3, BINARY 등 내부 표현 방식 |
| OCCURS 절 | 배열 크기, DEPENDING ON (가변 길이) |
| REDEFINES 절 | 동일 메모리 재정의 관계 |
| VALUE 절 | 초기값/상수값 |
| 88 레벨 조건명 | 비즈니스 규칙 정의 (상태 코드 등) |

### 데이터 타입 자동 변환 예시

| COBOL PIC | → 한글 타입 |
|-----------|-----------|
| `PIC X(10)` | 문자열(10) |
| `PIC 9(5)` | 숫자(5) |
| `PIC S9(7)V99` | 부호숫자(7.2) |
| `PIC S9(7)V99 COMP-3` | 부호숫자(7.2) 패킹십진수 |
| (PIC 없음) | GROUP |

### 입력 → 출력

```
입력:
  src/copybook/*.cpy      ← COPYBOOK 원본

출력:
  output/data-dict/CPYTRANS.md   ← COPYBOOK별 데이터 사전 (마크다운)
  output/data-dict/CPYSMRY.md
  output/data-dict/...           ← 총 13개 파일 생성
  output/diagrams/erd_copybooks.md  ← ERD 다이어그램 (Mermaid)
```

---

## Phase 4: Mermaid 다이어그램 생성

### 목적
Phase 2의 의존성 데이터를 기반으로 시각적 다이어그램을 자동 생성합니다.

### 스크립트
`catm/scripts/04_generate_diagrams.py`

### 실행
```bash
python catm/scripts/04_generate_diagrams.py
# 또는
./catm.sh --phase 4
```

### 전제 조건
`output/reports/dependency-scan.json` 파일이 존재해야 합니다 (Phase 2 선행 필수).

### 생성되는 다이어그램

| 다이어그램 | 파일 | 내용 |
|-----------|------|------|
| **호출관계 그래프** | `call_graph.md` | 프로그램 → 서브프로그램 CALL 관계, COPY 참조, DB2 연결 |
| **JCL 배치 흐름도** | `jcl_flow.md` | JOB → STEP → PGM 실행 순서 (JCL이 있는 경우) |
| **데이터 흐름도** | `data_flow.md` | 프로그램 ↔ DB2 테이블/VSAM 파일 데이터 입출력 |
| **ERD** | `erd_copybooks.md` | COPYBOOK 기반 엔티티 관계도 (Phase 3에서 생성) |

### 다이어그램 색상 규칙

```
호출관계 그래프:
  🔵 파란색 = 메인 프로그램 (다른 프로그램에서 CALL되지 않는 최상위)
  🟢 녹색 = 서브 프로그램 (CALL되는 프로그램)
  🟠 주황색 = COPYBOOK
  🟣 보라색 = DB2 테이블

데이터 흐름도:
  🔵 파란색 = 프로그램
  🟣 보라색 = DB2 테이블
  🟠 주황색 = VSAM 파일
```

### 입력 → 출력

```
입력:
  output/reports/dependency-scan.json   ← Phase 2 산출물

출력:
  output/diagrams/call_graph.md     ← 호출관계 그래프
  output/diagrams/jcl_flow.md       ← JCL 배치 흐름도
  output/diagrams/data_flow.md      ← 데이터 흐름도
  output/diagrams/README.md         ← 다이어그램 목차
```

---

## Phase 5: Claude AI 비즈니스 로직 분석

### 목적
Claude AI가 COBOL 소스코드를 읽고 **한국어 비즈니스 로직 분석서**를 자동 생성합니다. 이것이 CATM의 핵심 가치입니다.

### 스크립트
`catm/scripts/05_analyze_with_claude.py`

### 실행
```bash
# 전체 프로그램 분석 (logic 모드)
python catm/scripts/05_analyze_with_claude.py

# 단일 프로그램 분석
python catm/scripts/05_analyze_with_claude.py --single src/cobol/PGM001.cbl

# 특정 프로그램만 분석
python catm/scripts/05_analyze_with_claude.py --names PGM001,PGM002

# 데이터 구조 분석 (data 모드)
python catm/scripts/05_analyze_with_claude.py --mode data
```

### 두 가지 분석 모드

#### Logic 모드 (기본)
COBOL 프로그램의 비즈니스 로직을 분석합니다.

**Claude에게 제공하는 컨텍스트:**
1. COBOL 소스코드 전문
2. 참조하는 COPYBOOK 원본 (자동 로드)
3. 파서가 추출한 구조화 메타데이터 (복잡도, CALL/COPY 목록 등)
4. 교차 참조 정보 (호출자/피호출자, 공유 리소스, 영향도 점수)

**분석 결과 문서 구조:**
```
1. 프로그램 개요 (목적, 처리 유형, 입출력)
2. 데이터 흐름 (입력/출력/COPYBOOK/호출관계)
3. 비즈니스 로직 상세 (SECTION/PARAGRAPH별 설명)
4. 비즈니스 규칙 요약 (88-level 조건명 포함)
5. MES 관련도 평가 (모더나이제이션 관점)
6. 특이사항/리스크 (심각도 태그: HIGH/MEDIUM/LOW)
```

#### Data 모드
COPYBOOK/DCLGEN의 데이터 구조를 분석합니다.

**분석 결과 문서 구조:**
```
1. 구조 개요 (용도, 타입, 관련 업무)
2. 필드 분석 (필드별 한글 설명, MES 매핑 후보)
3. 비즈니스 규칙 (88-level 조건명 상세)
4. 엔티티 관계 분석 (PK/FK 후보, 정규화 제안)
5. MES 데이터 모델 매핑 제안
6. 데이터 품질 이슈
```

### 프롬프트 조립 과정

```
① 프롬프트 템플릿 로드 (catm/prompts/analyze-logic.md)
     ↓
② 소스코드 주입 ({{COBOL_SOURCE}} → 실제 COBOL 코드)
     ↓
③ COPYBOOK 자동 로드 및 주입 ({{COPYBOOK_CONTENTS}} → COPY 참조 내용)
     ↓
④ 메타데이터 주입 ({{STRUCTURED_METADATA}} → 파서 분석 결과)
     ↓
⑤ 교차참조 주입 ({{CROSS_REFERENCE_CONTEXT}} → 호출관계/영향도)
     ↓
⑥ Claude CLI 호출 (claude -p - --output-format text)
     ↓
⑦ 응답을 output/docs/{PGM명}.md로 저장
```

### Rate Limit 대응

```
배치 처리 전략:
  - 5개 프로그램 단위로 배치 실행
  - 배치 완료 후 10초 대기
  - Rate limit 감지 시 30초 × 재시도 횟수만큼 대기
  - 최대 3회 재시도
  - 프로그램당 최대 300초 타임아웃
```

### 입력 → 출력

```
입력:
  src/cobol/*.cbl                        ← COBOL 소스
  src/copybook/*.cpy                     ← 참조 COPYBOOK
  output/reports/dependency-scan.json    ← 교차참조 데이터 (선택)
  catm/prompts/analyze-logic.md          ← 프롬프트 템플릿

출력 (logic 모드):
  output/docs/PGM001.md ~ PGM006.md      ← 프로그램별 비즈니스 로직 분석서
  output/reports/analysis_log.json       ← 분석 실행 로그

출력 (data 모드):
  output/data-dict/CPYTRANS.md 등         ← Claude 해석 반영된 데이터 사전
  output/reports/data_analysis_log.json  ← 분석 실행 로그
```

---

## Phase 6: 모더나이제이션 우선순위 산정

### 목적
정적 분석 결과 + Claude AI 비즈니스 분석을 종합하여 **프로그램별 전환 우선순위**를 자동 산정합니다.

### 스크립트
`catm/scripts/06_prioritize.py`

### 실행
```bash
python catm/scripts/06_prioritize.py
# 또는
./catm.sh --phase 6
```

### 우선순위 산정 2단계 프로세스

```
┌──────────────────────────────┐     ┌──────────────────────────────┐
│  1단계: 정적 분석 기반 점수   │     │  2단계: Claude AI 보정        │
│                              │     │                              │
│  기술 복잡도  ← McCabe/Max   │     │  비즈니스 중요도 ← Claude 판단│
│  의존성 영향도 ← CALL+COPY 수│     │  Phase 배정 ← Claude 추천     │
│  전환 용이도  ← CICS/DB2/복잡│ ──▶ │  최종 점수 재계산             │
│  비즈니스 중요도 ← 기본값(5) │     │                              │
└──────────────────────────────┘     └──────────────────────────────┘
```

### 점수 산정 기준 (4개 지표)

| 지표 | 가중치 | 점수 범위 | 산정 방법 |
|------|--------|----------|----------|
| **비즈니스 중요도** | 35% | 1-10 | Claude AI가 분석 문서를 보고 판단 (기본값 5) |
| **기술 복잡도** | 25% | 1-10 | McCabe 복잡도를 전체 대비 상대값으로 환산 |
| **의존성 영향도** | 20% | 1-10 | CALL + COPY 수를 전체 대비 상대값으로 환산 |
| **전환 용이도** | 20% | 1-10 | 기본 8에서 CICS(-2), DB2(-1), 고복잡도(-2) 감점 |

**최종 점수** = (비즈니스 × 0.35) + (복잡도 × 0.25) + (영향도 × 0.20) + (용이도 × 0.20)

### 전환 방식 자동 판별

| 방식 | 판별 조건 | 설명 |
|------|----------|------|
| **자동 변환** | 용이도 ≥ 7 AND 복잡도 < medium(25) | 도구를 이용한 자동 코드 변환 |
| **리라이트** | CICS 사용 OR 복잡도 > high(50) | 새로 설계/개발 |
| **하이브리드** | 그 외 | 자동 변환 + 수동 보정 병행 |

### Phase 분류 기준

| Phase | 최종 점수 | 의미 | 시기 |
|-------|----------|------|------|
| Phase 1 | 7.0 이상 | 즉시 전환 대상 | 즉시 |
| Phase 2 | 4.0 ~ 6.9 | 중기 전환 대상 | 3개월 내 |
| Phase 3 | 4.0 미만 | 장기 전환 대상 | 6개월 내 |

### 입력 → 출력

```
입력:
  output/reports/dependency-scan.json       ← Phase 2 산출물 (필수)
  output/docs/*.md                          ← Phase 5 산출물 (선택, 비즈니스 중요도용)

출력:
  output/reports/priority_matrix.md         ← 우선순위 매트릭스 보고서
  output/reports/priority_data.json         ← 점수 상세 데이터
  output/reports/claude_priority_analysis.md ← Claude의 원본 분석 응답
```

---

## 전체 실행 요약

### 전체 자동 실행
```bash
./catm.sh
```
Phase 1 → 2 → 3 → 4 → 5 → 6 순차 실행

### Phase별 개별 실행
```bash
./catm.sh --phase 1    # 인벤토리 스캔
./catm.sh --phase 2    # 의존성 추출
./catm.sh --phase 3    # COPYBOOK 파싱
./catm.sh --phase 4    # 다이어그램 생성
./catm.sh --phase 5    # Claude 비즈니스 분석
./catm.sh --phase 6    # 우선순위 산정
```

### 최소 실행 조합 (Claude 없이)
Phase 1 + 2 + 3 + 4만 실행하면 **Claude 없이도** 인벤토리, 의존성, 데이터 사전, 다이어그램을 얻을 수 있습니다.

### 전체 입출력 데이터 흐름도

```
src/cobol/*.cbl ──────┐
src/copybook/*.cpy ───┤
src/jcl/*.jcl ────────┤
catm_config.yaml ─────┤
                      ▼
              ┌──────────────┐
              │   Phase 1    │──▶ inventory.json
              │   인벤토리    │──▶ 01_inventory_report.md
              └──────────────┘
                      │
                      ▼
              ┌──────────────┐
              │   Phase 2    │──▶ dependency-scan.json  ★
              │   의존성      │
              └──────────────┘
                   │     │
          ┌────────┘     └────────┐
          ▼                       ▼
  ┌──────────────┐       ┌──────────────┐
  │   Phase 3    │       │   Phase 4    │──▶ call_graph.md
  │   COPYBOOK   │       │   다이어그램   │──▶ data_flow.md
  └──────────────┘       └──────────────┘──▶ jcl_flow.md
          │
          ▼
  output/data-dict/*.md
  erd_copybooks.md

              ┌──────────────┐
              │   Phase 5    │──▶ PGM001.md ~ PGM006.md
              │   Claude AI  │──▶ analysis_log.json
              └──────────────┘
                      │
                      ▼
              ┌──────────────┐
              │   Phase 6    │──▶ priority_matrix.md
              │   우선순위    │──▶ priority_data.json
              └──────────────┘──▶ claude_priority_analysis.md
```
