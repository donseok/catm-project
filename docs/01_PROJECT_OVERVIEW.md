# 01. CATM 프로젝트 개요

> 대상: 분석가, 기획자, 설계자, 개발자 **전원 필독**

---

## CATM이란?

**CATM** (COBOL Analysis Task Manager)은 IBM 메인프레임의 COBOL 레거시 시스템을 **자동으로 분석**하여 모더나이제이션(현대화) 전환에 필요한 기초 자료를 생성하는 도구입니다.

```
┌─────────────────┐        ┌──────────┐        ┌──────────────────┐
│  메인프레임에서   │        │          │        │  분석 결과물      │
│  추출한 소스코드  │ ────▶  │   CATM   │ ────▶  │  (문서/다이어그램/ │
│  (COBOL/CPY/JCL) │        │          │        │   우선순위 보고서)  │
└─────────────────┘        └──────────┘        └──────────────────┘
```

### 핵심 기능

| 기능 | 설명 |
|------|------|
| **인벤토리 스캔** | 소스코드 전체 규모 파악 (프로그램 수, 라인 수, 크기) |
| **의존성 추출** | CALL/COPY/DB2/VSAM/CICS 관계를 정적 분석으로 자동 추출 |
| **데이터 사전 생성** | COPYBOOK 파싱으로 필드별 데이터 타입/구조 자동 문서화 |
| **다이어그램 생성** | 호출관계도, 데이터흐름도, ERD를 Mermaid로 자동 생성 |
| **비즈니스 로직 분석** | Claude AI가 COBOL 소스를 읽고 한국어 비즈니스 로직 문서 생성 |
| **우선순위 산정** | 복잡도/의존성/비즈니스 중요도를 종합한 전환 우선순위 자동 산정 |

---

## 왜 필요한가?

### 기존 방식의 문제점

```
❌ 수동 분석: COBOL 전문가가 1인당 하루 1~2개 프로그램 분석
❌ 속인 의존: 특정 개인의 지식에 의존, 퇴직 시 지식 소실
❌ 일관성 부족: 분석자마다 문서 형식/깊이가 다름
❌ 비용 과다: 수백 개 프로그램 × 수일/건 = 수개월 소요
```

### CATM의 해결 방식

```
✅ 자동 분석: 6개 프로그램 + 13개 COPYBOOK을 수분 내 분석
✅ 일관된 품질: 동일한 프롬프트 템플릿으로 균일한 분석 결과
✅ AI 기반 해석: Claude AI가 COBOL 비즈니스 로직을 한국어로 해석
✅ 정량적 우선순위: 가중치 기반 점수로 객관적 전환 순서 결정
```

---

## 현재 분석 대상 시스템

### COBOL 프로그램 (6개)

| 프로그램 | 카테고리 | 라인 수 | 복잡도 | 주요 기능 |
|---------|---------|---------|--------|----------|
| PGM001 | 조업업무 | 115 | 12 (낮음) | 일일 생산실적 집계 배치 |
| PGM002 | 출하업무 | 166 | 18 (중간) | 재고 트랜잭션 처리 |
| PGM003 | 품질관리 | 178 | 27 (높음) | 품질검사 결과 처리/집계 |
| PGM004 | 생산관리 | 204 | 24 (중간) | 생산계획 대비 실적 분석 |
| PGM005 | 조업업무 | 234 | 35 (높음) | 용광로 조업 데이터 처리 |
| PGM006 | 원자재관리 | 217 | 17 (중간) | 원자재 입출고 관리 |
| **합계** | | **1,114** | 평균 22 | |

### COPYBOOK (13개)

| COPYBOOK | 용도 | 참조 프로그램 |
|----------|------|-------------|
| CPYTRANS | 생산실적 트랜잭션 레코드 | PGM001 |
| CPYSMRY | 일일 생산집계 요약 | PGM001 |
| CPYINVMS | 재고 마스터 | PGM002 |
| CPYINVTR | 재고 트랜잭션 | PGM002 |
| CPYLEDGR | 재고 원장 | PGM002 |
| CPYQCRS | 품질검사 결과 | PGM003 |
| CPYQCHS | 품질검사 이력 | PGM003 |
| CPYPPLAN | 생산계획 | PGM004 |
| CPYPACT | 생산실적 | PGM004 |
| CPYBFOP | 용광로 조업 데이터 | PGM005 |
| CPYBFTM | 용광로 온도 측정 | PGM005 |
| CPYRMMS | 원자재 마스터 | PGM006 |
| CPYRMRC | 원자재 입출고 기록 | PGM006 |

### 기술 스택 분포

```
모든 프로그램 공통:
  ✅ DB2 SQL 사용 (6/6)
  ✅ VSAM 파일 I/O (6/6)
  ❌ CICS 온라인 (0/6) — 전부 배치 프로그램
```

---

## 프로젝트 디렉토리 구조

```
catm-project/
│
├── src/                          ← 메인프레임에서 추출한 원본 소스 (읽기 전용)
│   ├── cobol/                    ← COBOL 프로그램 (.cbl)
│   │   ├── PGM001.cbl ~ PGM006.cbl
│   ├── copybook/                 ← COPYBOOK (.cpy)
│   │   ├── CPYTRANS.cpy, CPYSMRY.cpy, ...
│   ├── jcl/                      ← JCL 파일 (현재 비어있음)
│   ├── proc/                     ← PROC 파일 (현재 비어있음)
│   ├── dclgen/                   ← DCLGEN 파일 (현재 비어있음)
│   └── ddl/                      ← DB2 DDL 파일 (현재 비어있음)
│
├── catm/                         ← CATM 분석 봇 코어
│   ├── scripts/                  ← 6-Phase 파이프라인 스크립트
│   │   ├── 01_scan_inventory.py
│   │   ├── 02_extract_dependencies.py
│   │   ├── 03_parse_copybook.py
│   │   ├── 04_generate_diagrams.py
│   │   ├── 05_analyze_with_claude.py
│   │   └── 06_prioritize.py
│   ├── utils/                    ← 공통 유틸리티 모듈
│   │   ├── cobol_parser.py       ← COBOL 파서 (핵심)
│   │   ├── claude_client.py      ← Claude CLI 호출 모듈
│   │   ├── file_utils.py         ← 파일 I/O 유틸리티
│   │   ├── mermaid_builder.py    ← Mermaid 다이어그램 빌더
│   │   ├── cross_reference.py    ← 교차 참조/영향도 분석
│   │   ├── config_validator.py   ← 설정 파일 검증
│   │   └── logger.py             ← 구조화 로깅 시스템
│   ├── prompts/                  ← Claude 프롬프트 템플릿
│   │   ├── analyze-logic.md      ← 비즈니스 로직 분석용
│   │   └── analyze-data.md       ← 데이터 구조 분석용
│   └── config/
│       └── catm_config.yaml      ← 전체 설정 파일
│
├── output/                       ← 모든 분석 결과물 (자동 생성)
│   ├── docs/                     ← 프로그램별 비즈니스 로직 문서
│   ├── data-dict/                ← COPYBOOK 데이터 사전
│   ├── diagrams/                 ← Mermaid 다이어그램
│   └── reports/                  ← 인벤토리/의존성/우선순위 보고서
│
├── tests/                        ← pytest 테스트
├── dashboard/                    ← 웹 대시보드 (app.js)
├── index.html                    ← 대시보드 진입점
├── docs/                         ← 이 문서들
├── catm.sh                       ← 메인 실행 스크립트
└── pyproject.toml                ← Python 패키지 설정
```

---

## 기술 스택

| 구성요소 | 기술 | 버전 |
|---------|------|------|
| 언어 | Python | 3.11+ |
| AI 엔진 | Claude Code (Max 20x 구독) | CLI |
| COBOL 파싱 | 자체 정규식 파서 | cobol_parser.py |
| 다이어그램 | Mermaid.js | 마크다운 내장 |
| 설정 | YAML | pyyaml 6.0+ |
| 문자 감지 | chardet | 5.0+ |
| 테스트 | pytest | 7.0+ |
| 대시보드 | Vanilla JS + HTML | - |

### Claude Code 호출 방식

```
CATM은 Anthropic API가 아닌 Claude Code Max 20x 구독을 사용합니다.

호출 흐름:
  Python → subprocess → claude -p - --output-format text → stdin으로 프롬프트 전달

특징:
  - API key 불필요 (Max 구독 인증)
  - Rate limit 자동 감지 및 재시도
  - 프로그램당 최대 300초 타임아웃
  - 5개 단위 배치 처리 (배치 간 10초 대기)
```

---

## 용어 정리

| 용어 | 설명 |
|------|------|
| **COBOL** | 메인프레임의 주력 프로그래밍 언어 (Common Business-Oriented Language) |
| **COPYBOOK** | COBOL의 공유 데이터 구조 정의 파일 (C의 헤더파일과 유사) |
| **JCL** | 메인프레임 배치 작업 제어 언어 (Job Control Language) |
| **DCLGEN** | DB2 테이블 구조를 COBOL 변수로 선언한 파일 |
| **VSAM** | 메인프레임의 인덱스 파일 시스템 (Virtual Storage Access Method) |
| **CICS** | 메인프레임 온라인 트랜잭션 처리 시스템 |
| **DB2** | IBM 관계형 데이터베이스 |
| **McCabe 복잡도** | 코드의 분기 수 기반 복잡도 지표 (높을수록 복잡) |
| **MES** | 제조실행시스템 (Manufacturing Execution System) |
| **모더나이제이션** | 레거시 시스템을 현대 기술로 전환하는 과정 |
