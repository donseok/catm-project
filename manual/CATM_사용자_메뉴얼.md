# CATM 사용자 메뉴얼

> **COBOL Analysis Task Manager** — IBM 메인프레임 COBOL 레거시 시스템 자동 분석 도구
>
> 버전: 0.1.0 | 최종 업데이트: 2026-02-14

---

## 목차

1. [개요](#1-개요)
2. [설치 및 환경 설정](#2-설치-및-환경-설정)
3. [소스코드 준비](#3-소스코드-준비)
4. [빠른 시작 (Quick Start)](#4-빠른-시작-quick-start)
5. [6-Phase 파이프라인 상세](#5-6-phase-파이프라인-상세)
6. [설정 가이드](#6-설정-가이드-catm_configyaml)
7. [대시보드 사용법](#7-대시보드-사용법)
8. [출력물 가이드](#8-출력물-가이드)
9. [Claude Code 슬래시 명령어](#9-claude-code-슬래시-명령어)
10. [문제 해결 (Troubleshooting)](#10-문제-해결-troubleshooting)
11. [FAQ](#11-faq)

---

## 1. 개요

### CATM이란?

**CATM** (COBOL Analysis Task Manager)은 IBM 메인프레임의 COBOL 레거시 시스템을 **자동으로 분석**하여 모더나이제이션(현대화) 전환에 필요한 기초 자료를 생성하는 도구입니다.

```
┌─────────────────┐        ┌──────────┐        ┌──────────────────┐
│  메인프레임에서   │        │          │        │  분석 결과물      │
│  추출한 소스코드  │ ────▶  │   CATM   │ ────▶  │  (문서/다이어그램/ │
│  (COBOL/CPY/JCL) │        │          │        │   우선순위 보고서)  │
└─────────────────┘        └──────────┘        └──────────────────┘
```

### 주요 기능

| 기능 | 설명 | 담당 Phase |
|------|------|-----------|
| **인벤토리 스캔** | 소스코드 전체 규모 파악 (프로그램 수, 라인 수, 크기) | Phase 1 |
| **의존성 추출** | CALL/COPY/DB2/VSAM/CICS 관계를 정적 분석으로 자동 추출 | Phase 2 |
| **데이터 사전 생성** | COPYBOOK 파싱으로 필드별 데이터 타입/구조 자동 문서화 | Phase 3 |
| **다이어그램 생성** | 호출관계도, 데이터흐름도, ERD를 Mermaid로 자동 생성 | Phase 4 |
| **비즈니스 로직 분석** | Claude AI가 COBOL 소스를 읽고 한국어 비즈니스 로직 문서 생성 | Phase 5 |
| **우선순위 산정** | 복잡도/의존성/비즈니스 중요도를 종합한 전환 우선순위 자동 산정 | Phase 6 |

### 왜 필요한가?

| 기존 수동 분석 | CATM 자동 분석 |
|--------------|---------------|
| COBOL 전문가가 1인당 하루 1~2개 프로그램 분석 | 수분 내 전체 프로그램 분석 |
| 특정 개인의 지식에 의존, 퇴직 시 지식 소실 | 도구 기반으로 반복 가능 |
| 분석자마다 문서 형식/깊이가 다름 | 동일한 템플릿으로 균일한 분석 결과 |
| 수백 개 프로그램 × 수일/건 = 수개월 소요 | 배치 처리로 대량 분석 |

### 시스템 구성도

```
catm-project/
│
├── src/                          ← 메인프레임에서 추출한 원본 소스 (읽기 전용)
│   ├── cobol/                    ← COBOL 프로그램 (.cbl)
│   ├── copybook/                 ← COPYBOOK (.cpy)
│   ├── jcl/                      ← JCL 파일
│   ├── proc/                     ← PROC 파일
│   ├── dclgen/                   ← DCLGEN 파일
│   └── ddl/                      ← DB2 DDL 파일
│
├── catm/                         ← CATM 분석 봇 코어
│   ├── scripts/                  ← 6-Phase 파이프라인 스크립트
│   ├── utils/                    ← 공통 유틸리티 모듈
│   ├── prompts/                  ← Claude 프롬프트 템플릿
│   └── config/                   ← 설정 파일 (catm_config.yaml)
│
├── output/                       ← 분석 결과물 (자동 생성)
│   ├── docs/                     ← 프로그램별 비즈니스 로직 문서
│   ├── data-dict/                ← COPYBOOK 데이터 사전
│   ├── diagrams/                 ← Mermaid 다이어그램
│   └── reports/                  ← 인벤토리/의존성/우선순위 보고서
│
├── dashboard/                    ← 웹 대시보드 (app.js, style.css)
├── index.html                    ← 대시보드 진입점
├── tests/                        ← pytest 테스트
├── catm.sh                       ← 메인 실행 스크립트
└── pyproject.toml                ← Python 패키지 설정
```

### 기술 스택

| 구성요소 | 기술 | 비고 |
|---------|------|------|
| 언어 | Python 3.11+ | 타입 힌트 사용 |
| AI 엔진 | Claude Code (Max 20x 구독) | CLI 기반 호출 |
| COBOL 파싱 | 자체 정규식 파서 | `cobol_parser.py` |
| 다이어그램 | Mermaid.js | 마크다운 내장 |
| 설정 | YAML | `pyyaml 6.0+` |
| 문자 감지 | chardet | 5.0+ |
| 테스트 | pytest | 7.0+ |
| 대시보드 | Vanilla JS + Chart.js | 별도 프레임워크 없음 |

---

## 2. 설치 및 환경 설정

### 2.1 사전 요구사항

| 항목 | 최소 버전 | 필수 여부 | 확인 명령 |
|------|----------|----------|----------|
| Python | 3.11+ | **필수** | `python --version` |
| pip | 최신 | **필수** | `pip --version` |
| Node.js / npm | 18+ | Phase 5, 6 | `node --version` |
| Claude Code CLI | 최신 | Phase 5, 6 | `claude --version` |
| Claude 구독 | Max 20x | Phase 5, 6 | `claude login` |
| Git | 최신 | 권장 | `git --version` |

> Phase 1~4는 Python만 있으면 실행 가능합니다. Claude Code는 Phase 5(비즈니스 로직 분석)와 Phase 6(우선순위 산정)에서만 필요합니다.

### 2.2 Python 설치

**Windows:**
1. [python.org](https://www.python.org/downloads/)에서 Python 3.11+ 다운로드
2. 설치 시 **"Add Python to PATH"** 반드시 체크
3. 확인: `python --version`

**macOS / Linux:**
```bash
# macOS (Homebrew)
brew install python@3.11

# Ubuntu/Debian
sudo apt update && sudo apt install python3.11 python3.11-venv python3-pip
```

### 2.3 프로젝트 설치

```bash
# 1. 프로젝트 클론
git clone <repository-url> catm-project
cd catm-project

# 2. Python 패키지 설치 (editable mode)
pip install -e .

# 3. (선택) 개발 도구 포함 설치 (pytest)
pip install -e ".[dev]"
```

설치되는 의존성:
- `pyyaml>=6.0` — 설정 파일 파싱
- `chardet>=5.0` — 소스 파일 인코딩 감지
- `pytest>=7.0` — 테스트 (dev 옵션)

### 2.4 Claude Code CLI 설치 (Phase 5, 6 사용 시)

```bash
# 1. Claude Code CLI 설치
npm install -g @anthropic-ai/claude-code

# 2. 로그인 (Claude Max 구독 필요)
claude login
```

> Claude Code는 Anthropic API key가 아닌 **Max 20x 구독** 인증을 사용합니다.

### 2.5 설치 확인

```bash
# Python 확인
python --version        # Python 3.11+ 이상

# CATM 패키지 확인
python -c "import catm; print('OK')"

# Claude Code 확인 (Phase 5, 6 사용 시)
claude --version
```

---

## 3. 소스코드 준비

### 3.1 메인프레임에서 추출할 대상

메인프레임 관리자에게 아래 PDS(Partitioned Data Set)를 요청하세요:

| 구분 | 일반적인 PDS 명명 패턴 | 설명 |
|------|----------------------|------|
| COBOL 소스 | `*.COBOL.SOURCE`, `*.CBL.SOURCE` | 메인 프로그램 |
| COPYBOOK | `*.COPYBOOK.SOURCE`, `*.CPY.SOURCE` | 공통 데이터 구조 |
| JCL | `*.JCL.SOURCE`, `*.JCL.CNTL` | 배치 작업 제어 |
| PROC | `*.PROC.SOURCE`, `*.PROCLIB` | JCL 프로시저 |
| DCLGEN | `*.DCLGEN.SOURCE` | DB2 테이블 COBOL 선언 |
| DB2 DDL | `*.DDL.SOURCE` | 데이터베이스 스키마 |
| CICS MAP | `*.MAP.SOURCE`, `*.BMS.SOURCE` | 화면 정의 |

### 3.2 추출 방법

#### 방법 A: FTP (가장 간단)

```bash
# 1. FTP 접속
ftp mainframe.yourcompany.com

# 2. ASCII 모드 설정 (EBCDIC → ASCII 자동 변환)
ascii

# 3. COBOL 소스 다운로드
cd 'YOUR.COBOL.SOURCE'
lcd src/cobol
mget *

# 4. COPYBOOK 다운로드
cd 'YOUR.COPYBOOK.SOURCE'
lcd ../copybook
mget *

# 5. JCL 다운로드
cd 'YOUR.JCL.SOURCE'
lcd ../jcl
mget *
```

> **중요:** 반드시 `ascii` 모드를 사용하세요. `binary`로 받으면 EBCDIC 인코딩 문제가 발생합니다.

#### 방법 B: z/OSMF REST API (자동화)

```bash
HOST="https://mainframe.yourcompany.com:443"
AUTH=$(echo -n 'userid:password' | base64)

# PDS 멤버 목록 조회 → 각 멤버 다운로드
MEMBERS=$(curl -sk -X GET "$HOST/zosmf/restfiles/ds/YOUR.COBOL.SOURCE/member" \
  -H "Authorization: Basic $AUTH" \
  -H "Accept: application/json" | python3 -c "
import sys, json
data = json.load(sys.stdin)
for m in data.get('items', []):
    print(m['member'])
")

mkdir -p src/cobol
for MEMBER in $MEMBERS; do
    curl -sk -X GET "$HOST/zosmf/restfiles/ds/YOUR.COBOL.SOURCE($MEMBER)" \
      -H "Authorization: Basic $AUTH" \
      -o "src/cobol/${MEMBER}.cbl"
done
```

#### 방법 C: Endevor (형상관리 도구)

Endevor BATCH 추출 JCL을 작성하여 일괄 추출합니다.

#### 방법 D: ISPF 수동 다운로드

TSO 로그인 → ISPF → Option 3.4 (DSLIST) → 멤버 선택 후 FTP 전송

### 3.3 디렉토리 배치

추출한 소스를 아래 구조로 배치합니다:

```
src/
├── cobol/         ← COBOL 프로그램 (.cbl, .cob, .cobol 또는 확장자 없음)
├── copybook/      ← COPYBOOK (.cpy, .copy 또는 확장자 없음)
├── jcl/           ← JCL (.jcl 또는 확장자 없음)
├── proc/          ← PROC (.prc, .proc 또는 확장자 없음)
├── dclgen/        ← DCLGEN (확장자 없음 또는 .dclgen)
└── ddl/           ← DB2 DDL (확장자 없음 또는 .ddl, .sql)
```

### 3.4 인코딩 확인

CATM은 **UTF-8** 인코딩을 기대합니다. FTP `ascii` 모드로 추출했으면 대부분 문제 없습니다.

```bash
# 인코딩 확인
file src/cobol/*

# EBCDIC → UTF-8 변환이 필요한 경우
for f in src/cobol/*; do
    iconv -f EBCDIC-US -t UTF-8 "$f" -o "${f}.tmp" && mv "${f}.tmp" "$f"
done
```

### 3.5 확장자 정리 (선택)

메인프레임 추출 시 확장자가 없을 수 있습니다. CATM은 확장자 없는 파일도 처리하지만, 정리하면 관리가 편합니다.

```bash
# COBOL 소스에 .cbl 확장자 추가
cd src/cobol
for f in *; do
    [ -f "$f" ] && [ "${f##*.}" = "$f" ] && mv "$f" "$f.cbl"
done
```

---

## 4. 빠른 시작 (Quick Start)

### 5분 안에 첫 분석 실행하기

**전제 조건:** Python 3.11+ 설치 완료, `src/cobol/`에 COBOL 소스 1개 이상 배치

```bash
# 1. 패키지 설치 (최초 1회)
pip install -e .

# 2. 전체 파이프라인 실행 (Phase 1~6 순차)
./catm.sh
```

> Phase 5, 6은 Claude Code CLI가 필요합니다. 미설치 시 Phase 1~4만 실행되며, 인벤토리/의존성/데이터 사전/다이어그램 결과를 얻을 수 있습니다.

**실행 결과 확인:**

```bash
# 결과물 위치
ls output/reports/      # 인벤토리, 의존성, 우선순위 보고서
ls output/docs/         # 프로그램별 비즈니스 로직 분석서
ls output/data-dict/    # COPYBOOK 데이터 사전
ls output/diagrams/     # Mermaid 다이어그램

# 대시보드로 시각적 확인
python -m http.server 8000
# 브라우저에서 http://localhost:8000 접속
```

### 단일 프로그램만 빠르게 분석하기

```bash
# Claude Code로 특정 프로그램 1개만 분석
./catm.sh --single src/cobol/PGM001.cbl
```

### Phase별 개별 실행

```bash
./catm.sh --phase 1    # 인벤토리 스캔만
./catm.sh --phase 2    # 의존성 추출만
./catm.sh --phase 5    # Claude 비즈니스 로직 분석만
```

---

## 5. 6-Phase 파이프라인 상세

### 파이프라인 전체 흐름

```
Phase 1          Phase 2          Phase 3          Phase 4
인벤토리 스캔 ──▶ 의존성 추출 ──▶ COPYBOOK 파싱 ──▶ 다이어그램 생성
                      │                                    │
                      ▼                                    ▼
                 Phase 5                            Phase 6
                 Claude AI 분석 ──────────────▶ 우선순위 산정
```

### Phase 간 의존성

| 실행하려는 Phase | 사전에 실행해야 할 Phase |
|----------------|----------------------|
| Phase 1 | (없음) |
| Phase 2 | (없음, `src/` 디렉토리에 소스 필요) |
| Phase 3 | (없음, `src/copybook/`에 파일 필요) |
| Phase 4 | **Phase 2** (`dependency-scan.json` 필요) |
| Phase 5 | **Phase 2** (권장, 교차참조 활용) |
| Phase 6 | **Phase 2** (필수) + **Phase 5** (권장) |

> Phase 1~4는 Claude 없이도 실행 가능합니다.

---

### Phase 1: 인벤토리 스캔

**목적:** 소스코드 전체 규모를 파악합니다.

**실행:**
```bash
./catm.sh --phase 1
# 또는
python catm/scripts/01_scan_inventory.py
```

**분석 대상:** `src/` 하위의 COBOL, COPYBOOK, JCL, PROC, DCLGEN, DDL 파일

**결과물:**

| 파일 | 설명 |
|------|------|
| `output/reports/inventory.json` | 인벤토리 데이터 (JSON) |
| `output/reports/01_inventory_report.md` | 인벤토리 보고서 (마크다운) |

**보고서 내용:**
- 카테고리별 파일 수, 총 라인 수, 총 크기
- COBOL 프로그램 통계 (평균/최대/최소 라인 수)
- 프로그램 목록 (라인 수 내림차순)
- COPYBOOK 목록

---

### Phase 2: 의존성 추출

**목적:** 프로그램 간 관계를 정적 분석으로 자동 추출합니다.

**실행:**
```bash
./catm.sh --phase 2
# 또는
python catm/scripts/02_extract_dependencies.py
```

**추출 항목:**
- CALL 관계 (프로그램 → 프로그램)
- COPY 관계 (프로그램 → COPYBOOK)
- DB2 테이블 참조 (프로그램 → DB2)
- VSAM 파일 참조 (프로그램 → VSAM)
- CICS MAP 참조
- JCL 실행 순서 (JOB → STEP → PGM)
- McCabe 복잡도 (근사치)
- 교차 참조 (어떤 COPYBOOK을 누가 사용하는지)
- 영향도 점수 (변경 시 파급 범위)
- 데드 코드 감지 (참조되지 않는 프로그램/COPYBOOK)

**결과물:**

| 파일 | 설명 |
|------|------|
| `output/reports/dependency-scan.json` | 의존성 데이터 (핵심 데이터 파일) |

**JSON 구조 예시:**
```json
{
  "programs": [
    {
      "name": "PGM001",
      "calls": ["ABNDPGM", "ERRLOG", "SQLERR"],
      "copies": ["CPYSMRY", "CPYTRANS"],
      "db2_tables": ["TB_DAILY_PROD"],
      "vsam_files": ["DLYSMRY", "PRODTRAN"],
      "complexity": 12,
      "category": "조업업무"
    }
  ],
  "cross_reference": { ... },
  "impact_scores": { ... },
  "dead_code": { ... }
}
```

---

### Phase 3: COPYBOOK 파싱

**목적:** COPYBOOK 파일을 파싱하여 필드별 데이터 사전을 생성합니다.

**실행:**
```bash
./catm.sh --phase 3
# 또는
python catm/scripts/03_parse_copybook.py
```

**결과물:**

| 파일 | 설명 |
|------|------|
| `output/data-dict/{COPYBOOK명}.md` | COPYBOOK별 데이터 사전 |
| `output/diagrams/erd_copybooks.md` | ERD 다이어그램 (Mermaid) |

**데이터 사전 구조:**

```
| Lv | 필드명 | PIC | 데이터 타입 | OCCURS | REDEFINES | 기본값 |
```

- **Lv 01~05**: 상위 구조 (그룹 레벨)
- **PIC X**: 문자열, **PIC 9**: 숫자, **PIC S9V99**: 부호 소수점
- **COMP-3**: 패킹 십진수 (마이그레이션 시 변환 필요)
- **OCCURS**: 배열 (설계 시 별도 테이블 분리 검토)
- **REDEFINES**: 동일 메모리 재정의 (데이터 해석 주의)

---

### Phase 4: 다이어그램 생성

**목적:** `dependency-scan.json`을 기반으로 Mermaid 다이어그램을 자동 생성합니다.

**실행:**
```bash
./catm.sh --phase 4
# 또는
python catm/scripts/04_generate_diagrams.py
```

> **전제:** Phase 2 실행 완료 (`dependency-scan.json` 필요)

**결과물:**

| 파일 | 설명 | 비활성화 설정 |
|------|------|-------------|
| `output/diagrams/call_graph.md` | 호출관계 그래프 | `generate_diagrams.call_graph: false` |
| `output/diagrams/data_flow.md` | 데이터 흐름도 | `generate_diagrams.data_flow: false` |
| `output/diagrams/jcl_flow.md` | JCL 배치 흐름도 | `generate_diagrams.jcl_flow: false` |
| `output/diagrams/README.md` | 다이어그램 목차 | — |

**다이어그램 읽는 법:**

| 다이어그램 | 노드/엣지 의미 |
|-----------|---------------|
| **호출관계 그래프** | 실선(→) = CALL, 점선(-..->) = COPY, 양방향(<-->) = DB2 |
| **데이터 흐름도** | SQL 라벨 = DB2, I/O 라벨 = VSAM, 둥근 사각형 = DB2 테이블 |
| **JCL 흐름도** | 배치 JOB → STEP → PGM 실행 순서 |

> Mermaid 다이어그램은 GitHub, VS Code, Notion, 대시보드 등에서 자동 렌더링됩니다.

---

### Phase 5: Claude 비즈니스 로직 분석

**목적:** Claude AI가 COBOL 소스코드를 읽고 한국어로 비즈니스 로직 문서를 생성합니다.

**실행:**
```bash
# 전체 프로그램 분석
./catm.sh --phase 5
# 또는
python catm/scripts/05_analyze_with_claude.py

# 단일 프로그램 분석
python catm/scripts/05_analyze_with_claude.py --single src/cobol/PGM001.cbl

# 특정 프로그램만 분석
python catm/scripts/05_analyze_with_claude.py --names PGM001,PGM002,PGM003

# 데이터 구조 분석 모드 (COPYBOOK/DCLGEN)
python catm/scripts/05_analyze_with_claude.py --mode data
```

> **필요:** Claude Code CLI 설치 + Max 20x 구독 로그인

**CLI 옵션:**

| 옵션 | 설명 | 예시 |
|------|------|------|
| (없음) | 전체 프로그램 분석 | `python catm/scripts/05_analyze_with_claude.py` |
| `--single FILE` | 단일 파일 분석 | `--single src/cobol/PGM001.cbl` |
| `--names LIST` | 특정 프로그램만 | `--names PGM001,PGM002` |
| `--mode logic` | 비즈니스 로직 분석 (기본값) | `--mode logic` |
| `--mode data` | 데이터 구조 분석 | `--mode data` |

**분석 과정:**
1. COBOL 소스 파일 로드
2. 참조하는 COPYBOOK 자동 인라인 확장
3. 파서 메타데이터 생성 (복잡도, 의존성, 카테고리 등)
4. 프롬프트 템플릿 + 소스코드 + 메타데이터를 Claude에 전달
5. Claude 응답을 마크다운 문서로 저장
6. 배치 처리 (5개 단위, 배치 간 10초 대기)

**결과물:**

| 파일 | 설명 |
|------|------|
| `output/docs/{프로그램명}.md` | 프로그램별 비즈니스 로직 분석서 |
| `output/reports/analysis_log.json` | 분석 실행 기록 |

**분석서 구조:**
1. 프로그램 개요 (ID, 목적, 처리 유형, 업무 카테고리)
2. 데이터 흐름 (입출력, COPYBOOK, 호출 관계)
3. 비즈니스 로직 상세 (SECTION/PARAGRAPH별 설명)
4. 비즈니스 규칙 요약
5. MES 관련도 평가
6. 특이사항/리스크

---

### Phase 6: 모더나이제이션 우선순위 산정

**목적:** 정적 분석 + Claude 분석 결과를 종합하여 프로그램별 전환 우선순위를 산정합니다.

**실행:**
```bash
./catm.sh --phase 6
# 또는
python catm/scripts/06_prioritize.py
```

> **전제:** Phase 2 필수, Phase 5 권장

**점수 산정 기준:**

| 지표 | 가중치 | 높은 점수 (10) | 낮은 점수 (1) |
|------|-------|-------------|-------------|
| 비즈니스 중요도 | 35% | 핵심 생산/품질 업무 | 보조/참조 업무 |
| 기술 복잡도 | 25% | 분기 많고 복잡 | 단순 순차 처리 |
| 의존성 영향도 | 20% | 많은 프로그램이 의존 | 독립적 |
| 전환 용이도 | 20% | 쉽게 변환 가능 | CICS/복잡도로 어려움 |

**전환 방식 분류:**

| 방식 | 의미 | 적용 조건 |
|------|------|----------|
| **자동 변환** | 도구 기반 코드 변환 | 단순 로직, CICS 미사용 |
| **하이브리드** | 자동 변환 + 수동 보정 | 중간 복잡도 |
| **리라이트** | 완전 새로 개발 | CICS 사용 또는 매우 복잡 |

**Phase 분류 (최종 점수 기준):**

| Phase | 점수 범위 | 의미 |
|-------|----------|------|
| Phase 1 | final ≥ 7 | 즉시 전환 (가장 높은 우선순위) |
| Phase 2 | 4 ≤ final < 7 | 3개월 내 전환 |
| Phase 3 | final < 4 | 6개월 내 전환 |

> 점수가 높을수록 모더나이제이션 우선순위가 높습니다 (비즈니스 중요도, 기술 복잡도, 의존성 영향도가 클수록 먼저 전환).

**결과물:**

| 파일 | 설명 |
|------|------|
| `output/reports/priority_matrix.md` | 우선순위 매트릭스 (의사결정 핵심 문서) |
| `output/reports/priority_data.json` | 점수 데이터 (JSON) |
| `output/reports/claude_priority_analysis.md` | Claude 원본 분석 |

---

## 6. 설정 가이드 (catm_config.yaml)

설정 파일 위치: `catm/config/catm_config.yaml`

> `load_config()` 함수가 `Path(__file__)` 기준으로 자동 탐색하므로, 어느 디렉토리에서 실행해도 올바르게 로드됩니다.

### 6.1 경로 설정 (paths)

```yaml
paths:
  source_root: "./src"            # 소스코드 루트 디렉토리
  output_root: "./output"         # 분석 결과 출력 디렉토리
  prompts_dir: "./catm/prompts"   # 프롬프트 템플릿 디렉토리
```

### 6.2 소스 디렉토리 매핑 (source_dirs)

```yaml
source_dirs:
  cobol: "cobol"          # *.cbl, *.cob
  copybook: "copybook"    # *.cpy
  jcl: "jcl"              # *.jcl
  proc: "proc"            # *.prc
  dclgen: "dclgen"        # DCLGEN 파일
  ddl: "ddl"              # DB2 DDL
  map: "map"              # CICS BMS MAP
```

> 실제 경로: `source_root` + `source_dirs.xxx` (예: `./src/cobol/`)

### 6.3 파일 확장자 매핑 (file_extensions)

```yaml
file_extensions:
  cobol: [".cbl", ".cob", ".cobol", ""]    # "" = 확장자 없는 파일 포함
  copybook: [".cpy", ".copy", ""]
  jcl: [".jcl", ""]
  proc: [".prc", ".proc", ""]
```

> 빈 문자열 `""`은 확장자 없는 파일도 수집한다는 뜻입니다 (메인프레임 직접 추출 시 유용).

### 6.4 Claude Code 설정 (claude)

```yaml
claude:
  mode: "cli"                    # 실행 모드 (cli = Max 구독)
  cli_command: "claude"          # Claude CLI 경로 (PATH에 없으면 절대 경로)
  max_concurrent: 3              # 동시 분석 제한
  timeout_per_program: 300       # 프로그램당 타임아웃 (초)
  max_retries: 3                 # 최대 재시도 횟수
  retry_delay_seconds: 30        # 재시도 대기 (초, 지수 백오프)
  batch_size: 5                  # 배치 단위 (N개마다 대기)
  batch_delay_seconds: 10        # 배치 간 대기 (초)
```

**조정 가이드:**

| 상황 | 권장 조정 |
|------|----------|
| 대형 프로그램 (500줄+) | `timeout_per_program: 600` |
| Rate limit 빈발 | `batch_delay_seconds: 30`, `max_retries: 5` |
| Windows에서 CLI 못 찾음 | `cli_command: "C:/Users/사용자/AppData/Roaming/npm/claude.cmd"` |

### 6.5 정적 분석 설정 (static_analysis)

```yaml
static_analysis:
  cobol_format: "fixed"       # fixed (1-80) 또는 free
  code_start_col: 7           # 코드 시작 컬럼 (0-based)
  code_end_col: 72            # 코드 끝 컬럼
  complexity_threshold:
    low: 10                   # 낮음 기준
    medium: 25                # 중간 기준
    high: 50                  # 높음 기준
```

| 복잡도 범위 | 분류 | 의미 |
|-----------|------|------|
| 1~10 | 낮음 | 단순한 순차 처리 |
| 11~25 | 중간 | 조건 분기가 있는 일반 프로그램 |
| 26~50 | 높음 | 복잡한 비즈니스 로직 |
| 50+ | 매우 높음 | 리라이트 권장 |

### 6.6 우선순위 가중치 (priority_weights)

```yaml
priority_weights:
  business_importance: 0.35   # 비즈니스 중요도 (35%)
  technical_complexity: 0.25  # 기술 복잡도 (25%)
  dependency_impact: 0.20     # 의존성 영향도 (20%)
  conversion_ease: 0.20       # 전환 용이도 (20%)
```

> **합계는 반드시 1.00 이어야 합니다.**

**시나리오별 조정 예시:**

| 시나리오 | 권장 조정 |
|---------|----------|
| 비즈니스 관점 우선 | `business_importance: 0.45`, 나머지 축소 |
| 기술 리스크 우선 | `technical_complexity: 0.35`, 나머지 축소 |
| 빠른 전환 우선 | `conversion_ease: 0.35`, 나머지 축소 |

### 6.7 출력 설정 (output)

```yaml
output:
  docs_dir: "docs"             # 비즈니스 로직 문서
  diagrams_dir: "diagrams"     # 다이어그램
  data_dict_dir: "data-dict"   # 데이터 사전
  reports_dir: "reports"       # 보고서
  mermaid_theme: "default"     # Mermaid 테마
  generate_diagrams:
    call_graph: true           # 호출관계 그래프
    jcl_flow: true             # JCL 배치 흐름도
    data_flow: true            # 데이터 흐름도
    erd: true                  # ERD
```

### 6.8 프로그램 업무 카테고리 (program_categories)

```yaml
program_categories:
  - name: "조업업무"
    programs: ["PGM001", "PGM005"]
  - name: "출하업무"
    programs: ["PGM002"]
  - name: "품질관리"
    programs: ["PGM003"]
  - name: "생산관리"
    programs: ["PGM004"]
  - name: "원자재관리"
    programs: ["PGM006"]
```

> 매핑되지 않은 프로그램은 `"미분류"`로 표시됩니다.

**새 프로그램 추가 시:**
1. `src/cobol/`에 COBOL 소스 배치
2. `program_categories`에 프로그램 추가
3. `./catm.sh` 재실행

### 6.9 로그 설정 (logging)

```yaml
logging:
  level: "INFO"               # DEBUG, INFO, WARNING, ERROR
  file: "./output/catm.log"   # 로그 파일 경로 (null이면 비활성화)
  console: true               # 콘솔 출력 여부
```

### 6.10 새 고객사 적용 체크리스트

1. `paths.source_root` → 고객 소스 경로로 변경
2. `file_extensions` → 고객 소스 확장자에 맞게 조정
3. `program_categories` → 고객 업무 기준으로 카테고리 재정의
4. `claude.cli_command` → 실행 환경의 Claude 경로 확인
5. `priority_weights` → 고객 요구사항에 맞게 가중치 조정

---

## 7. 대시보드 사용법

### 7.1 대시보드 실행

```bash
# 로컬 서버 시작
python -m http.server 8000

# 브라우저에서 접속
# http://localhost:8000
```

> `index.html`을 직접 열어도 되지만, 로컬 서버를 통해 열어야 데이터 파일(`output/` 하위 JSON)을 정상적으로 로드합니다.

**필요한 데이터 파일:**
- `output/reports/dependency-scan.json` (Phase 2 결과)
- `output/reports/priority_data.json` (Phase 6 결과)
- `output/reports/inventory.json` (Phase 1 결과, 선택)
- `output/reports/analysis_log.json` (Phase 5 결과, 선택)
- `output/docs/*.md` (Phase 5 결과, 선택)

### 7.2 6개 탭 상세 가이드

#### 탭 1: 개요

프로젝트 전체 현황을 한눈에 보여주는 메인 화면입니다.

**요약 카드 (6개):**
- 총 프로그램 수
- 총 라인 수
- 평균 복잡도
- DB2 프로그램 수
- COPYBOOK 수
- VSAM 파일 수

**추가 정보:**
- 카테고리별 요약 카드 + 도넛 차트
- 리스크 요약 (분석 문서에서 추출)
- 마지막 스캔 날짜

#### 탭 2: 프로그램 상세

모든 COBOL 프로그램의 상세 정보를 테이블로 표시합니다.

**표시 항목:** 프로그램명, 카테고리(배지), 라인 수, 복잡도, DB2/CICS/VSAM 여부, 호출 관계, COPYBOOK 참조, 최종 점수, 전환 방식

**기능:**
- 프로그램 행 클릭 → 비즈니스 로직 분석서(MD) 모달 뷰어로 열기
- 카테고리/기술/Phase 필터링
- 복잡도/라인 수 범위 필터
- 정렬 (이름순, 복잡도순, 점수순, 카테고리순)
- 페이지네이션 (대량 프로그램 대응)

#### 탭 3: 데이터 사전

COPYBOOK별 데이터 사전을 표시합니다.

**표시 항목:** 각 COPYBOOK의 필드 구조 (레벨, 필드명, PIC, 데이터 타입, OCCURS, REDEFINES)

#### 탭 4: 모더나이제이션

전환 우선순위 분석 결과를 시각적으로 보여줍니다.

**표시 항목:**
- Phase별 프로그램 분류 (Phase 1: 즉시 / Phase 2: 3개월 / Phase 3: 6개월)
- 프로그램별 점수 상세 (비즈니스 중요도, 기술 복잡도, 의존성 영향도, 전환 용이도)
- 전환 방식 (자동 변환 / 하이브리드 / 리라이트)

#### 탭 5: 의존성 그래프

프로그램 간 관계를 Mermaid 다이어그램으로 시각화합니다.

**뷰 전환:**
- **호출관계(Call Graph)**: 프로그램 간 CALL/COPY/DB2 관계
- **데이터 흐름(Data Flow)**: 프로그램이 어떤 데이터를 읽고 쓰는지

#### 탭 6: 교차 참조

공유 자원(COPYBOOK, 서브프로그램, DB2 테이블)의 참조 관계를 분석합니다.

**표시 항목:**
- COPYBOOK 사용 현황 (어떤 프로그램이 사용하는지)
- 서브프로그램 호출 현황 (누가 호출하는지)
- 영향도 점수 (변경 시 파급 범위)
- 데드 코드 감지 결과

### 7.3 역할별 뷰

개요 탭에서 **뷰 전환** 버튼으로 역할에 맞는 화면을 선택할 수 있습니다:

| 역할 | 초점 | 주로 보는 탭 |
|------|------|------------|
| **경영진** | 진행률, 리스크, 마일스톤 | 개요, 모더나이제이션 |
| **기획자** | Phase별 분류, 우선순위 | 개요, 모더나이제이션, 프로그램 상세 |
| **설계자** | 의존성, 기술 부채 | 의존성 그래프, 교차 참조, 데이터 사전 |
| **개발자** | 작업 체크리스트, 상세 구현 | 프로그램 상세, 데이터 사전, 교차 참조 |

### 7.4 테마 전환

헤더 우측의 테마 전환 버튼(해/달 아이콘)으로 **라이트 모드**와 **다크 모드**를 전환할 수 있습니다.

### 7.5 필터링 및 검색

프로그램 상세 탭에서 다음 필터를 사용할 수 있습니다:

- **카테고리 필터**: 특정 업무 카테고리만 표시
- **기술 필터**: DB2/CICS/VSAM 사용 여부
- **Phase 필터**: Phase 1/2/3 분류
- **복잡도 범위**: 최소~최대 복잡도
- **라인 수 범위**: 최소~최대 라인 수

---

## 8. 출력물 가이드

### 8.1 전체 출력물 맵

```
output/
├── reports/                          ← 정량 데이터 + 보고서
│   ├── inventory.json                   Phase 1: 인벤토리 데이터
│   ├── 01_inventory_report.md           Phase 1: 인벤토리 보고서
│   ├── dependency-scan.json             Phase 2: 의존성 데이터 (핵심)
│   ├── analysis_log.json                Phase 5: 분석 실행 기록
│   ├── priority_matrix.md               Phase 6: 우선순위 매트릭스
│   ├── priority_data.json               Phase 6: 점수 데이터
│   └── claude_priority_analysis.md      Phase 6: Claude 원본 분석
│
├── docs/                             ← 프로그램별 비즈니스 로직 분석서
│   ├── PGM001.md                        각 프로그램별 분석 문서
│   └── ...
│
├── data-dict/                        ← COPYBOOK 데이터 사전
│   ├── CPYTRANS.md                      각 COPYBOOK별 필드 정의
│   └── ...
│
└── diagrams/                         ← Mermaid 다이어그램
    ├── call_graph.md                    호출관계 그래프
    ├── data_flow.md                     데이터 흐름도
    ├── jcl_flow.md                      JCL 배치 흐름도
    ├── erd_copybooks.md                 ERD
    └── README.md                        다이어그램 목차
```

### 8.2 역할별 활용 가이드

#### 기획자: 전환 로드맵 수립

1. `priority_matrix.md` 확인 → Phase별 프로그램 분류
2. 각 프로그램의 `docs/*.md` 확인 → 업무 범위 파악
3. 전환 방식별(자동변환/하이브리드/리라이트) 리소스 계획
4. `dependency-scan.json`의 교차참조 → 전환 순서 최적화

#### 분석가: 요구사항 정의

1. `docs/*.md`의 "비즈니스 규칙 요약" → 기능 요구사항 도출
2. `data-dict/*.md` → 데이터 요구사항 도출
3. `diagrams/data_flow.md` → 인터페이스 요구사항 도출
4. `docs/*.md`의 "MES 관련도" → 비기능 요구사항 도출

#### 설계자: 신규 시스템 설계

1. `diagrams/call_graph.md` → 서비스/모듈 경계 설계
2. `diagrams/erd_copybooks.md` → 데이터 모델 설계
3. `dependency-scan.json`의 `impact_scores` → 공통 모듈 식별
4. `docs/*.md`의 "리스크" → 설계 시 고려사항 반영

#### 개발자: 변경 영향도 분석

1. `dependency-scan.json`에서 해당 프로그램의 의존성 확인
2. `cross_reference`로 역방향 참조 확인
3. `impact_scores`로 파급 범위 정량 파악
4. 영향받는 프로그램의 `docs/*.md`로 비즈니스 영향 평가

---

## 9. Claude Code 슬래시 명령어

CATM은 Claude Code CLI 내에서 사용할 수 있는 슬래시 명령어를 제공합니다. `claude` 세션에서 직접 입력합니다.

### 명령어 목록

| 명령어 | 설명 | 사용 예시 |
|--------|------|----------|
| `/catm-run` | 전체 파이프라인 실행 (Phase 1-6) 또는 특정 Phase | `/catm-run` 또는 `/catm-run 3` |
| `/catm-scan` | 인벤토리 스캔 (Phase 1) | `/catm-scan` |
| `/catm-deps` | 의존성 추출 + 다이어그램 생성 (Phase 2+4) | `/catm-deps` |
| `/catm-analyze` | 단일 프로그램 Claude 분석 (Phase 5) | `/catm-analyze src/cobol/PGM001.cbl` |
| `/catm-priority` | 우선순위 산정 (Phase 6) | `/catm-priority` |
| `/catm-test` | 프로젝트 테스트 실행 | `/catm-test` 또는 `/catm-test tests/test_cobol_parser.py` |

### 사용법 상세

#### /catm-run — 파이프라인 실행

```
/catm-run          # 전체 파이프라인 (Phase 1-6)
/catm-run 1        # Phase 1만 실행
/catm-run 5        # Phase 5만 실행
```

#### /catm-scan — 인벤토리 스캔

```
/catm-scan         # Phase 1 실행 후 요약 보고
```

실행 후 `output/reports/inventory.json` 결과를 읽어 COBOL 프로그램 수, 총 라인 수 등 핵심 지표를 보고합니다.

#### /catm-deps — 의존성 추출 + 다이어그램

```
/catm-deps         # Phase 2 + Phase 4 순차 실행
```

의존성 추출 후 Mermaid 다이어그램까지 자동 생성합니다.

#### /catm-analyze — Claude 분석

```
/catm-analyze src/cobol/PGM001.cbl     # COBOL 프로그램 분석
/catm-analyze src/copybook/CPYTRANS.cpy # COPYBOOK 데이터 분석
```

`.cbl`/`.cob` 파일은 logic 모드, `.cpy`/`.dclgen` 파일은 data 모드로 자동 판별합니다.

#### /catm-priority — 우선순위 산정

```
/catm-priority     # Phase 6 실행 후 우선순위 매트릭스 보고
```

#### /catm-test — 테스트 실행

```
/catm-test                              # 전체 테스트
/catm-test tests/test_cobol_parser.py   # 특정 테스트 파일만
```

---

## 10. 문제 해결 (Troubleshooting)

### 10.1 설치 관련

| 증상 | 원인 | 해결 |
|------|------|------|
| `import catm` 실패 | 패키지 미설치 | `pip install -e .` 실행 |
| `pip install -e .` 실패 | Python 3.11 미만 | Python 3.11 이상 설치 |
| YAML 파싱 에러 | 설정 파일 문법 오류 | YAML 들여쓰기 확인 (탭 대신 스페이스) |

### 10.2 Claude Code 관련

| 증상 | 원인 | 해결 |
|------|------|------|
| `claude: command not found` | Claude Code 미설치 | `npm install -g @anthropic-ai/claude-code` |
| Rate limit 반복 발생 | 과도한 호출 빈도 | `catm_config.yaml`에서 `batch_delay_seconds`를 30 이상으로 증가 |
| 타임아웃 발생 | 대형 프로그램 분석 시간 초과 | `timeout_per_program`을 600으로 증가 |
| 분석 결과가 비어있음 | CLI 경로 문제 | `catm_config.yaml`의 `cli_command` 절대 경로 확인 |
| Windows에서 claude 못 찾음 | npm 글로벌 경로 | `cli_command: "C:/Users/사용자/AppData/Roaming/npm/claude.cmd"` |

### 10.3 COBOL 파서 관련

| 증상 | 원인 | 해결 |
|------|------|------|
| COPYBOOK 필드 0개 파싱 | 비표준 COBOL 형식 또는 인코딩 | 소스 인코딩을 UTF-8로 변환 |
| 복잡도가 항상 1 | 주석 처리 오류 | 소스의 컬럼 7 표시자(*)가 올바른지 확인 |
| 파일을 찾을 수 없음 | 확장자 불일치 | `catm_config.yaml`의 `file_extensions`에 해당 확장자 추가 |
| PARAGRAPH 수 0 | PROCEDURE DIVISION 없음 | 소스가 완전한 COBOL 프로그램인지 확인 |

### 10.4 대시보드 관련

| 증상 | 원인 | 해결 |
|------|------|------|
| 데이터가 로드되지 않음 | CORS 차단 (파일 직접 열기) | `python -m http.server 8000`으로 로컬 서버 사용 |
| "데이터 로드 오류" | JSON 파일 없음 | Phase 2, 6 실행 완료 확인 |
| 다이어그램 미표시 | Mermaid CDN 접속 불가 | 인터넷 연결 확인 |
| 차트 미표시 | Chart.js CDN 접속 불가 | 인터넷 연결 확인 |

### 10.5 일반

| 증상 | 원인 | 해결 |
|------|------|------|
| 결과 파일이 생성되지 않음 | `output/` 디렉토리 쓰기 권한 | 디렉토리 권한 확인 |
| catm.sh 실행 오류 (Windows) | bash 미설치 | Git Bash 또는 WSL 사용 |
| `set -e`로 중간 중단 | 특정 Phase 에러 | 에러 로그 확인 후 해당 Phase 단독 재실행 |

---

## 11. FAQ

### 일반

**Q: CATM은 어떤 COBOL 시스템을 분석할 수 있나요?**

A: IBM 메인프레임의 표준 COBOL (fixed format, 컬럼 1-80) 프로그램을 분석합니다. COPYBOOK, JCL, DCLGEN, DB2 DDL도 지원합니다. CICS 온라인 프로그램과 배치 프로그램 모두 분석 가능합니다.

**Q: Claude Code 구독 없이도 사용할 수 있나요?**

A: Phase 1~4는 Claude 없이 실행 가능합니다. 인벤토리, 의존성 분석, 데이터 사전, 다이어그램 생성까지 얻을 수 있습니다. Phase 5(비즈니스 로직 분석)와 Phase 6(우선순위 산정)만 Claude Code Max 구독이 필요합니다.

**Q: 분석 가능한 프로그램 규모에 제한이 있나요?**

A: 기술적 제한은 없습니다. 프로그램 수가 많을 경우 `batch_size`와 `batch_delay_seconds`를 조정하여 Rate limit을 관리하면 됩니다. 대형 시스템(수백 개)의 경우 Phase 5 분석에 수시간이 소요될 수 있습니다.

### 설치 및 실행

**Q: Windows에서도 동작하나요?**

A: 네. Python 스크립트는 OS 무관하게 동작합니다. `catm.sh`는 bash 스크립트이므로 Git Bash 또는 WSL에서 실행하세요. 또는 Python 스크립트를 직접 실행할 수 있습니다:
```bash
python catm/scripts/01_scan_inventory.py
```

**Q: API key가 필요한가요?**

A: 아닙니다. CATM은 Anthropic API가 아닌 **Claude Code Max 20x 구독**을 사용합니다. `claude login`으로 인증하면 API key 없이 사용 가능합니다.

**Q: 소스코드가 EBCDIC 인코딩인데 어떻게 하나요?**

A: FTP `ascii` 모드로 다운로드하면 자동 변환됩니다. 그래도 문제가 있으면 `iconv`로 수동 변환하세요:
```bash
iconv -f EBCDIC-US -t UTF-8 소스파일 -o 소스파일.tmp && mv 소스파일.tmp 소스파일
```

### 분석 결과

**Q: Claude가 분석한 비즈니스 로직의 정확도는?**

A: Claude AI의 분석은 정적 분석 메타데이터(복잡도, 의존성, 카테고리)와 COPYBOOK 컨텍스트를 함께 제공하여 정확도를 높입니다. 다만, **반드시 현업 COBOL 전문가의 검증이 필요합니다**. 특히 비즈니스 규칙 요약과 MES 관련도 평가는 전문가 확인을 권장합니다.

**Q: 우선순위 점수를 커스터마이징할 수 있나요?**

A: `catm_config.yaml`의 `priority_weights`에서 4개 지표의 가중치를 조정할 수 있습니다. 가중치 합계는 1.00이어야 합니다. 변경 후 Phase 6을 재실행하면 새 가중치가 적용됩니다.

**Q: 분석 결과를 다른 도구에서 활용할 수 있나요?**

A: 모든 데이터는 JSON과 마크다운으로 생성되므로 다양한 도구와 연동 가능합니다:
- JSON 파일 → BI 도구, 스프레드시트, 커스텀 스크립트
- 마크다운 → Confluence, Notion, GitHub Wiki
- Mermaid 다이어그램 → GitHub, VS Code, Notion 자동 렌더링

### 대시보드

**Q: 대시보드를 외부에 공유할 수 있나요?**

A: 대시보드는 순수 HTML/JS로 만들어져 별도 서버 설치 없이 정적 파일로 배포 가능합니다. `index.html`, `dashboard/` 폴더, `output/` 폴더를 함께 배포하면 됩니다. 단, Chart.js와 Mermaid.js는 CDN에서 로드하므로 인터넷 접속이 필요합니다.

**Q: 대시보드에서 분석 문서를 볼 수 있나요?**

A: 프로그램 상세 탭에서 프로그램 행을 클릭하면 해당 프로그램의 비즈니스 로직 분석서(MD)가 모달 뷰어로 열립니다. 마크다운이 렌더링되어 읽기 쉽게 표시됩니다.

### 유지보수

**Q: 소스코드가 업데이트되면 어떻게 하나요?**

A: 업데이트된 소스를 `src/` 디렉토리에 다시 배치하고 `./catm.sh`를 재실행하면 됩니다. 기존 결과물은 덮어씁니다.

**Q: 새 프로그램을 추가하려면?**

A: 3단계입니다:
1. `src/cobol/`에 COBOL 소스 배치
2. `catm_config.yaml`의 `program_categories`에 프로그램-카테고리 매핑 추가
3. `./catm.sh` 재실행

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
| **PDS** | 메인프레임의 파티션 데이터셋 (라이브러리 형태의 파일 저장소) |
| **Mermaid** | 텍스트 기반 다이어그램 생성 도구 |
