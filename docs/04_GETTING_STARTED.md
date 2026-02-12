# 04. 설치 및 실행 가이드

> 대상: **전체**

---

## 사전 요구사항

| 항목 | 최소 버전 | 필수 여부 | 확인 명령 |
|------|----------|----------|----------|
| Python | 3.11+ | 필수 | `python --version` |
| pip | 최신 | 필수 | `pip --version` |
| Claude Code | 최신 | Phase 5, 6만 | `claude --version` |
| Claude 구독 | Max 20x | Phase 5, 6만 | `claude login` |
| Git | 최신 | 권장 | `git --version` |

---

## 설치 단계

### 1. 프로젝트 클론

```bash
git clone <repository-url> catm-project
cd catm-project
```

### 2. Python 패키지 설치

```bash
# editable mode 설치 (개발 + 실행 통합)
pip install -e .

# 개발 도구 포함 설치 (pytest)
pip install -e ".[dev]"
```

설치되는 의존성:
- `pyyaml>=6.0` — 설정 파일 파싱
- `chardet>=5.0` — 소스 파일 인코딩 감지
- `pytest>=7.0` — 테스트 (dev)

### 3. Claude Code 설치 (Phase 5, 6 사용 시)

```bash
# Claude Code CLI 설치
npm install -g @anthropic-ai/claude-code

# 로그인
claude login
```

### 4. 소스코드 배치

메인프레임에서 추출한 소스코드를 아래 디렉토리에 배치합니다:

```
src/
├── cobol/         ← COBOL 프로그램 (.cbl, .cob, .cobol 또는 확장자 없음)
├── copybook/      ← COPYBOOK (.cpy, .copy 또는 확장자 없음)
├── jcl/           ← JCL (.jcl 또는 확장자 없음)
├── proc/          ← PROC (.prc, .proc 또는 확장자 없음)
├── dclgen/        ← DCLGEN (확장자 없음 또는 .dclgen)
└── ddl/           ← DB2 DDL (확장자 없음 또는 .ddl, .sql)
```

> 소스 추출 시 참고사항은 [고객 체크리스트](./CUSTOMER_CHECKLIST.md)를 확인하세요.

---

## 실행 방법

### 방법 1: 전체 자동 실행 (권장)

```bash
./catm.sh
```

Phase 1~6을 순차적으로 자동 실행합니다.

### 방법 2: Phase별 개별 실행

```bash
./catm.sh --phase 1    # 인벤토리 스캔
./catm.sh --phase 2    # 의존성 추출
./catm.sh --phase 3    # COPYBOOK 파싱
./catm.sh --phase 4    # 다이어그램 생성
./catm.sh --phase 5    # Claude AI 비즈니스 로직 분석
./catm.sh --phase 6    # 우선순위 산정
```

### 방법 3: Python 직접 실행

```bash
python catm/scripts/01_scan_inventory.py
python catm/scripts/02_extract_dependencies.py
python catm/scripts/03_parse_copybook.py
python catm/scripts/04_generate_diagrams.py
python catm/scripts/05_analyze_with_claude.py
python catm/scripts/06_prioritize.py
```

### 방법 4: 단일 프로그램 분석

```bash
# CLI 방식
./catm.sh --single src/cobol/PGM001.cbl

# Python 방식
python catm/scripts/05_analyze_with_claude.py --single src/cobol/PGM001.cbl
```

### 방법 5: 특정 프로그램만 분석

```bash
python catm/scripts/05_analyze_with_claude.py --names PGM001,PGM002,PGM003
```

### 방법 6: 데이터 구조 분석

```bash
python catm/scripts/05_analyze_with_claude.py --mode data
```

---

## 실행 순서 가이드

### Claude 없이 실행 가능한 Phase

```
Phase 1 → Phase 2 → Phase 3 → Phase 4
```

이 4개 Phase는 Claude 없이도 동작합니다. 인벤토리, 의존성, 데이터 사전, 다이어그램을 얻을 수 있습니다.

### Claude가 필요한 Phase

```
Phase 5 (비즈니스 로직 분석) — Claude Code 필수
Phase 6 (우선순위 산정) — Claude Code 필수
```

### Phase 간 의존성 주의

| 실행하려는 Phase | 사전에 실행해야 할 Phase |
|----------------|----------------------|
| Phase 1 | (없음) |
| Phase 2 | (없음, src/ 디렉토리에 소스 필요) |
| Phase 3 | (없음, src/copybook/ 에 파일 필요) |
| Phase 4 | **Phase 2** (`dependency-scan.json` 필요) |
| Phase 5 | **Phase 2** (권장, 교차참조 활용) |
| Phase 6 | **Phase 2** (필수) + **Phase 5** (권장) |

---

## 결과 확인

### 산출물 위치

```
output/
├── reports/
│   ├── inventory.json              ← Phase 1: 인벤토리 데이터
│   ├── 01_inventory_report.md      ← Phase 1: 인벤토리 보고서
│   ├── dependency-scan.json        ← Phase 2: 의존성 데이터 (핵심)
│   ├── analysis_log.json           ← Phase 5: 분석 실행 로그
│   ├── priority_matrix.md          ← Phase 6: 우선순위 매트릭스
│   ├── priority_data.json          ← Phase 6: 점수 데이터
│   └── claude_priority_analysis.md ← Phase 6: Claude 원본 분석
├── docs/
│   ├── PGM001.md ~ PGM006.md      ← Phase 5: 비즈니스 로직 분석서
├── data-dict/
│   ├── CPYTRANS.md, CPYSMRY.md... ← Phase 3: 데이터 사전
└── diagrams/
    ├── call_graph.md               ← Phase 4: 호출관계 그래프
    ├── data_flow.md                ← Phase 4: 데이터 흐름도
    ├── jcl_flow.md                 ← Phase 4: JCL 배치 흐름도
    ├── erd_copybooks.md            ← Phase 3: ERD
    └── README.md                   ← Phase 4: 다이어그램 목차
```

### 대시보드로 확인

`index.html`을 브라우저에서 열면 웹 대시보드에서 분석 결과를 시각적으로 확인할 수 있습니다.

```bash
# 로컬 서버로 열기
python -m http.server 8000
# 브라우저에서 http://localhost:8000 접속
```

대시보드 기능:
- 프로그램별 의존성/복잡도 표
- 카테고리별 필터/정렬
- 마크다운 문서 미리보기 (모달)
- 카테고리별 요약 도넛 차트

---

## 테스트 실행

```bash
# 전체 테스트 실행
python -m pytest tests/ -v

# 특정 테스트만
python -m pytest tests/test_cobol_parser.py -v
```

---

## 문제 해결 (Troubleshooting)

### Claude Code 관련

| 증상 | 원인 | 해결 |
|------|------|------|
| `claude 명령어를 찾을 수 없습니다` | Claude Code 미설치 | `npm install -g @anthropic-ai/claude-code` |
| Rate limit 반복 | 과도한 호출 | `batch_delay_seconds`를 30 이상으로 증가 |
| 타임아웃 | 대형 프로그램 | `timeout_per_program`을 600으로 증가 |
| 분석 결과가 비어있음 | CLI 경로 문제 | `catm_config.yaml`의 `cli_command` 확인 |

### 파서 관련

| 증상 | 원인 | 해결 |
|------|------|------|
| COPYBOOK 필드 0개 | 비표준 COBOL 형식 | 소스 인코딩 확인 (UTF-8 필요) |
| 복잡도가 항상 1 | 주석 처리 오류 | 컬럼 7 표시자 확인 |
| 파일을 찾을 수 없음 | 확장자 불일치 | `catm_config.yaml`의 `file_extensions` 확인 |

### 일반

| 증상 | 원인 | 해결 |
|------|------|------|
| `import catm` 실패 | 패키지 미설치 | `pip install -e .` |
| YAML 파싱 에러 | 설정 파일 문법 | YAML 들여쓰기 확인 |
| 결과 파일 없음 | 출력 디렉토리 권한 | `output/` 디렉토리 쓰기 권한 확인 |
