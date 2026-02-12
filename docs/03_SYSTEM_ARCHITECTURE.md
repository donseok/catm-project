# 03. 시스템 아키텍처

> 대상: **설계자, 개발자** 필독

---

## 레이어 구조

```
┌─────────────────────────────────────────────────────────┐
│                   실행 레이어 (Entry Point)               │
│                                                         │
│   catm.sh          CLI 실행 스크립트                     │
│   dashboard/       웹 대시보드 (index.html + app.js)     │
│                                                         │
├─────────────────────────────────────────────────────────┤
│                   파이프라인 레이어 (Scripts)              │
│                                                         │
│   01_scan_inventory.py         Phase 1: 인벤토리        │
│   02_extract_dependencies.py   Phase 2: 의존성 추출      │
│   03_parse_copybook.py         Phase 3: COPYBOOK 파싱   │
│   04_generate_diagrams.py      Phase 4: 다이어그램       │
│   05_analyze_with_claude.py    Phase 5: Claude AI 분석  │
│   06_prioritize.py             Phase 6: 우선순위 산정    │
│                                                         │
├─────────────────────────────────────────────────────────┤
│                   유틸리티 레이어 (Utils)                  │
│                                                         │
│   cobol_parser.py        COBOL 파서 (핵심 엔진)          │
│   claude_client.py       Claude CLI 호출 래퍼            │
│   file_utils.py          파일 I/O + 설정 로드            │
│   mermaid_builder.py     Mermaid 다이어그램 생성기        │
│   cross_reference.py     교차 참조 + 영향도 분석          │
│   config_validator.py    설정 파일 스키마 검증            │
│   logger.py              구조화 로깅 시스템               │
│                                                         │
├─────────────────────────────────────────────────────────┤
│                   프롬프트 레이어 (Prompts)               │
│                                                         │
│   analyze-logic.md       비즈니스 로직 분석 프롬프트      │
│   analyze-data.md        데이터 구조 분석 프롬프트        │
│   prioritize.md          우선순위 판정 프롬프트           │
│                                                         │
├─────────────────────────────────────────────────────────┤
│                   설정 레이어 (Config)                    │
│                                                         │
│   catm_config.yaml       전체 설정 (경로/확장자/가중치)   │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

---

## 모듈별 상세

### cobol_parser.py — COBOL 파서 (핵심 엔진)

CATM의 핵심 모듈. IBM Enterprise COBOL 고정 형식(Fixed Format)을 파싱합니다.

#### 데이터 클래스

```python
@dataclass
class CobolField:
    """COPYBOOK 필드 정보"""
    level: int          # 레벨 번호 (01~49, 66, 77, 88)
    name: str           # 필드명
    picture: str        # PIC 절 (예: "X(10)", "S9(7)V99")
    usage: str          # USAGE 절 (COMP, COMP-3, BINARY 등)
    occurs: int         # OCCURS 횟수
    occurs_min: int     # OCCURS n TO m 의 n
    occurs_max: int     # OCCURS n TO m 의 m
    occurs_depending: str  # DEPENDING ON 변수명
    redefines: str      # REDEFINES 대상 필드명
    value: str          # VALUE 절
    line_number: int    # 원본 라인 번호

    @property
    def data_type(self) -> str:
        """PIC → 한글 데이터 타입 변환"""
        # "문자열(10)", "숫자(5)", "부호숫자(7.2) 패킹십진수" 등

@dataclass
class CobolProgram:
    """프로그램 분석 결과"""
    name: str           # 프로그램명 (파일명 기준)
    file_path: str
    line_count: int
    program_id: str     # PROGRAM-ID 절
    calls: list         # CALL 대상 목록
    copies: list        # COPY 참조 목록
    db2_tables: list    # DB2 테이블 목록
    vsam_files: list    # VSAM 파일 목록
    cics_maps: list     # CICS MAP 목록
    complexity: int     # McCabe 순환복잡도
    paragraph_count: int
    has_cics: bool
    has_db2: bool
    has_vsam: bool
```

#### 핵심 함수

| 함수 | 설명 | 입력 → 출력 |
|------|------|------------|
| `analyze_program(file_path)` | **단일 프로그램 종합 분석** | 파일 경로 → `CobolProgram` |
| `parse_copybook_fields(source)` | COPYBOOK 필드 파싱 | 소스 문자열 → `list[CobolField]` |
| `extract_calls(source)` | CALL 문 추출 | 정제된 소스 → 프로그램명 목록 |
| `extract_copies(source)` | COPY 문 추출 | 정제된 소스 → COPYBOOK명 목록 |
| `extract_db2_tables(source)` | DB2 테이블 추출 | EXEC SQL 블록 → 테이블명 목록 |
| `extract_file_assigns(source)` | VSAM 파일 추출 | ASSIGN TO → 파일명 목록 |
| `extract_cics_maps(source)` | CICS MAP 추출 | SEND/RECEIVE MAP → MAP명 목록 |
| `calculate_complexity(source)` | McCabe 복잡도 계산 | 소스 → 정수 |
| `count_paragraphs(source)` | PARAGRAPH 수 카운트 | 소스 → 정수 (PROCEDURE DIVISION 이후만) |
| `clean_cobol_line(line)` | 단일 라인 정제 | 원본 라인 → 코드 영역만 (컬럼 8-72) |
| `merge_continuation_lines(source)` | 연속 라인 병합 | 원본 소스 → 병합된 소스 |
| `read_source_file(file_path)` | 소스 파일 읽기 | 경로 → 문자열 (인코딩 자동 감지) |

#### COBOL 컬럼 규칙 처리

```
컬럼 위치:  1-6    7     8-11    12-72    73-80
역할:      시퀀스  표시자  A영역   B영역    식별영역
                   * → 주석
                   - → 연속라인
                   / → 페이지넘김

clean_cobol_line() 함수가 컬럼 8-72만 추출
merge_continuation_lines() 함수가 '-' 연속라인을 이전 라인에 병합
```

---

### claude_client.py — Claude CLI 호출 래퍼

Claude Code Max 20x 구독을 통한 CLI 호출을 일관되게 처리합니다.

#### 호출 인터페이스

```python
def call_claude(
    prompt: str,              # 전체 프롬프트 텍스트
    cli_command: str = "claude",  # Claude CLI 명령어 경로
    timeout: int = 300,       # 최대 대기 시간 (초)
    max_retries: int = 3,     # 최대 재시도 횟수
    retry_delay: int = 30,    # 재시도 간 대기 (초)
) -> str:
    """
    실행 명령: claude -p - --output-format text
    프롬프트는 stdin으로 전달

    에러 처리:
      - Rate limit 감지 → retry_delay × 시도횟수 만큼 대기 후 재시도
      - 타임아웃 → 재시도
      - FileNotFoundError → 설치 안내 메시지 반환
      - 기타 에러 → 재시도 후 에러 메시지 반환

    에러 응답은 "[CATM 에러]" 접두사로 시작
    """
```

```python
def is_error_response(response: str) -> bool:
    """Claude 응답이 에러인지 확인 (접두사 기반)"""
```

#### 호출 흐름

```
call_claude(prompt)
    │
    ├── 시도 1: subprocess.run([claude, -p, -, --output-format, text], input=prompt)
    │   ├── returncode == 0 → 응답 반환 ✅
    │   ├── rate limit 감지 → 30초 대기 → 재시도
    │   ├── 타임아웃 → 30초 대기 → 재시도
    │   └── 기타 에러 → 30초 대기 → 재시도
    │
    ├── 시도 2: 대기 시간 × 2 ...
    │
    └── 시도 3: 실패 시 "[CATM 에러]" 메시지 반환
```

---

### file_utils.py — 파일 I/O 유틸리티

#### 핵심 함수

| 함수 | 설명 |
|------|------|
| `load_config(config_path)` | YAML 설정 로드 + 로깅 초기화 + 설정 검증. 경로 미지정 시 `Path(__file__)` 기준 자동 탐색 |
| `find_files(directory, extensions)` | 숨김파일(`.gitkeep` 등) 제외, 확장자별 파일 목록 반환 |
| `save_json(data, file_path)` | JSON 저장 (디렉토리 자동 생성, UTF-8, ensure_ascii=False) |
| `load_json(file_path)` | JSON 로드 |
| `save_markdown(content, file_path)` | 마크다운 저장 (디렉토리 자동 생성) |
| `ensure_dir(dir_path)` | 디렉토리 생성 (parents=True, exist_ok=True) |

#### 설정 로드 시 동작

```
load_config()
    ├── YAML 파일 파싱
    ├── 로깅 시스템 초기화 (setup_logging)
    └── 설정 검증 (validate_config) → 경고 로그 출력
```

---

### cross_reference.py — 교차 참조 모듈

#### 데이터 클래스

```python
@dataclass
class CrossReferenceIndex:
    """역방향 의존성 맵"""
    copybook_usage: dict[str, list[str]]         # COPYBOOK → 사용 프로그램 목록
    db2_table_usage: dict[str, list[str]]        # DB2 테이블 → 사용 프로그램 목록
    called_by: dict[str, list[str]]              # 서브프로그램 → 호출하는 프로그램 목록
    vsam_usage: dict[str, list[str]]             # VSAM 파일 → 사용 프로그램 목록
    program_executed_by_jcl: dict[str, list[str]] # 프로그램 → 실행 JCL 목록

@dataclass
class DeadCodeReport:
    orphan_programs: list[str]     # 어디에서도 참조되지 않는 프로그램
    unused_copybooks: list[str]    # 어떤 프로그램도 사용하지 않는 COPYBOOK
```

#### 핵심 함수

| 함수 | 설명 |
|------|------|
| `build_cross_reference(programs, jcl_jobs)` | 순방향 의존성 → 역방향 교차 참조 인덱스 생성 |
| `get_impact_analysis(target, cross_ref)` | 특정 리소스 변경 시 영향받는 프로그램 조회 |
| `build_impact_scores(cross_ref, programs)` | 리소스별 영향도 점수 (0.0~1.0) 산출 |
| `detect_dead_code(programs, cross_ref)` | 고아 프로그램/미사용 COPYBOOK 탐지 |
| `serialize_cross_reference(cross_ref)` | JSON 직렬화 |

---

### mermaid_builder.py — 다이어그램 생성기

#### MermaidBuilder 클래스

```python
class MermaidBuilder:
    """Mermaid 다이어그램 마크다운 래퍼"""
    def add(self, line) → self        # 라인 추가 (메서드 체이닝)
    def blank() → self                # 빈 줄 추가
    def build() → str                 # 원본 Mermaid 코드 반환
    def build_markdown(title) → str   # ```mermaid 코드블록으로 감싸서 반환
```

#### 다이어그램 빌더 함수

| 함수 | 출력 형식 | 노드 색상 |
|------|----------|----------|
| `build_call_graph(programs)` | `graph TD` (위→아래) | 파랑=메인, 녹색=서브, 주황=CPY, 보라=DB2 |
| `build_jcl_flow(jcl_jobs)` | `graph LR` (왼→오른) | 빨강=JOB, 파랑=STEP |
| `build_data_flow(programs)` | `graph LR` (왼→오른) | 파랑=PGM, 보라=DB2, 주황=VSAM |
| `build_erd(copybook_fields)` | `erDiagram` | 엔티티-필드 관계도 |

---

### config_validator.py — 설정 검증

`catm_config.yaml`의 필수 키 존재 여부와 타입을 검증합니다.

#### 검증 스키마

```python
REQUIRED_SCHEMA = {
    "paths.source_root": str,
    "paths.output_root": str,
    "source_dirs.cobol": str,
    "source_dirs.copybook": str,
    "file_extensions.cobol": list,
    "file_extensions.copybook": list,
    "claude.cli_command": str,
    "priority_weights.business_importance": (int, float),
    "priority_weights.technical_complexity": (int, float),
}
```

---

### logger.py — 구조화 로깅

#### 로그 형식
```
2026-02-12 20:54:14 [INFO   ] catm.scripts.01_scan_inventory - 스캔 중: COBOL 프로그램
```

#### 함수

| 함수 | 설명 |
|------|------|
| `setup_logging(level, log_file, console)` | 루트 로거 설정 (한 번만 실행) |
| `get_logger(name)` | `catm.{name}` 하위 로거 반환 |
| `reset_logging()` | 테스트용 상태 초기화 |

---

## 모듈 의존성 그래프

```
catm.sh
  └── scripts/01~06 (각 스크립트)
        ├── utils/file_utils
        │     ├── utils/logger
        │     └── utils/config_validator
        ├── utils/cobol_parser        (Phase 2, 3, 5)
        ├── utils/cross_reference     (Phase 2, 5)
        ├── utils/mermaid_builder     (Phase 3, 4)
        └── utils/claude_client       (Phase 5, 6)

의존 방향: scripts → utils (단방향)
utils 모듈 간 의존: file_utils → logger, config_validator
```

---

## 데이터 흐름 상세

### 핵심 데이터 파일: dependency-scan.json

이 파일이 CATM 시스템의 **중추 데이터**입니다.

```
생성: Phase 2 (02_extract_dependencies.py)
소비: Phase 4 (다이어그램), Phase 5 (교차참조 컨텍스트), Phase 6 (정적 점수)
위치: output/reports/dependency-scan.json

구조:
  programs[]          ← 프로그램별 의존성 + 메트릭
  jcl_jobs[]          ← JCL 배치 작업 정보
  summary{}           ← 전체 통계 요약
  categories_summary[] ← 카테고리별 요약
  cross_reference{}   ← 역방향 교차 참조 인덱스
  impact_scores{}     ← 리소스별 영향도 점수
  dead_code{}         ← 데드 코드 탐지 결과
```

### 프롬프트 템플릿 변수

| 변수 | 주입 시점 | 내용 |
|------|----------|------|
| `{{PROGRAM_ID}}` | Phase 5 | 프로그램명 |
| `{{COBOL_SOURCE}}` | Phase 5 | COBOL 원본 소스 전문 |
| `{{COPYBOOK_CONTENTS}}` | Phase 5 | 참조 COPYBOOK 원본 (자동 로드) |
| `{{STRUCTURED_METADATA}}` | Phase 5 | 파서 메타데이터 (복잡도, 의존성 등) |
| `{{CROSS_REFERENCE_CONTEXT}}` | Phase 5 | 교차 참조 (호출자, 공유 리소스, 영향도) |
| `{{STATIC_ANALYSIS_JSON}}` | Phase 6 | 정적 분석 점수 JSON |
| `{{BUSINESS_LOGIC_SUMMARY}}` | Phase 6 | Phase 5 산출물 요약 (각 문서 첫 80줄) |
| `{{COPYBOOK_SOURCE}}` | Phase 5 (data) | COPYBOOK 원본 |
| `{{DDL_SOURCE}}` | Phase 5 (data) | DDL 원본 (있으면) |
| `{{USAGE_CONTEXT}}` | Phase 5 (data) | COPYBOOK 사용 현황 |

---

## 에러 처리 전략

| 상황 | 처리 방식 |
|------|----------|
| COBOL 파일 읽기 실패 | UTF-8 → chardet 자동 감지 → latin-1 폴백 |
| 파서 정규식 불일치 | 해당 요소 건너뛰기, 에러 로그 기록 |
| Claude Rate Limit | 지수 백오프 재시도 (30s × 시도횟수, 최대 3회) |
| Claude 타임아웃 | 300초 후 재시도, 최대 3회 |
| Claude 미설치 | `[CATM 에러]` 메시지 반환 + 설치 안내 |
| 설정 파일 오류 | validate_config로 경고 로그, 실행은 계속 |
| 출력 디렉토리 없음 | ensure_dir로 자동 생성 |
| JSON 파싱 실패 (Phase 6) | Claude 응답에서 여러 JSON 블록 순차 시도 |
