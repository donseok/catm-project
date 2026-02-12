# 06. 설정 참조 (catm_config.yaml)

> 대상: **설계자, 개발자**

---

## 설정 파일 위치

```
catm/config/catm_config.yaml
```

`file_utils.py`의 `load_config()` 함수가 `Path(__file__)` 기준으로 자동 탐색하므로, 어느 디렉토리에서 실행해도 설정을 올바르게 로드합니다.

---

## 전체 설정 구조

```yaml
paths:            # 경로 설정
source_dirs:      # 소스 디렉토리 매핑
file_extensions:  # 파일 확장자 매핑
claude:           # Claude Code CLI 설정
static_analysis:  # 정적 분석 설정
priority_weights: # 우선순위 가중치
output:           # 출력 설정
program_categories: # 업무 카테고리
logging:          # 로그 설정
```

---

## 섹션별 상세

### paths — 경로 설정

```yaml
paths:
  source_root: "./src"            # 소스코드 루트 (상대/절대 경로)
  output_root: "./output"         # 분석 결과 출력 루트
  prompts_dir: "./catm/prompts"   # 프롬프트 템플릿 디렉토리
```

| 키 | 타입 | 설명 | 참조 스크립트 |
|----|------|------|-------------|
| `source_root` | str | 메인프레임 소스 최상위 디렉토리 | 전체 |
| `output_root` | str | 산출물 저장 최상위 디렉토리 | 전체 |
| `prompts_dir` | str | Claude 프롬프트 템플릿 디렉토리 | Phase 5, 6 |

---

### source_dirs — 소스 디렉토리 매핑

```yaml
source_dirs:
  cobol: "cobol"        # source_root 하위 COBOL 디렉토리
  copybook: "copybook"  # source_root 하위 COPYBOOK 디렉토리
  jcl: "jcl"            # source_root 하위 JCL 디렉토리
  proc: "proc"          # source_root 하위 PROC 디렉토리
  dclgen: "dclgen"      # source_root 하위 DCLGEN 디렉토리
  ddl: "ddl"            # source_root 하위 DDL 디렉토리
  map: "map"            # source_root 하위 CICS MAP 디렉토리
```

> 실제 디렉토리 경로는 `source_root` + `source_dirs.xxx`로 조합됩니다.
> 예: `./src` + `cobol` = `./src/cobol/`

---

### file_extensions — 파일 확장자 매핑

```yaml
file_extensions:
  cobol: [".cbl", ".cob", ".cobol", ""]      # COBOL 프로그램
  copybook: [".cpy", ".copy", ""]             # COPYBOOK
  jcl: [".jcl", ""]                           # JCL
  proc: [".prc", ".proc", ""]                 # PROC
```

| 값 | 의미 |
|----|------|
| `""` (빈 문자열) | 확장자 없는 파일도 포함 (메인프레임에서 직접 추출 시) |
| `.cbl` | 표준 COBOL 확장자 |

> `find_files()` 함수는 이 확장자 목록에 해당하는 파일만 수집합니다.
> 숨김파일(`.gitkeep` 등)은 자동 제외됩니다.

---

### claude — Claude Code CLI 설정

```yaml
claude:
  mode: "cli"                                          # 실행 모드
  cli_command: "C:/Users/donse/AppData/Roaming/npm/claude.cmd"  # CLI 경로
  max_concurrent: 3                                    # 동시 분석 제한 (미사용)
  timeout_per_program: 300                             # 프로그램당 타임아웃 (초)
  max_retries: 3                                       # 최대 재시도 횟수
  retry_delay_seconds: 30                              # 재시도 대기 (초)
  batch_size: 5                                        # 배치 단위
  batch_delay_seconds: 10                              # 배치 간 대기 (초)
```

| 키 | 기본값 | 조정 가이드 |
|----|--------|-----------|
| `cli_command` | `"claude"` | PATH에 없으면 절대 경로 지정. Windows: `.cmd` 확장자 포함 |
| `timeout_per_program` | 300 | 대형 프로그램(500줄+)은 600으로 증가 권장 |
| `max_retries` | 3 | Rate limit 빈발 시 5로 증가 |
| `retry_delay_seconds` | 30 | Rate limit 대응. 지수 백오프(30s × 시도횟수)로 동작 |
| `batch_size` | 5 | 프로그램 N개마다 batch_delay_seconds 만큼 대기 |
| `batch_delay_seconds` | 10 | Rate limit 방지 대기 시간. 빈발 시 30으로 증가 |

---

### static_analysis — 정적 분석 설정

```yaml
static_analysis:
  cobol_format: "fixed"          # COBOL 형식 (fixed/free)
  code_start_col: 7              # 코드 시작 컬럼 (0-based)
  code_end_col: 72               # 코드 끝 컬럼
  complexity_threshold:
    low: 10                      # 낮음 기준
    medium: 25                   # 중간 기준
    high: 50                     # 높음 기준
```

| 복잡도 범위 | 분류 | 의미 |
|-----------|------|------|
| 1~10 | 낮음 | 단순한 순차 처리 |
| 11~25 | 중간 | 조건 분기가 있는 일반 프로그램 |
| 26~50 | 높음 | 복잡한 비즈니스 로직 |
| 50+ | 매우 높음 | 리라이트 권장 |

---

### priority_weights — 우선순위 가중치

```yaml
priority_weights:
  business_importance: 0.35   # 비즈니스 중요도 (35%)
  technical_complexity: 0.25  # 기술 복잡도 (25%)
  dependency_impact: 0.20     # 의존성 영향도 (20%)
  conversion_ease: 0.20       # 전환 용이도 (20%)
```

**합계는 반드시 1.00 이어야 합니다.**

| 가중치 조정 시나리오 | 권장 조정 |
|-------------------|----------|
| 비즈니스 관점 우선 | `business_importance: 0.45`, 나머지 축소 |
| 기술 리스크 우선 | `technical_complexity: 0.35`, 나머지 축소 |
| 빠른 전환 우선 | `conversion_ease: 0.35`, 나머지 축소 |

---

### output — 출력 설정

```yaml
output:
  docs_dir: "docs"             # 비즈니스 로직 문서 하위 디렉토리
  diagrams_dir: "diagrams"     # 다이어그램 하위 디렉토리
  data_dict_dir: "data-dict"   # 데이터 사전 하위 디렉토리
  reports_dir: "reports"       # 보고서 하위 디렉토리
  mermaid_theme: "default"     # Mermaid 테마 (미사용)
  generate_diagrams:
    call_graph: true           # 호출관계 그래프 생성 여부
    jcl_flow: true             # JCL 배치 흐름도 생성 여부
    data_flow: true            # 데이터 흐름도 생성 여부
    erd: true                  # ERD 생성 여부
```

> 특정 다이어그램이 필요 없으면 `false`로 비활성화 가능

---

### program_categories — 업무 카테고리

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

| 필드 | 설명 |
|------|------|
| `name` | 카테고리명 (한글) |
| `programs` | 해당 카테고리에 속하는 프로그램 ID 목록 (대문자) |

**매핑되지 않은 프로그램은 `"미분류"`로 표시됩니다.**

카테고리는 다음에서 활용됩니다:
- `dependency-scan.json`의 각 프로그램 `category` 필드
- `categories_summary` 섹션 (카테고리별 통계)
- 웹 대시보드의 카테고리 필터/배지/도넛 차트
- Phase 5 분석 시 Claude에게 제공되는 메타데이터

#### 새 프로그램 추가 시

1. `src/cobol/`에 COBOL 소스 배치
2. `program_categories`에 프로그램 추가
3. `./catm.sh` 재실행

---

### logging — 로그 설정

```yaml
logging:
  level: "INFO"                # 로그 레벨 (DEBUG/INFO/WARNING/ERROR)
  file: "./output/catm.log"    # 로그 파일 경로 (null이면 파일 로깅 비활성화)
  console: true                # 콘솔 출력 여부
```

| 레벨 | 용도 |
|------|------|
| `DEBUG` | 개발/디버깅 시 상세 로그 |
| `INFO` | 일반 실행 (기본값, 권장) |
| `WARNING` | 경고만 표시 |
| `ERROR` | 에러만 표시 |

---

## 설정 검증

`config_validator.py`가 아래 필수 키를 자동 검증합니다:

```
paths.source_root        → str
paths.output_root        → str
source_dirs.cobol        → str
source_dirs.copybook     → str
file_extensions.cobol    → list
file_extensions.copybook → list
claude.cli_command       → str
priority_weights.business_importance  → number
priority_weights.technical_complexity → number
```

누락 또는 타입 불일치 시 경고 로그가 출력되지만 실행은 계속됩니다.

---

## 커스터마이징 가이드

### 새 고객사 적용 시

1. `paths.source_root` → 고객 소스 경로로 변경
2. `file_extensions` → 고객 소스 확장자에 맞게 조정
3. `program_categories` → 고객 업무 기준으로 카테고리 재정의
4. `claude.cli_command` → 실행 환경의 Claude 경로 확인
5. `priority_weights` → 고객 요구사항에 맞게 가중치 조정

### 대형 시스템 적용 시

```yaml
claude:
  timeout_per_program: 600    # 대형 프로그램 대응
  batch_size: 3               # 배치 크기 축소
  batch_delay_seconds: 30     # 배치 대기 증가
  max_retries: 5              # 재시도 횟수 증가
```
