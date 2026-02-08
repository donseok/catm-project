# CLAUDE.md - CATM 프로젝트 컨텍스트

## 프로젝트 정보
이 프로젝트는 IBM 메인프레임 COBOL 레거시 시스템을 자동 분석하는
"COBOL Analysis Task Manager (CATM)"입니다.

## Claude Code 사용 방식
- **구독**: Claude Max 20x 구독 사용 (API 아님)
- **실행**: `claude` CLI로 직접 호출
- **용도**: COBOL 소스코드 → 한글 비즈니스 로직 해석, 데이터 구조 분석, 우선순위 산정

## 디렉토리 규칙
- `src/` : 메인프레임에서 추출한 원본 COBOL 소스 (읽기 전용으로 취급)
- `catm/` : 분석 봇 코어 모듈
  - `catm/scripts/` : 6-Phase 파이프라인 스크립트 (01~06)
  - `catm/utils/` : 공통 유틸리티 (`cobol_parser`, `file_utils`, `claude_client`, `mermaid_builder`)
  - `catm/prompts/` : Claude에게 보낼 프롬프트 템플릿
  - `catm/config/` : `catm_config.yaml` 설정 파일
- `output/` : 모든 분석 결과물은 여기에 생성 (git 추적 제외, `.gitkeep`만 유지)
  - `output/reports/` : `dependency-scan.json`, 우선순위 매트릭스, 분석 로그
  - `output/docs/` : 프로그램별 비즈니스 로직 문서
  - `output/data-dict/` : COPYBOOK 데이터 사전
  - `output/diagrams/` : Mermaid 다이어그램
- `tests/` : pytest 테스트 (22개)
- `tests/sample-cobol/` : 테스트용 COBOL 샘플 (PGM001.cbl, CPYTRANS.cpy)

## 6-Phase 파이프라인
| Phase | 스크립트 | 기능 |
|-------|---------|------|
| 1 | `01_scan_inventory.py` | 소스코드 인벤토리 스캔 |
| 2 | `02_extract_dependencies.py` | CALL/COPY/DB2 의존성 추출 → `output/reports/dependency-scan.json` |
| 3 | `03_parse_copybook.py` | COPYBOOK 파싱 → 데이터 사전 |
| 4 | `04_generate_diagrams.py` | Mermaid 다이어그램 생성 |
| 5 | `05_analyze_with_claude.py` | Claude Code 비즈니스 로직 분석 (`--mode logic`/`--mode data`) |
| 6 | `06_prioritize.py` | 모더나이제이션 우선순위 산정 |

## 코딩 규칙
- Python 3.11+
- 타입 힌트 필수
- 한글 주석 사용
- 파일 인코딩: UTF-8
- COBOL 소스 파싱 시 컬럼 규칙 준수 (1-6: 시퀀스, 7: 표시자, 8-72: 코드)
- `pyproject.toml`로 패키지 관리 (`pip install -e .` 지원)

## 주요 명령어
```bash
# 패키지 설치 (editable mode)
pip install -e .

# 전체 분석 실행 (Phase 1-6 순차)
./catm.sh

# 단계별 실행
./catm.sh --phase 1    # 인벤토리 스캔
./catm.sh --phase 5    # Claude 비즈니스 로직 분석만

# 단일 프로그램 Claude 분석
python catm/scripts/05_analyze_with_claude.py --single src/cobol/PGM001.cbl

# 데이터 구조 분석 (COPYBOOK/DCLGEN → Claude 해석)
python catm/scripts/05_analyze_with_claude.py --mode data

# 테스트 (22개)
python -m pytest tests/ -v

# 인벤토리 스캔만
python catm/scripts/01_scan_inventory.py
```

## Claude Code 호출 패턴
이 프로젝트에서 Claude Code를 호출할 때는 반드시 다음 패턴을 따릅니다:
1. 프롬프트 템플릿 로드 (`catm/prompts/analyze-logic.md` 또는 `analyze-data.md`)
2. COBOL 소스 + COPYBOOK을 컨텍스트로 주입
3. `claude -p - --output-format text` 명령으로 실행 (stdin으로 프롬프트 전달)
4. 결과를 `output/` 디렉토리에 마크다운으로 저장

> **주의**: `--print` 플래그는 사용하지 않음. 반드시 `-p -` (stdin에서 읽기) + `--output-format text` 조합 사용.

## 공통 모듈
- **`catm/utils/claude_client.py`**: Claude CLI 호출 공통 모듈 (`call_claude()`, `is_error_response()`)
  - 재시도, rate-limit 감지, 타임아웃 처리 포함
  - `05_analyze_with_claude.py`, `06_prioritize.py`에서 공유
- **`catm/utils/cobol_parser.py`**: COBOL 파서
  - `count_paragraphs()`: PROCEDURE DIVISION 이후만 카운트
  - `parse_copybook_fields()`: 88 레벨 조건명, COMP-3 직접 표기 지원
- **`catm/utils/file_utils.py`**: 파일 유틸
  - `find_files()`: 숨김파일(`.gitkeep` 등) 자동 제외
  - `load_config()`: `Path(__file__)` 기준 상대 경로로 설정 로드 (실행 위치 무관)

## 주의사항
- `dependency-scan.json`은 `output/reports/` 아래에 생성됨 (`output/` 루트 아님)
- `catm.sh`는 `set -e` 사용 중이므로, 에러 핸들링은 `if python3 ...; then` 패턴 사용
- `src/` 디렉토리의 빈 폴더는 `.gitkeep`으로 구조만 유지
