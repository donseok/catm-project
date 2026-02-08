#!/bin/bash
# ============================================================
#  CATM - COBOL Analysis Task Manager
#  메인 실행 스크립트
#
#  ⚡ Claude Code Max 20x 구독 사용 (API 아님)
#
#  사용법:
#    ./catm.sh                  # 전체 분석 (Phase 1-6)
#    ./catm.sh --phase 1        # 인벤토리 스캔만
#    ./catm.sh --phase 2        # 의존성 추출만
#    ./catm.sh --phase 3        # COPYBOOK 파싱만
#    ./catm.sh --phase 4        # 다이어그램 생성만
#    ./catm.sh --phase 5        # Claude 비즈니스 로직 분석만
#    ./catm.sh --phase 6        # 우선순위 산정만
#    ./catm.sh --single FILE    # 단일 프로그램 분석
# ============================================================

set -e

# 색상 정의
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# 프로젝트 루트 (이 스크립트의 위치)
PROJECT_ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$PROJECT_ROOT"

# 인자 파싱
PHASE=""
SINGLE_FILE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --phase)
            PHASE="$2"
            shift 2
            ;;
        --single)
            SINGLE_FILE="$2"
            shift 2
            ;;
        --help|-h)
            echo "CATM - COBOL Analysis Task Manager"
            echo ""
            echo "사용법:"
            echo "  ./catm.sh                  전체 분석"
            echo "  ./catm.sh --phase N        특정 단계만 실행 (1-6)"
            echo "  ./catm.sh --single FILE    단일 프로그램 분석"
            echo ""
            echo "단계:"
            echo "  1: 인벤토리 스캔"
            echo "  2: 의존성 추출"
            echo "  3: COPYBOOK 파싱"
            echo "  4: 다이어그램 생성"
            echo "  5: Claude Code 비즈니스 로직 분석"
            echo "  6: 모더나이제이션 우선순위 산정"
            exit 0
            ;;
        *)
            echo "알 수 없는 옵션: $1"
            exit 1
            ;;
    esac
done

# 헤더
echo ""
echo -e "${CYAN}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║                                                          ║${NC}"
echo -e "${CYAN}║   ${GREEN}CATM - COBOL Analysis Task Manager${CYAN}                    ║${NC}"
echo -e "${CYAN}║   ${YELLOW}⚡ Claude Code Max 20x 구독${CYAN}                           ║${NC}"
echo -e "${CYAN}║                                                          ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════╝${NC}"
echo ""

# 환경 확인
echo -e "${BLUE}[사전 확인]${NC}"

# Python 확인
if command -v python3 &> /dev/null; then
    PYTHON_VER=$(python3 --version 2>&1)
    echo -e "  ✅ Python: $PYTHON_VER"
else
    echo -e "  ${RED}❌ Python3가 설치되어 있지 않습니다.${NC}"
    exit 1
fi

# Claude Code 확인
if command -v claude &> /dev/null; then
    echo -e "  ✅ Claude Code: 설치됨"
else
    echo -e "  ${YELLOW}⚠️  Claude Code 미설치 (Phase 5, 6은 실행 불가)${NC}"
    echo -e "     설치: npm install -g @anthropic-ai/claude-code"
    echo -e "     로그인: claude login"
fi

# 패키지 설치 확인 (editable mode)
if ! python3 -c "import catm" 2>/dev/null; then
    echo -e "  ${YELLOW}📦 catm 패키지 설치 중 (editable mode)...${NC}"
    python3 -m pip install -e "$PROJECT_ROOT" --quiet 2>/dev/null || true
fi

# 소스코드 확인
COBOL_COUNT=$(find src/cobol/ -type f 2>/dev/null | wc -l)
CPY_COUNT=$(find src/copybook/ -type f 2>/dev/null | wc -l)
JCL_COUNT=$(find src/jcl/ -type f 2>/dev/null | wc -l)
echo -e "  📂 소스: COBOL=${COBOL_COUNT}, COPYBOOK=${CPY_COUNT}, JCL=${JCL_COUNT}"

if [ "$COBOL_COUNT" -eq 0 ]; then
    echo ""
    echo -e "  ${YELLOW}⚠️  src/cobol/ 에 COBOL 소스파일이 없습니다!${NC}"
    echo -e "     메인프레임에서 소스를 추출하여 배치하세요."
    echo -e "     가이드: EXTRACTION_GUIDE.md 참조"
    echo ""
fi

echo ""

# 단일 프로그램 모드
if [ -n "$SINGLE_FILE" ]; then
    echo -e "${GREEN}[단일 프로그램 분석 모드]${NC} $SINGLE_FILE"
    python3 catm/scripts/05_analyze_with_claude.py --single "$SINGLE_FILE"
    exit 0
fi

# 실행 함수
run_phase() {
    local phase_num=$1
    local phase_name=$2
    local script=$3

    echo ""
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}  Phase $phase_num: $phase_name${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

    if python3 "$script"; then
        echo -e "\n  ${GREEN}✅ Phase $phase_num 완료${NC}"
    else
        echo -e "\n  ${RED}❌ Phase $phase_num 에러 발생${NC}"
        return 1
    fi
}

# 실행
if [ -z "$PHASE" ]; then
    # 전체 실행
    echo -e "${YELLOW}[전체 분석 모드] Phase 1 → 6 순차 실행${NC}"
    
    run_phase 1 "소스코드 인벤토리 스캔"         "catm/scripts/01_scan_inventory.py"
    run_phase 2 "의존성 추출 (정적 분석)"         "catm/scripts/02_extract_dependencies.py"
    run_phase 3 "COPYBOOK 파싱 → 데이터 사전"     "catm/scripts/03_parse_copybook.py"
    run_phase 4 "Mermaid 다이어그램 생성"          "catm/scripts/04_generate_diagrams.py"
    run_phase 5 "Claude Code 비즈니스 로직 분석"   "catm/scripts/05_analyze_with_claude.py"
    run_phase 6 "모더나이제이션 우선순위 산정"      "catm/scripts/06_prioritize.py"
    
else
    # 특정 Phase만 실행
    case $PHASE in
        1) run_phase 1 "소스코드 인벤토리 스캔"         "catm/scripts/01_scan_inventory.py" ;;
        2) run_phase 2 "의존성 추출 (정적 분석)"         "catm/scripts/02_extract_dependencies.py" ;;
        3) run_phase 3 "COPYBOOK 파싱 → 데이터 사전"     "catm/scripts/03_parse_copybook.py" ;;
        4) run_phase 4 "Mermaid 다이어그램 생성"          "catm/scripts/04_generate_diagrams.py" ;;
        5) run_phase 5 "Claude Code 비즈니스 로직 분석"   "catm/scripts/05_analyze_with_claude.py" ;;
        6) run_phase 6 "모더나이제이션 우선순위 산정"      "catm/scripts/06_prioritize.py" ;;
        *) echo -e "${RED}❌ 유효하지 않은 Phase: $PHASE (1-6)${NC}"; exit 1 ;;
    esac
fi

# 완료
echo ""
echo -e "${CYAN}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║  ${GREEN}✅ CATM 분석 완료!${CYAN}                                      ║${NC}"
echo -e "${CYAN}║                                                          ║${NC}"
echo -e "${CYAN}║  📂 결과: ./output/                                      ║${NC}"
echo -e "${CYAN}║    📝 docs/       비즈니스 로직 문서                     ║${NC}"
echo -e "${CYAN}║    📐 diagrams/   Mermaid 다이어그램                     ║${NC}"
echo -e "${CYAN}║    📋 data-dict/  데이터 사전                            ║${NC}"
echo -e "${CYAN}║    📊 reports/    우선순위 보고서                        ║${NC}"
echo -e "${CYAN}║                                                          ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════╝${NC}"
echo ""
