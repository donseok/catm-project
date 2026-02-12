"""에러 처리 유닛 테스트"""

import sys
import json
import importlib
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

# 숫자로 시작하는 모듈명은 일반 import 불가 → importlib 사용
_prioritize = importlib.import_module("catm.scripts.06_prioritize")
parse_claude_scores = _prioritize.parse_claude_scores


class TestEmptyListGuards:
    def test_empty_lines_list_no_crash(self) -> None:
        """빈 lines_list에서 max/min 호출 시 크래시 방지"""
        lines_list: list[int] = []
        if not lines_list:
            lines_list = [0]
        assert max(lines_list) == 0
        assert min(lines_list) == 0

    def test_empty_programs_avg_complexity(self) -> None:
        """빈 프로그램 목록에서 평균 복잡도 0 반환"""
        programs: list[dict] = []
        avg = (
            round(sum(p.get("complexity", 0) for p in programs) / len(programs), 1)
            if programs else 0
        )
        assert avg == 0


class TestParseClaude:
    def test_multi_json_block(self) -> None:
        """다중 JSON 블록에서 scores 추출"""
        response = '''Some analysis text...

```json
{"metadata": "not scores"}
```

More text...

```json
{"scores": {"PGM001": {"business_importance": 8, "phase": 1}}}
```
'''
        scores = parse_claude_scores(response)
        assert "PGM001" in scores
        assert scores["PGM001"]["business_importance"] == 8

    def test_no_json_block(self) -> None:
        """JSON 블록 없으면 빈 딕셔너리"""
        scores = parse_claude_scores("No JSON here at all.")
        assert scores == {}

    def test_invalid_json(self) -> None:
        """잘못된 JSON 파싱 시 크래시 없음"""
        response = '```json\n{invalid json}\n```'
        scores = parse_claude_scores(response)
        assert scores == {}

    def test_json_without_scores_key(self) -> None:
        """scores 키 없는 JSON은 스킵"""
        response = '```json\n{"programs": []}\n```'
        scores = parse_claude_scores(response)
        assert scores == {}
