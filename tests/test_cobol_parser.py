"""cobol_parser 유닛 테스트"""

import sys
from pathlib import Path

# 프로젝트 루트를 경로에 추가
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.cobol_parser import (
    clean_cobol_line,
    extract_code_lines,
    count_paragraphs,
    extract_calls,
    extract_copies,
    extract_program_id,
    parse_copybook_fields,
    calculate_complexity,
)


class TestCleanCobolLine:
    def test_normal_line(self) -> None:
        # 컬럼 1-6: 시퀀스, 7: 공백, 8+: 코드
        line = "000100 IDENTIFICATION DIVISION.                                 PGM001"
        result = clean_cobol_line(line)
        assert "IDENTIFICATION DIVISION." in result

    def test_comment_line(self) -> None:
        line = "000400*                                                         PGM001"
        assert clean_cobol_line(line) == ""

    def test_short_line(self) -> None:
        assert clean_cobol_line("ABC") == ""

    def test_page_break(self) -> None:
        line = "000400/                                                         PGM001"
        assert clean_cobol_line(line) == ""


class TestCountParagraphs:
    def test_sample_pgm001(self, sample_cobol_source: str) -> None:
        """PGM001은 8개 paragraph를 포함해야 한다"""
        count = count_paragraphs(sample_cobol_source)
        assert count == 8

    def test_empty_source(self) -> None:
        assert count_paragraphs("") == 0

    def test_no_procedure_division(self) -> None:
        source = "000100 IDENTIFICATION DIVISION.                                 X\n"
        assert count_paragraphs(source) == 0


class TestExtractCalls:
    def test_sample_pgm001(self, sample_cobol_source: str) -> None:
        calls = extract_calls(sample_cobol_source)
        assert "ERRLOG" in calls
        assert "SQLERR" in calls
        assert "ABNDPGM" in calls

    def test_no_calls(self) -> None:
        assert extract_calls("MOVE A TO B.") == []


class TestExtractCopies:
    def test_sample_pgm001(self, sample_cobol_source: str) -> None:
        copies = extract_copies(sample_cobol_source)
        assert "CPYTRANS" in copies
        assert "CPYSMRY" in copies

    def test_no_copies(self) -> None:
        assert extract_copies("MOVE A TO B.") == []


class TestExtractProgramId:
    def test_sample_pgm001(self, sample_cobol_source: str) -> None:
        pid = extract_program_id(sample_cobol_source)
        assert pid == "PGM001"


class TestParseCopybookFields:
    def test_sample_cpytrans(self, sample_copybook_source: str) -> None:
        fields = parse_copybook_fields(sample_copybook_source)
        assert len(fields) > 0

        # 필드명 확인
        names = [f.name for f in fields]
        assert "PT-KEY" in names
        assert "PT-PLANT-CD" in names
        # PT-QTY는 COMP-3 직접 표기로 인해 파서 개선(Phase 4B) 후 인식됨
        assert "PT-QTY" in names

    def test_field_levels(self, sample_copybook_source: str) -> None:
        fields = parse_copybook_fields(sample_copybook_source)
        levels = {f.name: f.level for f in fields}
        assert levels["PT-KEY"] == 5
        assert levels["PT-PLANT-CD"] == 10

    def test_empty_source(self) -> None:
        assert parse_copybook_fields("") == []


class TestCalculateComplexity:
    def test_sample_pgm001(self, sample_cobol_source: str) -> None:
        complexity = calculate_complexity(sample_cobol_source)
        # 기본 1 + IF + EVALUATE + WHEN(3개) + PERFORM UNTIL + AT END(2개) = 9 이상
        assert complexity > 1
