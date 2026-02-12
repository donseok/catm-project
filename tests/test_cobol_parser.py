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
    resolve_redefines_chains,
    CobolField,
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


class TestCompoundPicLength:
    """Phase 4: 복합 PIC 패턴 길이 계산"""

    def test_x3_xx(self) -> None:
        """X(3)XX → 5"""
        f = CobolField(level=5, name="TEST", picture="X(3)XX")
        assert f.data_type == "문자열(5)"

    def test_9_5_9_2(self) -> None:
        """9(5)9(2) → 7"""
        f = CobolField(level=5, name="TEST", picture="9(5)9(2)")
        assert f.data_type == "숫자(7)"

    def test_simple_x4(self) -> None:
        """X(04) → 4"""
        f = CobolField(level=5, name="TEST", picture="X(04)")
        assert f.data_type == "문자열(4)"

    def test_individual_x(self) -> None:
        """XXX → 3"""
        f = CobolField(level=5, name="TEST", picture="XXX")
        assert f.data_type == "문자열(3)"


class TestOccursDependingOn:
    """Phase 4: OCCURS DEPENDING ON 파싱"""

    def test_occurs_depending_on(self) -> None:
        from catm.utils.cobol_parser import merge_continuation_lines
        # 연속 라인으로 OCCURS DEPENDING ON (컬럼 72 제한 준수)
        source = (
            "000100     05 WI PIC X OCCURS 1 TO 50 TIMES                \n"
            "000200-         DEPENDING ON WC.                              \n"
        )
        merged = merge_continuation_lines(source)
        fields = parse_copybook_fields(merged)
        assert len(fields) == 1
        f = fields[0]
        assert f.occurs_min == 1
        assert f.occurs_max == 50
        assert f.occurs_depending == "WC"

    def test_fixed_occurs(self) -> None:
        source = "000100     05 WS-ITEMS PIC X(05) OCCURS 10 TIMES.                   \n"
        fields = parse_copybook_fields(source)
        assert len(fields) == 1
        assert fields[0].occurs == 10
        assert fields[0].occurs_depending == ""


class TestUsageLabel:
    """Phase 4: USAGE 한글 라벨"""

    def test_comp3_label(self) -> None:
        f = CobolField(level=5, name="TEST", picture="S9(9)V99", usage="COMP-3")
        assert "패킹십진수" in f.data_type

    def test_comp5_label(self) -> None:
        f = CobolField(level=5, name="TEST", picture="S9(9)", usage="COMP-5")
        assert "네이티브이진수" in f.data_type

    def test_binary_label(self) -> None:
        f = CobolField(level=5, name="TEST", picture="9(4)", usage="BINARY")
        assert "이진수" in f.data_type


class TestRedefinesChains:
    """Phase 4: REDEFINES 체인 추적"""

    def test_simple_redefines(self) -> None:
        fields = [
            CobolField(level=5, name="ORIG-FIELD", picture="X(10)"),
            CobolField(level=5, name="REDEF-A", picture="9(10)", redefines="ORIG-FIELD"),
            CobolField(level=5, name="REDEF-B", picture="X(5)", redefines="ORIG-FIELD"),
        ]
        chains = resolve_redefines_chains(fields)
        assert "ORIG-FIELD" in chains
        assert "REDEF-A" in chains["ORIG-FIELD"]
        assert "REDEF-B" in chains["ORIG-FIELD"]

    def test_no_redefines(self) -> None:
        fields = [
            CobolField(level=5, name="FIELD-A", picture="X(10)"),
            CobolField(level=5, name="FIELD-B", picture="9(5)"),
        ]
        chains = resolve_redefines_chains(fields)
        assert chains == {}
