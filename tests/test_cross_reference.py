"""cross_reference 유닛 테스트"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.cross_reference import (
    build_cross_reference,
    get_impact_analysis,
    build_impact_scores,
    detect_dead_code,
    detect_dead_code_with_inventory,
    serialize_cross_reference,
    CrossReferenceIndex,
)

# 테스트 데이터 fixture
SAMPLE_PROGRAMS = [
    {
        "name": "PGM001",
        "copies": ["CPYTRANS", "CPYSMRY"],
        "calls": ["ERRLOG", "SQLERR"],
        "db2_tables": ["TB_DAILY_PROD"],
        "vsam_files": ["PRODFILE"],
    },
    {
        "name": "PGM002",
        "copies": ["CPYTRANS"],
        "calls": ["ERRLOG"],
        "db2_tables": ["TB_DAILY_PROD", "TB_MASTER"],
        "vsam_files": [],
    },
    {
        "name": "PGM003",
        "copies": ["CPYQCHK"],
        "calls": ["ERRLOG", "ABNDPGM"],
        "db2_tables": [],
        "vsam_files": ["QCFILE"],
    },
]

SAMPLE_JCL_JOBS = [
    {
        "job_name": "JOBBATCH",
        "steps": [
            {"step": "STEP01", "program": "PGM001"},
            {"step": "STEP02", "program": "PGM002"},
        ],
    },
]


class TestBuildCrossReference:
    def test_copybook_reverse_mapping(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        assert "CPYTRANS" in idx.copybook_usage
        assert sorted(idx.copybook_usage["CPYTRANS"]) == ["PGM001", "PGM002"]

    def test_db2_table_reverse_mapping(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        assert "TB_DAILY_PROD" in idx.db2_table_usage
        assert sorted(idx.db2_table_usage["TB_DAILY_PROD"]) == ["PGM001", "PGM002"]

    def test_called_by_reverse_mapping(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        assert "ERRLOG" in idx.called_by
        assert sorted(idx.called_by["ERRLOG"]) == ["PGM001", "PGM002", "PGM003"]

    def test_vsam_reverse_mapping(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        assert "PRODFILE" in idx.vsam_usage
        assert idx.vsam_usage["PRODFILE"] == ["PGM001"]

    def test_jcl_program_mapping(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS, SAMPLE_JCL_JOBS)
        assert "PGM001" in idx.program_executed_by_jcl
        assert idx.program_executed_by_jcl["PGM001"] == ["JOBBATCH"]

    def test_empty_programs(self) -> None:
        idx = build_cross_reference([])
        assert idx.copybook_usage == {}
        assert idx.called_by == {}


class TestGetImpactAnalysis:
    def test_copybook_impact(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        result = get_impact_analysis("CPYTRANS", idx)
        assert "copybook_users" in result
        assert sorted(result["copybook_users"]) == ["PGM001", "PGM002"]

    def test_no_impact(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        result = get_impact_analysis("NONEXISTENT", idx)
        assert result == {}


class TestBuildImpactScores:
    def test_scores_range(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        scores = build_impact_scores(idx, SAMPLE_PROGRAMS)
        for score in scores.values():
            assert 0.0 <= score <= 1.0

    def test_errlog_highest_impact(self) -> None:
        """ERRLOG는 3개 프로그램 모두에서 호출 → 1.0"""
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        scores = build_impact_scores(idx, SAMPLE_PROGRAMS)
        assert scores["ERRLOG"] == 1.0

    def test_empty_programs(self) -> None:
        idx = build_cross_reference([])
        scores = build_impact_scores(idx, [])
        assert scores == {}


class TestDetectDeadCode:
    def test_orphan_program(self) -> None:
        """PGM003는 JCL에서 실행되지 않고 다른 프로그램이 CALL하지 않음 → 고아"""
        idx = build_cross_reference(SAMPLE_PROGRAMS, SAMPLE_JCL_JOBS)
        report = detect_dead_code(SAMPLE_PROGRAMS, idx)
        assert "PGM003" in report.orphan_programs

    def test_non_orphan_programs(self) -> None:
        """PGM001, PGM002는 JCL에서 실행되므로 고아가 아님"""
        idx = build_cross_reference(SAMPLE_PROGRAMS, SAMPLE_JCL_JOBS)
        report = detect_dead_code(SAMPLE_PROGRAMS, idx)
        assert "PGM001" not in report.orphan_programs
        assert "PGM002" not in report.orphan_programs

    def test_called_program_not_orphan(self) -> None:
        """ERRLOG는 프로그램 목록에 없지만 CALL 당함 → 검사 대상 아님"""
        idx = build_cross_reference(SAMPLE_PROGRAMS, SAMPLE_JCL_JOBS)
        report = detect_dead_code(SAMPLE_PROGRAMS, idx)
        assert "ERRLOG" not in report.orphan_programs

    def test_no_jcl_all_orphans(self) -> None:
        """JCL 없으면 CALL 받지 않는 프로그램 모두 고아"""
        idx = build_cross_reference(SAMPLE_PROGRAMS, [])
        report = detect_dead_code(SAMPLE_PROGRAMS, idx)
        # PGM001, PGM002, PGM003 모두 다른 프로그램에서 CALL되지 않음
        assert "PGM001" in report.orphan_programs
        assert "PGM002" in report.orphan_programs
        assert "PGM003" in report.orphan_programs


class TestDetectDeadCodeWithInventory:
    def test_unused_copybook(self) -> None:
        """인벤토리에 있지만 아무도 사용하지 않는 COPYBOOK"""
        idx = build_cross_reference(SAMPLE_PROGRAMS)
        report = detect_dead_code_with_inventory(
            SAMPLE_PROGRAMS, idx, ["CPYTRANS", "CPYSMRY", "CPYQCHK", "CPYOLD"]
        )
        assert "CPYOLD" in report.unused_copybooks
        assert "CPYTRANS" not in report.unused_copybooks


class TestSerializeCrossReference:
    def test_all_keys_present(self) -> None:
        idx = build_cross_reference(SAMPLE_PROGRAMS, SAMPLE_JCL_JOBS)
        data = serialize_cross_reference(idx)
        assert "copybook_usage" in data
        assert "db2_table_usage" in data
        assert "called_by" in data
        assert "vsam_usage" in data
        assert "program_executed_by_jcl" in data

    def test_serializable(self) -> None:
        """JSON 직렬화 가능 확인"""
        import json
        idx = build_cross_reference(SAMPLE_PROGRAMS, SAMPLE_JCL_JOBS)
        data = serialize_cross_reference(idx)
        json_str = json.dumps(data, ensure_ascii=False)
        assert isinstance(json_str, str)
