"""mermaid_builder 유닛 테스트"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.mermaid_builder import (
    MermaidBuilder,
    build_call_graph,
    build_jcl_flow,
    build_data_flow,
    build_erd,
)


class TestMermaidBuilder:
    def test_add_line(self) -> None:
        b = MermaidBuilder()
        b.add("graph TD")
        assert b.build() == "graph TD"

    def test_chaining(self) -> None:
        b = MermaidBuilder()
        b.add("graph TD").add("  A --> B")
        assert "A --> B" in b.build()

    def test_blank_line(self) -> None:
        b = MermaidBuilder()
        b.add("graph TD").blank().add("  A --> B")
        lines = b.build().split("\n")
        assert lines[1] == ""

    def test_build_markdown(self) -> None:
        b = MermaidBuilder()
        b.add("graph TD")
        md = b.build_markdown("Test Title")
        assert "# Test Title" in md
        assert "```mermaid" in md
        assert "```" in md

    def test_build_markdown_no_title(self) -> None:
        b = MermaidBuilder()
        b.add("graph TD")
        md = b.build_markdown()
        assert "# " not in md
        assert "```mermaid" in md


class TestBuildCallGraph:
    PROGRAMS = [
        {"name": "PGM001", "calls": ["ERRLOG"], "copies": ["CPYTRANS"], "db2_tables": ["TB_PROD"]},
        {"name": "PGM002", "calls": ["ERRLOG"], "copies": [], "db2_tables": []},
    ]

    def test_contains_mermaid_block(self) -> None:
        result = build_call_graph(self.PROGRAMS)
        assert "```mermaid" in result
        assert "graph TD" in result

    def test_call_edges(self) -> None:
        result = build_call_graph(self.PROGRAMS)
        assert "PGM001" in result
        assert "ERRLOG" in result
        assert "CALL" in result

    def test_copy_edges(self) -> None:
        result = build_call_graph(self.PROGRAMS)
        assert "CPYTRANS" in result
        assert "COPY" in result

    def test_empty_programs(self) -> None:
        result = build_call_graph([])
        assert "```mermaid" in result


class TestBuildJclFlow:
    JOBS = [
        {
            "job_name": "JOBBATCH",
            "steps": [
                {"step": "STEP01", "program": "PGM001"},
                {"step": "STEP02", "program": "PGM002"},
            ],
        },
    ]

    def test_contains_mermaid_block(self) -> None:
        result = build_jcl_flow(self.JOBS)
        assert "```mermaid" in result
        assert "graph LR" in result

    def test_job_and_steps(self) -> None:
        result = build_jcl_flow(self.JOBS)
        assert "JOBBATCH" in result
        assert "STEP01" in result
        assert "PGM001" in result

    def test_empty_jobs(self) -> None:
        result = build_jcl_flow([])
        assert "```mermaid" in result


class TestBuildDataFlow:
    PROGRAMS = [
        {"name": "PGM001", "db2_tables": ["TB_PROD"], "vsam_files": ["PRODFILE"]},
        {"name": "PGM002", "db2_tables": [], "vsam_files": []},
    ]

    def test_db2_edges(self) -> None:
        result = build_data_flow(self.PROGRAMS)
        assert "TB_PROD" in result
        assert "SQL" in result

    def test_vsam_edges(self) -> None:
        result = build_data_flow(self.PROGRAMS)
        assert "PRODFILE" in result
        assert "I/O" in result

    def test_empty_programs(self) -> None:
        result = build_data_flow([])
        assert "```mermaid" in result


class TestBuildErd:
    FIELDS = {
        "CPYTRANS": [
            {"name": "PT-PLANT-CD", "picture": "X(04)", "data_type": "문자열(4)"},
            {"name": "PT-QTY", "picture": "S9(9)V99", "data_type": "숫자(9.2)"},
        ],
    }

    def test_contains_er_diagram(self) -> None:
        result = build_erd(self.FIELDS)
        assert "erDiagram" in result

    def test_entity_and_fields(self) -> None:
        result = build_erd(self.FIELDS)
        assert "CPYTRANS" in result
        assert "PT_PLANT_CD" in result  # - 는 _ 로 변환

    def test_empty_fields(self) -> None:
        result = build_erd({})
        assert "erDiagram" in result
