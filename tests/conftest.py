"""CATM 테스트 공통 fixture"""

import pytest
from pathlib import Path


@pytest.fixture
def project_root() -> Path:
    """프로젝트 루트 디렉토리"""
    return Path(__file__).resolve().parent.parent


@pytest.fixture
def sample_cobol_dir(project_root: Path) -> Path:
    """테스트용 COBOL 샘플 디렉토리"""
    return project_root / "tests" / "sample-cobol"


@pytest.fixture
def sample_cobol_source(sample_cobol_dir: Path) -> str:
    """PGM001.cbl 소스코드"""
    return (sample_cobol_dir / "PGM001.cbl").read_text(encoding="utf-8")


@pytest.fixture
def sample_copybook_source(sample_cobol_dir: Path) -> str:
    """CPYTRANS.cpy 소스코드"""
    return (sample_cobol_dir / "CPYTRANS.cpy").read_text(encoding="utf-8")
