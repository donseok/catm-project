"""file_utils 유닛 테스트"""

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.file_utils import find_files, ensure_dir


class TestFindFiles:
    def test_excludes_gitkeep(self, tmp_path: Path) -> None:
        """확장자 없는 파일 검색 시 .gitkeep이 제외되는지 확인"""
        (tmp_path / ".gitkeep").touch()
        (tmp_path / "TESTPGM").write_text("test")
        (tmp_path / "REAL.cbl").write_text("test")

        # 확장자 없는 파일만 검색
        files = find_files(str(tmp_path), [""])
        names = [f.name for f in files]
        assert ".gitkeep" not in names
        assert "TESTPGM" in names

    def test_excludes_hidden_with_extension(self, tmp_path: Path) -> None:
        """확장자 있는 숨김파일도 제외하는지 확인"""
        (tmp_path / ".hidden.cbl").write_text("test")
        (tmp_path / "REAL.cbl").write_text("test")

        files = find_files(str(tmp_path), [".cbl"])
        names = [f.name for f in files]
        assert ".hidden.cbl" not in names
        assert "REAL.cbl" in names

    def test_nonexistent_directory(self) -> None:
        """존재하지 않는 디렉토리에 대해 빈 리스트 반환"""
        files = find_files("/nonexistent/path/xyz", [".cbl"])
        assert files == []

    def test_multiple_extensions(self, tmp_path: Path) -> None:
        """여러 확장자로 검색"""
        (tmp_path / "A.cbl").write_text("test")
        (tmp_path / "B.cob").write_text("test")
        (tmp_path / "C.txt").write_text("test")

        files = find_files(str(tmp_path), [".cbl", ".cob"])
        names = [f.name for f in files]
        assert "A.cbl" in names
        assert "B.cob" in names
        assert "C.txt" not in names


class TestEnsureDir:
    def test_creates_directory(self, tmp_path: Path) -> None:
        new_dir = tmp_path / "sub" / "dir"
        result = ensure_dir(str(new_dir))
        assert new_dir.exists()
        assert result == new_dir

    def test_existing_directory(self, tmp_path: Path) -> None:
        result = ensure_dir(str(tmp_path))
        assert tmp_path.exists()
        assert result == tmp_path
