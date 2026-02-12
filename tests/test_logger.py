"""logger 유닛 테스트"""

import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.logger import setup_logging, get_logger, reset_logging


class TestSetupLogging:
    def setup_method(self) -> None:
        reset_logging()

    def teardown_method(self) -> None:
        reset_logging()

    def test_creates_logger_with_correct_level(self) -> None:
        setup_logging(level="DEBUG", console=False)
        logger = logging.getLogger("catm")
        assert logger.level == logging.DEBUG

    def test_console_handler_added(self) -> None:
        setup_logging(level="INFO", console=True)
        logger = logging.getLogger("catm")
        assert any(isinstance(h, logging.StreamHandler) for h in logger.handlers)

    def test_file_handler_added(self, tmp_path: Path) -> None:
        log_file = str(tmp_path / "test.log")
        setup_logging(level="INFO", log_file=log_file, console=False)
        logger = logging.getLogger("catm")
        assert any(isinstance(h, logging.FileHandler) for h in logger.handlers)

    def test_no_duplicate_handlers(self) -> None:
        """setup_logging 중복 호출 시 핸들러가 중복 추가되지 않아야 함"""
        setup_logging(level="INFO", console=True)
        handler_count = len(logging.getLogger("catm").handlers)
        setup_logging(level="INFO", console=True)  # 두 번째 호출
        assert len(logging.getLogger("catm").handlers) == handler_count


class TestGetLogger:
    def test_returns_child_logger(self) -> None:
        logger = get_logger("test_module")
        assert logger.name == "catm.test_module"
