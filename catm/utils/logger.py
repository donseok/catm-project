"""
logger.py - CATM 구조화 로깅 시스템

catm_config.yaml의 logging 섹션 설정을 활용하여
파일 + 콘솔 핸들러를 구성합니다.
"""

import logging
import sys
from pathlib import Path
from typing import Optional


_initialized: bool = False


def setup_logging(
    level: str = "INFO",
    log_file: Optional[str] = None,
    console: bool = True,
) -> None:
    """CATM 루트 로거 설정

    Args:
        level: 로그 레벨 (DEBUG, INFO, WARNING, ERROR)
        log_file: 로그 파일 경로 (None이면 파일 로깅 비활성화)
        console: 콘솔 출력 활성화 여부
    """
    global _initialized
    if _initialized:
        return
    _initialized = True

    root_logger = logging.getLogger("catm")
    root_logger.setLevel(getattr(logging, level.upper(), logging.INFO))

    formatter = logging.Formatter(
        "%(asctime)s [%(levelname)-7s] %(name)s - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )

    if console:
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setFormatter(formatter)
        root_logger.addHandler(console_handler)

    if log_file:
        log_path = Path(log_file)
        log_path.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(str(log_path), encoding="utf-8")
        file_handler.setFormatter(formatter)
        root_logger.addHandler(file_handler)


def get_logger(name: str) -> logging.Logger:
    """CATM 하위 로거 반환

    Args:
        name: 모듈/스크립트 이름 (예: "scripts.01_scan_inventory")

    Returns:
        catm.{name} 로거
    """
    return logging.getLogger(f"catm.{name}")


def reset_logging() -> None:
    """로깅 상태 초기화 (테스트용)"""
    global _initialized
    _initialized = False
    root_logger = logging.getLogger("catm")
    root_logger.handlers.clear()
