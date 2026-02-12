"""
config_validator.py - CATM 설정 파일 검증

catm_config.yaml의 필수 키 존재 및 타입을 검증합니다.
"""

from catm.utils.logger import get_logger

logger = get_logger("config_validator")

# 필수 설정 스키마: {키 경로: 예상 타입}
REQUIRED_SCHEMA: dict[str, type] = {
    "paths.source_root": str,
    "paths.output_root": str,
    "source_dirs.cobol": str,
    "source_dirs.copybook": str,
    "file_extensions.cobol": list,
    "file_extensions.copybook": list,
    "claude.cli_command": str,
    "priority_weights.business_importance": (int, float),
    "priority_weights.technical_complexity": (int, float),
}


def _get_nested(config: dict, path: str):
    """점(.) 구분 경로로 중첩 딕셔너리 값 접근"""
    keys = path.split(".")
    current = config
    for key in keys:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def validate_config(config: dict) -> list[str]:
    """설정 딕셔너리 검증

    Args:
        config: 로드된 YAML 설정 딕셔너리

    Returns:
        오류 메시지 리스트 (빈 리스트면 검증 통과)
    """
    errors: list[str] = []

    for path, expected_type in REQUIRED_SCHEMA.items():
        value = _get_nested(config, path)
        if value is None:
            errors.append(f"필수 설정 누락: {path}")
        elif not isinstance(value, expected_type):
            errors.append(
                f"타입 불일치: {path} (기대: {expected_type.__name__ if isinstance(expected_type, type) else expected_type}, 실제: {type(value).__name__})"
            )

    if errors:
        for err in errors:
            logger.warning(err)

    return errors
