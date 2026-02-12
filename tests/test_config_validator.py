"""config_validator 유닛 테스트"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.config_validator import validate_config


class TestValidateConfig:
    def test_valid_config(self) -> None:
        """완전한 설정은 에러 없음"""
        config = {
            "paths": {"source_root": "./src", "output_root": "./output"},
            "source_dirs": {"cobol": "cobol", "copybook": "copybook"},
            "file_extensions": {"cobol": [".cbl"], "copybook": [".cpy"]},
            "claude": {"cli_command": "claude"},
            "priority_weights": {
                "business_importance": 0.35,
                "technical_complexity": 0.25,
            },
        }
        errors = validate_config(config)
        assert errors == []

    def test_missing_key(self) -> None:
        """필수 키 누락 감지"""
        config = {"paths": {"source_root": "./src"}}
        errors = validate_config(config)
        assert any("paths.output_root" in e for e in errors)

    def test_type_mismatch(self) -> None:
        """타입 불일치 감지"""
        config = {
            "paths": {"source_root": 123, "output_root": "./output"},
            "source_dirs": {"cobol": "cobol", "copybook": "copybook"},
            "file_extensions": {"cobol": [".cbl"], "copybook": [".cpy"]},
            "claude": {"cli_command": "claude"},
            "priority_weights": {
                "business_importance": 0.35,
                "technical_complexity": 0.25,
            },
        }
        errors = validate_config(config)
        assert any("paths.source_root" in e for e in errors)

    def test_empty_config(self) -> None:
        """빈 설정은 모든 필수 키 누락"""
        errors = validate_config({})
        assert len(errors) > 0
