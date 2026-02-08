"""
file_utils.py - 파일 입출력 및 경로 관리 유틸리티
"""

import os
import json
import yaml
from pathlib import Path
from typing import Optional


def load_config(config_path: str = "") -> dict:
    """CATM 설정 파일 로드 (기본: 프로젝트 루트 기준 catm/config/catm_config.yaml)"""
    if not config_path:
        # __file__ 기준으로 프로젝트 루트를 추정: file_utils.py → utils/ → catm/ → project root
        project_root = Path(__file__).resolve().parent.parent.parent
        config_path = str(project_root / "catm" / "config" / "catm_config.yaml")
    with open(config_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def ensure_dir(dir_path: str) -> Path:
    """디렉토리가 없으면 생성"""
    path = Path(dir_path)
    path.mkdir(parents=True, exist_ok=True)
    return path


def save_json(data: dict, file_path: str) -> None:
    """JSON 파일 저장"""
    ensure_dir(os.path.dirname(file_path))
    with open(file_path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    print(f"  💾 저장: {file_path}")


def load_json(file_path: str) -> dict:
    """JSON 파일 로드"""
    with open(file_path, "r", encoding="utf-8") as f:
        return json.load(f)


def save_markdown(content: str, file_path: str) -> None:
    """마크다운 파일 저장"""
    ensure_dir(os.path.dirname(file_path))
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(content)
    print(f"  📝 저장: {file_path}")


def find_files(directory: str, extensions: list[str]) -> list[Path]:
    """특정 확장자의 파일 목록 반환"""
    path = Path(directory)
    if not path.exists():
        return []
    
    files = []
    for ext in extensions:
        if ext == "":
            # 확장자 없는 파일 (메인프레임 추출물), 숨김파일(.gitkeep 등) 제외
            for f in path.iterdir():
                if f.is_file() and f.suffix == "" and not f.name.startswith("."):
                    files.append(f)
        else:
            files.extend(f for f in path.glob(f"*{ext}") if not f.name.startswith("."))

    return sorted(set(files))


def get_relative_path(file_path: str, base_dir: str = ".") -> str:
    """상대 경로 반환"""
    return str(Path(file_path).relative_to(Path(base_dir).resolve()))
