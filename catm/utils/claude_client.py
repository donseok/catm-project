"""
claude_client.py - Claude Code CLI 호출 공통 모듈

Claude Code Max 20x 구독을 통한 CLI 호출을 일관되게 처리합니다.
재시도, rate-limit 대응, 타임아웃 로직을 포함합니다.
"""

import subprocess
import time
from typing import Optional

from catm.utils.logger import get_logger

logger = get_logger("claude_client")


def is_error_response(response: str) -> bool:
    """Claude 응답이 에러인지 확인"""
    return response.startswith("[CATM 에러]") or response.startswith("[에러]")


def call_claude(
    prompt: str,
    cli_command: str = "claude",
    timeout: int = 300,
    max_retries: int = 3,
    retry_delay: int = 30,
) -> str:
    """
    Claude Code CLI 호출 (Max 20x 구독 사용)

    stdin으로 프롬프트를 전달하고, `-p -`로 stdin에서 읽도록 지시합니다.

    Args:
        prompt: Claude에게 보낼 프롬프트 전체 텍스트
        cli_command: Claude CLI 실행 명령어
        timeout: 최대 대기 시간 (초)
        max_retries: 최대 재시도 횟수
        retry_delay: 재시도 간 대기 시간 (초)

    Returns:
        Claude의 응답 텍스트
    """
    for attempt in range(1, max_retries + 1):
        try:
            result = subprocess.run(
                [cli_command, "-p", "-", "--output-format", "text"],
                input=prompt,
                capture_output=True,
                text=True,
                timeout=timeout,
                encoding="utf-8",
            )

            if result.returncode == 0:
                return result.stdout.strip()

            stderr = result.stderr.strip()

            # Rate limit 감지
            if "rate" in stderr.lower() or "limit" in stderr.lower():
                wait = retry_delay * attempt
                logger.warning("Rate limit 감지. %d초 대기 후 재시도 (%d/%d)", wait, attempt, max_retries)
                time.sleep(wait)
                continue

            # 기타 에러
            logger.warning("Claude Code 에러 (시도 %d/%d): %s", attempt, max_retries, stderr[:200])
            if attempt < max_retries:
                time.sleep(retry_delay)
                continue

            return f"[CATM 에러] Claude Code 호출 실패: {stderr[:500]}"

        except subprocess.TimeoutExpired:
            logger.warning("타임아웃 (%d초). 재시도 %d/%d", timeout, attempt, max_retries)
            if attempt < max_retries:
                time.sleep(retry_delay)
                continue
            return "[CATM 에러] Claude Code 타임아웃"

        except FileNotFoundError:
            return (
                "[CATM 에러] claude 명령어를 찾을 수 없습니다.\n"
                "Claude Code를 설치해주세요: npm install -g @anthropic-ai/claude-code\n"
                "그리고 로그인: claude login"
            )

        except Exception as e:
            logger.error("예외 발생: %s", e)
            if attempt < max_retries:
                time.sleep(retry_delay)
                continue
            return f"[CATM 에러] 예외: {str(e)}"

    return "[CATM 에러] 최대 재시도 횟수 초과"
