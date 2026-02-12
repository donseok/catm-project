"""claude_client 유닛 테스트"""

import sys
from pathlib import Path
from unittest.mock import patch, MagicMock

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from catm.utils.claude_client import is_error_response, call_claude


class TestIsErrorResponse:
    def test_catm_error(self) -> None:
        assert is_error_response("[CATM 에러] 타임아웃") is True

    def test_generic_error(self) -> None:
        assert is_error_response("[에러] 알 수 없는 문제") is True

    def test_normal_response(self) -> None:
        assert is_error_response("정상적인 분석 결과입니다.") is False

    def test_empty_response(self) -> None:
        assert is_error_response("") is False

    def test_partial_match(self) -> None:
        """중간에 [CATM 에러]가 있으면 에러 아님 (startswith 체크)"""
        assert is_error_response("분석 결과: [CATM 에러] 없음") is False


class TestCallClaude:
    @patch("catm.utils.claude_client.subprocess.run")
    def test_success(self, mock_run: MagicMock) -> None:
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="분석 결과\n",
            stderr="",
        )
        result = call_claude("테스트 프롬프트", max_retries=1)
        assert result == "분석 결과"

    @patch("catm.utils.claude_client.subprocess.run")
    def test_timeout(self, mock_run: MagicMock) -> None:
        import subprocess
        mock_run.side_effect = subprocess.TimeoutExpired(cmd="claude", timeout=10)
        result = call_claude("테스트", timeout=10, max_retries=1, retry_delay=0)
        assert "[CATM 에러]" in result
        assert "타임아웃" in result

    @patch("catm.utils.claude_client.subprocess.run")
    def test_file_not_found(self, mock_run: MagicMock) -> None:
        mock_run.side_effect = FileNotFoundError()
        result = call_claude("테스트", max_retries=1)
        assert "[CATM 에러]" in result
        assert "찾을 수 없습니다" in result

    @patch("catm.utils.claude_client.subprocess.run")
    def test_rate_limit_retry(self, mock_run: MagicMock) -> None:
        """Rate limit 시 재시도 후 성공"""
        mock_run.side_effect = [
            MagicMock(returncode=1, stdout="", stderr="rate limit exceeded"),
            MagicMock(returncode=0, stdout="성공!", stderr=""),
        ]
        result = call_claude("테스트", max_retries=2, retry_delay=0)
        assert result == "성공!"
        assert mock_run.call_count == 2

    @patch("catm.utils.claude_client.subprocess.run")
    def test_generic_error_max_retries(self, mock_run: MagicMock) -> None:
        """일반 에러 max_retries 소진"""
        mock_run.return_value = MagicMock(
            returncode=1,
            stdout="",
            stderr="unknown error",
        )
        result = call_claude("테스트", max_retries=2, retry_delay=0)
        assert "[CATM 에러]" in result

    @patch("catm.utils.claude_client.subprocess.run")
    def test_exception_handling(self, mock_run: MagicMock) -> None:
        mock_run.side_effect = RuntimeError("연결 실패")
        result = call_claude("테스트", max_retries=1, retry_delay=0)
        assert "[CATM 에러]" in result
        assert "예외" in result

    @patch("catm.utils.claude_client.subprocess.run")
    def test_cli_command_parameter(self, mock_run: MagicMock) -> None:
        """cli_command 파라미터가 전달되는지 확인"""
        mock_run.return_value = MagicMock(
            returncode=0, stdout="ok", stderr=""
        )
        call_claude("테스트", cli_command="/usr/local/bin/claude", max_retries=1)
        args = mock_run.call_args[0][0]
        assert args[0] == "/usr/local/bin/claude"

    @patch("catm.utils.claude_client.subprocess.run")
    def test_stdin_prompt(self, mock_run: MagicMock) -> None:
        """프롬프트가 stdin으로 전달되는지 확인"""
        mock_run.return_value = MagicMock(
            returncode=0, stdout="ok", stderr=""
        )
        call_claude("내 프롬프트", max_retries=1)
        assert mock_run.call_args[1]["input"] == "내 프롬프트"
