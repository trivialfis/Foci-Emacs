"""Agent shell in right click menu of nautilus.  Put this file under:

  ~/.local/share/nautilus-python/extensions/agent-shell-nautilus.py

and install `python3-nautilus` on Ubuntu.  Restart nautilus service if necessary.

"""

import subprocess
from typing import List
from urllib.parse import unquote, urlparse

from gi.repository import GObject, Nautilus


class AgentShellExtension(GObject.GObject, Nautilus.MenuProvider):
    def menu_activate_agent_shell(
        self, menu: Nautilus.MenuItem, f: Nautilus.FileInfo
    ) -> None:
        parsed = urlparse(f.get_uri())
        path = unquote(parsed.path)
        cmd = (
            f'emacs --chdir="{path}"'
            f' --eval "(agent-shell)"'
        )
        if f.is_directory():
            subprocess.Popen(cmd, shell=True)

    def get_background_items(
        self, current_folder: Nautilus.FileInfo
    ) -> List[Nautilus.MenuItem]:
        item = Nautilus.MenuItem(
            name="MenuProvider::AgentShell",
            label="Open in Agent Shell",
            tip="Start agent shell here.",
            icon="",
        )
        item.connect("activate", self.menu_activate_agent_shell, current_folder)
        return [
            item,
        ]
