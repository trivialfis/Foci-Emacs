"""Eat in right click menu of nautilus.  Put this file under:

  ~/.local/share/nautilus-python/extensions/eat.py

and install `python3-nautilus` on Ubuntu.  Restart nautilus service if necessary.

"""

import os
import subprocess
from typing import List
from urllib.parse import unquote, urlparse

from gi.repository import GObject, Nautilus

version = Nautilus._version.split(".")

if int(version[0]) >= 4:

    class EatExtension(GObject.GObject, Nautilus.MenuProvider):
        def menu_activate_cb(
            self, menu: Nautilus.MenuItem, f: Nautilus.FileInfo
        ) -> None:
            parsed = urlparse(f.get_uri())
            path = unquote(parsed.path)
            cmd = f'emacs --chdir="{path}" --eval="(trivialfis/eat)"'
            if f.is_directory():
                subprocess.Popen(cmd, shell=True)

        def get_background_items(
            self, current_folder: Nautilus.FileInfo
        ) -> List[Nautilus.MenuItem]:
            item = Nautilus.MenuItem(
                name="MenuProvider::Eat",
                label="Open in Eat",
                tip="Open Eat here.",
                icon="",
            )
            item.connect("activate", self.menu_activate_cb, current_folder)
            return [
                item,
            ]

else:

    class EatExtension(GObject.GObject, Nautilus.MenuProvider):
        def menu_activate_cb(
            self, menu: Nautilus.MenuItem, f: Nautilus.FileInfo
        ) -> None:
            parsed = urlparse(f.get_uri())
            path = unquote(parsed.path)
            cmd = f'emacs --chdir="{path}" --eval="(trivialfis/eat)"'
            if f.is_directory():
                subprocess.Popen(cmd, shell=True)

        def get_background_items(
            self, window: Nautilus.FileInfo, f
        ) -> List[Nautilus.MenuItem]:
            item = Nautilus.MenuItem(
                name="MenuProvider::Eat",
                label="Open in Eat",
                tip="Open Eat here.",
                icon="",
            )
            item.connect("activate", self.menu_activate_cb, f)
            return [
                item,
            ]
