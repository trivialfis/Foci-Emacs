'''VTerm in right click menu of nautilus.  Put this file under:

  ~/.local/share/nautilus-python/extensions/vterm.py

and install `python3-nautilus` on Ubuntu.  Restart nautilus service if necessary.

'''

import os
import subprocess
from urllib.parse import urlparse, unquote

from gi.repository import Nautilus, GObject

class ColumnExtension(GObject.GObject, Nautilus.MenuProvider):
    def __init__(self):
        pass

    def menu_activate_cb(self, menu, f):
        parsed = urlparse(f.get_uri())
        path = unquote(parsed.path)
        cmd = f'emacs --chdir="{path}" --eval="(trivialfis/vterm)"'
        if f.is_directory():
            subprocess.Popen(cmd, shell=True)

    def get_background_items(self, window, file):
        item = Nautilus.MenuItem(name='MenuProvider::VTerm',
                                 label='Open in VTerm',
                                 tip='Open VTerm here.',
                                 icon='')
        item.connect('activate', self.menu_activate_cb, file)
        return item,
