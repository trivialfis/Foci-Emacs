'''OfflineImap gpg reading.'''
from subprocess import check_output
import subprocess


def mailpasswd():
    '''Entry point.'''
    try:
        return check_output(
            "gpg -dq ~/.authinfo.gpg", shell=True).strip("\n").split(' ')[-1]
    except subprocess.CalledProcessError:
        return check_output(
            "gpg2 -dq ~/.authinfo.gpg", shell=True).strip("\n").split(' ')[-1]
