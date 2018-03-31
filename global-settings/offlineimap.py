from subprocess import check_output


def mailpasswd():
    return check_output("gpg2 -dq ~/.authinfo.gpg", shell=True).strip("\n").split(' ')[-1]
