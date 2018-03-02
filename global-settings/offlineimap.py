from subprocess import check_output


def mailpasswd():
    return check_output("gpg2 -dq ~/.imap.gpg", shell=True).strip("\n")
