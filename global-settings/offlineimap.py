from subprocess import check_output


def mailpasswd():
    return check_output("gpg2 -dq ~/.mail.gpg", shell=True).strip("\n")
