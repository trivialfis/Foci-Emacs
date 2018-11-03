'''OfflineImap gpg reading.'''
from subprocess import check_output, CalledProcessError


def mailpasswd(address):
    '''Entry point.'''
    def get_account_pass(accounts):
        '''Return the password from correct account.'''
        for account in accounts:
            if account.find(address) != -1:
                current_account = account
                break
        current_account = current_account.split(' ')
        password = current_account[-1]
        return password

    try:
        all_accounts = check_output(
            "gpg2 -dq ~/.authinfo.gpg", shell=True).strip("\n").split('\n')[-1]
        password = get_account_pass(all_accounts)
        return password
    except CalledProcessError:
        all_accounts = check_output(
            "gpg -dq ~/.authinfo.gpg", shell=True).strip("\n").split('\n')
        password = get_account_pass(all_accounts)
        return password