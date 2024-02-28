#!/usr/bin/env python

from subprocess import check_output
from re import sub

def get_pass(account):
    data = check_output("pass " + account, shell=True).splitlines()
    password = data[1]
    user = data[2]
    remotehost = data[3]

    return {"password": password, "user": user, "remotehost": remotehost}
