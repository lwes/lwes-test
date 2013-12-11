#!/usr/bin/env python
import socket
import subprocess
import random
import time
import sys
import os
import logging

BASE_PATH = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
TESTS = {'java':   [['java', '-jar',  BASE_PATH + '/java/target/lwes-test.jar'], []] ,
         'erlang': [['erl', '-pa', os.path.join(BASE_PATH, 'erlang/ebin'),
                            '-pa', os.path.join(BASE_PATH, 'erlang/deps/*/ebin'),
                            '-run', 'lwes_test', 'start'], ['-noshell']]}
LOGS = os.path.join(BASE_PATH, 'log') 
TEST_DURATION = 60
JSON_FILE = os.path.join(BASE_PATH ,  'jsons/testevent.json')
RUN_ID = time.strftime("%Y%m%d%H%M%S")

log = None

def init_log():
    global log
    logging.basicConfig(format='%(asctime)s %(message)s', level=logging.DEBUG)
    log = logging.getLogger()

def main():
    init_log()
    language_ports = assign_language_ports(TESTS)
    language_tests = launch_tests(TESTS, language_ports)
    sys.exit(parse_results(language_tests))

def parse_results(language_tests):
    all_pass = True
    for language in language_tests:
        test = language_tests[language]
        retcode = test.wait()
        if retcode != 0:
            log.info('The test for ' + language + ' failed ')
            all_pass = False
        else:
            log.info('The test for ' + language + ' passed ')

    if all_pass:
	return 0
    else:
        return 1    

def launch_tests(tests, language_ports):
    language_tests = {}
    for language in tests:
        log_file = os.path.join(LOGS, RUN_ID + '_' + language + '.log')
        pre_args = tests[language][0]
        post_args = tests[language][1]
        subp_args = pre_args + [ \
                      str(TEST_DURATION), str(language_ports[language]), \
                      emit_on(language, language_ports), JSON_FILE]  \
                    + post_args
        log.info('launching the ' + str(language) + ' test with ' + str(subp_args))
        language_tests[language] = \
            subprocess.Popen(subp_args, stdout=open(log_file, 'w+'), stderr=open(log_file, 'w+'))
    return language_tests

def emit_on(language, language_ports):
    emit_to_ports = []
    for l in language_ports:
        if l != language:
            emit_to_ports.append(l + ':' + str(language_ports[l]))
    return ",".join(emit_to_ports)

def assign_language_ports(tests):
    language_ports = {}
    for language in tests:
        language_ports[language] = get_unused_port()
    return language_ports

def get_unused_port():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(('', 0))
    _, port = sock.getsockname()
    sock.close()
    return port
        
if __name__=="__main__":
    main()
