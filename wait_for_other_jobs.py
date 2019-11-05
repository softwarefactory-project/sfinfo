#!/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2016 Red Hat, Inc
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

import argparse
import os
import sys
import time
import requests


def log(msg):
    sys.stdout.write("%s\n" % str(msg))
    sys.stdout.flush()


def look_for_my_change(gate, cid):
    first = True
    for queue in gate['change_queues']:
        if queue['heads']:
            for head in queue['heads']:
                for change in head:
                    if change['id'].split(',')[0] == cid:
                        log("Found change in shared queue: " +
                            "%s" % queue['name'])
                        change['first'] = first
                        return change
                    first = False


def check_jobs_status(my_change):
    status = {}
    for job in my_change['jobs']:
        if job['name'] == myname:
            continue
        status[job['name']] = None
        if job['end_time']:
            log("Job: %s terminated with status: %s" % (
                job['name'], job['result']))
        else:
            log("Job: %s still running" % job['name'])
            status[job['name']] = 2
            continue
        if job['result'] == 'SUCCESS':
            status[job['name']] = 0
        else:
            status[job['name']] = 1
    return status


def fetch_get_pipeline_status(url, pipeline_name):
    log("Fetching Zuul status")
    r = requests.get(url).json()
    return [pipeline for pipeline in r['pipelines'] if
            pipeline['name'] == pipeline_name][0]


def check_non_voting(status, my_change):
    for k, v in status.items():
        if v == 1:
            job = [j for j in my_change['jobs'] if j['name'] == k][0]
            if job['voting']:
                log("Job: %s is voting !" % k)
                return False
    return True


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("--url", default="https://softwarefactory-project.io/"
                   "zuul/status.json")
    p.add_argument("--round-delay", default=20, type=int)
    p.add_argument("--independent", action='store_true')
    args = p.parse_args()
    myname = os.environ.get('JOB_NAME', os.environ["WORKSPACE"].split('/')[-1])
    change = os.environ.get('ZUUL_CHANGE', os.environ.get('ZUUL_COMMIT'))
    pipeline_name = os.environ['ZUUL_PIPELINE']
    while True:
        log("Looking for my change in %s" % pipeline_name)
        gate = fetch_get_pipeline_status(args.url, pipeline_name)
        my_change = look_for_my_change(gate, change)
        if not my_change:
            log("Error. Change does not exists !")
            sys.exit(1)
        if (args.independent and my_change['first']) or \
           (not args.independent and my_change['item_ahead'] is None):
            log("Check current jobs running along with me")
            status = check_jobs_status(my_change)
            if len([v for v in status.values() if v == 0]) == \
               len(my_change['jobs']) - 1:
                log("All jobs succeed for this change")
                break
            elif len([v for v in status.values() if v == 2]):
                log("At least one job is in progress. Waiting ...")
                time.sleep(args.round_delay)
                continue
            else:
                if check_non_voting(status, my_change):
                    log("All jobs in failure are non voting")
                    break
                else:
                    log("Jobs finished but at least one voting job failed")
                    sys.exit(1)
        else:
            log("Change is not ahead of the shared queue. waiting ...")
            time.sleep(args.round_delay)
