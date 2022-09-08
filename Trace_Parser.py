#!/usr/bin/env python3
import logging
import pathlib
import csv
import re
from argparse import ArgumentParser
​
​
log = logging.getLogger('Nextflow Metric Parser')
ch = logging.StreamHandler()
formatter = logging.Formatter(
    "%(asctime)s | %(name)s | %(levelname)7s | %(message)s",
    "%Y-%m-%d %H:%M:%S"
)
ch.setFormatter(formatter)
log.addHandler(ch)
​
​
TRACE_PATTERN = '**/pipeline_trace.tsv'
TRACE_MEM_COLS = [
    'memory', 'vmem', 'rss', 'peak_rss', 'peak_vmem',
    'rchar', 'wchar', 'read_bytes', 'write_bytes'
]
​
parser = ArgumentParser('Pipeline Stats')
parser.add_argument(
    'parent_dir',
    help='Path of the folder which contains the correctly strutcured results folders'
)
​
args = parser.parse_args()
​
parent_dir = pathlib.Path(args.parent_dir)
​
traces = parent_dir.glob(TRACE_PATTERN)
​
​
def convert_memstring_to_GB(memstring):
    memstring = '0 GB' if memstring in ['0', '-'] else memstring
    divisors = {
        'TB': 0.001,
        'GB': 1,
        'MB': 1_000,
        'KB': 1_000_000,
        'B': 1_000_000_000,
    }
    val, unit = memstring.split(' ')
    val = float(val)
    val = val / divisors[unit]
    return val
​
​
def percstring_to_float(percstring):
    percstring = '0' if percstring == '-' else percstring
    return float(percstring.replace('%', ''))
​
​
def timestring_to_seconds(timestring):
    pat = re.compile(
        r'(?:(?P<days>[0-9\.]+)d(?:\s|$))?'
        r'(?:(?P<hours>[0-9\.]+)h(?:\s|$))?(?:(?P<mins>[0-9\.]+)m(?:\s|$))?'
        r'(?:(?P<secs>[0-9\.]+)s(?:\s|$))?(?:(?P<milli>[0-9\.]+)ms(?:\s|$))?'
    )
    m = pat.match(timestring)
    days = int(m['days'] or 0) * 86400
    hrs = int(m['hours'] or 0) * 3600
    mins = int(m['mins'] or 0) * 60
    secs = float(m['secs'] or 0)
    milli = float(m['milli'] or 0) / 1000
    return days + hrs + mins + secs + milli
​
​
per_process_metrics = []
for tfile in traces:
    # Use known strutcure part of path to determine some values
    fileparts = tfile.relative_to(parent_dir).parts
    participant_id = fileparts[0]
    if '/qc_' in str(tfile):
        runtype = 'qc'
    elif '/analysis_' in str(tfile):
        runtype = 'cancer_analysis'
    else:
        runtype = 'unknown'
​
    with open(tfile, 'r') as fh:
        reader = csv.DictReader(fh, delimiter='\t')
        for r in reader:
            r['runtype'] = runtype
            r['participant_id'] = participant_id
            r['trace_path'] = tfile
            r['cpu_perc'] = percstring_to_float(r['%cpu'])
            r['mem_perc'] = percstring_to_float(r['%mem'])
            r['duration'] = timestring_to_seconds(r['duration'])
            r['realtime'] = timestring_to_seconds(r['realtime'])
            for col in TRACE_MEM_COLS:
                r[col] = convert_memstring_to_GB(r[col])
            per_process_metrics.append(r)
​
with open('collated_metrics.csv', 'w') as fh:
    headers = list(per_process_metrics[0].keys())
    writer = csv.DictWriter(fh, fieldnames=headers)
    writer.writeheader()
    writer.writerows(per_process_metrics)
