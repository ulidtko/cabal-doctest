#!/usr/bin/env python

"""
Translation of stackage-lts-map.hs into Python.

(Because Cabal makes it too hard to run.)

Fetches Stackage LTS -> GHC version map, prints as YAML.
"""

import http.client
import json

endpoint1 = "/download/lts-snapshots.json"
endpoint2 = lambda tag: "/" + tag + "/ghc-major-version"

#-- reuse one connection for all requests
http = http.client.HTTPSConnection('www.stackage.org')

http.request('GET', endpoint1)
resp = json.load(http.getresponse())
ltslist = dict(resp).values()
for lts_tag in ltslist:
    http.request('GET', endpoint2(lts_tag))
    ghc_ver = http.getresponse().read().decode('ascii')
    print(f"{ghc_ver}: {lts_tag}")
